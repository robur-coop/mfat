type entry = { name: string; is_dir: bool; size: int32 }

(* BPB (BIOS Parameter Block) parsed from sector 0 *)
type bpb = {
    bytes_per_sector: int
  ; sectors_per_cluster: int
  ; reserved_sectors: int
  ; num_fats: int
  ; total_sectors_32: int32
  ; fat_size_32: int32
  ; root_cluster: int32
}

type 'blk t = { blk: 'blk; cache: 'blk Cachet.t; bpb: bpb }

let error_msgf fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt
let ( let* ) = Result.bind

(* BPB parsing *)

module Bpb = struct
  let parse cache =
    let bytes_per_sector = Cachet.get_uint16_le cache 11 in
    let sectors_per_cluster = Cachet.get_uint8 cache 13 in
    let reserved_sectors = Cachet.get_uint16_le cache 14 in
    let num_fats = Cachet.get_uint8 cache 16 in
    let total_sectors_32 = Cachet.get_int32_le cache 32 in
    let fat_size_32 = Cachet.get_int32_le cache 36 in
    let root_cluster = Cachet.get_int32_le cache 44 in
    if
      bytes_per_sector <> 512
      && bytes_per_sector <> 1024
      && bytes_per_sector <> 2048
      && bytes_per_sector <> 4096
    then error_msgf "invalid bytes_per_sector: %d" bytes_per_sector
    else if sectors_per_cluster = 0 then error_msgf "sectors_per_cluster is 0"
    else if num_fats = 0 then error_msgf "num_fats is 0"
    else
      Ok
        {
          bytes_per_sector
        ; sectors_per_cluster
        ; reserved_sectors
        ; num_fats
        ; total_sectors_32
        ; fat_size_32
        ; root_cluster
        }

  let cluster_size bpb = bpb.bytes_per_sector * bpb.sectors_per_cluster
  let fat_offset bpb = bpb.reserved_sectors * bpb.bytes_per_sector

  let data_offset bpb =
    (bpb.reserved_sectors + (bpb.num_fats * Int32.to_int bpb.fat_size_32))
    * bpb.bytes_per_sector

  let cluster_offset bpb cluster =
    let cluster_num = Int32.to_int cluster - 2 in
    data_offset bpb + (cluster_num * cluster_size bpb)
end

(* FAT table operations *)

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module Fat (Blk : BLOCK) = struct
  let eoc = 0x0FFFFFFFl
  let eoc_min = 0x0FFFFFF8l
  let free = 0x00000000l
  let mask = 0x0FFFFFFFl

  let read_entry cache bpb cluster =
    let off = Bpb.fat_offset bpb + (Int32.to_int cluster * 4) in
    Int32.logand (Cachet.get_int32_le cache off) mask

  let write_entry blk cache bpb cluster value =
    let off = Bpb.fat_offset bpb + (Int32.to_int cluster * 4) in
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    let rel = off - page_off in
    (* Write the 32-bit LE value, preserving high 4 bits *)
    let old_val = Bstr.get_int32_le buf rel in
    let high_bits = Int32.logand old_val 0xF0000000l in
    let new_val = Int32.logor high_bits (Int32.logand value mask) in
    Bstr.set_int32_le buf rel new_val;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  let follow_chain cache bpb start =
    let rec go acc cluster =
      if Int32.unsigned_compare cluster eoc_min >= 0 then List.rev acc
      else if cluster = free then List.rev acc
      else
        let next = read_entry cache bpb cluster in
        go (cluster :: acc) next
    in
    go [] start

  let alloc_cluster blk cache bpb =
    let total_data_clusters =
      (Int32.to_int bpb.total_sectors_32
      - bpb.reserved_sectors
      - (bpb.num_fats * Int32.to_int bpb.fat_size_32))
      / bpb.sectors_per_cluster
    in
    let max_cluster = total_data_clusters + 1 in
    let rec search i =
      if i > max_cluster then error_msgf "no free cluster"
      else
        let v = read_entry cache bpb (Int32.of_int i) in
        if v = free then (
          write_entry blk cache bpb (Int32.of_int i) eoc;
          Ok (Int32.of_int i))
        else search (i + 1)
    in
    search 2

  let free_chain blk cache bpb start =
    let rec go cluster =
      if Int32.unsigned_compare cluster eoc_min >= 0 then ()
      else if cluster = free then ()
      else
        let next = read_entry cache bpb cluster in
        write_entry blk cache bpb cluster free;
        go next
    in
    go start
end

module Dir (Blk : BLOCK) = struct
  let entry_size = 32
  let attr_directory = 0x10
  let attr_long_name = 0x0F
  let deleted_marker = 0xE5

  module Fat = Fat (Blk)

  type raw_entry = {
      name: string
    ; ext: string
    ; attr: int
    ; first_cluster: int32
    ; file_size: int32
  }

  let parse_entry cache off =
    let first_byte = Cachet.get_uint8 cache off in
    if first_byte = 0x00 then `End
    else if first_byte = deleted_marker then `Deleted
    else
      let attr = Cachet.get_uint8 cache (off + 11) in
      if attr = attr_long_name then `LongName
      else
        let name = Cachet.get_string cache ~len:8 off in
        let ext = Cachet.get_string cache ~len:3 (off + 8) in
        let cluster_high = Cachet.get_uint16_le cache (off + 20) in
        let cluster_low = Cachet.get_uint16_le cache (off + 26) in
        let first_cluster =
          Int32.logor
            (Int32.shift_left (Int32.of_int cluster_high) 16)
            (Int32.of_int cluster_low)
        in
        let file_size = Cachet.get_int32_le cache (off + 28) in
        `Entry { name; ext; attr; first_cluster; file_size }

  let format_name raw =
    let name = String.trim raw.name in
    let ext = String.trim raw.ext in
    if ext = "" then name else name ^ "." ^ ext

  let to_entry raw =
    let name = String.uppercase_ascii (format_name raw) in
    let is_dir = raw.attr land attr_directory <> 0 in
    { name; is_dir; size= raw.file_size }

  let read_dir cache bpb cluster =
    let clusters = Fat.follow_chain cache bpb cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let entries = ref [] in
    let stop = ref false in
    let fn cl =
      if not !stop then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !stop then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> stop := true
            | `Deleted | `LongName -> ()
            | `Entry raw ->
                if raw.name <> ".       " && raw.name <> "..      " then
                  entries := raw :: !entries
        done
    in
    List.iter fn clusters; List.rev !entries

  let find_in_dir cache bpb cluster name =
    let target = String.uppercase_ascii name in
    let entries = read_dir cache bpb cluster in
    let matches raw =
      let formatted = String.uppercase_ascii (format_name raw) in
      formatted = target
    in
    match List.find_opt matches entries with
    | Some e -> Ok e
    | None -> error_msgf "%s: not found" name

  (* Convert a filename to 8.3 format (padded with spaces) *)
  let to_8_3 name =
    let name = String.uppercase_ascii name in
    let base, ext =
      match String.rindex_opt name '.' with
      | None -> (name, "")
      | Some i ->
          ( String.sub name 0 i
          , String.sub name (i + 1) (String.length name - i - 1) )
    in
    let base =
      let b = Bytes.make 8 ' ' in
      let len = min 8 (String.length base) in
      Bytes.blit_string base 0 b 0 len;
      Bytes.to_string b
    in
    let ext =
      let e = Bytes.make 3 ' ' in
      let len = min 3 (String.length ext) in
      Bytes.blit_string ext 0 e 0 len;
      Bytes.to_string e
    in
    (base, ext)

  (* Write a 32-byte directory entry at the given offset *)
  let write_entry_at blk cache off ~name_8 ~ext_3 ~attr ~first_cluster
      ~file_size =
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    let rel = off - page_off in
    Bstr.blit_from_string name_8 ~src_off:0 buf ~dst_off:rel ~len:8;
    Bstr.blit_from_string ext_3 ~src_off:0 buf ~dst_off:(rel + 8) ~len:3;
    Bstr.set_uint8 buf (rel + 11) attr;
    (* Bytes 12-19: reserved/time fields, zero them *)
    Bstr.memset buf ~off:(rel + 12) ~len:8 '\000';
    Bstr.set_uint16_le buf (rel + 20)
      (Int32.to_int (Int32.shift_right_logical first_cluster 16));
    (* Bytes 22-25: time fields, zero them *)
    Bstr.memset buf ~off:(rel + 22) ~len:4 '\000';
    Bstr.set_uint16_le buf (rel + 26)
      (Int32.to_int (Int32.logand first_cluster 0xFFFFl));
    Bstr.set_int32_le buf (rel + 28) file_size;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  (* Find a free slot in a directory or extend it, returns offset *)
  let find_free_slot blk cache bpb dir_cluster =
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let result = ref None in
    let fn cl =
      if !result = None then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if !result = None then
            let off = base + (i * entry_size) in
            let first_byte = Cachet.get_uint8 cache off in
            if first_byte = 0x00 || first_byte = deleted_marker then
              result := Some off
        done
    in
    List.iter fn clusters;
    match !result with
    | Some off -> Ok off
    | None -> begin
        (* Need to extend the directory: allocate a new cluster *)
        let last_cluster = List.nth clusters (List.length clusters - 1) in
        match Fat.alloc_cluster blk cache bpb with
        | Error _ as e -> e
        | Ok new_cl ->
            Fat.write_entry blk cache bpb last_cluster new_cl;
            (* Zero out the new cluster *)
            let pagesize = Blk.pagesize blk in
            let base = Bpb.cluster_offset bpb new_cl in
            let n_pages = cluster_sz / pagesize in
            for p = 0 to n_pages - 1 do
              let off = base + (p * pagesize) in
              let buf = Bstr.create pagesize in
              Bstr.fill buf '\000';
              Blk.write blk ~dst_off:off buf;
              Cachet.invalidate cache ~off ~len:pagesize
            done;
            Ok base
      end

  let add_entry blk cache bpb dir_cluster ~name_8 ~ext_3 ~attr ~first_cluster
      ~file_size =
    match find_free_slot blk cache bpb dir_cluster with
    | Error _ as err -> err
    | Ok off ->
        write_entry_at blk cache off ~name_8 ~ext_3 ~attr ~first_cluster
          ~file_size;
        Ok ()

  let remove_entry blk cache bpb dir_cluster name =
    let target = String.uppercase_ascii name in
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let found = ref false in
    let fn cl =
      if not !found then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !found then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> ()
            | `Deleted | `LongName -> ()
            | `Entry raw ->
                let formatted = String.uppercase_ascii (format_name raw) in
                if formatted = target then (
                  let pagesize = Blk.pagesize blk in
                  let page_off = off / pagesize * pagesize in
                  let buf = Bstr.create pagesize in
                  Blk.read blk ~src_off:page_off buf;
                  Bstr.set_uint8 buf (off - page_off) deleted_marker;
                  Blk.write blk ~dst_off:page_off buf;
                  Cachet.invalidate cache ~off:page_off ~len:pagesize;
                  found := true)
        done
    in
    List.iter fn clusters;
    if !found then Ok () else error_msgf "%s: not found" name

  (* Update the file_size and first_cluster of an existing directory entry *)
  let update_entry blk cache bpb dir_cluster name ~first_cluster ~file_size =
    let target = String.uppercase_ascii name in
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let found = ref false in
    let fn cl =
      if not !found then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !found then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> ()
            | `Deleted | `LongName -> ()
            | `Entry raw ->
                let formatted = String.uppercase_ascii (format_name raw) in
                if formatted = target then (
                  let name_8, ext_3 = to_8_3 name in
                  write_entry_at blk cache off ~name_8 ~ext_3 ~attr:raw.attr
                    ~first_cluster ~file_size;
                  found := true)
        done
    in
    List.iter fn clusters;
    if !found then Ok () else error_msgf "%s: not found" name
end

module Path (Blk : BLOCK) = struct
  module Dir = Dir (Blk)

  let split path =
    let path =
      if String.length path > 0 && path.[0] = '/' then
        String.sub path 1 (String.length path - 1)
      else path
    in
    if path = "" then [] else String.split_on_char '/' path

  let parent_and_name path =
    let parts = split path in
    match List.rev parts with
    | [] -> error_msgf "empty path"
    | name :: rev_parent -> Ok (List.rev rev_parent, name)

  let resolve_dir cache bpb parts =
    let rec go cluster = function
      | [] -> Ok cluster
      | name :: rest ->
          let* raw = Dir.find_in_dir cache bpb cluster name in
          if raw.Dir.attr land Dir.attr_directory <> 0 then
            go raw.Dir.first_cluster rest
          else error_msgf "%s: not a directory" name
    in
    go bpb.root_cluster parts

  let resolve cache bpb path =
    let parts = split path in
    match List.rev parts with
    | [] -> Ok (bpb.root_cluster, None)
    | name :: rev_parent ->
        let* dir_cluster = resolve_dir cache bpb (List.rev rev_parent) in
        let* raw = Dir.find_in_dir cache bpb dir_cluster name in
        Ok (dir_cluster, Some raw)
end

module Make (Blk : BLOCK) = struct
  module Fat = Fat (Blk)
  module Dir = Dir (Blk)
  module Path = Path (Blk)

  let create blk =
    let pagesize = Blk.pagesize blk in
    let map blk ~pos _len =
      let buf = Bstr.create pagesize in
      let src_off = pos / pagesize * pagesize in
      Blk.read blk ~src_off buf; buf
    in
    let cache = Cachet.make ~pagesize ~map blk in
    let* bpb = Bpb.parse cache in
    Ok { blk; cache; bpb }

  let ls t path =
    let parts = Path.split path in
    let* cluster = Path.resolve_dir t.cache t.bpb parts in
    let raws = Dir.read_dir t.cache t.bpb cluster in
    Ok (List.map Dir.to_entry raws)

  let read t path =
    let* _, v = Path.resolve t.cache t.bpb path in
    match v with
    | None -> error_msgf "%s: is root directory" path
    | Some raw ->
        if raw.Dir.attr land Dir.attr_directory <> 0 then
          error_msgf "%s: is a directory" path
        else
          let size = Int32.to_int raw.Dir.file_size in
          if size = 0 then Ok ""
          else
            let clusters =
              Fat.follow_chain t.cache t.bpb raw.Dir.first_cluster
            in
            let cluster_sz = Bpb.cluster_size t.bpb in
            let buf = Buffer.create size in
            let remaining = ref size in
            List.iter
              (fun cl ->
                if !remaining > 0 then (
                  let base = Bpb.cluster_offset t.bpb cl in
                  let to_read = min !remaining cluster_sz in
                  let s = Cachet.get_string t.cache ~len:to_read base in
                  Buffer.add_string buf s;
                  remaining := !remaining - to_read))
              clusters;
            Ok (Buffer.contents buf)

  let write_data_to_clusters blk cache bpb clusters data =
    let cluster_sz = Bpb.cluster_size bpb in
    let pagesize = Blk.pagesize blk in
    let data_len = String.length data in
    let offset = ref 0 in
    let fn cl =
      if !offset < data_len then begin
        let base = Bpb.cluster_offset bpb cl in
        let to_write = min (data_len - !offset) cluster_sz in
        (* Write page by page *)
        let written = ref 0 in
        while !written < to_write do
          let page_base = base + !written in
          let page_aligned = page_base / pagesize * pagesize in
          let buf = Bstr.create pagesize in
          (* Read existing page if partial write *)
          if page_base <> page_aligned || to_write - !written < pagesize then
            Blk.read blk ~src_off:page_aligned buf
          else Bstr.fill buf '\000';
          let in_page_off = page_base - page_aligned in
          let in_page_len =
            min (pagesize - in_page_off) (to_write - !written)
          in
          Bstr.blit_from_string data ~src_off:(!offset + !written) buf
            ~dst_off:in_page_off ~len:in_page_len;
          Blk.write blk ~dst_off:page_aligned buf;
          Cachet.invalidate cache ~off:page_aligned ~len:pagesize;
          written := !written + in_page_len
        done;
        offset := !offset + to_write
      end
    in
    List.iter fn clusters

  let write t path data =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    let data_len = String.length data in
    let cluster_sz = Bpb.cluster_size t.bpb in
    let needed_clusters =
      if data_len = 0 then 0 else (data_len + cluster_sz - 1) / cluster_sz
    in
    (* Check if file already exists *)
    let existing = Dir.find_in_dir t.cache t.bpb dir_cluster name in
    let existing = Result.to_option existing in
    (* Free old clusters if file exists *)
    let fn raw =
      if raw.Dir.first_cluster <> 0l then
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster
    in
    Option.iter fn existing;
    (* Allocate new clusters *)
    let rec alloc_n n acc =
      if n = 0 then Ok (List.rev acc)
      else
        let* cl = Fat.alloc_cluster t.blk t.cache t.bpb in
        alloc_n (n - 1) (cl :: acc)
    in
    let* clusters = alloc_n needed_clusters [] in
    (* Chain the clusters together *)
    let rec chain = function
      | [] | [ _ ] -> ()
      | a :: (b :: _ as rest) ->
          Fat.write_entry t.blk t.cache t.bpb a b;
          chain rest
    in
    chain clusters;
    (* Write data *)
    if data_len > 0 then
      write_data_to_clusters t.blk t.cache t.bpb clusters data;
    let first_cluster = match clusters with [] -> 0l | c :: _ -> c in
    let file_size = Int32.of_int data_len in
    match existing with
    | Some _ ->
        Dir.update_entry t.blk t.cache t.bpb dir_cluster name ~first_cluster
          ~file_size
    | None ->
        let name_8, ext_3 = Dir.to_8_3 name in
        Dir.add_entry t.blk t.cache t.bpb dir_cluster ~name_8 ~ext_3 ~attr:0x20
          ~first_cluster ~file_size

  let mkdir t path =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    (* Check if already exists *)
    match Dir.find_in_dir t.cache t.bpb dir_cluster name with
    | Ok _ -> error_msgf "%s: already exists" name
    | Error _ -> begin
        (* Allocate a cluster for the new directory *)
        match Fat.alloc_cluster t.blk t.cache t.bpb with
        | Error _ as e -> e
        | Ok new_cl ->
            (* Zero out the new cluster *)
            let cluster_sz = Bpb.cluster_size t.bpb in
            let pagesize = Blk.pagesize t.blk in
            let base = Bpb.cluster_offset t.bpb new_cl in
            let n_pages = cluster_sz / pagesize in
            for p = 0 to n_pages - 1 do
              let off = base + (p * pagesize) in
              let buf = Bstr.create pagesize in
              Bstr.fill buf '\000';
              Blk.write t.blk ~dst_off:off buf;
              Cachet.invalidate t.cache ~off ~len:pagesize
            done;
            (* Create . entry *)
            Dir.write_entry_at t.blk t.cache base ~name_8:".       "
              ~ext_3:"   " ~attr:Dir.attr_directory ~first_cluster:new_cl
              ~file_size:0l;
            (* Create .. entry *)
            Dir.write_entry_at t.blk t.cache (base + 32) ~name_8:"..      "
              ~ext_3:"   " ~attr:Dir.attr_directory ~first_cluster:dir_cluster
              ~file_size:0l;
            (* Add entry in parent directory *)
            let name_8, ext_3 = Dir.to_8_3 name in
            Dir.add_entry t.blk t.cache t.bpb dir_cluster ~name_8 ~ext_3
              ~attr:Dir.attr_directory ~first_cluster:new_cl ~file_size:0l
      end

  let remove t path =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    let* raw = Dir.find_in_dir t.cache t.bpb dir_cluster name in
    (* If directory, check it's empty *)
    if raw.Dir.attr land Dir.attr_directory <> 0 then
      let entries = Dir.read_dir t.cache t.bpb raw.Dir.first_cluster in
      if entries <> [] then error_msgf "%s: directory not empty" name
      else (
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster;
        Dir.remove_entry t.blk t.cache t.bpb dir_cluster name)
    else (
      if raw.Dir.first_cluster <> 0l then
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster;
      Dir.remove_entry t.blk t.cache t.bpb dir_cluster name)

  let exists t path =
    match Path.resolve t.cache t.bpb path with Ok _ -> true | Error _ -> false

  let stat t path =
    let parts = Path.split path in
    if parts = [] then Ok { name= "/"; is_dir= true; size= 0l }
    else
      match Path.resolve t.cache t.bpb path with
      | Error _ as e -> e
      | Ok (_, None) -> Ok { name= "/"; is_dir= true; size= 0l }
      | Ok (_, Some raw) -> Ok (Dir.to_entry raw)
end
