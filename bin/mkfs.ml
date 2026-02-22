let failwithf fmt = Fmt.kstr failwith fmt

module Blk = struct
  external getpagesize : unit -> int = "mkfs_getpagesize" [@@noalloc]

  external pread :
    Unix.file_descr -> Bstr.t -> off:int -> len:int -> at:int -> int
    = "mkfs_pread"

  external pwrite :
    Unix.file_descr -> Bstr.t -> off:int -> len:int -> at:int -> int
    = "mkfs_pwrite"

  type t = Unix.file_descr

  let pagesize _ = getpagesize ()

  let read fd ~src_off ?(dst_off = 0) bstr =
    let len = Bstr.length bstr - dst_off in
    ignore (pread fd bstr ~off:dst_off ~len ~at:src_off)

  let write fd ?(src_off = 0) ~dst_off bstr =
    let len = Bstr.length bstr - src_off in
    let ret = pwrite fd bstr ~off:src_off ~len ~at:dst_off in
    if ret < len then
      failwithf "Impossible to write %d byte(s) at %d" len dst_off
end

module Mfat = Mfat.Make (Blk)

(* --- mkfs logic --- *)

let bytes_per_sector = 512

let sectors_per_cluster total_sectors =
  let size_mb = total_sectors * bytes_per_sector / (1024 * 1024) in
  if size_mb <= 260 then 1
  else if size_mb <= 8192 then 8
  else if size_mb <= 16384 then 16
  else if size_mb <= 32768 then 32
  else 64

let fat_size ~total_sectors ~reserved_sectors ~sectors_per_cluster ~num_fats =
  let tmp1 = total_sectors - reserved_sectors in
  let tmp2 = ((256 * sectors_per_cluster) + num_fats) / 2 in
  (tmp1 + tmp2 - 1) / tmp2

let format_fs fd ~total_sectors =
  let spc = sectors_per_cluster total_sectors in
  let reserved_sectors = 32 in
  let num_fats = 2 in
  let fat_sz =
    fat_size ~total_sectors ~reserved_sectors ~sectors_per_cluster:spc ~num_fats
  in
  let boot = Bstr.create bytes_per_sector in
  Bstr.fill boot '\000';
  Bstr.set_uint8 boot 0 0xEB;
  Bstr.set_uint8 boot 1 0x58;
  Bstr.set_uint8 boot 2 0x90;
  Bstr.blit_from_string "MFAT    " ~src_off:0 boot ~dst_off:3 ~len:8;
  Bstr.set_uint16_le boot 11 bytes_per_sector;
  Bstr.set_uint8 boot 13 spc;
  Bstr.set_uint16_le boot 14 reserved_sectors;
  Bstr.set_uint8 boot 16 num_fats;
  Bstr.set_uint16_le boot 17 0;
  Bstr.set_uint16_le boot 19 0;
  Bstr.set_uint8 boot 21 0xF8;
  Bstr.set_uint16_le boot 22 0;
  Bstr.set_uint16_le boot 24 63;
  Bstr.set_uint16_le boot 26 255;
  Bstr.set_int32_le boot 28 0l;
  Bstr.set_int32_le boot 32 (Int32.of_int total_sectors);
  Bstr.set_int32_le boot 36 (Int32.of_int fat_sz);
  Bstr.set_uint16_le boot 40 0;
  Bstr.set_uint16_le boot 42 0;
  Bstr.set_int32_le boot 44 2l;
  Bstr.set_uint16_le boot 48 1;
  Bstr.set_uint16_le boot 50 6;
  Bstr.set_uint8 boot 64 0x80;
  Bstr.set_uint8 boot 66 0x29;
  Bstr.set_int32_le boot 67 0x12345678l;
  Bstr.blit_from_string "NO NAME    " ~src_off:0 boot ~dst_off:71 ~len:11;
  Bstr.blit_from_string "FAT32   " ~src_off:0 boot ~dst_off:82 ~len:8;
  Bstr.set_uint8 boot 510 0x55;
  Bstr.set_uint8 boot 511 0xAA;
  Blk.write fd ~dst_off:0 boot;
  let fsinfo = Bstr.create bytes_per_sector in
  Bstr.fill fsinfo '\000';
  Bstr.set_int32_le fsinfo 0 0x41615252l;
  Bstr.set_int32_le fsinfo 484 0x61417272l;
  let total_data_clusters =
    (total_sectors - reserved_sectors - (num_fats * fat_sz)) / spc
  in
  Bstr.set_int32_le fsinfo 488 (Int32.of_int (total_data_clusters - 1));
  Bstr.set_int32_le fsinfo 492 3l;
  Bstr.set_int32_le fsinfo 508 0xAA550000l;
  Blk.write fd ~dst_off:bytes_per_sector fsinfo;
  Blk.write fd ~dst_off:(6 * bytes_per_sector) boot;
  Blk.write fd ~dst_off:(7 * bytes_per_sector) fsinfo;
  let fat_offset = reserved_sectors * bytes_per_sector in
  let fat_first = Bstr.create bytes_per_sector in
  Bstr.fill fat_first '\000';
  Bstr.set_int32_le fat_first 0 0x0FFFFFF8l;
  Bstr.set_int32_le fat_first 4 0x0FFFFFFFl;
  Bstr.set_int32_le fat_first 8 0x0FFFFFFFl;
  Blk.write fd ~dst_off:fat_offset fat_first;
  let fat2_offset = fat_offset + (fat_sz * bytes_per_sector) in
  Blk.write fd ~dst_off:fat2_offset fat_first;
  let data_offset =
    (reserved_sectors + (num_fats * fat_sz)) * bytes_per_sector
  in
  let cluster_size = spc * bytes_per_sector in
  let zero = Bstr.create bytes_per_sector in
  Bstr.fill zero '\000';
  for i = 0 to (cluster_size / bytes_per_sector) - 1 do
    Blk.write fd ~dst_off:(data_offset + (i * bytes_per_sector)) zero
  done

(* --- helpers --- *)

let with_image filename f =
  let fd = Unix.openfile filename [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  match Mfat.create fd with
  | Ok fs -> f fs
  | Error (`Msg msg) -> Fmt.epr "Error: %s\n" msg; exit 1

let or_exit = function
  | Ok v -> v
  | Error (`Msg msg) -> Fmt.epr "Error: %s\n" msg; exit 1

(* --- commands --- *)

open Cmdliner

let filename_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")

let format_cmd =
  let total_sectors =
    let doc = "Total number of sectors (512 bytes each)." in
    Arg.(
      required & opt (some int) None & info [ "s"; "sectors" ] ~docv:"N" ~doc)
  in
  let run filename total_sectors =
    let fd =
      Unix.openfile filename [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
    in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    let total_size = total_sectors * bytes_per_sector in
    Unix.ftruncate fd total_size;
    format_fs fd ~total_sectors;
    Fmt.pr "Formatted %s: %d sectors (%d bytes), FAT32\n" filename total_sectors
      total_size
  in
  let doc = "Format a file as FAT32" in
  let info = Cmd.info "format" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ total_sectors)

let ls_cmd =
  let path = Arg.(value & pos 1 string "/" & info [] ~docv:"PATH") in
  let run filename path =
    with_image filename @@ fun fs ->
    let entries = or_exit (Mfat.ls fs path) in
    let pp_entry entry =
      if entry.Mfat.is_dir then Fmt.pr "%s/\n" entry.Mfat.name
      else Fmt.pr "%s (%ld)\n" entry.Mfat.name entry.Mfat.size
    in
    List.iter pp_entry entries
  in
  let doc = "List directory contents" in
  let info = Cmd.info "ls" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ path)

let cat_cmd =
  let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH") in
  let run filename path =
    with_image filename @@ fun fs ->
    let data = or_exit (Mfat.read_file fs path) in
    print_string data
  in
  let doc = "Read a file" in
  let info = Cmd.info "cat" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ path)

let write_cmd =
  let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH") in
  let data =
    let doc = "Data to write (reads from stdin if not provided)." in
    Arg.(
      value & opt (some string) None & info [ "d"; "data" ] ~docv:"DATA" ~doc)
  in
  let run filename path data =
    let data =
      match data with
      | Some d -> d
      | None ->
          let buf = Buffer.create 4096 in
          (try
             while true do
               Buffer.add_char buf (input_char stdin)
             done
           with End_of_file -> ());
          Buffer.contents buf
    in
    with_image filename @@ fun fs -> or_exit (Mfat.write_file fs path data)
  in
  let doc = "Write a file" in
  let info = Cmd.info "write" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ path $ data)

let mkdir_cmd =
  let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH") in
  let run filename path =
    with_image filename @@ fun fs -> or_exit (Mfat.mkdir fs path)
  in
  let doc = "Create a directory" in
  let info = Cmd.info "mkdir" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ path)

let rm_cmd =
  let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH") in
  let run filename path =
    with_image filename @@ fun fs -> or_exit (Mfat.remove fs path)
  in
  let doc = "Remove a file or empty directory" in
  let info = Cmd.info "rm" ~doc in
  Cmd.v info Term.(const run $ filename_arg $ path)

let () =
  let doc = "FAT32 filesystem tool" in
  let info = Cmd.info "mkfs.fat" ~doc in
  let group =
    Cmd.group info [ format_cmd; ls_cmd; cat_cmd; write_cmd; mkdir_cmd; rm_cmd ]
  in
  exit (Cmd.eval group)
