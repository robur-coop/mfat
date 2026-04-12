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

let bytes_per_sector = 512

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
    Mfat.format fd ~total_sectors;
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
