let bytes_per_sector = 512

let run fpath total_sectors =
  let fd = Unix.openfile fpath Unix.[ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let total_size = total_sectors * bytes_per_sector in
  Unix.ftruncate fd total_size;
  Fs.format fd ~total_sectors;
  Fmt.pr "Formatted %s: %d sectors (%d bytes), FAT32\n" fpath total_sectors
    total_size

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")

let total_sectors =
  let doc = "Total number of sectors (512 bytes each)." in
  let open Arg in
  required & opt (some int) None & info [ "s"; "sectors" ] ~docv:"NUMBER" ~doc

let cmd =
  let doc = "Format a file as FAT32 filesystem." in
  let info = Cmd.info "make" ~doc in
  Cmd.v info Term.(const run $ image $ total_sectors)
