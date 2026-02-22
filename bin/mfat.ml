open Cmdliner

let cmd =
  let doc = "FAT32 filesystem tool in OCaml." in
  let info = Cmd.info "mfat" ~doc in
  Cmd.group info
    [ Make.cmd; Ls.cmd; Cat.cmd; Write.cmd; Mkdir.cmd; Rm.cmd; Defrag.cmd ]

let () = Cmd.(exit @@ eval cmd)
