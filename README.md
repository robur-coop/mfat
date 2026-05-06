# `mfat`, a FAT32 file system in OCaml

`mfat` is an implementation of the FAT32 file system. It allows users to
manipulate an image (in the form of a file) using the `mfat` utility, as well
as in the form of a library, provided the user provides an implementation of a
block device. For example, here is a possible implementation for unikernels
using [mkernel][mkernel]:

```ocaml
module Blk = struct
  type t = Mkernel.Block.t

  let pagesize = Mkernel.Block.pagesize
  let read = Mkernel.Block.atomic_read
  let write = Mkernel.Block.atomic_write
end

module FS = Mfat_bos.Make (Blk)

let fs ~name =
  let fn blk () =
    let v = FS.create blk in
    let v = Result.map_error (fun (`Msg msg) -> msg) v in
    Result.error_to_failure v
  in
  Mkernel.map fn [ Mkernel.block name ]

let () =
  Mkernel.(run [ fs ~name:"fs"]) @@ fun fs () ->
  ...
```

The user can then create an image using the `mfat` tool:
```shell
$ mfat make -s 2048 fs.img
$ mount fs.img /mnt
$ cd /mnt
$ echo "Hello World!" > foo.txt
$ cd -
$ umount /mnt
$ mfat cat fs.img /foo.txt
Hello World!
```

Finally, the image can be manipulated from OCaml code using, among other things,
the [Mfat_bos.Make][Mfat_bos] interface (a replica of the [bos][bos] interface).

[mkernel]: https://github.com/robur-coop/mkernel.git
[bos]: https://github.com/dbuenzli/bos
[Mfat_bos]: ./lib/mfat_bos.mli
