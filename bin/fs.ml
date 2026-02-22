let failwithf fmt = Fmt.kstr failwith fmt
let ( let* ) = Result.bind

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

include Mfat.Make (Blk)

let with_image fpath fn =
  let fd = Unix.openfile fpath Unix.[ O_RDWR ] 0 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let* t = create fd in
  fn t
