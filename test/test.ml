module Blk = struct
  type t = Bstr.t

  let pagesize _ = 4096

  let read src ~src_off ?(dst_off = 0) dst =
    Bstr.blit src ~src_off dst ~dst_off ~len:(Bstr.length dst)

  let write dst ?(src_off = 0) ~dst_off src =
    Bstr.blit src ~src_off dst ~dst_off ~len:(Bstr.length dst)
end

module A = Mfat.Make (Blk)
module B = Mfat_bos.Make (Blk)

let () = Alcotest.run "mfat" []
