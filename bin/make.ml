let bytes_per_sector = 512

let sectors_per_cluster total_sectors =
  let size_mb = total_sectors * bytes_per_sector / (1024 * 1024) in
  if size_mb <= 260 then 1
  else if size_mb <= 8192 then 8
  else if size_mb <= 16384 then 16
  else if size_mb <= 32768 then 32
  else 64

let size ~total_sectors ~reserved_sectors ~sectors_per_cluster ~num_fats =
  let tmp1 = total_sectors - reserved_sectors in
  let tmp2 = ((256 * sectors_per_cluster) + num_fats) / 2 in
  (tmp1 + tmp2 - 1) / tmp2

let format fd ~total_sectors =
  let spc = sectors_per_cluster total_sectors in
  let reserved_sectors = 32 in
  let num_fats = 2 in
  let size =
    size ~total_sectors ~reserved_sectors ~sectors_per_cluster:spc ~num_fats
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
  Bstr.set_int32_le boot 36 (Int32.of_int size);
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
  Fs.Blk.write fd ~dst_off:0 boot;
  let fsinfo = Bstr.create bytes_per_sector in
  Bstr.fill fsinfo '\000';
  Bstr.set_int32_le fsinfo 0 0x41615252l;
  Bstr.set_int32_le fsinfo 484 0x61417272l;
  let total_data_clusters =
    (total_sectors - reserved_sectors - (num_fats * size)) / spc
  in
  Bstr.set_int32_le fsinfo 488 (Int32.of_int (total_data_clusters - 1));
  Bstr.set_int32_le fsinfo 492 3l;
  Bstr.set_int32_le fsinfo 508 0xAA550000l;
  Fs.Blk.write fd ~dst_off:bytes_per_sector fsinfo;
  Fs.Blk.write fd ~dst_off:(6 * bytes_per_sector) boot;
  Fs.Blk.write fd ~dst_off:(7 * bytes_per_sector) fsinfo;
  let fat_offset = reserved_sectors * bytes_per_sector in
  let fat_first = Bstr.create bytes_per_sector in
  Bstr.fill fat_first '\000';
  Bstr.set_int32_le fat_first 0 0x0FFFFFF8l;
  Bstr.set_int32_le fat_first 4 0x0FFFFFFFl;
  Bstr.set_int32_le fat_first 8 0x0FFFFFFFl;
  Fs.Blk.write fd ~dst_off:fat_offset fat_first;
  let fat2_offset = fat_offset + (size * bytes_per_sector) in
  Fs.Blk.write fd ~dst_off:fat2_offset fat_first;
  let data_offset = (reserved_sectors + (num_fats * size)) * bytes_per_sector in
  let cluster_size = spc * bytes_per_sector in
  let zero = Bstr.create bytes_per_sector in
  Bstr.fill zero '\000';
  for i = 0 to (cluster_size / bytes_per_sector) - 1 do
    Fs.Blk.write fd ~dst_off:(data_offset + (i * bytes_per_sector)) zero
  done

let run fpath total_sectors =
  let fd = Unix.openfile fpath Unix.[ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let total_size = total_sectors * bytes_per_sector in
  Unix.ftruncate fd total_size;
  format fd ~total_sectors;
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
