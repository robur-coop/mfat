module Blk = struct
  type t = Bstr.t

  let pagesize _ = 4096

  let read src ~src_off ?(dst_off = 0) dst =
    Bstr.blit src ~src_off dst ~dst_off ~len:(Bstr.length dst)

  let write dst ?(src_off = 0) ~dst_off src =
    Bstr.blit src ~src_off dst ~dst_off ~len:(Bstr.length src - src_off)
end

module A = Mfat.Make (Blk)
module B = Mfat_bos.Make (Blk)

let total_sectors = 2048

let make_fs () =
  let size = total_sectors * 512 in
  let blk = Bstr.create size in
  Bstr.fill blk '\000';
  A.format blk ~total_sectors;
  match A.create blk with
  | Ok t -> (blk, t)
  | Error (`Msg msg) -> Alcotest.failf "create: %s" msg

let ok_or_fail = function
  | Ok v -> v
  | Error (`Msg msg) -> Alcotest.failf "%s" msg

let test_file_write_and_read () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "hello.txt") "Hello, world!");
  let content = ok_or_fail (B.File.read t (Fpath.v "hello.txt")) in
  Alcotest.(check string) "file content" "Hello, world!" content

let test_file_read_lines () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "lines.txt") "a\nb\nc");
  let lines = ok_or_fail (B.File.read_lines t (Fpath.v "lines.txt")) in
  Alcotest.(check (list string)) "lines" [ "a"; "b"; "c" ] lines

let test_file_write_lines () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write_lines t (Fpath.v "out.txt") [ "x"; "y"; "z" ]);
  let content = ok_or_fail (B.File.read t (Fpath.v "out.txt")) in
  Alcotest.(check string) "joined" "x\ny\nz" content

let test_file_fold_lines () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "nums.txt") "1\n2\n3");
  let fn _line acc = succ acc in
  let count = ok_or_fail (B.File.fold_lines fn 0 t (Fpath.v "nums.txt")) in
  Alcotest.(check int) "line count" 3 count

let test_file_exists () =
  let _blk, t = make_fs () in
  let before = ok_or_fail (B.File.exists t (Fpath.v "nope.txt")) in
  Alcotest.(check bool) "not exists" false before;
  ok_or_fail (B.File.write t (Fpath.v "nope.txt") "data");
  let after = ok_or_fail (B.File.exists t (Fpath.v "nope.txt")) in
  Alcotest.(check bool) "exists" true after

let test_file_must_exist () =
  let _blk, t = make_fs () in
  begin match B.File.must_exist t (Fpath.v "ghost.txt") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should error on missing file"
  end;
  ok_or_fail (B.File.write t (Fpath.v "ghost.txt") "boo");
  let p = ok_or_fail (B.File.must_exist t (Fpath.v "ghost.txt")) in
  Alcotest.(check string) "path" "ghost.txt" (Fpath.to_string p)

let test_file_delete () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "tmp.txt") "temp");
  ok_or_fail (B.File.delete t (Fpath.v "tmp.txt"));
  let gone = ok_or_fail (B.File.exists t (Fpath.v "tmp.txt")) in
  Alcotest.(check bool) "deleted" false gone

let test_file_delete_nonexistent () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.delete t (Fpath.v "no-such.txt"));
  match B.File.delete t ~must_exist:true (Fpath.v "no-such.txt") with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "should error with must_exist"

let test_dir_create () =
  let _blk, t = make_fs () in
  let created = ok_or_fail (B.Dir.create t (Fpath.v "mydir")) in
  Alcotest.(check bool) "created" true created;
  let exists = ok_or_fail (B.Dir.exists t (Fpath.v "mydir")) in
  Alcotest.(check bool) "exists" true exists

let test_dir_create_already_exists () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "d")) in
  let created = ok_or_fail (B.Dir.create t (Fpath.v "d")) in
  Alcotest.(check bool) "already existed" false created

let test_dir_create_with_path () =
  let _blk, t = make_fs () in
  let created = ok_or_fail (B.Dir.create t ~path:true (Fpath.v "a/b/c")) in
  Alcotest.(check bool) "created" true created;
  let a = ok_or_fail (B.Dir.exists t (Fpath.v "a")) in
  let b = ok_or_fail (B.Dir.exists t (Fpath.v "a/b")) in
  let c = ok_or_fail (B.Dir.exists t (Fpath.v "a/b/c")) in
  Alcotest.(check bool) "a exists" true a;
  Alcotest.(check bool) "a/b exists" true b;
  Alcotest.(check bool) "a/b/c exists" true c

let test_dir_create_no_path () =
  let _blk, t = make_fs () in
  match B.Dir.create t ~path:false (Fpath.v "x/y") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "We should fail with intermediate dirs"

let test_dir_must_exist () =
  let _blk, t = make_fs () in
  (match B.Dir.must_exist t (Fpath.v "nope") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should error on missing dir");
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "yep")) in
  let p = ok_or_fail (B.Dir.must_exist t (Fpath.v "yep")) in
  Alcotest.(check string) "path" "yep" (Fpath.to_string p)

let test_dir_contents () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "stuff")) in
  ok_or_fail (B.File.write t (Fpath.v "stuff/a.txt") "a");
  ok_or_fail (B.File.write t (Fpath.v "stuff/b.txt") "b");
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "stuff/sub")) in
  let entries = ok_or_fail (B.Dir.contents t (Fpath.v "stuff")) in
  let names =
    List.map (fun p -> Fpath.basename (Fpath.rem_empty_seg p)) entries
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "contents" [ "a.txt"; "b.txt"; "sub" ] names

let test_dir_contents_rel () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "rel")) in
  ok_or_fail (B.File.write t (Fpath.v "rel/f.txt") "f");
  let entries = ok_or_fail (B.Dir.contents t ~rel:true (Fpath.v "rel")) in
  let names =
    List.map (fun p -> Fpath.basename (Fpath.rem_empty_seg p)) entries
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "rel contents" [ "f.txt" ] names

let test_dir_delete () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "delme")) in
  ok_or_fail (B.Dir.delete t (Fpath.v "delme"));
  let gone = ok_or_fail (B.Dir.exists t (Fpath.v "delme")) in
  Alcotest.(check bool) "deleted" false gone

let test_dir_delete_recurse () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "tree/inner")) in
  ok_or_fail (B.File.write t (Fpath.v "tree/inner/f.txt") "data");
  ok_or_fail (B.Dir.delete t ~recurse:true (Fpath.v "tree"));
  let gone = ok_or_fail (B.Dir.exists t (Fpath.v "tree")) in
  Alcotest.(check bool) "recursively deleted" false gone

let test_dir_fold_contents () =
  let _blk, t = make_fs () in
  let _ = ok_or_fail (B.Dir.create t (Fpath.v "fc")) in
  ok_or_fail (B.File.write t (Fpath.v "fc/one.txt") "1");
  ok_or_fail (B.File.write t (Fpath.v "fc/two.txt") "2");
  let count =
    ok_or_fail
      (B.Dir.fold_contents ~elements:`Files t
         (fun _p acc -> acc + 1)
         0 (Fpath.v "fc"))
  in
  Alcotest.(check int) "file count" 2 count

let test_path_exists () =
  let _blk, t = make_fs () in
  let no = ok_or_fail (B.exists t (Fpath.v "absent")) in
  Alcotest.(check bool) "not exists" false no;
  ok_or_fail (B.File.write t (Fpath.v "present.txt") "hi");
  let yes = ok_or_fail (B.exists t (Fpath.v "present.txt")) in
  Alcotest.(check bool) "exists" true yes

let test_path_delete () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "bye.txt") "bye");
  ok_or_fail (B.delete t (Fpath.v "bye.txt"));
  let gone = ok_or_fail (B.exists t (Fpath.v "bye.txt")) in
  Alcotest.(check bool) "deleted via path" false gone

let test_path_stat () =
  let _blk, t = make_fs () in
  ok_or_fail (B.File.write t (Fpath.v "info.txt") "data");
  let entry = ok_or_fail (B.stat t (Fpath.v "info.txt")) in
  Alcotest.(check bool) "is not dir" false entry.Mfat.is_dir;
  Alcotest.(check int32) "size" 4l entry.Mfat.size

let () =
  Alcotest.run "mfat"
    [
      ( "file"
      , [
          Alcotest.test_case "write and read" `Quick test_file_write_and_read
        ; Alcotest.test_case "read_lines" `Quick test_file_read_lines
        ; Alcotest.test_case "write_lines" `Quick test_file_write_lines
        ; Alcotest.test_case "fold_lines" `Quick test_file_fold_lines
        ; Alcotest.test_case "exists" `Quick test_file_exists
        ; Alcotest.test_case "must_exist" `Quick test_file_must_exist
        ; Alcotest.test_case "delete" `Quick test_file_delete
        ; Alcotest.test_case "delete nonexistent" `Quick
            test_file_delete_nonexistent
        ] )
    ; ( "dir"
      , [
          Alcotest.test_case "create" `Quick test_dir_create
        ; Alcotest.test_case "create already exists" `Quick
            test_dir_create_already_exists
        ; Alcotest.test_case "create with path" `Quick test_dir_create_with_path
        ; Alcotest.test_case "create no path" `Quick test_dir_create_no_path
        ; Alcotest.test_case "must_exist" `Quick test_dir_must_exist
        ; Alcotest.test_case "contents" `Quick test_dir_contents
        ; Alcotest.test_case "contents rel" `Quick test_dir_contents_rel
        ; Alcotest.test_case "delete" `Quick test_dir_delete
        ; Alcotest.test_case "delete recurse" `Quick test_dir_delete_recurse
        ; Alcotest.test_case "fold_contents" `Quick test_dir_fold_contents
        ] )
    ; ( "path"
      , [
          Alcotest.test_case "exists" `Quick test_path_exists
        ; Alcotest.test_case "delete" `Quick test_path_delete
        ; Alcotest.test_case "stat" `Quick test_path_stat
        ] )
    ]
