Test against a fat32 filesystem created by the host
  $ ./setup.sh
  mkfs.fat 4.2 (2021-01-31)

We can list the files
  $ mfat ls test.mfat
  folder/
  MyFoLdEr/
  a-big-file.txt (613)
  a-very-long-file-name-entry.txt (15)
  hello.txt (13)
  $ mfat cat test.mfat hello.txt
  Hello, mfat!
  $ mfat cat test.mfat a-very-long-file-name-entry.txt
  Short content.
  $ mfat ls test.mfat folder
  some/
  $ mfat ls test.mfat folder/some
  deep/
  $ mfat ls test.mfat folder/some/deep
  nested/
  $ mfat ls test.mfat folder/some/deep/nested
  folder/
  $ mfat ls test.mfat folder/some/deep/nested/folder
  file (29)
  $ mfat cat test.mfat folder/some/deep/nested/folder/file
  A rabbit stares back at you.

