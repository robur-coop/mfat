  $ mfat make -s 2048 test.fat
  Formatted test.fat: 2048 sectors (1048576 bytes), FAT32
  $ mfat ls test.fat
  $ mfat write test.fat /HELLO.TXT - <<EOF
  > Hello, World!
  > EOF
  $ mfat ls test.fat
  HELLO.TXT (14)
  $ mfat cat test.fat /HELLO.TXT
  Hello, World!

  $ mfat mkdir test.fat /DOCS
  $ mfat ls test.fat
  HELLO.TXT (14)
  DOCS/
  $ mfat write test.fat /DOCS/README.TXT - <<EOF
  > Coucou!
  > EOF
  $ mfat ls test.fat /DOCS
  README.TXT (8)
  $ mfat cat test.fat /DOCS/README.TXT
  Coucou!
  $ mfat write test.fat /HELLO.TXT - <<EOF
  > New version!
  > EOF
  $ mfat cat test.fat /HELLO.TXT
  New version!
  $ mfat ls test.fat
  HELLO.TXT (13)
  DOCS/
  $ mfat rm test.fat /HELLO.TXT
  $ mfat ls test.fat
  DOCS/
  $ mfat rm test.fat /DOCS
  mfat: DOCS: directory not empty
  [124]
  $ mfat rm test.fat /DOCS/README.TXT
  $ mfat rm test.fat /DOCS
  $ mfat ls test.fat
  $ mfat make -s 2048 defrag.fat
  Formatted defrag.fat: 2048 sectors (1048576 bytes), FAT32
  $ echo -n "AAAA" | mfat write defrag.fat /A.TXT -
  $ echo -n "BBBB" | mfat write defrag.fat /B.TXT -
  $ echo -n "CCCC" | mfat write defrag.fat /C.TXT -
  $ mfat mkdir defrag.fat /SUB
  $ echo -n "SUB"| mfat write defrag.fat /SUB/N.TXT -
  $ mfat rm defrag.fat /B.TXT
  $ echo -n "DDDDDDDD" | mfat write defrag.fat /D.TXT -
  $ mfat ls defrag.fat
  A.TXT (4)
  D.TXT (8)
  C.TXT (4)
  SUB/
  $ mfat defrag defrag.fat
  Defragmented defrag.fat: 4 file(s), 1 directory(ies)
  $ mfat ls defrag.fat
  A.TXT (4)
  D.TXT (8)
  C.TXT (4)
  SUB/
  $ mfat cat defrag.fat /A.TXT
  AAAA
  $ mfat cat defrag.fat /C.TXT
  CCCC
  $ mfat cat defrag.fat /D.TXT
  DDDDDDDD
  $ mfat cat defrag.fat /SUB/N.TXT
  SUB
  $ mfat ls defrag.fat /SUB
  N.TXT (3)
