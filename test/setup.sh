#!/usr/bin/env bash
set -e

# Inspiration from https://github.com/FedericoPonzi/vfat-rs/blob/master/tests/setup.sh

diskimg=test.mfat

mnt_dir="$(mktemp -d mfat.XXXXXXXX)"

cleanup () {
	[ -n "$mnt_dir" ] && [ -d "$mnt_dir" ] && rm -rf "$mnt_dir"
}
trap cleanup EXIT

# Create 64 MB image. It has to be somewhat larger than 32 MB otherwise a warning is printed.
mkfs.fat -C -F32 "$diskimg" $((64 << 10))

# Write test files
# 1. Mount the FS:
mkdir -p "$mnt_dir"
# sync option: every write is flushed right away.
sudo mount -o sync,loop,uid=1000,gid=1000,dmask=0000,fmask=0001 "$diskimg" "$mnt_dir"

# Create test files:
mkdir -p "${mnt_dir}/folder/some/deep/nested/folder/"
echo "A rabbit stares back at you." > "${mnt_dir}/folder/some/deep/nested/folder/file"
mkdir "${mnt_dir}/MyFoLdEr"

cat > "${mnt_dir}/a-big-file.txt" <<EOF
From fairest creatures we desire increase,
That thereby beauty's rose might never die,
But as the riper should by time decrease,
His tender heir mught bear his memeory:
But thou, contracted to thine own bright eyes,
Feed'st thy light'st flame with self-substantial fuel,
Making a famine where abundance lies,
Thyself thy foe, to thy sweet self too cruel.
Thou that art now the world's fresh ornament
And only herald to the gaudy spring,
Within thine own bud buriest thy content
And, tender churl, makest waste in niggarding.
Pity the world, or else this glutton be,
To eat the world's due, by the grave and thee.
EOF

echo "Short content." > "${mnt_dir}/a-very-long-file-name-entry.txt"
echo 'Hello, mfat!' > "${mnt_dir}/hello.txt"

## Then unmount the fs
sudo umount "$mnt_dir"
