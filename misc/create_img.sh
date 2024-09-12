#!/bin/bash
set -e 

if [ $# -lt 2 ] ; then
	echo "Usage: SCRIPT KERNEL-NAME \"KERNEL-OPTIONS\""
	echo "e.g: sh create_img.sh simple-fw --private-ipv4=10.10.0.2/24 --public-ipv4=10.0.0.2/24"
	exit 10
fi

NAME=$1
shift
OPTIONS=$*
UNIKERNEL=$NAME.virtio
IMAGE=$NAME.qcow2

##### create initial image
rm -f $IMAGE
qemu-img create -f qcow2 $IMAGE 50M

##### paritition and mount
DEVICE=/dev/nbd0
MOUNT=/mnt
sudo modprobe nbd max_parts=10
sudo qemu-nbd --connect $DEVICE $IMAGE

#no partition
#printf "n\np\n1\n\n\na\np\nw\n" | sudo fdisk $DEVICE
#BOOTDEV=${DEVICE}p1
BOOTDEV=${DEVICE}

sudo /sbin/mkfs.fat $BOOTDEV
sudo mount -o uid=user $BOOTDEV $MOUNT
mkdir -p $MOUNT/boot

# and create /boot -> . symlink so it doesn't matter if grub looks for
# /boot/grub or /grub
#ln -s . "$INSTALLDIR/boot/boot"

cp $UNIKERNEL $MOUNT/boot/kernel

mkdir -p $MOUNT/boot/grub2
cat > $MOUNT/boot/grub2/grub.cfg <<EOF
set timeout=0
grub_hidden_timeout_quiet
menuentry mirage {
    set root="(hd0)"
	multiboot /boot/kernel $OPTIONS
}
EOF

cat $MOUNT/boot/grub2/grub.cfg

#cat > $MOUNT/boot/grub2/menu.lst <<EOF
#default 0
#timeout 0
#title Mirage
#  root (hd0)
#  kernel /boot/kernel
#EOF

sudo grub2-install --no-floppy --boot-directory=$MOUNT/boot $DEVICE --force

##### save properly
sudo umount $MOUNT
sudo qemu-nbd --disconnect $DEVICE
