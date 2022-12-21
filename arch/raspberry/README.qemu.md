Raspbian qemu installation - 28 mar 2021
========================================

This document explains how to prepare qemu Raspbian image so that Bigloo
and Bigloo debian packages can be built.

1. Prerequisite
---------------

Install qemu for arm on the host:

```shell[:@shell-host]
(in host) sudo apt install qemu-system-arm
```


2. Preparing the qemu image 
---------------------------

Extra information that be found at [1].


1. clone the rpi kernel

```shell[:@shell-host]
(in host) git clone https://github.com/dhruvvyas90/qemu-rpi-kernel.git
```

2. Download the Raspbian image

As of November 22, raspios images provide no default login credentials.
It is then necessary to build a custom image with a default login already
configured. For that proceed as follow:

Install an image on a regular sdcard using the Rapberry `rpi-imager`
tool.  After choosing the 32 bit OS, click the setting button (bottom
right or press ctl-shift-x) to configure a default login. 

```shell[:@shell-host]
dd bs=4M if=/dev/mmcblk0 > raspios.img
```

3. Expand the image size

```shell[:@shell-host]
(in host) qemu-img resize -f raw raspios.img +16G
```

5. Alternatively (why ?) the qemu image can be converted in qcow format:

```shell[:@shell-host]
(in host) qemu-img convert -f raw -O qcow2 raspios.img raspios.qcow
(in host) qemu-img resize raspios.qcow +16G
```


3. Running qemu
---------------

1. Default executing using the raw image

```shell[:@shell-host]
(in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -drive "file=raspios.img,media=disk,format=raw" -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22 -net tap,ifname=vnet0,script=no,downscript=no
```

2. Default execution (using qcow)

```shell[:@shell-host]
(in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -hda raspios.qcow -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22 -net tap,ifname=vnet0,script=no,downscript=no
```

3. Alternative execution (without network interface) (using qcow)

If you don't need the network interface:

```shell[:@shell-host]
(in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -hda raspios.qcow -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22
```


4. Raspbian configuration
-------------------------

Default user is:

  username: pi
  password: raspberry
  
1. Resize the partition

As the qemu image is not an SD card the Raspbian tool `raspi-config` cannot
be used. Instead:

```shell[:@shell-guest]
(in guest) sudo fdisk /dev/sda
# print the partition (and pay attention to the starting sector of partition 2)
p 
# delete partition 2
d 2 
# new partition 2 (primary)
# start at the same index that the partition 2 previously delete, max space
# do not remove the signature
n 
w
(in guest) touch /etc/mtab # need a fake mtab file so resize2fs can run
(in guest) sudo resize2fs /dev/sda2
````

2. Enable guest ssh server

```shell[:@shell-guest]
(in guest) sudo update-rc.d ssh defaults
(in guest) sudo /etc/init.d/ssh start
```

or 
   
```shell[:@shell-guest]
(in guest) sudo update-rc.d ssh enable 2
(in guest) sudo /etc/init.d/ssh start
```

3. Create the hop user

```shell[:@shell-guest]
(in guest) sudo adduser --home /home/hop --shell /bin/bash hop
```

4. Add hop in the sudoers list (as "pi" user)

```shell[:@shell-guest]
(in guest) sudo sh -c "echo \"hop ALL=NOPASSWD: ALL\" > /etc/sudoers.d/hop"
```

5. Configuration the hop account

```shell[:@shell-guest]
(un guest) sudo su - hop
(in guest) ssh-keygen
```

```shell[:@shell-guest]
(in guest) cat > ~/.ssh/authorized_keys
```

6. Host ssh configuration

```shell[:@shell-host]
(in host) cat >> ~/.ssh/config <<EOF
Host=raspbian
Hostname=localhost
Port=2022
User=hop
Compression=no
EOF
```


5. Raspbian toolchain
---------------------

Before installing Bigloo, the following packages should be installed:

```shell[:@shell-guest]
(in guest) sudo apt update
(in guest) sudo apt dist-upgrade
(in guest) sudo apt install -y dh-make libssl1.1 libssl-dev libsqlite3-0 libsqlite3-dev libasound2 libasound2-dev libflac8 libflac-dev libmpg123-0 libmpg123-dev libavahi-core7 libavahi-core-dev libavahi-common-dev libavahi-common3 libavahi-client3 libavahi-client-dev libunistring2 libunistring-dev libpulse-dev libpulse0 automake libtool libgmp-dev libgmp3-dev libgmp10 alsa-utils
```


6. Building Bigloo
------------------

Bigloo can be installed manually using conventional 

```shell[:@shell-guest]
./configure && make && sudo make install
```

Debian packages can be built using the build debian procedure
described in `../debian/REAME.md`, section "To build the debian packages 
on a remote machine via ssh".


7. Misc
-------

It is possible to mount the Raspbian disk of the qemu image directly
from the host:

```shell[:@shell-host]
(in host) export RPI_FS=$PWD/raspios.img
(in host) SECTOR1=$( fdisk -l $RPI_FS | grep FAT32 | awk '{ print $2 }' )
(in host) SECTOR2=$( fdisk -l $RPI_FS | grep Linux | awk '{ print $2 }' )
(in host) OFFSET1=$(( SECTOR1 * 512 ))
(in host) OFFSET2=$(( SECTOR2 * 512 ))
(in host) sudo mount $RPI_FS -o offset=$OFFSET2 MNTPOINT
````

Notes
-----

[1] https://www.raspberrypi.org/documentation/installation/installing-images/linux.md
