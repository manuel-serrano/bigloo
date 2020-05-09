Raspberry Cross Compilation
---------------------------

_9 Dec 2019_

This note describes how to cross compile and install Bigloo on a
Raspberry PI. The procedure is complex because of the different ARM
platforms and because of the variety of tools that are needed for
completing this installation. 

In all these documents we refer to the "host", as the machine used to
compile Bigloo (typically a laptop or a desktop, running an x86 or
x86/64 architecture), and we refer to the "guest" as the arm platform
that is the target of the cross compilation.

In this document, we use the Qemu emulator for cross compilation.
Qemu can be replaced with an actual raspberry device.

The three main steps of the cross compilation procedure are:

  1. getting a toolchain that is used for the low level cross compilation.
  2. preparing qemu to emulate a raspberry device.
  3. cross compiling Bigloo.
  
  
### Prerequisite

The procedure described in these documents has been tested only under
Debian platforms. However, it should not depend on that specific Linux
version. Any complete GNU C development kit (gcc, autoconf, automake, libtool,
...) should work.

We are assuming that raspbian is the operating system running on the guest.


### Qemu

The cross compilation can be executed with a hardware guest platform
(i.e., a real raspberry computer) or with the Qemu emulator. This section
explains how to prepare the emulator if the option is chosen.

A. Download the raspbian image

```shell[:@shell-host]
(in host) wget https://downloads.raspberrypi.org/raspbian_lite_latest
```

B. Download the raspbian kernel

```shell[:@shell-host]
(in host) git clone https://github.com/dhruvvyas90/qemu-rpi-kernel.git
```

C. Convert the image for qemu

```shell[:@shell-host]
(in host) qemu-img convert -f raw -O qcow2 2019-09-26-raspbian-buster-lite.img 2019-09-26-raspbian-buster-lite.qcow
```

D. Run qemu

From the directory containing the img file:

```shell[:@shell-host]
(in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -hda 2019-09-26-raspbian-buster-lite.qcow -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22
   
```

The port forwarding 2022:22 can be changed, but if you do so, you will
have to adapt the Bigloo `ssh-copy.sh` script used for the cross-compiation
(see below section 3).
   
E. Configure ssh

```shell[:@shell-guest]
(in guest) sudo update-rc.d ssh defaults
```

or 
   
```shell[:@shell-guest]
(in guest) sudo update-rc.d ssh enable 2
```

F. Create the hop user
   
```shell[:@shell-guest]
(in guest) sudo adduser --home /home/hop --shell /bin/bash hop
```

G. Generate an ssh-key

```shell[:@shell-guest]
(in guest) ssh-keygen
```

H. Copy personnal public key

```shell[:@shell-guest]
(in guest) cat > ~/.ssh/authorized_keys
```

I. Add hop in the sudoers list (as "pi" user)

```shell[:@shell-guest]
(in guest) sudo sh -c "echo \"hop ALL=NOPASSWD: ALL\" > /etc/sudoers.d/hop"
```

J. Expand the image size

```shell[:@shell-host]
(in host) qemu-img resize 2019-09-26-raspbian-buster-lite.qcow +16G
(in host) cp 2019-09-26-raspbian-buster-lite.qcow 2019-09-26-raspbian-buster-lite16GB.qcow
```

Boot qemy with a second disk
   
```shell[:@shell-host]
(in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -hda 2019-09-26-raspbian-buster-lite.qcow -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22 -hdb 2019-09-26-raspbian-buster-lite16GB.qcow
```

Resize the partition from guest
   
```shell[:@shell-guest]
(in guest) sudo cfdisk /dev/sdb
```

Delete sdb2 and create a new partitition with all the space
   
```shell[:@shell-guest]
(in guest) sudo fsck -f /dev/sdb2
(in guest) sudo resize2fs /dev/sdb2
(in guest) sudo fsck -f /dev/sdb2
(in guest) sudo halt
```
   
### The toolchain

Getting a correct toolchain for compiling C files executable on the
guest is challenging. Arm processors have different characteristics
(arm <= 6 that supports soft floats and arm >= 7 that supports hard floats)
and the Raspbian distribution probably uses different versions of the
glibc and gcc compiler than the host. Before proceeding to the Bigloo
cross compilation a compatible toochain must be build. The following
will show the different versions of the tools and libraries that are
involved.

```shell[:@shell-host]
(in host) ld -v
(in host) gcc --version
(in host) ldd --version

(in guest) ld -v
(in guest) gcc --version
(in guest) ldd --version
```

If these versions are the same, then you can use the native gcc cross
compiler packages provided by your distribution. On debian it can be
installed with:

```shell[:@shell-host]
(host) sudo apt install gcc-arm-linux-gnueabihf
```

If the versions differ, then you have to install your own custom
version. This can be done with

```shell[:@shell-host]
(host) bigloo/arch/raspberry/build-toolchain.sh
```

This script installs the linux header, binutils (the loader), the glibc,
and gcc. This script is automatic but if it fails, it should be easy
to fix as it simply proceeds to a serie of downloads, configures, and
installs.


### Cross compilation

To cross-compile:

A. Add the path to the arm gcc compiler to the PATH shell variable.

B. Test cross compilation

```shell[:@shell-host]
(in host) cat > foo.c <<eof
#include <stdio.h>

int main() {
   fprintf( stderr, "hello world\n" );
}
EOF
(in host) arm-linux-gnueabihf-gcc foo.c
(in host) file ./a.out
(in host) scp -P 2022 a.out hop@localhost:

(in guest) ./a.out
```

C. Bigloo cross-compilation

```shell[:@shell-host]
(in host) ./configure --cc=arm-linux-gnueabi-gcc --hostsh=$PWD/examples/hostsh/ssh/ssh-copy.sh --build-bindir=$BIGLOOBINDIR --prefix=/opt/bigloo
(in host) make 
(in host) make install DESTDIR=/tmp/raspbian
```

The default `ssh-copy.sh` script uses a configuration compatible with the
arguments passed to qemu and the raspberry configuration (ssh port 
and use credentials).

D. Bundle Bigloo 

```shell[:@shell-host]
(in host) (cd /tmp/raspbian/opt; tar cvfz bigloo.tgz bigloo)
(in host) scp -P 2020 bigloo.tgz hop@localhost:
(in guest) (cd /opt; sudo tar xvfz /home/hop/bigloo.tgz)
(in guest) sudo chown $USER -R /opt/bigloo
(in guest) chmod a+rx -R /opt/bigloo
```


### Bigloo installation

Before installing Bigloo, the following packages should be installed:

```shell[:@shell-guest]
(in guest) sudo apt update
(in guest) sudo apt install -y dh-make libssl1.0.2 libssl-dev libsqlite3-0 libsqlite3-dev libasound2 libasound2-dev libflac8 libflac-dev libmpg123-0 libmpg123-dev libavahi-core7 libavahi-core-dev libavahi-common-dev libavahi-common3 libavahi-client3 libavahi-client-dev libunistring2 libunistring-dev libpulse-dev libpulse0 automake libtool libgmp-dev libgmp3-dev libgmp10
```
