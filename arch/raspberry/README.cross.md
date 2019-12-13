Bigloo Raspberry Cross Compilation - 9 Dec 2019
===============================================

This note describes how to cross compile and install Bigloo on a
Raspberry PI. The procedure is complex because of the different ARM
platforms and because of the variety of tools that are needed for
completing this installation. In all these documents we refer to the
"host", as the machine used to compile Bigloo (typically a laptop or a
desktop, running an x86 or x86_64 architecture), and we refer to the
"guest" as the arm platform that is the target of the cross
compilation.

In this document, we use the Qemu emulator for the cross compilation.
Qemu can be replaced with an actual raspberry device.

The three main steps of the cross compilation procedure are:

  1- get a toolchain that is used for the low level cross compilation.
  2- prepare qemu to emulate a raspberry device.
  3- cross compile Bigloo.
  
  
0. Prerequisite
---------------

The procedure described in these documents has been tested only under
Debian platforms. However, it should not depend on that specific Linux
version. Any complete GNU C development kit (gcc, autoconf, automake, libtool,
...) should work.

We are assuming that raspbian is the operating system running on the guest.

1. Qemu
-------

1. Download the raspbian image

  (in host) wget https://downloads.raspberrypi.org/raspbian_lite_latest
  
2. Download the raspbian kernel

  (in host) git clone https://github.com/dhruvvyas90/qemu-rpi-kernel.git

3. Convert the image for qemu

  (in host) qemu-img convert -f raw -O qcow2 2019-09-26-raspbian-buster-lite.img 2019-09-26-raspbian-buster-lite.qcow

4. Expand the image size

  (in host) qemu-img resize 2019-09-26-raspbian-buster-lite.qcow +6G
  
5. Run qemu

   (in host) sudo qemu-system-arm -nographic -kernel qemu-rpi-kernel/kernel-qemu-4.19.50-buster -dtb qemu-rpi-kernel/versatile-pb.dtb -append "root=/dev/sda2 panic=1 rootfstype=ext4 rw" -hda 2019-09-26-raspbian-buster-lite.qcow -cpu arm1176 -m 256 -M versatilepb -no-reboot -nic user,hostfwd=tcp::2022-:22
   

The port forwarding 2022:22 can be changed, but if you do so, you will
have to adapt the Bigloo ssh-copy.sh script used for the cross-compiation
(see below section 3).
   
6. Configure ssh

   (in guest) sudo update-rc.d ssh defaults

7. Create the hop user
   
   (in guest) sudo adduser --home /home/hop --shell /bin/bash hop
   
   
8. Generate an ssh-key

   (in guest) ssh-keygen
   
   
9. Copy personnal public key
   (in guest) cat > ~/.ssh/authorized_keys


2. The toolchain
----------------

Getting a correct toolchain for compiling C files executable on the
guest is challenging. Arm processors have different characteristics
(arm <= 6 that support soft floats and arm >= 7 that support hard floats)
and the Raspbian distribution probably uses different versions of the
glibc and gcc compiler than the host. Before proceeding to the Bigloo
cross compilation a compatible toochain must be build. The following
will show the different versions of the tools and libraries that are
involved.

  (in host) ld -v
  (in host) gcc --version
  (in host) ldd --version

  (in guest) ld -v
  (in guest) gcc --version
  (in guest) ldd --version

If these versions are the same, then you can use the native gcc cross
compiler packages provided by your distribution. On debian it can be
installed with:

  (host) sudo apt install gcc-arm-linux-gnueabihf
  
If the versions differ, then you have to install your own cursom
version. This can be done with

  (host) bigloo/arch/raspberry/build-toolchain.sh
  
This script installs the linux header, binutils (the loader), the glibc,
and gcc. This script is automatic but if it fails, it should be easy
to fix as it simply proceeds to a serie of downloads, configures, and
installs.


3. Cross compilation
--------------------

1. Add the path to the arm gcc compiler to the PATH shell variable.

2. Test cross compilation

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
   
3. Bigloo cross-compilation

   (in host) ./configure --cc=arm-linux-gnueabi-gcc --hostsh=$PWD/examples/hostsh/ssh/ssh-copy.sh --build-bindir=$BIGLOOBINDIR --prefix=/opt/bigloo
   (in host) make 
   (in host) make install DESTDIR=/tmp/raspbian
   
The default ssh-copy.sh script uses a configuration compatible with the
arguments passed to qemu and the raspberry configuration (ssh port 
and use credentials).

4. Bundle Bigloo 

   (in host) (cd /tmp/raspbian/opt; tar cvfz bigloo.tgz bigloo)
   (in host) scp -P 2020 bigloo.tgz hop@localhost:
   (in guest) (cd /opt; sudo tar xvfz /home/hop/bigloo.tgz)
   (in guest) sudo chown $USER -R /opt/bigloo
   (in guest) chmod a+rx -R /opt/bigloo
   
