#!/bin/bash
#*=====================================================================*/
#*    .../project/bigloo/bigloo/arch/raspberry/build-toolchain.sh      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jan  2 06:56:37 2020                          */
#*    Last change :                                                    */
#*    Copyright   :  2020 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    This script install a complete toolchain for raspian cross       */
#*    compilation.                                                     */
#*=====================================================================*/

# inspired by:
# https://solarianprogrammer.com/2018/05/06/building-gcc-cross-compiler-raspberry-pi/

guest=rpi

builddir=build-rpi-gcc
destdir=/opt/rpi-gcc

updatedebian=false
download=true

jobs=8

# ending routine
oldpwd=`pwd`

end() {
  cd $oldpwd
}

trap end EXIT

# debian prerequisite

if [ "$updatedebian " = "true " ]; then
  echo "update debian..."
  sudo apt update
  sudo apt upgrade
  sudo apt install build-essential gawk git texinfo bison file wget
fi  

# checking raspberry version
binutilsv=`ssh rpi "ld -v" | grep -o "[0-9][0-9]*[.][0-9][0-9]*[.][0-9][0-9]*$"`
gccv=`ssh rpi "gcc --version" | grep -o "[0-9][0-9]*[.][0-9][0-9]*[.][0-9][0-9]*$"`
glibcv=`ssh rpi "ldd --version" | grep -o "[0-9][0-9]*[.][0-9][0-9]*$"`

# abi
if [ "`ssh rpi \"cat /proc/cpuinfo\" | grep Features | grep vfp3` " = " " ]; then
  abi=gnueabi
  configurefloat=
else  
  abi=gnueabihf
  configurefloat=--with-float=hard
fi

  abi=gnueabihf
  configurefloat=--with-float=hard

builddir=$builddir-$abi
destdir=$destdir-$abi

echo "building $destdir (gcc=$gccv, glibc=$glibcv, binutils=$binutilsv)..."

export PATH=$destdir/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/bin:/usr/bin:/bin
export LD_LIBRARY_PATH=

mkdir -p $builddir
sudo mkdir -p $destdir && sudo chown $USER $destdir

# downloading the needed packages
if [ "$download " = "true " ]; then
  echo "downloading packages..."
  mkdir -p download
  cd download

  if [ ! -f binutils-$binutilsv.tar.bz2 ]; then
    wget https://ftpmirror.gnu.org/binutils/binutils-$binutilsv.tar.bz2 || exit 1
  fi
  if [ ! -f glibc-$glibcv.tar.bz2 ]; then
    wget https://ftpmirror.gnu.org/glibc/glibc-$glibcv.tar.bz2 || exit 1
  fi
  if [ ! -f gcc-$gccv.tar.gz ]; then
    wget https://ftpmirror.gnu.org/gcc/gcc-$gccv/gcc-$gccv.tar.gz || exit 1
  fi
#wget https://ftpmirror.gnu.org/gcc/gcc-9.2.0/gcc-9.2.0.tar.gz

  cd ..

  tar xf download/binutils-$binutilsv.tar.bz2
  tar xf download/glibc-$glibcv.tar.bz2
  tar xf download/gcc-$gccv.tar.gz
  
  cd gcc-$gccv
  contrib/download_prerequisites
  cd ..
fi  

cd $builddir

git clone --depth=1 https://github.com/raspberrypi/linux

# kernel (step 1)
echo "preparing kernel (step 1)..."
cd linux
KERNEL=kernel7
make ARCH=arm INSTALL_HDR_PATH=$destdir/arm-linux-$abi headers_install
cd ..

# binutils (step 2)
echo "build binutils (step 2)..."
mkdir -p build-binutils
cd build-binutils
../../binutils-$binutilsv/configure --prefix=$destdir --target=arm-linux-$abi --with-arch=armv6 --with-fpu=vfp $configurefloat --disable-multilib
make -j$jobs
make install
cd ..

# gcc (step 3)
echo "build gcc (step 3)..."
mkdir -p build-gcc
cd build-gcc
../../gcc-$gccv/configure --prefix=$destdir --target=arm-linux-$abi --enable-languages=c,c++ --with-arch=armv6 --with-fpu=vfp $configurefloat --disable-multilib
make -j$jobs all-gcc
make install-gcc
cd ..

# glibc (step 4)
echo "build glibc (step 4)..."
mkdir -p build-glibc
cd build-glibc
../../glibc-$glibcv/configure --prefix=$destdir/arm-linux-$abi --build=$MACHTYPE --host=arm-linux-$abi --target=arm-linux-$abi --with-arch=armv6 --with-fpu=vfp $configurefloag --with-headers=$destdir/arm-linux-$abi/include --disable-multilib libc_cv_forced_unwind=yes --disable-werror
make install-bootstrap-headers=yes install-headers
make -j$jobs csu/subdir_lib
install csu/crt1.o csu/crti.o csu/crtn.o $destdir/arm-linux-$abi/lib
arm-linux-$abi-gcc -nostdlib -nostartfiles -shared -x c /dev/null -o $destdir/arm-linux-$abi/lib/libc.so
touch $destdir/arm-linux-$abi/include/gnu/stubs.h
cd ..

# gcc (step 5)
echo "build gcc (step 5)..."
cd build-gcc
make -j$jobs all-target-libgcc
make install-target-libgcc
cd ..

# glibc (step 6)
echo "build glibc (step 6)..."
cd build-glibc
make -j$jobs
make install
cd ..

# gcc (step 7)
echo "build gcc (step 7)..."
cd build-gcc
make -j$jobs
make install
cd ..

