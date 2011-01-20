#!/bin/sh

# configuration and variables
version=3.6a
minor=
rootdir=/users/serrano
repodir=$rootdir/prgm/distrib
basedir=`dirname $0`

if [ "$REPODIR " != " " ]; then
  repodir=$REPODIR;
fi

maemo=`pkg-config maemo-version --modversion`
pkg=bigloo

bglprefix=/opt/bigloo

if [ $? = 0 ]; then
  debian=maemo`echo $maemo | sed -e "s/[.].*$//"`
else
  debian=debian
fi

curdir=`pwd`

cd $curdir

/bin/rm -rf build.$pkg
mkdir build.$pkg
cd build.$pkg

tar xfz $repodir/bigloo$version$minor.tar.gz
mv bigloo$version$minor bigloo-$version

cp $repodir/bigloo$version$minor.tar.gz bigloo-$version.tar.gz
cd bigloo-$version

dh_make -C gpl -s -e Manuel.Serrano@inria.fr -f ../bigloo-$version.tar.gz <<EOF

EOF
  
# debian specific
for p in control rules postinst changelog; do
  if [ -f $basedir/$p.in ]; then
    cat $basedir/$p.in \
      | sed -e "s/@BIGLOOVERSION@/$version$minor/g" \
      | sed -e "s|@BGLPREFIX@|$bglprefix|g" \
      > debian/$p
  elif [ -f $basedir/$p.$pkg ]; then
    cat $basedir/$p.$pkg \
      | sed -e "s/@BIGLOOVERSION@/$version$minor/g" \
      | sed -e "s|@BGLPREFIX@|$bglprefix|g" > debian/$p
  elif [ -f $basedir/$p ]; then
    cat $basedir/$p \
      | sed -e "s/@BIGLOOVERSION@/$version$minor/g" \
      | sed -e "s|@BGLPREFIX@|$bglprefix|g" > debian/$p
  fi
done

dpkg-buildpackage -rfakeroot && 

for subpkg in bigloo libbigloo-full bigloo-doc; do
  cp ../"$subpkg"_"$version""$minor"_*.deb $repodir/$debian
done

cd $curdir

