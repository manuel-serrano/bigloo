#!/bin/sh

# configuration and variables
version=3.7b
minor=
repodir=/users/serrano/prgm/distrib
basedir=`dirname $0`
bglconfigureopt=$1

if [ "$REPODIR " != " " ]; then
  repodir=$REPODIR;
fi

pkg=bigloo
bglprefix=/opt/bigloo

maemo=`pkg-config maemo-version --modversion`

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

if [ ! -f $basedir/makedeb.sh ]; then
  echo "ERROR: Cannot find \"$basedir/makedeb.sh\"" >&2 
  echo "current directory is: $PWD" >&2
  echo "exiting..." >&2;
fi
  
# debian specific
for p in control rules postinst changelog; do
  if [ -f $basedir/$p.in ]; then
    cat $basedir/$p.in \
      | sed -e "s/@BIGLOOVERSION@/$version$minor/g" \
      | sed -e "s|@BGLPREFIX@|$bglprefix|g" \
      | sed -e "s|@BGLCONFIGUREOPT@|$bglconfigureopt|g" \
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

if [ -d $repodir/$debian ]; then
  for subpkg in bigloo libbigloo-full bigloo-doc; do
    cp ../"$subpkg"_"$version""$minor"_*.deb $repodir/$debian
  done
fi

cd $curdir

