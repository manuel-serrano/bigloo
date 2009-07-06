Name: bigloo
Version: @RELEASE@
Release: 6
Copyright: see README file
Group: Development/Languages
Source: ftp://ftp-sop.inria.fr/mimosa/fp/Bigloo/bigloo@RELEASE@.tar.gz
Summary: Bigloo is compiler for the Scheme programming language
%description
Bigloo is a compiler and interpreter for an extended version of the
Scheme programming language. Bigloo allows a full connection between
Scheme and C programs. It delivers fast and small executables.

%prep

%setup -n bigloo@RELEASE@

%build
configure --bindir=/usr/bin --libdir=/usr/lib --mandir=/usr/share/man/man1 --docdir=/usr/share/doc/bigloo-@RELEASE@ --lispdir=/usr/share/xemacs/lisp-site --infodir=/usr/share/info --bee=full
make boot install compile-bee

%install
install -s -m 755 -o 0 -g 0 bin/bigloo /usr/bin/bigloo@RELEASE@
(cd /usr/bin; rm -f bigloo; ln bigloo@RELEASE@ bigloo)
install -s -m 755 -o 0 -g 0 bin/cigloo /usr/bin/cigloo
install -s -m 755 -o 0 -g 0 bin/afile /usr/bin/afile
install -s -m 755 -o 0 -g 0 bin/jfile /usr/bin/jfile
install -s -m 755 -o 0 -g 0 bin/bdepend /usr/bin/bdepend
install -s -m 755 -o 0 -g 0 bin/bmake /usr/bin/bmake
install -s -m 755 -o 0 -g 0 bin/mco /usr/bin/mco
install -s -m 755 -o 0 -g 0 bin/bprof /usr/bin/bprof
install -s -m 755 -o 0 -g 0 bin/bpp /usr/bin/bpp
install -s -m 755 -o 0 -g 0 bin/btags /usr/bin/btags
if [ ! -d /usr/lib/bigloo ]; then 
   mkdir /usr/lib/bigloo
fi
if [ ! -d /usr/lib/bigloo/@RELEASE@ ]; then 
   mkdir /usr/lib/bigloo/@RELEASE@
fi
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo.h /usr/lib/bigloo/@RELEASE@/bigloo.h
install -m 644 -o 0 -g 0 Makefile.config /usr/lib/bigloo/@RELEASE@/Makefile.config
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo_config.h /usr/lib/bigloo/@RELEASE@/bigloo_config.h
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo.heap /usr/lib/bigloo/@RELEASE@/bigloo.heap
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo.jheap /usr/lib/bigloo/@RELEASE@/bigloo.jheap
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloo_s-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloo_s-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloo_s-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloo_s-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloo_s-@RELEASE@.so libbigloo_s-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloo_u-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloo_u-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloo_u-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloo_u-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloo_u-@RELEASE@.so libbigloo_u-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo_s.zip /usr/lib/bigloo/@RELEASE@/bigloo_s.zip
install -m 644 -o 0 -g 0 lib/@RELEASE@/bigloo_u.zip /usr/lib/bigloo/@RELEASE@/bigloo_u.zip
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloogc-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloogc-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloogc-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloogc-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloogc-@RELEASE@.so libbigloogc-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloogc_fth-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloogc_fth-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloogc_fth-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloogc_fth-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloogc_fth-@RELEASE@.so libbigloogc_fth-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/fthread.heap /usr/lib/bigloo/@RELEASE@/fthread.heap
install -m 644 -o 0 -g 0 lib/@RELEASE@/fthread.jheap /usr/lib/bigloo/@RELEASE@/fthread.jheap
install -m 644 -o 0 -g 0 lib/@RELEASE@/fthread.init /usr/lib/bigloo/@RELEASE@/fthread.init
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloofth_s-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloofth_s-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloofth_s-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloofth_s-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloofth_s-@RELEASE@.so libbigloofth_s-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloofth_u-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloofth_u-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloofth_u-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloofth_u-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloofth_u-@RELEASE@.so libbigloofth_u-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/bdl.heap /usr/lib/bigloo/@RELEASE@/bdl.heap
install -m 644 -o 0 -g 0 lib/@RELEASE@/bdl.jheap /usr/lib/bigloo/@RELEASE@/bdl.jheap
install -m 644 -o 0 -g 0 lib/@RELEASE@/bdl.init /usr/lib/bigloo/@RELEASE@/bdl.init
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdl_s-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloobdl_s-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdl_s-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloobdl_s-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloobdl_s-@RELEASE@.so libbigloobdl_s-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdl_u-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloobdl_u-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdl_u-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloobdl_u-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloobdl_u-@RELEASE@.so libbigloobdl_u-@RELEASE@.so)
install -m 644 -o 0 -g 0 lib/@RELEASE@/bdl_s.zip /usr/lib/bigloo/@RELEASE@/bdl_s.zip
install -m 644 -o 0 -g 0 lib/@RELEASE@/bdb.heap /usr/lib/bigloo/@RELEASE@/bdb.heap
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdb_s-@RELEASE@.a /usr/lib/bigloo/@RELEASE@/libbigloobdb_s-@RELEASE@.a
install -m 644 -o 0 -g 0 lib/@RELEASE@/libbigloobdb_s-@RELEASE@.so /usr/lib/bigloo/@RELEASE@/libbigloobdb_s-@RELEASE@.so
(cd /usr/lib; ln -s /usr/lib/bigloo/@RELEASE@/libbigloobdb_s-@RELEASE@.so libbigloobdb_s-@RELEASE@.so)
install -m 644 -o 0 -g 0 manuals/bigloo.man /usr/share/man/man1/bigloo.1
install -m 644 -o 0 -g 0 manuals/afile.man /usr/share/man/man1/afile.1
install -m 644 -o 0 -g 0 manuals/jfile.man /usr/share/man/man1/jfile.1
install -m 644 -o 0 -g 0 manuals/bdepend.man /usr/share/man/man1/bdepend.1
install -m 644 -o 0 -g 0 manuals/bmake.man /usr/share/man/man1/bmake.1
install -m 644 -o 0 -g 0 manuals/mco.man /usr/share/man/man1/mco.1
install -m 644 -o 0 -g 0 manuals/bpp.man /usr/share/man/man1/bpp.1
install -m 644 -o 0 -g 0 manuals/bprof.man /usr/share/man/man1/bprof.1
install -m 644 -o 0 -g 0 manuals/btags.man /usr/share/man/man1/btags.1
chown root.root README ChangeLog
if [ ! -d /usr/share/xemacs ]; then 
   mkdir /usr/share/xemacs
fi
if [ ! -d /usr/share/xemacs/site-lisp ]; then 
   mkdir /usr/share/xemacs/site-lisp
fi
if [ ! -d /usr/share/xemacs/site-lisp/bigloo ]; then 
   mkdir /usr/share/xemacs/site-lisp/bigloo
fi
(cd bmacs; for p in *.el*; do
   install -m 644 -o 0 -g 0 $p /usr/share/xemacs/site-lisp/bigloo/$p;
 done)
(cd bmacs/bee; for p in *.el*; do
   install -m 644 -o 0 -g 0 $p /usr/share/xemacs/site-lisp/bigloo/$p;
 done)
(cd bmacs/cee; for p in *.el*; do
   install -m 644 -o 0 -g 0 $p /usr/share/xemacs/site-lisp/bigloo/$p;
 done)
(cd bmacs/dbg; for p in *.el*; do
   install -m 644 -o 0 -g 0 $p /usr/share/xemacs/site-lisp/bigloo/$p;
 done)
(cd bmacs/ude; for p in *.el*; do
   install -m 644 -o 0 -g 0 $p /usr/share/xemacs/site-lisp/bigloo/$p;
 done)
(cd manuals; for p in bigloo.info*; do
   install -m 644 -o 0 -g 0 $p /usr/share/info/$p;
 done)
/sbin/install-info manuals/bigloo.info /usr/share/info/dir

%files
%doc README ChangeLog
%dir /usr/lib/bigloo/@RELEASE@

/usr/bin/bigloo@RELEASE@
/usr/bin/bigloo
/usr/bin/cigloo
/usr/bin/afile
/usr/bin/jfile
/usr/bin/bdepend
/usr/bin/bmake
/usr/bin/mco
/usr/bin/bprof
/usr/bin/bpp
/usr/bin/btags
/usr/lib/bigloo/@RELEASE@/Makefile.config
/usr/lib/bigloo/@RELEASE@/bigloo.h
/usr/lib/bigloo/@RELEASE@/bigloo_config.h
/usr/lib/bigloo/@RELEASE@/bigloo.heap
/usr/lib/bigloo/@RELEASE@/bigloo.jheap
/usr/lib/bigloo/@RELEASE@/libbigloo_s-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloo_s-@RELEASE@.so
/usr/lib/libbigloo_s-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/libbigloo_u-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloo_u-@RELEASE@.so
/usr/lib/libbigloo_u-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/bigloo_s.zip
/usr/lib/bigloo/@RELEASE@/bigloo_u.zip
/usr/lib/bigloo/@RELEASE@/libbigloogc-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloogc-@RELEASE@.so
/usr/lib/libbigloogc-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/libbigloogc_fth-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloogc_fth-@RELEASE@.so
/usr/lib/libbigloogc_fth-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/fthread.heap
/usr/lib/bigloo/@RELEASE@/fthread.init
/usr/lib/bigloo/@RELEASE@/libbigloofth_s-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloofth_s-@RELEASE@.so
/usr/lib/libbigloofth_s-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/libbigloofth_u-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloofth_u-@RELEASE@.so
/usr/lib/libbigloofth_u-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/fthread_s.zip
/usr/lib/bigloo/@RELEASE@/fthread_u.zip
/usr/lib/bigloo/@RELEASE@/bdl.heap
/usr/lib/bigloo/@RELEASE@/bdl.init
/usr/lib/bigloo/@RELEASE@/libbigloobdl_s-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloobdl_s-@RELEASE@.so
/usr/lib/libbigloobdl_s-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/libbigloobdl_u-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloobdl_u-@RELEASE@.so
/usr/lib/libbigloobdl_u-@RELEASE@.so
/usr/lib/bigloo/@RELEASE@/bdl_s.zip
/usr/lib/bigloo/@RELEASE@/bdb.heap
/usr/lib/bigloo/@RELEASE@/libbigloobdb_s-@RELEASE@.a
/usr/lib/bigloo/@RELEASE@/libbigloobdb_s-@RELEASE@.so
/usr/lib/libbigloobdb_s-@RELEASE@.so
/usr/share/man/man1/bigloo.1
/usr/share/man/man1/afile.1
/usr/share/man/man1/jfile.1
/usr/share/man/man1/bdepend.1
/usr/share/man/man1/bmake.1
/usr/share/man/man1/mco.1
/usr/share/man/man1/bpp.1
/usr/share/man/man1/bprof.1
/usr/share/man/man1/btags.1
/usr/share/info/bigloo.info*
/usr/share/xemacs/site-lisp/bigloo/*.el*
