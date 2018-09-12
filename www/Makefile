#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/www/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Oct  5 01:24:16 2001                          */
#*    Last change :  Wed Sep 12 10:33:02 2018 (serrano)                */
#*    Copyright   :  2001-18 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that handles the Bigloo web page.                   */
#*=====================================================================*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Official release (not necessarily the configured release)        */
#*---------------------------------------------------------------------*/
OFFICIALRELEASE	= 4.3c
OFFICIALDATE	= September 2018

#*---------------------------------------------------------------------*/
#*    Destination                                                      */
#*---------------------------------------------------------------------*/
INDESHTTP	= www-sop.inria.fr/indes/fp
HOSTHTTP	= $(INDESHTTP)
HOSTHTTPDIR	= /users/serrano/public_html/bigloo
HOSTURL		= http://$(HOSTHTTP)/Bigloo
BUGLOOURL	= http://$(HOSTHTTP)/Bugloo
SCMPKGURL	= http://hop.inria.fr/hop/scmpkg
ARCHIVEURL1	= https://sympa.inria.fr/sympa/arc/bigloo
ARCHIVEURL2	= http://news.gmane.org/gmane.lisp.scheme.bigloo
WIKIURL		= http://bigloo.pbwiki.com

KAOLINFTP	= kaolin.unice.fr
INDESFTP	= ftp-sop.inria.fr
HOSTFTP		= $(INDESFTP)

KAOLINFTPDIR	= /pub/Bigloo
INDESFTPDIR	= /indes/fp/Bigloo
HOSTFTPDIR	= $(INDESFTPDIR)

KAOLINFTPHOMEDIR= /home/ftp
INDESFTPHOMEDIR = /net/servers/ftp-sop/indes/fp	
HOSTFTPHOMEDIR	= $(INDESFTPHOMEDIR)

HOSTFTPAPPSDIR	= apps
HOSTFTPLIBSDIR	= libraries
HOSTFTPCONTSDIR	= contribs

SRFIURL		= http://srfi.schemers.org

#*---------------------------------------------------------------------*/
#*    Rpm configuration                                                */
#*---------------------------------------------------------------------*/
LIBCVERSION	= 6
RPMARCH		= i686
RPMDISTRIB	= Mandrake 8.1

#*---------------------------------------------------------------------*/
#*    File to be copied                                                */
#*---------------------------------------------------------------------*/
REPOSITORY	= $$HOME/prgm/distrib

BIGLOOTARGZ	= bigloo$(OFFICIALRELEASE).tar.gz
BIGLOOZIP	= bigloo`echo $(OFFICIALRELEASE) | sed -e 's/[.]//'`.zip
BIGLOOWINZIP	= bigloo2.6a-Win32-build3.zip
BIGLOORPM	= bigloo-$(OFFICIALRELEASE)-$(LIBCVERSION).$(RPMARCH).rpm
BIGLOODEB	= http://packages.qa.debian.org/b/bigloo.html
MAEMOFTP	= ftp://ftp-sop.inria.fr/indes/fp/Hop/maemo/dists/freemantle/free/binary-armel/
BIGLOOMAEMO	= $(MAEMOFTP)/bigloo_$(OFFICIALRELEASE)_armel.deb \
                  $(MAEMOFTP)/libbigloo-full_$(OFFICIALRELEASE)_armel.deb
ARMFTP		= ftp://ftp-sop.inria.fr/indes/fp/Bigloo/armel-debian
BIGLOOARM	= $(ARMFTP)/bigloo_$(OFFICIALRELEASE)_armel.deb \
                  $(ARMFTP)/libbigloo-full_$(OFFICIALRELEASE)_armel.deb
MACOSXFTP	= ftp://ftp-sop.inria.fr/indes/fp/Bigloo
BIGLOOMACOSX	= $(MACOSXFTP)/macosx/bigloo-$(OFFICIALRELEASE).dmg
BIGLOOFDR	= fedora
BIGLOOWIN32	= http://www.yannis.bres.name/Bigloo
BIGLOOWIN32b	= http://code.google.com/p/winbigloo/
BIGLOOMINGW	= http://jwd.sandjsite.net/projects/mingw-bigloo
BIGLOOMINGW64	= https://github.com/donaldsonjw/bigloo/wiki/Building-Bigloo-Under-Mingw64

CHANGELOG	= $(BOOTDIR)/ChangeLog

BGLSTONETARGZ	= bglstone.tar.gz

LIBRARIES	= lib-example.tar.gz \
                  format.tar.gz

CONTRIBS	= apropos.tar.gz \
                  scmdoc.tar.gz \
                  slibinit.tar.gz \
                  stepper-0.1.tar.gz \
                  iread.tgz

#*---------------------------------------------------------------------*/
#*    Compilers and tools                                              */
#*---------------------------------------------------------------------*/
SKRIBE	     	= skribe

#*---------------------------------------------------------------------*/
#*    Bglstone results                                                 */
#*---------------------------------------------------------------------*/
BGLSTONE	= $$HOME/prgm/project/bglstone
RESDIR		= $(BGLSTONE)/results
STATINFO	= $(BGLSTONE)/bin/statinfo
R2B		= $(BGLSTONE)/bin/r2b
BCHART		= $(BGLSTONE)/bin/bchart
MAX_SCALE_VALUE	= 8
BCHAR_HEIGHT	= 12
BENCHMARKS	= -only bigloo -only nucleic -only beval

#*---------------------------------------------------------------------*/
#*    Benchmark names                                                  */
#*---------------------------------------------------------------------*/
NAMES		= ((\"i686-1002\" . \"Pentium III 1000Mhz (coppermine)\") \
		   (\"i586-233\" . \"Pentium 233Mhz\") \
                   (\"i686-1412\" . \"AMD Tbird 1400Mhz\") \
                   (\"i686-1600\" . \"AMD Tbird 1600Mhz bi-pro\") \
                   (\"i686-262\" . \"Pentium II 262Mhz\") \
                   (\"i686----\" . \"WMWare/AMD Tbird 1400Mhz\") \
                   (\"i686-847\" . \"Pentium III 850Mhz (mobile)\") \
                   (\"i686-860\" . \"Crusoe 866Mhz (TM5800)\") \
                   (\"i686-2196\" . \"Pentium IV 2200Mhz (xeon)\") \
                   (\"alpha----\" . \"Alpha 500Mhz\") \
                   (\"sun4u----\" . \"Sparc 300Mhz\") \
                   (\"Power----\" . \"G4 MacOS X 450Mhz\"))

BNAME		= "(lambda (t c) \
                      (let* ((arch (cadr (assq 'arch c))) \
                             (mhz (cadr (assq 'mhz c))) \
                             (os (cadr (assq 'os c))) \
                             (key (string-append arch \"-\" mhz)) \
                             (as (assoc key '$(NAMES)))) \
                         (if (pair? as) \
                             (string-append (cdr as) \" (\" os \")\") \
                             (error \"r2b\" \"Can't find name for\" key))))"

#*---------------------------------------------------------------------*/
#*    Skribe variables                                                 */
#*---------------------------------------------------------------------*/
SKRIBEVARS	= --eval "(define *bigloo-version* \"$(OFFICIALRELEASE)\")" \
                  --eval "(define *bigloo-date* \"$(OFFICIALDATE)\")" \
                  --eval "(define *bigloo.tar.gz* \"$(BIGLOOTARGZ)\")" \
                  --eval "(define *bigloo.rpm* \"$(BIGLOORPM)\")" \
                  --eval "(define *bigloo.deb* \"$(BIGLOODEB)\")" \
                  --eval "(define *bigloo.maemo* \"$(BIGLOOMAEMO)\")" \
                  --eval "(define *bigloo.arm* \"$(BIGLOOARM)\")" \
                  --eval "(define *bigloo.fdr* \"$(BIGLOOFDR)\")" \
                  --eval "(define *bigloo.zip* \"$(BIGLOOZIP)\")" \
                  --eval "(define *bigloo.winzip* \"$(BIGLOOWINZIP)\")" \
                  --eval "(define *bigloo.win32* \"$(BIGLOOWIN32)\")" \
                  --eval "(define *bigloo.win32b* \"$(BIGLOOWIN32b)\")" \
                  --eval "(define *bigloo.mingw* \"$(BIGLOOMINGW)\")" \
                  --eval "(define *bigloo.mingw64* \"$(BIGLOOMINGW64)\")" \
                  --eval "(define *bigloo.macosx* \"$(BIGLOOMACOSX)\")" \
                  --eval "(define *bigloo-changelog* \"$(CHANGELOG)\")" \
                  --eval "(define *ftp-dir* \"$(HOSTFTPDIR)\")" \
                  --eval "(define *http-host* \"$(HOSTHTTP)\")" \
                  --eval "(define *ftp-host* \"$(HOSTFTP)\")" \
                  --eval "(define *repository* \"$(REPOSITORY)\")" \
                  --eval "(define *apps-dir* \"$(HOSTFTPAPPSDIR)\")" \
                  --eval "(define *libs-dir* \"$(HOSTFTPLIBSDIR)\")" \
                  --eval "(define *contribs-dir* \"$(HOSTFTPCONTSDIR)\")" \
                  --eval "(define *bglstone.tar.gz* \"$(BGLSTONETARGZ)\")" \
                  --eval "(define *bigloo.url* \"$(HOSTURL)\")" \
                  --eval "(define *bugloo.url* \"$(BUGLOOURL)\")" \
                  --eval "(define *scmpkg.url* \"$(SCMPKGURL)\")" \
                  --eval "(define *r5rs.url* \"$(HOSTURL)/doc/r5rs.html\")" \
                  --eval "(define *srfi.url* \"$(SRFIURL)\")" \
                  --eval "(define *wiki.url* \"$(WIKIURL)\")" \
                  --eval "(define *rpm.distrib* \"$(RPMDISTRIB)\")" \
	 	  --eval "(define *mailing-archive.url1* \"$(ARCHIVEURL1)\")" \
	 	  --eval "(define *mailing-archive.url2* \"$(ARCHIVEURL2)\")"

#*---------------------------------------------------------------------*/
#*    Destination and sources                                          */
#*---------------------------------------------------------------------*/
BASE		= bigloo
BIGLOOHTML	= $(BASE).html
DOCUMENT	= $(BASE).skb

SOURCES		= $(DOCUMENT) \
                  download.skb \
                  documentation.skb \
                  features.skb \
                  benchmark.skb \
                  technical.skb \
                  ide.skb \
                  applications.skb \
                  libraries.skb \
                  contribs.skb \
                  log.skb \
                  old-bigloo.skb \
                  changelog.skr

EXAMPLES	= src/jvm.scm \
                  src/c.scm \
                  src/object.scm \
                  src/thread.scm \
                  src/exception.scm \
                  src/io.scm \
                  src/socket.scm \
                  src/process.scm \
                  src/library.scm

DOC		= doc/bigloo.html \
                  doc/r5rs.html \
                  doc/bigloo.ps \
                  doc/bigloo.pdf \
                  doc/bigloo.dvi \
                  doc/bigloo.info.tar.gz

BENCHGIF	= bench/c.png \
                  bench/jvm.png

SCREENSHOTS	= screenshots/bee-bdb.gif \
                  screenshots/bee-error.gif \
                  screenshots/bee-online.gif \
                  screenshots/bee-plain.gif

STYLES		= 

POPULATION	= $(SOURCES) $(EXAMPLES) $(SCREENSHOTS) $(STYLES) \
                  bigloo.css Makefile

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: $(BIGLOOHTML) $(DOC)
$(BIGLOOHTML): $(SOURCES) $(EXAMPLES) $(STYLES) $(CHANGELOG) Makefile # $(BENCHGIF)
	$(SKRIBE) $(DOCUMENT) $(SKRIBEVARS) -o $(BIGLOOHTML)

old: old-bigloo.html
old-bigloo.html: old-bigloo.skb
	$(SKRIBE) old-bigloo.skb $(SKRIBEVARS) -o old-bigloo.html

forcedoc: 
	(cd ../manuals; $(MAKE) allwww)
	$(MAKE) $(DOC)

#*---------------------------------------------------------------------*/
#*    Install                                                          */
#*---------------------------------------------------------------------*/
install: all $(DOC)
	cleanup
	$(MAKE) install.start
	$(MAKE) install.html
	$(MAKE) install.download
	$(MAKE) install.doc
	$(MAKE) install.libraries
	$(MAKE) install.contribs
	$(MAKE) install.apps
	$(MAKE) install.stop

#*--- install.start ---------------------------------------------------*/
install.start:
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod u+w -R ."
	ssh $(HOSTHTTP) "mkdir -p $(HOSTHTTPHOMEDIR)$(HOSTHTTPDIR); cd $(HOSTHTTPHOMEDIR)$(HOSTHTTPDIR); chmod u+w -R ."

#*--- install.stop ----------------------------------------------------*/
install.stop:
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod a-w -R ."
	ssh $(HOSTHTTP) "cd $(HOSTHTTPHOMEDIR)$(HOSTHTTPDIR); chmod a-w -R ."

#*--- install.html ----------------------------------------------------*/
install.html:
	for p in $(BASE)*.html; do \
	  scp $$p $(HOSTHTTP):$(HOSTHTTPDIR)/$$p; \
        done
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); chmod a+r $(BASE)*.html"
	for d in bench screenshots; do \
	  scp -r $$d $(HOSTHTTP):$(HOSTHTTPDIR);\
	done
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); chmod a+rx bench screenshots; chmod a+r bench/* screenshots/*"
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); rm -f index.html; ln -s $(BASE).html index.html"

#*--- install.download ------------------------------------------------*/
install.download:
	for p in $(BIGLOOTARGZ) $(BIGLOOZIP) $(BIGLOORPM); do \
	   scp $(REPOSITORY)/$$p $(HOSTFTP):$(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$$p; \
	done
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod a+r $(BIGLOOTARGZ) $(BIGLOOZIP) $(BIGLOORPM)"

#*--- install.doc -----------------------------------------------------*/
install.doc: doc/bigloo.html doc/r5rs.html doc/bigloo.ps doc/bigloo.pdf doc/bigloo.info.tar.gz
	scp -r doc $(HOSTHTTP):$(HOSTHTTPDIR)
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); chmod a+rx doc; chmod a+r doc/*"

#*--- install.libraries -----------------------------------------------*/
install.libraries:
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); mkdir -p $(HOSTFTPLIBSDIR)"
	for p in $(LIBRARIES); do \
	   scp $(HOSTFTPLIBSDIR)/$$p $(HOSTFTP):$(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPLIBSDIR)/$$p; \
	done
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod a+rx $(HOSTFTPLIBSDIR); cd $(HOSTFTPLIBSDIR); chmod a+r $(LIBRARIES)"

#*--- install.contribs ------------------------------------------------*/
install.contribs:
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); mkdir -p $(HOSTFTPCONTSDIR)"
	for p in $(CONTRIBS); do \
	   scp $(HOSTFTPCONTSDIR)/$$p $(HOSTFTP):$(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPCONTSDIR)/$$p; \
	done
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod a+rx $(HOSTFTPCONTSDIR); cd $(HOSTFTPCONTSDIR); chmod a+r $(CONTRIBS)"

#*--- install.apps ----------------------------------------------------*/
install.apps:
	for p in $(BGLSTONETARGZ); do \
	   scp $(REPOSITORY)/$$p $(HOSTFTP):$(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPAPPSDIR)/$$p; \
	done
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); chmod a+rx $(HOSTFTPAPPSDIR); cd $(HOSTFTPAPPSDIR); chmod a+r $(BGLSTONETARGZ)"

#*---------------------------------------------------------------------*/
#*    Uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); rm -f $(BASE)*.html"
	ssh $(HOSTHTTP) "cd $(HOSTHTTPDIR); rm -rf doc"
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR); rm -f $(BIGLOOTARGZ) $(BIGLOOZIP) $(BIGLOORPM)"
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPAPPSDIR); rm -f $(BGLSTONETARGZ)"
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPLIBSDIR); rm -f $(LIBRARIES)"
	ssh $(HOSTFTP) "cd $(HOSTFTPHOMEDIR)$(HOSTFTPDIR)/$(HOSTFTPCONTSDIR); rm -f $(CONTRIBS)"
		  
#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=www/%)

#*---------------------------------------------------------------------*/
#*    clean:                                                           */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall

clean:
	$(RM) -f $(BASE)*.html $(BENCHGIF) 
	$(RM) -f old-bigloo.html
	$(RM) -f *~

cleanall: clean
	rm -rf doc

#*---------------------------------------------------------------------*/
#*    Benchmarks                                                       */
#*---------------------------------------------------------------------*/
bench/c.png:
	$(RM) -f /tmp/c.bc /tmp/c.eps
	$(R2B) -R $(RESDIR)/base/base.stat -t "C back-ends vs C back-end of `$(STATINFO) $(RESDIR)/base/base.stat arch` (`$(STATINFO) $(RESDIR)/base/base.stat mhz`MHz)" $(RESDIR)/[^b]*/bigloo.stat -o /tmp/c.bc -bname $(BNAME) $(BENCHMARKS)
	$(BCHART) -h $(BCHAR_HEIGHT) /tmp/c.bc -lv -max $(MAX_SCALE_VALUE) -o /tmp/c.eps -r 45
	convert /tmp/c.eps bench/c.png

bench/jvm.png:
	$(RM) -f /tmp/jvm.bc /tmp/jvm.eps
	$(R2B) -R $(RESDIR)/base/base.stat -t "JVM (jdk1.3) back-ends vs C back-end of `$(STATINFO) $(RESDIR)/base/base.stat arch` (`$(STATINFO) $(RESDIR)/base/base.stat mhz`MHz)" $(RESDIR)/[^b]*/bigloo-jvm.stat -o /tmp/jvm.bc -bname $(BNAME) $(BENCHMARKS)
	$(BCHART) -h $(BCHAR_HEIGHT) /tmp/jvm.bc -lv -max $(MAX_SCALE_VALUE) -o /tmp/jvm.eps -r 45
	convert /tmp/jvm.eps bench/jvm.png

#*---------------------------------------------------------------------*/
#*    doc                                                              */
#*---------------------------------------------------------------------*/
doc:
	mkdir doc

doc/bigloo.html: doc ../manuals/bigloo.html
	@ cp ../manuals/bigloo*.html doc

doc/r5rs.html: doc ../manuals/r5rs.html
	@ cp ../manuals/r5rs*.html doc

doc/bigloo.ps: doc ../manuals/bigloo.ps
	@ cp ../manuals/bigloo.ps doc/bigloo.ps

doc/bigloo.pdf: doc ../manuals/bigloo.pdf
	@ cp ../manuals/bigloo.pdf doc/bigloo.pdf

doc/bigloo.dvi: doc ../manuals/bigloo.dvi
	@ cp ../manuals/bigloo.dvi doc/bigloo.dvi

doc/bigloo.info.tar.gz: doc ../manuals/bigloo.info.tar.gz
	@ cp ../manuals/bigloo.info.tar.gz doc/bigloo.info.tar.gz

../manuals/bigloo.info.tar.gz:
	(cd ../manuals; $(MAKE) bigloo.info.tar.gz)
