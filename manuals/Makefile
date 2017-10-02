#*=====================================================================*/
#*    serrano/prgm/project/bigloo/manuals/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jun 15 07:21:19 1998                          */
#*    Last change :  Mon Oct  2 08:43:50 2017 (serrano)                */
#*    Copyright   :  1998-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Bigloo documentation                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compiler and flags ...                                           */
#*---------------------------------------------------------------------*/
DVIPS		= dvips
MAKEDVI		= export TEXINPUTS=; $(TEXI2DVI) $(TEXI2DVIOPT)
BIGLOO		= $(BOOTBINDIR)/bigloo.sh
SKRIBE		= skribe
SKRIBEINFO	= skribeinfo --sui r5rs.info r5rs.sui

TEXI2PDF	= texi2pdf

SFLAGS		= --custom emit-sui=yes
SBFLAGS		= --split-chapter "Standard Library"

#*---------------------------------------------------------------------*/
#*    Sources ...                                                      */
#*---------------------------------------------------------------------*/
MASTER		= bigloo.texi
SOURCES		= modules.texi stdlib.texi rgc.texi lalr.texi match.texi \
                  error.texi struct.texi process.texi \
                  eval.texi macro.texi typing.texi foreign.texi \
                  library.texi object.texi dsssl.texi dsssl.texi \
                  compiler.texi extension.texi socket.texi ssl.texi \
                  bee.texi oldtexinfo.texi cmdline.texi \
                  java.texi pregexp.texi srfi.texi system.texi \
                  core.texi serialization.texi bit.texi hash.texi weakptr.texi \
                  io.texi custom.texi params.texi \
                  thread.texi fthread.texi pthread.texi \
                  database.texi sqlite.texi search.texi digest.texi \
                  internet.texi multimedia.texi mail.texi cryptography.texi \
                  trace.texi crc.texi date.texi cross.texi text.texi csv.texi

BDB_MASTER	= bdb.texi
BDB_SOURCES	=

R5RS_MASTER	= r5rs.texi

MAN		= bigloo.man bglafile.man bgldepend.man bglmake.man bglpp.man \
                  bglprof.man bgltags.man bglmco.man bgljfile.man bdb.man

POPULATION	= Makefile \
                  $(MASTER) $(SOURCES) \
                  $(BDB_MASTER) $(BDB_SOURCES) \
                  $(MAN) \
                  r5rs.texi

#*---------------------------------------------------------------------*/
#*    Everything ...                                                   */
#*---------------------------------------------------------------------*/
all: info ps sui html pdf
allwww: all bigloo.info.tar.gz # bdb.info.tar.gz

dvi: bigloo.dvi
bigloo.dvi: $(MASTER) $(SOURCES) option.texi version.texi
	($(MAKEDVI) $(MASTER)) || $(TEXI2DVI) $(TEXI2DVIOPT) $(MASTER)

ps: bigloo.ps bdb.ps
bigloo.ps: bigloo.dvi
	$(DVIPS) -o bigloo.ps bigloo.dvi

info: bigloo.info
bigloo.info: $(MASTER) $(SOURCES) option.texi version.texi
	$(MAKEINFO) $(MAKEINFOOPT) $(MASTER)

bigloo.info.tar.gz: bigloo.info
	tar cvfz bigloo.info.tar.gz bigloo.info*

#*---------------------------------------------------------------------*/
#*    html                                                             */
#*---------------------------------------------------------------------*/
html: bigloo.html r5rs.html bdb.html
sui: bigloo.sui r5rs.sui bdb.sui

bigloo.html bigloo.sui: r5rs.sui $(MASTER) $(SOURCES) option.texi version.texi
	$(SKRIBEINFO) $(MASTER) | $(SKRIBE) $(SFLAGS) $(SBFLAGS) -o bigloo.html

r5rs.html r5rs.sui: $(R5RS_MASTER)
	$(SKRIBEINFO) $(R5RS_MASTER) | $(SKRIBE) $(SFLAGS) -o r5rs.html

bdb.html bdb.sui: $(BDB_MASTER) $(BDB_SOURCES)
	$(SKRIBEINFO) $(BDB_MASTER) | $(SKRIBE) $(SFLAGS) -o bdb.html

#*---------------------------------------------------------------------*/
#*    pdf                                                              */
#*---------------------------------------------------------------------*/
pdf: bigloo.pdf bdb.pdf
bigloo.pdf: $(MASTER) $(SOURCES) option.texi version.texi
	$(TEXI2PDF) $(TEXI2PDFOPT) $(MASTER)

bdb.pdf: $(BDB_MASTER) $(BDB_SOURCES)
	$(TEXI2PDF) $(TEXI2PDFOPT) $(BDB_MASTER)

#*---------------------------------------------------------------------*/
#*    compile-bee                                                      */
#*---------------------------------------------------------------------*/
compile-bee: info-bee dvi-bee ps-bee

dvi-bee: bdb.dvi
bdb.dvi: $(BDB_MASTER) $(BDB_SOURCES)
	($(MAKEDVI) $(BDB_MASTER)) || $(TEXI2DVI) $(TEXI2DVIOPT) $(BDB_MASTER)

ps-bee: bdb.ps
bdb.ps: bdb.dvi
	$(DVIPS) -o bdb.ps bdb.dvi

info-bee: bdb.info
bdb.info: $(BDB_MASTER) $(BDB_SOURCES)
	$(MAKEINFO) $(MAKEINFOOPT) $(BDB_MASTER)

bdb.info.tar.gz: bdb.info
	tar cvfz bdb.info.tar.gz bdb.info*

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(SOURCES)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	-$(RM) -f *.cp *.cps *.fn *.fns *.ky *.op *.pg *.tp *.vr *.toc *.log *.aux
	find . \( -name '*[~%]' \
                     -o -name '.??*[~%]' \
                     -o -name '#*#' \
                     -o -name '?*#' \
                     -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	-$(RM) -f bigloo.aux bigloo.log bdb.toc bdb.log

distclean:

cleanall: clean
	-$(RM) -f r5rs*.html
	-$(RM) -f bigloo*.html
	-$(RM) -f bigloo.pdf
	-$(RM) -f bigloo.ps
	-$(RM) -f bigloo.dvi
	-$(RM) -f bigloo.sui
	-$(RM) -f r5rs.sui
	-$(RM) -f bdb.info*
	-$(RM) -f bdb*.html
	-$(RM) -f bdb.ps
	-$(RM) -f bdb.dvi
	-$(RM) -f bigloo.info*
	-$(RM) -f option.texi
	-$(RM) -f version.texi

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
pop: 
	@ echo $(POPULATION:%=manuals/%)

#*---------------------------------------------------------------------*/
#*    .tar.gz.uu                                                       */
#*---------------------------------------------------------------------*/
$(DOCUMENTNAME).tar.gz.uu: $(DOCUMENTNAME).tar.gz
	@ echo "uuencoding..."
	@ uuencode (DOCUMENTNAME).tar.gz (DOCUMENTNAME).tar.gz > (DOCUMENTNAME).tar.gz.uu
	@ echo "done."

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib: option.texi version.texi bigloo.html r5rs.html bigloo.info bdb.html bdb.info bdb.sui bigloo.sui r5rs.sui

#*---------------------------------------------------------------------*/
#*    option.texi                                                      */
#*---------------------------------------------------------------------*/
option.texi: ../Makefile.config
	@ echo "@c -- The Bigloo command line -------------------------------------- @c" > $*.texi
	@ echo "@subsection The Bigloo command line" >> $*.texi
	@ echo "@cindex compiler options" >> $*.texi
	@ echo "" >> $*.texi
	@ echo "If no input file is specified, Bigloo enters its interpreter." >> $*.texi
	@ echo "Here is the exhaustive list of Bigloo options and configuration variables:" >> $*.texi
	@ echo "" >> $*.texi
	@ echo "@display" >> $*.texi
	@ $(BIGLOO) -help-manual | tail --lines=+9 >> $*.texi
	@ echo "@end display" >> $*.texi
	@ echo "" >> $*.texi

#*---------------------------------------------------------------------*/
#*    version.texi                                                     */
#*---------------------------------------------------------------------*/
version.texi: ../Makefile.config
	@ echo "@c -- Version and date --------------------------------------------- @c" > $*.texi
	@ echo "@set bigloovers $(RELEASE)" >> $*.texi
	@ echo "@set bigloodate `LC_ALL=C date +'%B %Y'`" >> $*.texi

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install install-html install-man install-info

install: install-html install-man install-info

install-html:
	@if [ ! -d $(DOCDIR) ]; then \
           echo ""; \
           echo "----------------------------------------------------------"; \
           echo "*** ERROR:Can't find directory -- $(DOCDIR)"; \
           echo "Can't install documentations."; \
           echo "----------------------------------------------------------"; \
           echo ""; \
        else \
	  if [ $(DOCDIR) != $(BOOTDIR)/manuals ]; then \
	    echo "cp bigloo*.html $(DOCDIR)"; \
	    echo "cp r5rs*html $(DOCDIR)"; \
	    echo "cp bigloo.sui $(DOCDIR)"; \
	    echo "cp r5rs.sui $(DOCDIR)"; \
	    cp bigloo*.html $(DOCDIR) && \
	    cp r5rs*html $(DOCDIR) && \
	    cp bigloo.sui $(DOCDIR) && \
	    cp r5rs.sui $(DOCDIR) && \
            chmod $(MODFILE) $(DOCDIR)/bigloo*.html && \
            chmod $(MODFILE) $(DOCDIR)/bigloo.sui && \
            chmod $(MODFILE) $(DOCDIR)/r5rs*.html && \
            chmod $(MODFILE) $(DOCDIR)/r5rs.sui; \
          fi \
        fi

install-man:
	@if [ ! -d $(MANDIR) ]; then \
           echo ""; \
           echo "----------------------------------------------------------"; \
           echo "*** ERROR:Can't find directory -- $(MANDIR)"; \
           echo "Can't install man page."; \
           echo "----------------------------------------------------------"; \
           echo ""; \
        else \
	  if [ $(MANDIR) != $(BOOTDIR)/manuals ]; then \
	    cp bigloo.man $(MANDIR)/bigloo.1 && \
	    echo "cp bigloo.man $(MANDIR)/bigloo.1" && \
	    if [ -f $(MANDIR)/bigloo.1 ]; then \
	       chmod $(MODFILE) $(MANDIR)/bigloo.1 && \
	       echo "chmod $(MODFILE) $(MANDIR)/bigloo.1"; \
            fi \
          fi \
        fi

install-info:
	@if [ ! -d $(INFODIR) ]; then \
           echo ""; \
           echo "----------------------------------------------------------"; \
           echo "*** ERROR:Can't find directory -- $(INFODIR)"; \
           echo "Can't install Info pages."; \
           echo "----------------------------------------------------------"; \
           echo ""; \
        else \
	  if [ $(INFODIR) != $(BOOTDIR)/manuals ]; then \
	    if [ -f bigloo.info ]; then \
	      cp bigloo.info* $(INFODIR) && \
	      echo "cp bigloo.info* $(INFODIR)" && \
              if [ -f $(INFODIR)/bigloo.info ]; then \
	         chmod $(MODFILE) $(INFODIR)/bigloo.info* && \
	         echo "chmod $(MODFILE) $(INFODIR)/bigloo.info*"; \
              fi \
            fi && \
	    if [ "$(INSTALLINFO) " = " " -o "$(INSTALLINFODIROPT) " = " " ]; then \
             if [ ! -f $(INFODIR)/dir ]; then \
               echo "-*- Text -*-" > $(INFODIR)/dir; \
	       echo "This is the file .../info/dir, which contains the topmost node of the" >> $(INFODIR)/dir; \
               echo "Info hierarchy.  The first time you invoke Info you start off" >> $(INFODIR)/dir; \
               echo "looking at that node, which is (dir)Top." >> $(INFODIR)/dir; \
               echo "\\" >> $(INFODIR)/dir; \
               echo "File: dir	Node: Top	This is the top of the INFO tree" >> $(INFODIR)/dir; \
               echo "  This (the Directory node) gives a menu of major topics." >> $(INFODIR)/dir; \
               echo "  Typing \"d\" returns here, \"q\" exits, \"?\" lists all INFO commands, \"h\"" >> $(INFODIR)/dir; \
               echo "  gives a primer for first-timers, \"mTexinfo<Return>\" visits Texinfo topic," >> $(INFODIR)/dir; \
               echo "  etc." >> $(INFODIR)/dir; \
               echo "  --- PLEASE ADD DOCUMENTATION TO THIS TREE. (See INFO topic first.) ---" >> $(INFODIR)/dir; \
               echo "" >> $(INFODIR)/dir; \
               echo "* Menu: The list of major topics begins on the next line." >> $(INFODIR)/dir; \
               echo "" >> $(INFODIR)/dir; \
               echo "* bigloo: (bigloo.info). The Bigloo documentation." >> $(INFODIR)/dir; \
               chmod $(MODFILE) $(INFODIR)/dir; \
             else \
              if [ "`grep "bigloo.info" $(INFODIR)/dir` " = " " ]; then \
               echo "* bigloo: (bigloo.info). The Bigloo documentation." >> \
                $(INFODIR)/dir; \
               chmod $(MODFILE) $(INFODIR)/dir; \
              fi \
             fi \
            else \
             $(INSTALLINFO) bigloo.info $(INSTALLINFODIROPT) $(INFODIR)/dir && \
             chmod $(MODFILE) $(INFODIR)/dir && \
             echo "$(INSTALLINFO) bigloo.info $(INSTALLINFODIROPT) $(INFODIR)/dir"; \
            fi \
          fi \
        fi

#*---------------------------------------------------------------------*/
#*    install-bee                                                      */
#*---------------------------------------------------------------------*/
.PHONY: install-bee install-man-bee install-info-bee

install-bee: install-man-bee install-info-bee

install-man-bee:
	@if [ ! -d $(MANDIR) ]; then \
           echo ""; \
           echo "----------------------------------------------------------"; \
           echo "*** ERROR:Can't find directory -- $(MANDIR)"; \
           echo "Can't install man Bee pages."; \
           echo "----------------------------------------------------------"; \
           echo ""; \
        else \
           $(MAKE) do-install-man-bee; \
        fi
        
do-install-man-bee:
	cp bglafile.man $(MANDIR)/bglafile.1;
	if [ -f $(MANDIR)/bglafile.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bglafile.1; \
          fi;
	cp bgljfile.man $(MANDIR)/bgljfile.1;
	if [ -f $(MANDIR)/bgljfile.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bgljfile.1; \
          fi;
	cp bgldepend.man $(MANDIR)/bgldepend.1;
	if [ -f $(MANDIR)/bgldepend.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bgldepend.1; \
          fi;
	cp bglmake.man $(MANDIR)/bglmake.1;
	if [ -f $(MANDIR)/bglmake.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bglmake.1; \
          fi;
	cp bglmco.man $(MANDIR)/bglmco.1;
	if [ -f $(MANDIR)/bglmco.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bglmco.1; \
          fi;
	cp bglpp.man $(MANDIR)/bglpp.1;
	if [ -f $(MANDIR)/bglpp.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bglpp.1; \
          fi;
	cp bglprof.man $(MANDIR)/bglprof.1;
	if [ -f $(MANDIR)/bglprof.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bglprof.1; \
          fi;
	cp bgltags.man $(MANDIR)/bgltags.1;
	if [ -f $(MANDIR)/bgltags.1 ]; then \
	     chmod $(MODFILE) $(MANDIR)/bgltags.1; \
          fi;

install-info-bee:
	@if [ ! -d $(INFODIR) ]; then \
           echo ""; \
           echo "----------------------------------------------------------"; \
           echo "*** ERROR:Can't find directory -- $(INFODIR)"; \
           echo "Can't install Info pages."; \
           echo "----------------------------------------------------------"; \
           echo ""; \
        else \
           $(MAKE) do-install-info-bee; \
        fi

do-install-info-bee:
	- if [ -f bdb.info ]; then \
	     cp bdb.info* $(INFODIR) && \
             if [ -f $(INFODIR)/bdb.info ]; then \
	        chmod $(MODFILE) $(INFODIR)/bdb.info*; \
             fi \
          fi
	- if [ ! -f $(INFODIR)/dir ]; then \
            echo "$(INFODIR)/dir file does not exist, can't install Bee info files"; \
          else \
	    if [ "$(INSTALLINFO) " = " " ]; then \
              if [ "`grep "bdb.info" $(INFODIR)/dir` " = " " ]; then \
                echo "* bdb: (bdb.info). The Bdb documentation." >> \
                $(INFODIR)/dir; \
              fi \
            else \
              $(INSTALLINFO) bdb.info $(INSTALLINFODIROPT) $(INFODIR)/dir; \
            fi \
          fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall uninstall-html uninstall-man uninstall-info

uninstall: uninstall-html uninstall-bee uninstall-man uninstall-info

uninstall-html:
	- $(RM) -f $(DOCDIR)/bigloo*.html
	- $(RM) -f $(DOCDIR)/r5rs*.html
	- $(RM) -f $(DOCDIR)/bigloo.sui
	- $(RM) -f $(DOCDIR)/r5rs.sui

uninstall-man:
	- $(RM) -f $(MANDIR)/bigloo.1;

uninstall-info:
	- $(RM) -f $(INFODIR)/bigloo.info*;
	- if [ "$(UNINSTALLINFO) " != " " ]; then \
             $(INSTALLINFO) --delete $(INFODIR)/bigloo.info \
                            $(INSTALLINFODIROPT) $(INFODIR)/dir; \
          fi

#*---------------------------------------------------------------------*/
#*    uninstall-bee                                                    */
#*---------------------------------------------------------------------*/
.PHONY: uninstall-bee uninstall-man-bee uninstall-info-bee

uninstall-bee: uninstall-man-bee uninstall-info-bee

uninstall-man-bee:
	- $(RM) -f $(MANDIR)/bglafile.1;
	- $(RM) -f $(MANDIR)/bgljfile.1;
	- $(RM) -f $(MANDIR)/bgldepend.1;
	- $(RM) -f $(MANDIR)/bglmake.1;
	- $(RM) -f $(MANDIR)/bglmco.1;
	- $(RM) -f $(MANDIR)/bglpp.1;
	- $(RM) -f $(MANDIR)/bglprof.1;
	- $(RM) -f $(MANDIR)/bgltags.1;

uninstall-info-bee:
	- $(RM) -f $(INFODIR)/bdb.info*;
	- if [ "$(UNINSTALLINFO) " != " " ]; then \
             $(INSTALLINFO) --delete $(INFODIR)/bdb.info \
                            $(INSTALLINFODIROPT) $(INFODIR)/dir; \
          fi
