#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/bmacs/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jun  8 08:36:43 1998                          */
#*    Last change :  Tue Mar 10 17:49:34 2020 (serrano)                */
#*    Copyright   :  1998-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile for the Unified Development environment.            */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, and Tools                                             */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL           = /bin/sh
# etags and etags file name
ETAGS		= etags
TAGSFILE	= .etags
# configure
CONFIGURE	= $(BOOTDIR)/configure

#*---------------------------------------------------------------------*/
#*    Emacs settings                                                   */
#*---------------------------------------------------------------------*/
LOADPATH	= ude bee cee bug lee dbg

XEMACSFLAGS	= -no-site-file
GNUEMACSFLAGS	= --no-site-file

COMMONFLAGS	= -batch -q -f batch-byte-compile
            
#*---------------------------------------------------------------------*/
#*    Sources and Objects                                              */
#*---------------------------------------------------------------------*/
UDE_FILE	= ude-autoload ude-config ude-custom ude-icon ude-tools \
                  ude-error ude-root ude-compile ude-info ude-repl ude-sui \
                  ude-paren ude-makefile ude-mouse ude-profile ude-about \
                  etags-add plugin id-select \
                  ude-ident ude-docline ude-version ude-balloon

BEE_FILE	= bee-autoload bee-config bee-toolbar bee-indent \
                  bee-mode bee-keymap bee-expand bee-flock bee-module \
                  bee-repl bee-tags bee-profile bee-usage \
                  bee-about-icon bee-ibuilder bee-browse \
                  bee-doc

CEE_FILE	= cee-autoload cee-config cee-toolbar cee-indent \
                  cee-profile cee-gdb cee-flock cee-hook cee-keymap \
                  cee-tags cee-profile cee-kbdb

LEE_FILE	= lee-autoload lee-config lee-toolbar lee-keymap \
                  lee-flock lee-hook lee-indent lee-tags

DBG_FILE	= dbg-args dbg-config dbg-filter dbg-stack \
                  dbg-autoload dbg-connect dbg-locals dbg-toolbar \
                  dbg-console dbg-mode dbg \
                  dbg-display dbg-process dbg-complete \
                  dbg-docline dbg-source dbg-about-icon dbg-breakpoint

BUG_FILE	= bug-args bug-connect bug-hooking bug-source \
                  bug-autoload bug-custom bug-images bug-stack \
                  bug-breakpoint bug bug-toolbar \
                  bug-class bug-filter bug-mode bug-xemacs \
                  bug-config bug-gnu-emacs bug-process

OLD_SOURCES 	= bee/bee-bdb.el bee/bee-kbdb.el

EMACS_DIST_FILE	= $(UDE_FILE:%=ude/%) \
                  $(BEE_FILE:%=bee/%) \
                  $(BUG_FILE:%=bug/%) \
                  $(DBG_FILE:%=dbg/%) \
                  $(CEE_FILE:%=cee/%) \
                  $(LEE_FILE:%=lee/%)

EMACS_FILE	= $(EMACS_DIST_FILE) \
                  $(LEE_FILE:%=lee/%)

BMACS_FILE	= bmacs.el \
                  bmacs-gnu-emacs.el \
                  bmacs-xemacs.el xemacs-etags.el

EMACS_DIST_NODIR= $(UDE_FILE:%=%.el) $(UDE_FILE:%=%.elc) \
                  $(BEE_FILE:%=%.el) $(BEE_FILE:%=%.elc) \
                  $(BUG_FILE:%=%.el) $(BUG_FILE:%=%.elc) \
                  $(DBG_FILE:%=%.el) $(DBG_FILE:%=%.elc) \
                  $(CEE_FILE:%=%.el) $(CEE_FILE:%=%.elc) \
                  $(LEE_FILE:%=%.el) $(LEE_FILE:%=%.elc) \
		  $(BMACS_FILE)                  

EMACS_DIST_OBJ	= $(EMACS_DIST_FILE:%=%.el) $(EMACS_DIST_FILE:%=%.elc) \
                  $(BMACS_FILE)
EMACS_OBJ	= $(EMACS_FILE:%=%.elc)
EMACS_SOURCES	= $(EMACS_FILE:%=%.el) $(BMACS_FILE)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile README $(EMACS_SOURCES) $(OLD_SOURCES) \
                  etc/bass-snap.au images/*.xpm

#*---------------------------------------------------------------------*/
#*     Suffixes ...                                                    */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o .el .elc

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: $(EMACSBRAND)

emacs22 emacs23 emacs24 emacs25 emacs26 emacs27:
	(expr=load-path; \
          for p in . $(LOADPATH); do \
             expr="(cons \"../$$p\" (cons \"$$p\" $$expr))"; \
          done; \
	$(MAKE) elc EFLAGS="$(XEMACSFLAGS) -eval '(setq load-path $$expr)' $(COMMONFLAGS)")

emacs21: 
	(expr=load-path; \
         for p in . $(LOADPATH); do \
            expr="(cons \"../$$p\" (cons \"$$p\" $$expr))"; \
         done; \
	$(MAKE) elc EFLAGS="$(XEMACSFLAGS) -eval '(setq load-path $$expr)' $(COMMONFLAGS)")

emacs21.old: 
	(path=; \
         for p in . $(LOADPATH); do \
            path="--directory \"$$p\" $$path"; \
         done; \
	$(MAKE) elc EFLAGS="$(GNUEMACSFLAGS) $$path $(COMMONFLAGS)")

xemacs: 
	(expr=load-path; \
         for p in . $(LOADPATH); do \
            expr="(cons \"$$p\" $$expr)"; \
         done; \
	$(MAKE) elc EFLAGS="$(XEMACSFLAGS) -eval '(setq load-path $$expr)' $(COMMONFLAGS)")

generic: 
	@ echo "*** WARNING: generic (i.e. too old) Emacs not supported."
	@ exit 0

false: 
	@ echo "*** WARNING: Emacs not configured, can't compile."
	@ exit 0

elc: bmacs-config.el $(EMACS_OBJ)

#*---------------------------------------------------------------------*/
#*    compile-bee:                                                     */
#*---------------------------------------------------------------------*/
compile-bee: all

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	-if [ "$(EMACSDIR) " != " " ]; then \
          for f in $(EMACS_DIST_NODIR); do \
            if [ -f $(EMACSDIR)/$$f ]; then \
              $(RM) -f $(EMACSDIR)/$$f; \
            fi; \
          done; \
        fi

#*---------------------------------------------------------------------*/
#*    install ...                                                      */
#*---------------------------------------------------------------------*/
install: install.$(EMACSBRAND)

install.emacs21: doinstall
install.emacs22: doinstall
install.emacs23: doinstall
install.emacs24: doinstall
install.emacs25: doinstall
install.emacs26: doinstall
install.emacs27: doinstall
install.xemacs: doinstall

install.generic:
	@ echo "*** WARNING: generic Emacs(?) not supported, can't install Emacs support."
	@ exit 0

install.false:
	@ echo "*** WARNING: Emacs not configured, can't install Emacs support."
	@ exit 0

doinstall: bmacs-config.el site-lisp
	@ echo "Installing Emacs file in $(DESTDIR)$(EMACSDIR)..."
	@ for f in $(EMACS_DIST_OBJ) bmacs-config.el; do \
          if [ -f $$f ]; then \
            echo "   ...$$f"; \
	    cp $$f "$(DESTDIR)$(EMACSDIR)"; \
          else \
            echo "   ...$$f (can't find)"; \
          fi; \
        done
	(cd "$(DESTDIR)$(EMACSDIR)"; chmod $(MODFILE) *.el*)
	if [ ! -d "$(DESTDIR)$(EMACSDIR)/etc" ]; then \
          mkdir -p "$(DESTDIR)$(EMACSDIR)/etc" && \
            chmod $(MODDIR) "$(DESTDIR)$(EMACSDIR)/etc"; \
        fi
	cp etc/bass-snap.au "$(DESTDIR)$(EMACSDIR)/etc"
	if [ ! -d "$(DESTDIR)$(EMACSDIR)/images" ]; then \
          mkdir -p "$(DESTDIR)$(EMACSDIR)/images" && \
            chmod $(MODDIR) "$(DESTDIR)$(EMACSDIR)/images"; \
        fi
	cp images/*.xpm "$(DESTDIR)$(EMACSDIR)/images"

site-lisp:
	if [ "$(EMACSDIR) " = " " ]; then \
          echo "*** ERROR: Emacs lisp dir not configured!"; \
          exit 1; \
        else \
	  if [ ! -d "$(DESTDIR)$(EMACSDIR)" ]; then \
            mkdir -p "$(DESTDIR)$(EMACSDIR)" && \
              chmod $(MODDIR) "$(DESTDIR)$(EMACSDIR)"; \
          fi \
        fi

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(EMACS_SOURCES)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(ETAGS) $(EMACS_SOURCES) -o $(TAGSFILE)

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean: 
	@- $(RM) -f $(EMACS_OBJ)
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@ $(RM) -f $(TAGSFILE)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare Cigloo for a distribution                                */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/bmacs ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=bmacs/%)

#*---------------------------------------------------------------------*/
#*    bmacs-config.el                                                  */
#*---------------------------------------------------------------------*/
bmacs-config.el: $(BOOTDIR)/configure $(BOOTDIR)/Makefile
	$(BOOTDIR)/configure --bigloo_config.el=$(BOOTDIR)/bmacs/bmacs-config.el \
                             --libdir=$(BOOTLIBDIR) \
                             --zipdir=$(BOOTLIBDIR) \
                             --indent=stdindent \
                             --no-summary \
                             --bootdir=$(BOOTDIR)
	$(RM) -f configure.log

#*---------------------------------------------------------------------*/
#*    .el --> .elc                                                     */
#*---------------------------------------------------------------------*/
.el.elc:
	@ echo --- $*.el ------------------------------------------
	@ $(RM) -f $*.elc
	$(EMACS) $(EWARN) $(EFLAGS) $*.el

