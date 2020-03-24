#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/bglpkg/etc/Makefile           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Jan  2 08:48:29 2007                          */
#*    Last change :  Tue Mar 24 12:23:14 2020 (serrano)                */
#*    Copyright   :  2007-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The generic Makefile for building Bigloo ScmPkg packages.        */
#*    -------------------------------------------------------------    */
#*    This Makefile is intended to be used from the directory          */
#*    mentionned during the bglpkg installation (e.g. the value        */
#*    of the -C command line argument).                                */
#*---------------------------------------------------------------------*/

#*---------------------------------------------------------------------*/
#*    Compilers and flags                                              */
#*---------------------------------------------------------------------*/
BGLAFILEFLAGS	= --suffix spi --module-keyword interface

# Common compilation flags
EFLAGS		=
BYFLAGS		= -dload-init-sym "@DLOADSYM@_s"

# Back-end depend compilation flags
BCFLAGS		= -copt $(CPICFLAGS)
BJVMFLAGS	= -jvm
BDOTFLAGS	= -dotnet

# Flags to build a heap
BHEAPFLAGS	= -unsafe -q -mkaddheap -mkaddlib -L bigloo -I bigloo \
                  -heap-library $(LIBNAME) -afile .afile

BACKEND		= native

LIBNAME		= @LIBNAME@
VERSION		= @VERSION@

CBGLFLAGS	= -I$(DESTDIR)$(LIBDIR)/$(FILDIR)

#*---------------------------------------------------------------------*/
#*    Private variables                                                */
#*---------------------------------------------------------------------*/
BUILDDIR	= @BUILDDIR@
SOURCES		= @SOURCES@
PACKAGES	= @PACKAGES@
BGLLIBDIR	= @LIBDIR@

ODIR		= o

@LIBNAME@	= $(BYFLAGS)

#*---------------------------------------------------------------------*/
#*    The public targets ...                                           */
#*---------------------------------------------------------------------*/
.PHONY: build sources install uninstall clean cleanall

build: build.$(BACKEND)

compile: compile.$(BACKEND)

obj: obj.$(BACKEND)

sources: 
	@ echo $(SOURCES)

install: install-init install.$(BACKEND)

uninstall: uninstall-init uninstall.$(BACKEND)

clean: clean.$(BACKEND)

cleanall: cleanall.$(BACKEND)
	$(RM) -f .afile

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .spi .bgl .c .o .class .obj

#*---------------------------------------------------------------------*/
#*    Standard Bigloo Makefiles                                        */
#*---------------------------------------------------------------------*/
include $(BGLLIBDIR)/Makefile.misc
include $(BGLLIBDIR)/Makefile.config

#*---------------------------------------------------------------------*/
#*    Common targets                                                   */
#*---------------------------------------------------------------------*/
install-init:
	cp $(LIBNAME).init $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).init \
          && chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).init

uninstall-init:
	$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).init

.afile:
	$(BGLAFILE) $(BGLAFILEFLAGS) $(SOURCES) -o $@

#*---------------------------------------------------------------------*/
#*    Native backend targets and rules                                 */
#*---------------------------------------------------------------------*/
HEAP		= $(LIBNAME).heap
INTERFACES	= $(notdir $(SOURCES))
OBJECTS		= $(INTERFACES:%.spi=$(ODIR)/%.o)
EOBJECTS	= $(ODIR)/make_lib.o
BOOTLIBDIR	= $(BUILDDIR)

#*---------------------------------------------------------------------*/
#*    Tuning objects Makefiles                                         */
#*---------------------------------------------------------------------*/
@TUNING_MAKEFILE_OBJECTS@

compile.native: .afile $(ODIR) $(OBJECTS)

build.native: $(HEAP) $(ODIR) $(OBJECTS) lib-c-static lib-c-shared

obj.native:
	@ echo $(OBJECTS:%=$(BUILDDIR)/%)

clean.native:
	$(RM) -rf $(ODIR)
	$(RM) -f $(HEAP)
	$(RM) -f libbigloo$(LIBNAME)_s-$(VERSION).a
	$(RM) -f libbigloo$(LIBNAME)_u-$(VERSION).a
	$(RM) -f libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX)
	$(RM) -f libbigloo$(LIBNAME)_u-$(VERSION).$(SHAREDSUFFIX)
	$(RM) -f libbigloo$(LIBNAME)_e-$(VERSION).$(SHAREDSUFFIX)

cleanall.native:

install.native:
	cp $(LIBNAME).heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).heap \
          && chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).heap; \
	$(MAKE) install-lib LIB=libbigloo$(LIBNAME)_s-$(VERSION);
	$(MAKE) install-shared-lib LIB=libbigloo$(LIBNAME)_e-$(VERSION);
	$(MAKE) install-lnlib LIB=libbigloo$(LIBNAME)_s-$(VERSION) LN=libbigloo$(LIBNAME)_u-$(VERSION);

uninstall.native:
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).heap
	-$(MAKE) uninstall-lib LIB=libbigloo$(LIBNAME)_s-$(VERSION)
	-$(MAKE) uninstall-lib LIB=libbigloo$(LIBNAME)_e-$(VERSION)
	-$(MAKE) uninstall-lib LIB=libbigloo$(LIBNAME)_u-$(VERSION)

$(HEAP): make_lib.scm .afile
	$(RM) -f $@
	$(BIGLOO) -spi $(BCFLAGS) $(EFLAGS) $(BHEAPFLAGS) $< -addheap $@

$(ODIR):
	mkdir -p $@

lib-c-static: libbigloo$(LIBNAME)_s-$(VERSION).a \
              libbigloo$(LIBNAME)_u-$(VERSION).a

lib-c-shared:
	if [ "$(SHAREDLIBRARYSUPPORT)" = "yes" ]; then \
           $(MAKE) lib-c.$(SHAREDSUFFIX); \
        fi

lib-c.$(SHAREDSUFFIX): libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX) \
                       libbigloo$(LIBNAME)_u-$(VERSION).$(SHAREDSUFFIX) \
                       libbigloo$(LIBNAME)_e-$(VERSION).$(SHAREDSUFFIX)

libbigloo$(LIBNAME)_u-$(VERSION).$(SHAREDSUFFIX): libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX)
	$(RM) -f libbigloo$(LIBNAME)_u-$(VERSION).$(SHAREDSUFFIX) && \
        $(LN_S) libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX) \
              libbigloo$(LIBNAME)_u-$(VERSION).$(SHAREDSUFFIX)

libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ $(MAKE) shared-lib \
             FORCELD=true \
             LIBDEST=$@ \
	     LIBPATH=$(BOOTLIBDIR) \
             SONAME=libbigloo$(LIBNAME)_s-$(VERSION).$(SHAREDSUFFIX) \
             CLOSELIBS="-lbigloo_s-$(VERSION)"

libbigloo$(LIBNAME)_e-$(VERSION).$(SHAREDSUFFIX): .afile $(EOBJECTS)
	@ $(MAKE) shared-lib \
             OBJECTS=$(EOBJECTS) \
             LDOPTS="-L. $(LDOPTS)" \
             FORCELD=true \
             LIBDEST=$@ \
	     LIBPATH=$(BOOTLIBDIR) \
             SONAME=libbigloo$(LIBNAME)_e-$(VERSION).$(SHAREDSUFFIX) \
             LDLIBS="-lbigloo$(LIBNAME)_s-$(VERSION) $(LDLIBS)" \
             CLOSELIBS="-lbigloo$(LIBNAME)_s-$(VERSION) -lbigloo_s-$(VERSION)"

libbigloo$(LIBNAME)_u-$(VERSION).a: libbigloo$(LIBNAME)_s-$(VERSION).a
	$(RM) -f libbigloo$(LIBNAME)_u-$(VERSION).a && \
        $(LN_S) libbigloo$(LIBNAME)_s-$(VERSION).a libbigloo$(LIBNAME)_u-$(VERSION).a

libbigloo$(LIBNAME)_s-$(VERSION).a: .afile $(OBJECTS)
	@ $(RM) -f libbigloo$(LIBNAME)_s-$(VERSION).a
	@ $(AR) $(ARFLAGS) libbigloo$(LIBNAME)_s-$(VERSION).a $(OBJECTS)
	@ $(RANLIB) libbigloo$(LIBNAME)_s-$(VERSION).a
	@ echo "$@ done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    Native backend compilation rules                                 */
#*---------------------------------------------------------------------*/
$(ODIR)/%.o: */%.spi */%.scm
	$(BIGLOO) -c -spi $($(*F)) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

$(ODIR)/%.o: */%.scm
	$(BIGLOO) -c $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

$(ODIR)/%.o: %.scm
	$(BIGLOO) -c -spi $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

$(ODIR)/%.o: */%.spi
	$(BIGLOO) -c -spi $($(*F)) $(BFLAGS) $(BCFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    C compilation rules (located in tunings)                         */
#*---------------------------------------------------------------------*/
$(ODIR)/%.o: */*/%.c
	$(CC) -c $(CBGLFLAGS) $(CFLAGS) $(CEFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    jvm backend targets and rules                                    */
#*---------------------------------------------------------------------*/
CLASSES	= $(SOURCES:%=$(ODIR)/class_s/bigloo/$(LIBNAME)/%.class)

compile.jvm: build.jvm

build.jvm: $(ODIR)/class_s/bigloo/$(LIBNAME) $(CLASSES)

obj.jvm:
	@ echo $(CLASSES)

clean.jvm:
	$(RM) -rf $(ODIR)/class_s

$(ODIR)/class_s/bigloo/$(LIBNAME):
	mkdir -p $@

$(ODIR)/class_s/bigloo/$(LIBNAME)/%.class: %.spi  %.scm
	$(BIGLOO) -c -spi $($(*F)) $(BFLAGS) $(BJVMFLAGS) $(EFLAGS) $< -o $@

$(ODIR)/class_s/bigloo/$(LIBNAME)/%.class: %.scm
	$(BIGLOO) -c $(BFLAGS) $(BJVMFLAGS) $(EFLAGS) $< -o $@

#*---------------------------------------------------------------------*/
#*    Tuning Makefiles of rules                                        */
#*---------------------------------------------------------------------*/
@TUNING_MAKEFILE_RULES@

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

install-jvm:
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BUILDLIBDIR) ]; then \
	  cp $(BUILDLIBDIR)/$(LIBNAME).jheap $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).jheap && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).jheap && \
	  cp $(BUILDLIBDIR)/bigloo$(LIBNAME)_s-$(VERSION).zip $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).zip && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).zip && \
	  cp $(BUILDLIBDIR)/bigloo$(LIBNAME)_e-$(VERSION).zip $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).zip && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).zip && \
	  (cd $(DESTDIR)$(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloo$(LIBNAME)_u-$(VERSION).zip && \
           ln bigloo$(LIBNAME)_s-$(VERSION).zip bigloo$(LIBNAME)_u-$(VERSION).zip); \
        fi

install-dotnet:
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BUILDLIBDIR) ]; then \
	  cp $(BUILDLIBDIR)/$(LIBNAME).jheap $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).jheap && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).jheap && \
	  cp $(BUILDLIBDIR)/bigloo$(LIBNAME)_s-$(VERSION).dll $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).dll && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).dll && \
	  cp $(BUILDLIBDIR)/bigloo$(LIBNAME)_e-$(VERSION).dll $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).dll && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).dll && \
	  cp $(BUILDLIBDIR)/bigloo$(LIBNAME)_u-$(VERSION).dll $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_u-$(VERSION).dll && \
	  chmod $(BMASKFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_u-$(VERSION).dll ; \
        fi

uninstall:
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/$(LIBNAME).jheap
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).zip
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).zip
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_u-$(VERSION).zip
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_s-$(VERSION).dll
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_e-$(VERSION).dll
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo$(LIBNAME)_u-$(VERSION).dll
