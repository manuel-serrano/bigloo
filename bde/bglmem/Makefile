#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/bde/bglmem/Makefile            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Apr 20 09:34:24 2003                          */
#*    Last change :  Fri Jul 31 09:36:24 2026 (serrano)                */
#*    Copyright   :  2003-26 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The makefile to build bglmem                                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config
include ../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Objects                                                          */
#*---------------------------------------------------------------------*/
SRC		= 
INCLUDE		= 
OBJECT		= $(SRC:%.scm=bglmem/%.o)
LIBSRC		= init.c gchook.c trace.c alloc.c backtrace.c hash.c \
                  wrapper.c list.c
LIBINCLUDE	= bglmem.h
LIBOBJECT	= $(LIBSRC:%.c=lib/%.o)

#*---------------------------------------------------------------------*/
#*    Compilers & Flags                                                */
#*---------------------------------------------------------------------*/
IFLAGS		= -Ilib -I$(BOOTLIBDIR) -I$(LIBBACKTRACEBOOTINCDIR)
CMEMFLAGS	= -DBGLMEMDEBUG $(CPICFLAGS) -g
BGLMEMFLAGS	= $(BFLAGS) -g -I bglmem $(EFLAGS) -I /tmp/include

LDBGLMEMOPTS	= 

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .bgl .class .o .c .obj

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
.PHONY: lib bglmem 

all: lib bglmem

bglmem: $(BOOTBINDIR)/bglmem
$(BOOTBINDIR)/bglmem: etc/bglmem.in
	cat etc/bglmem.in | \
            sed -e "s|@FILDIR@|$(FILDIR)|g" \
                -e "s|@LIBDIR@|$(LIBDIR)|g" > \
            $(BOOTBINDIR)/bglmem
	chmod a+x $(BOOTBINDIR)/bglmem

#*--- lib -------------------------------------------------------------*/
lib: $(BOOTLIBDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX)

$(BOOTLIBDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bglmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_u LIBSUFFIXTHREAD= \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bglmem_u.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBGLMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bglmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_s LIBSUFFIXTHREAD= \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bglmem_s.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBGLMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bglmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_u LIBSUFFIXTHREAD=_mt \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bglmem_u_mt.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBGLMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bglmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_s LIBSUFFIXTHREAD=_mt \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bglmem_s_mt.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBGLMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

#*--- lib/bglmem --------------------------------------------------------*/
$(BOOTLIBDIR)/bglmem:
	mkdir -p $(BOOTLIBDIR)/bglmem

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	  cp $(BOOTBINDIR)/bglmem $(DESTDIR)$(BINDIR)/bglmem || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglmem; \
        fi
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_s.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_u.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_s_mt.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem_u_mt.$(SHAREDSUFFIX); \
        fi
	cp lib/bglmem.h $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem.h
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglmem/bglmem.h

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglmem

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
cleanbglmem:
	- $(RM) -f .afile .etags
	- $(RM) -f $(OBJECT)

cleanlib:
	- $(RM) -f $(LIBOBJECT)
	- $(RM) -f $(BOOTLIBDIR)/bglmem/bglmem.$(SHAREDSUFFIX)

clean: cleanbglmem cleanlib
	- $(RM) -f $(BOOTBINDIR)/bglmem

#*---------------------------------------------------------------------*/
#*    Automatic rule                                                   */
#*---------------------------------------------------------------------*/
lib/%.o: lib/%.c $(LIBINCLUDE:%=lib/%)
	$(CC) $(CEFLAGS) $(CPICFLAGS) $(CFLAGS) $(CMEMFLAGS) $(IFLAGS) $< -o $@ -c

bglmem/%.o: bglmem/%.scm $(INCLUDE:%=bglmem/%)
	$(BIGLOO) $(EFLAGS) $(BGLMEMFLAGS) $< -o $@ -c

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude .etags .afile

ude:
	@ $(MAKE) -f Makefile .afile .etags

.afile:
	@ $(BGLAFILE) -o .afile $(SRC:%=bglmem/%)

.etags:
	@ $(BGLTAGS) -o .etags $(SRC:%=bglmem/%)

dep:
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(BGLDEPEND) -search-path . -fno-mco $(SRC:%=bglmem/%) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

getbinary:
	@ echo ""

getsources:
	@ echo $(SRC:%=bglmem/%)

