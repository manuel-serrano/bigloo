#*=====================================================================*/
#*    serrano/prgm/project/bigloo/nanh/bde/bmem/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Apr 20 09:34:24 2003                          */
#*    Last change :  Fri Nov  1 12:52:59 2024 (serrano)                */
#*    Copyright   :  2003-24 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The makefile to build bmem                                       */
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
OBJECT		= $(SRC:%.scm=bmem/%.o)
LIBSRC		= init.c gchook.c trace.c alloc.c backtrace.c hash.c \
                  wrapper.c list.c
LIBINCLUDE	= bmem.h
LIBOBJECT	= $(LIBSRC:%.c=lib/%.o)

POPULATION	= $(SRC:%=bmem/%) $(INCLUDE:%=bmem/%) \
                  Makefile etc/bmemrun.in etc/bmemrun-gdb.in

#*---------------------------------------------------------------------*/
#*    Compilers & Flags                                                */
#*---------------------------------------------------------------------*/
IFLAGS		= -Ilib -I$(BOOTLIBDIR) -I$(LIBBACKTRACEBOOTINCDIR)
CMEMFLAGS	= -DBMEMDEBUG $(CPICFLAGS) -g
BMEMFLAGS	= $(BFLAGS) -g -I bmem $(EFLAGS) -I /tmp/include

LDBMEMOPTS	= 

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .bgl .class .o .c .obj

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
.PHONY: lib bmemrun

all: lib bmemrun

#*--- bmemrun ---------------------------------------------------------*/
bmemrun: $(BOOTBINDIR)/$(BMEMRUN_EXE)
$(BOOTBINDIR)/$(BMEMRUN_EXE): etc/bmemrun.in
	cat etc/bmemrun.in | \
            sed -e "s|@FILDIR@|$(LIBDIR)/$(FILDIR)|g" > \
            $(BOOTBINDIR)/$(BMEMRUN_EXE)
	chmod a+x $(BOOTBINDIR)/$(BMEMRUN_EXE)
etc/bmemrun-gdb: etc/bmemrun-gdb.in
	cat etc/bmemrun-gdb.in | \
            sed -e "s|@FILDIR@|$(LIBDIR)/$(FILDIR)|g" > \
            etc/bmemrun-gdb
	chmod a+x etc/bmemrun-gdb

#*--- lib -------------------------------------------------------------*/
lib: $(BOOTLIBDIR)/bmem/bmem_s.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bmem/bmem_u.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX) \
  $(BOOTLIBDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX)

$(BOOTLIBDIR)/bmem/bmem_u.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_u LIBSUFFIXTHREAD= \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bmem/bmem_u.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bmem_u.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bmem/bmem_s.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_s LIBSUFFIXTHREAD= \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bmem/bmem_s.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bmem_s.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_u LIBSUFFIXTHREAD=_mt \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bmem_u_mt.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

$(BOOTLIBDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX): $(LIBOBJECT) $(BOOTLIBDIR)/bmem
	$(MAKE) bigloo-shared-lib \
           LIBSUFFIX=_s LIBSUFFIXTHREAD=_mt \
	   OBJECTS="$(LIBOBJECT)" \
           FORCELD=true \
           LIBDEST=$(BOOTLIBDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX) \
	   LIBPATH=$(BOOTLIBDIR) \
           SONAME=bmem_s_mt.$(SHAREDSUFFIX) \
	   LDOPTS="$(LDOPTS) $(LDBUILDOPTS) $(LDBMEMOPTS)" \
           EXTRALIBS=$(LIBBACKTRACELIBS)

#*--- lib/bmem --------------------------------------------------------*/
$(BOOTLIBDIR)/bmem:
	mkdir -p $(BOOTLIBDIR)/bmem

#*---------------------------------------------------------------------*/
#*    Pop                                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=bde/bmem/%)
	@ (cd example; $(MAKE) pop)

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	  cp $(BOOTBINDIR)/$(BMEMRUN_EXE) $(DESTDIR)$(BINDIR)/$(BMEMRUN_EXE) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/$(BMEMRUN_EXE); \
        fi
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/bmem/bmem_s.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_s.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_s.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bmem/bmem_u.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_u.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_u.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_s_mt.$(SHAREDSUFFIX); \
	  cp $(BOOTLIBDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX) \
             $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX) || exit 1; \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem_u_mt.$(SHAREDSUFFIX); \
        fi
	cp lib/bmem.h $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem.h
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bmem/bmem.h

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/$(BMEMRUN_EXE)

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
cleanbmem:
	- $(RM) -f .afile .etags
	- $(RM) -f $(OBJECT)

cleanlib:
	- $(RM) -f $(LIBOBJECT)
	- $(RM) -f $(BOOTLIBDIR)/bmem/bmem.$(SHAREDSUFFIX)

clean: cleanbmem cleanlib
	- $(RM) -f $(BOOTBINDIR)/$(BMEMRUN_EXE)

#*---------------------------------------------------------------------*/
#*    Automatic rule                                                   */
#*---------------------------------------------------------------------*/
lib/%.o: lib/%.c $(LIBINCLUDE:%=lib/%)
	$(CC) $(CEFLAGS) $(CPICFLAGS) $(CFLAGS) $(CMEMFLAGS) $(IFLAGS) $< -o $@ -c

bmem/%.o: bmem/%.scm $(INCLUDE:%=bmem/%)
	$(BIGLOO) $(EFLAGS) $(BMEMFLAGS) $< -o $@ -c

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude .etags .afile

ude:
	@ $(MAKE) -f Makefile .afile .etags

.afile:
	@ $(BGLAFILE) -o .afile $(SRC:%=bmem/%)

.etags:
	@ $(BGLTAGS) -o .etags $(SRC:%=bmem/%)

dep:
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(BGLDEPEND) -search-path . -fno-mco $(SRC:%=bmem/%) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

getbinary:
	@ echo ""

getsources:
	@ echo $(SRC:%=bmem/%)

