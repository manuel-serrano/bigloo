#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/tools/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Jan 30 15:19:19 1996                          */
#*    Last change :  Wed Dec 13 10:10:50 2023 (serrano)                */
#*    Copyright   :  1996-2023 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The makefile to build args                                       */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
LICENSE		= ../LICENSE

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/
C_FILE	= copyright libcversion

C_OBJ		= $(C_FILE:%=%.o)
C_SRC		= $(C_OBJ:%.o=%.c)

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
OBJ		= $(C_OBJ) $(SCM_OBJ)
SRC		= $(C_SRC) $(SCM_SRC)

POPULATION	= $(SRC) basm hglog.scm Makefile cfg.scm

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: $(BINDIR)/bglcfg

pop:
	@ echo $(POPULATION:%=tools/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f license.c license.o
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	@- $(RM) -f $(OBJ)
	@- $(RM) -f $(DEST) 

cleanall: clean
distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/tools ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) clean

#*---------------------------------------------------------------------*/
#*    $(BINDIR)/copyright                                              */
#*---------------------------------------------------------------------*/
.PHONY: copyright
copyright: $(BINDIR)/copyright

$(BINDIR)/copyright: copyright.o license.o
	$(CC) -o $(BINDIR)/copyright $(CFLAGS) copyright.o license.o

$(BINDIR)/libcversion: libcversion.o
	$(CC) -o $(BINDIR)/libcversion $(CFLAGS) libcversion.o

license.c: $(LICENSE)
	@- $(RM) -f $@
	@ echo "/* Automatically generated file (don't edit) */" > $@
	@ echo "char *license() {" >> $@
	@ echo " return \"`cat ../LICENSE`\";" >> $@
	@ echo "}" >> $@

#*---------------------------------------------------------------------*/
#*    cfg ...                                                          */
#*---------------------------------------------------------------------*/
.PHONY: bglcfg
bglcfg: $(BINDIR)/bglcfg

$(BINDIR)/bglcfg: cfg.o
	$(BIGLOO) cfg.o -o $(BINDIR)/bglcfg

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .c .o .scm

#*---------------------------------------------------------------------*/
#*    .c.o                                                             */
#*---------------------------------------------------------------------*/
.c.o:
	$(CC) -c $(CFLAGS) $*.c -o $*.o

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(BFLAGS) $*.scm -o $*.o

