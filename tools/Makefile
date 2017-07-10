#*=====================================================================*/
#*    serrano/prgm/project/bigloo/tools/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Jan 30 15:19:19 1996                          */
#*    Last change :  Sun Jul  9 10:27:29 2017 (serrano)                */
#*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
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

POPULATION	= $(SRC) basm hglog.scm Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
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
#*    $(BIN)/copyright                                                 */
#*---------------------------------------------------------------------*/
.PHONY: copyright
copyright: $(BIN)/copyright

$(BIN)/copyright: copyright.o license.o
	@ $(CC) -o $(BIN)/copyright $(CFLAGS) copyright.o license.o

$(BIN)/libcversion: libcversion.o
	@ $(CC) -o $(BIN)/libcversion $(CFLAGS) libcversion.o

license.c: $(LICENSE)
	@- $(RM) -f $@
	@ echo "/* Automatically generated file (don't edit) */" > $@
	@ echo "char *license() {" >> $@
	@ echo " return \"`cat ../LICENSE`\";" >> $@
	@ echo "}" >> $@

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .c .o

#*---------------------------------------------------------------------*/
#*    .c.o                                                             */
#*---------------------------------------------------------------------*/
.c.o:
	@ echo $*.c:
	@ $(CC) -c $(CFLAGS) $*.c -o $*.o

