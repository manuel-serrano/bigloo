#*=====================================================================*/
#*    serrano/prgm/project/bigloo/examples/DNet/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Jan 30 15:19:19 1996                          */
#*    Last change :  Sun Jul  9 10:22:48 2017 (serrano)                */
#*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The makefile to build a small C#/Bigloo example                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
BIGLOO          = $(BOOTBINDIR)/bigloo
BGLOPTFLAGS	= -O +rm -shapem
BFLAGS		= $(BGLOPTFLAGS)
BDNETFLAGS	= $(BFLAGS) -g -dotnet-managed -init-read -dotnet-mono-workaround -dotnet-pnet-workaround

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

#*--- scm -------------------------------------------------------------*/
SCM_FILE	= main

SCM_OBJ		= $(SCM_FILE:%=%.obj)
SCM_SRC		= $(SCM_OBJ:%.obj=%.scm)

#*--- dnet ------------------------------------------------------------*/
DNET_FILE	= lib

DNET_OBJ	= $(DNET_FILE:%=%.obj)
DNET_SRC	= $(DNET_OBJ:%.obj=%.cs)

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
OBJ		= $(SCM_OBJ)
SRC		= $(DNET_SRC) $(SCM_SRC)

POPULATION	= $(SRC:%=examples/DNet/%) \
                  examples/DNet/Makefile \
                  examples/DNet/README

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
test: dnet-test$(SCRIPTEXTENSION)
	./dnet-test$(SCRIPTEXTENSION)

dnet-test$(SCRIPTEXTENSION): $(OBJ)
	$(BIGLOO) $(EFLAGS) \
            -dotnet \
            -dotnet-clr $(DOTNETCLR) \
            -dotnet-ld-style $(DOTNETLDSTYLE) \
            -dotnet-clr-style $(DOTNETCLRSTYLE) \
            -dotnet-clr-opt "$(DOTNETCLROPT)" -o $@ $^ $(DNET_SRC)

pop:
	@ echo $(POPULATION)

#*---------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	@ find . \( -name '*[~%]' \
                 -o -name '.??*[~%]' \
                 -o -name '#*#' \
                 -o -name '?*#' \
                 -o -name \*core \) \
                 -type f -exec $(RM) {} \;   
	@- $(RM) -f $(OBJ)
	@- $(RM) -f dnet-test dnet-test.exe *.jas *.jast *.jar

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .obj

#*---------------------------------------------------------------------*/
#*    .scm.obj                                                         */
#*---------------------------------------------------------------------*/
.scm.obj:
	$(BIGLOO) -dotnet -c $(BFLAGS) $(BDNETFLAGS) $*.scm -o $*.obj
