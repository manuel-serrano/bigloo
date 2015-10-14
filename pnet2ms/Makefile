#*=====================================================================*/
#*    serrano/prgm/project/bigloo/pnet2ms/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 14:46:31 1998                          */
#*    Last change :  Wed Oct 14 07:14:32 2015 (serrano)                */
#*    Copyright   :  1998-2015 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build pnet2ms.                                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL=         /bin/sh
# the produced executable
PNET2MS_DEST=  $(BGLBUILDBINDIR)/bglpnet2ms$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    Compilation flags                                                */
#*---------------------------------------------------------------------*/
BFLAGS=        -rm -afile .afile -O2 -fsharing -unsafe $(SHRD_BDE_OPT)

#*---------------------------------------------------------------------*/
#*    Object files                                                     */
#*---------------------------------------------------------------------*/
OBJECTS=       pnet2ms.o win32_base.o win32_registry.o win32_helpers.o

SOURCE_FILES=  $(OBJECTS:%.o=%.scm)

POPULATION=    $(SOURCE_FILES) Makefile bigloo.dotnetkey

#*---------------------------------------------------------------------*/
#*     Suffixes ...                                                    */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .c .o

#*---------------------------------------------------------------------*/
#*    The `bglpnet2ms' binary                                          */
#*---------------------------------------------------------------------*/
bglpnet2ms: $(SOURCE_FILES) afile
	@ $(MAKE) $(PNET2MS_DEST)

#*---------------------------------------------------------------------*/
#*    $(PNET2MS_DEST)                                                  */
#*---------------------------------------------------------------------*/
$(PNET2MS_DEST): $(OBJECTS)
	@ echo "Linking:"
	@ $(BIGLOO) $(BFLAGS) $(OBJECTS) -o $(PNET2MS_DEST)
	@- $(STRIP) $(PNET2MS_DEST)
	@ echo "$(PNET2MS_DEST) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/bglpnet2ms$(EXE_SUFFIX) $(DESTDIR)$(BINDIR); \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglpnet2ms$(EXE_SUFFIX); \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglpnet2ms$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    afile                                                            */
#*---------------------------------------------------------------------*/
afile: $(SOURCE_FILES)
	@ echo "   - " $ .afile
	@ $(BGLAFILE) -o .afile $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/pnet2ms ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) cleanall
	@ $(MAKE) afile

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f $(OBJECTS)
	@- $(RM) -f $(OBJECTS:%.o=%.c)
	@- $(RM) -f .afile
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(PNET2MS_DEST)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
touchall:
	@ touch $(SOURCE_FILES)
	@ echo "touchall done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    link ...                                                         */
#*---------------------------------------------------------------------*/
link:
	@ echo "Linking:"
	@ $(BIGLOO) $(BFLAGS) -o $(PNET2MS_DEST) $(OBJECTS) -lm
	@ echo "link done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=pnet2ms/%)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
.scm.o:
	@ $(BIGLOO) $(BFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .c                                                     */
#*---------------------------------------------------------------------*/
.scm.c:
	@ $(BIGLOO) $(BFLAGS) -C $*.scm -o $*.o
