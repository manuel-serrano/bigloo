#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0a/jigloo/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 14:46:31 1998                          */
#*    Last change :  Thu Feb  5 07:49:16 2026 (serrano)                */
#*    Copyright   :  1998-2026 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build Jigloo. This makefile has to be processed  */
#*    either by Gnu make or Sun make.                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL           = /bin/sh
# the produced executable
JIGLOO_DEST	= $(BOOTLIBDIR)/jigloo.class

#*---------------------------------------------------------------------*/
#*    The source                                                       */
#*---------------------------------------------------------------------*/
SOURCE		= jigloo.java
OBJECTS		= jigloo.class

#*---------------------------------------------------------------------*/
#*    Tout le code ...                                                 */
#*---------------------------------------------------------------------*/
O_OBJECTS       = $(OBJECTS)

POPULATION	= $(SOURCE) README Makefile

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .java .class

#*---------------------------------------------------------------------*/
#*    L'executable `jigloo'                                            */
#*---------------------------------------------------------------------*/
jvm: jigloo
jigloo: 
	@ $(MAKE) $(JIGLOO_DEST)

#*---------------------------------------------------------------------*/
#*    $(JIGLOO_DEST)                                                   */
#*---------------------------------------------------------------------*/
$(JIGLOO_DEST): $(O_OBJECTS)
	@ mv jigloo.class $@
	@ echo "class \e[1;34m$(JIGLOO_DEST)\e[0m done."

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	if [ $(DESTDIR)$(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(JIGLOO_DEST) $(ZIPDIR); \
	   chmod $(MODFILE) $(ZIPDIR)/jigloo.class; \
	   cp $(BOOTBINDIR)/jigloo.sh $(DESTDIR)$(BINDIR)/jigloo; \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/jigloo; \
        fi

uninstall:
	- $(RM) -f $(ZIPDIR)/jigloo.class
	- $(RM) -f $(DESTDIR)$(BINDIR)/jigloo

#*---------------------------------------------------------------------*/
#*    tags                                                             */
#*---------------------------------------------------------------------*/
tags:
	@ etags -o .etags $(SOURCE) 

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/jigloo ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) cleanall

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f $(O_OBJECTS)
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(JIGLOO_DEST)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
touchall:
	@ touch $(SOURCE)
	@ echo "touchall done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=jigloo/%)

#*---------------------------------------------------------------------*/
#*     .java --> .class                                                */
#*---------------------------------------------------------------------*/
.java.class:
	@ $(JAVAC) $*.java
