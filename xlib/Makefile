#*=====================================================================*/
#*    serrano/prgm/project/bigloo/xlib/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Dec  4 10:25:59 1995                          */
#*    Last change :  Sun Jul  9 10:28:20 2017 (serrano)                */
#*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    This makefile build the X11 Bigloo declaration and the examples  */
#*    using the Xlib with Bigloo.                                      */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*     Les flags ...                                                   */
#*---------------------------------------------------------------------*/
INCLUDEDIR	= /usr/include
XINCLUDEDIR	= $(INCLUDEDIR)/X11
CGFLAGS		= -gcc -w -type Status -type Bool -type wchar_t -no-directives -macro -no-define-fun
CPPFLAGS	= -DNeedFunctionPrototypes=1 -D_Xconst= -I$(XINCLUDEDIR) -I$(INCLUDEDIR)
SHELL           = /bin/sh
POPULATION	= Makefile README xlib-macro.sch example
                  
#*---------------------------------------------------------------------*/
#*    Les flags par defauts (pour pouvoir faire make *.o)              */
#*---------------------------------------------------------------------*/
BDBGFLAGS	= -afile .afile -g2 -cg -O -cc $(CC) +rm -fsharing
DOPTFLAGS	= -afile .afile -O2 -cc $(CC) -rm -fsharing -unsafe
BFLAGS		= $(BDBGFLAGS)

#*---------------------------------------------------------------------*/
#*    examples                                                         */
#*---------------------------------------------------------------------*/
XEXAMPLES	= hello hello2

#*---------------------------------------------------------------------*/
#*    The sources                                                      */
#*---------------------------------------------------------------------*/
#*--- Xincludes -------------------------------------------------------*/
XINCLUDE	= X Xlib Xutil Xatom Xresource XShm cursorfont keysym

XINCLUDE_H	= $(XINCLUDE:%=include/%.h)
XINCLUDE_EH	= $(XINCLUDE:%=include/%.eh)

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .sch .c .h .eh .o

#*---------------------------------------------------------------------*/
#*    lib                                                              */
#*---------------------------------------------------------------------*/
lib: doinclude

#*---------------------------------------------------------------------*/
#*    include                                                          */
#*---------------------------------------------------------------------*/
include:
	mkdir -p include

doinclude: include $(XINCLUDE_H) $(XINCLUDE_EH) xlib.sch

#*---------------------------------------------------------------------*/
#*    xlib.sch                                                         */
#*---------------------------------------------------------------------*/
xlib.sch: Makefile xlib-macro.sch 
	@ echo xlib.sch:
	@ $(RM) -f xlib.sch
	@ echo "(directives " > xlib.sch
	@ echo " (extern  (type status long \"long\")" >> xlib.sch
	@ echo "          (type wchar_t ulong \"wchar_t\")" >> xlib.sch
	@ echo "          (include \"/usr/include/sys/types.h\")" >> xlib.sch
	@ echo "          (include \"stdio.h\")" >> xlib.sch
	@ echo "          (include \"X11/Xlib.h\")" >> xlib.sch
	@ echo "          (include \"X11/Xutil.h\")" >> xlib.sch
	@ echo "          (include \"X11/extensions/XShm.h\")" >> xlib.sch
	@ echo "          )" >> xlib.sch
	@ $(CIGLOO) -v $(CGFLAGS) $(XINCLUDE_EH) >> xlib.sch
	@ cat xlib-macro.sch >> xlib.sch
	@ echo "   )" >> xlib.sch

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/xlib ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) clean

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: xlib.sch
	@ cp xlib.sch `$(BIGLOO) -s -eval '(begin (print (car (last-pair *lib-dir*))) (exit 1))'`

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@ echo "Cleaning..."
	@- if [ -d include ]; then \
           find include \( -name '*~' -o -name '*.BAK' \) -exec \rm -f {} \; ;\
           fi
	@- $(RM) -f xlib.sch *~ *.BAK
	@- $(RM) -f include/*.eh
	@ echo "done."

cleanall: clean
distclean: cleanall

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=xlib/%)

#*---------------------------------------------------------------------*/
#*    .h --> .eh                                                       */
#*---------------------------------------------------------------------*/
.h.eh:
	@ echo $*.h:
	@ gcc $(CPPFLAGS) -E -dD -P $*.h > $*.eh

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
.scm.o:
	@ $(BIGLOO) -c $(BFLAGS) $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*    .h --> .h                                                        */
#*---------------------------------------------------------------------*/
include/X.h: $(XINCLUDEDIR)/X.h
	@ echo $(XINCLUDEDIR)/X.h:
	@ cp $(XINCLUDEDIR)/X.h include

include/Xatom.h: $(XINCLUDEDIR)/Xatom.h
	@ echo $(XINCLUDEDIR)/Xatom.h:
	@ cp $(XINCLUDEDIR)/Xatom.h include

include/Xlib.h:  $(XINCLUDEDIR)/Xlib.h
	@ echo $(XINCLUDEDIR)/Xlib.h:
	@ cat $(XINCLUDEDIR)/Xlib.h | egrep -v "#include <(sys|X11/X.h|stddef)" > include/Xlib.h

include/Xresource.h: $(XINCLUDEDIR)/Xresource.h
	@ echo $(XINCLUDEDIR)/Xresource.h:
	@ cat $(XINCLUDEDIR)/Xresource.h | egrep -v "_XFUNCPROTO(BEGIN|END)" > include/Xresource.h

include/Xutil.h:  $(XINCLUDEDIR)/Xutil.h
	@ echo $(XINCLUDEDIR)/Xutil.h:
	@ cat $(XINCLUDEDIR)/Xutil.h | egrep -v "_XFUNCPROTO(BEGIN|END)" > include/Xutil.h

include/XShm.h:  $(XINCLUDEDIR)/extensions/XShm.h
	@ echo $(XINCLUDEDIR)/extensions/XShm.h:
	@ cat $(XINCLUDEDIR)/extensions/XShm.h | egrep -v "_XFUNCPROTO(BEGIN|END)" > include/XShm.h

include/cursorfont.h:  $(XINCLUDEDIR)/cursorfont.h
	@ echo $(XINCLUDEDIR)/cursorfont.h:
	@ cp $(XINCLUDEDIR)/cursorfont.h include

include/keysym.h:  $(XINCLUDEDIR)/keysym.h
	@ echo $(XINCLUDEDIR)/keysym.h:
	@ cp $(XINCLUDEDIR)/keysym.h include

