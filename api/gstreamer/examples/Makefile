#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/gstreamer/examples/Makefile      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:12:34 2017 (serrano)                */
#*    Copyright   :  2002-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Gstreamer examples                     */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.buildconfig 
include ../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BCFLAGS		= 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
SOURCES		= typefind bgst_inspect bgst_launch bgst_play \
                  bgst_repl bgst_mm bgst_png

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES:%=%.scm) Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: $(SOURCES)

typefind: typefind.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_inspect: bgst_inspect.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_launch: bgst_launch.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_play: bgst_play.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_repl: bgst_repl.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_mm: bgst_mm.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

bgst_png: bgst_png.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

pop:
	@ echo $(POPULATION:%=gstreamer/examples/%)

clean:
	@- $(RM) -f *~ '#*#' core
	@- $(RM) -f *.escm *.ast recette.c recette$(EXE_SUFFIX)
	@- $(RM) -f $(SOURCES:%=%$(EXE_SUFFIX)) $(SOURCES:%=%.o)

distclean: clean

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .bgl .scm .o .class .obj

#*---------------------------------------------------------------------*/
#*    .scm -> .o/.class/.obj                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BIGLOO) -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class

.scm.obj:
	$(BIGLOO) -c -dotnet -dotnet-managed $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.obj
