#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/alsa/examples/Makefile           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Mon Feb 25 08:55:49 2013 (serrano)                */
#*    Copyright   :  2002-13 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Alsa examples                          */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BIGLOO = $(BOOTBINDIR)/bigloo
BCFLAGS = 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
SOURCES = pcm-min cards ctl mixer

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION = $(SOURCES:%=%.scm) Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: $(SOURCES)

pcm-min: pcm-min.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

cards: cards.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

ctl: ctl.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

mixer: mixer.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

pop:
	@ echo $(POPULATION:%=alsa/examples/%)

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
.scm.o: $*.scm
	$(BIGLOO) -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class: $*.scm
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class

.scm.obj: $*.scm
	$(BIGLOO) -c -dotnet -dotnet-managed $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.obj
