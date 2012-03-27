#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/phidget/examples/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Tue Mar 27 09:19:39 2012 (serrano)                */
#*    Copyright   :  2002-12 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Phidget examples                       */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Standard path                                                    */
#*---------------------------------------------------------------------*/
include ../../../Makefile.config 

#*---------------------------------------------------------------------*/
#*    Bigloo                                                           */
#*---------------------------------------------------------------------*/
BIGLOO		= $(BOOTBINDIR)/bigloo
BCFLAGS		= 

#*---------------------------------------------------------------------*/
#*    Sources                                                          */
#*---------------------------------------------------------------------*/
SOURCES		= manager_simple interfacekit_simple spatial_simple

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
POPULATION	= $(SOURCES:%=%.scm) Makefile

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/
all: $(SOURCES)

manager_simple: manager_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

interfacekit_simple: interfacekit_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

spatial_simple: spatial_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

pop:
	@ echo $(POPULATION:%=phidget/examples/%)

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
