#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/phidget/examples/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jan  7 14:55:33 2002                          */
#*    Last change :  Sun Jul  9 10:14:59 2017 (serrano)                */
#*    Copyright   :  2002-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Phidget examples                       */
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
SOURCES		= manager_simple interfacekit_simple spatial_simple	\
		  servo_simple advancedservo_simple stepper_simple	\
		  motor-control

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

servo_simple: servo_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

advancedservo_simple: advancedservo_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

stepper_simple: stepper_simple.o
	$(BIGLOO) $(EFLAGS) $(BFLAGS) $< -o $@$(EXE_SUFFIX)

motor-control: motor-control.o
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
.scm.o:
	$(BIGLOO) -c $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.o

.scm.class:
	$(BIGLOO) -c -jvm $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.class

.scm.obj:
	$(BIGLOO) -c -dotnet -dotnet-managed $(EFLAGS) $(BCFLAGS) $(BFLAGS) $*.scm -o $*.obj
