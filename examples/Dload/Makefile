#*=====================================================================*/
#*    serrano/prgm/project/bigloo/examples/Dload/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Tue Jan 30 15:19:19 1996                          */
#*    Last change :  Sun Jul  9 10:23:03 2017 (serrano)                */
#*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The makefile to build process                                    */
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
BGLOPTFLAGS	= -O +rm
BFLAGS		= $(BGLOPTFLAGS)
DEST		= process

SRC		= loaded.scm loader.scm

POPULATION	= $(SRC:%=examples/Dload/%) \
                  examples/Dload/c-loader.c \
                  examples/Dload/README \
                  examples/Dload/Makefile

all: c jvm

c: loader loaded.$(SHAREDSUFFIX) c-loader

loader: loader.o
	$(BIGLOO) loader.o -o loader
loader.o: loader.scm
	$(BIGLOO) loader.scm -o loader.o -c

c-loader: c-loader.c
	$(BIGLOO) c-loader.c -o c-loader
       
loaded.$(SHAREDSUFFIX): loaded.o
	$(LD) -o loaded.$(SHAREDSUFFIX) loaded.o -lm -lc
loaded.o: loaded.scm
	$(BIGLOO) loaded.scm -o loaded.o -dload-sym -c
   
jvm: loader.sh loaded.class
loader.sh:
	$(BIGLOO) -jvm loader.class -o loader.sh
loader.class: loader.scm
	$(BIGLOO) -jvm loader.scm -o loader.class -c
       
loaded.class: loaded.scm
	$(BIGLOO) -jvm loaded.scm -o loaded.class -dload-sym -c

loader.sh: loader.class

test: c-test jvm-test

c-test: loader loaded.$(SHAREDSUFFIX)
	./loader ./loaded.$(SHAREDSUFFIX)
	./c-loader ./loaded.$(SHAREDSUFFIX)

jvm-test: loader.sh loaded.class
	./loader.sh loaded.class
    
clean:
	- $(RM) -f *.o *.$(SHAREDSUFFIX) *.class loader c-loader *.jar
	- $(RM) -f loader.sh

pop:
	@ echo $(POPULATION)
