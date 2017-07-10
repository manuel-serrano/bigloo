#*=====================================================================*/
#*    serrano/prgm/project/bigloo/examples/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jun 19 20:48:50 1997                          */
#*    Last change :  Sun Jul  9 10:24:04 2017 (serrano)                */
#*    Copyright   :  1997-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The global example Makefile (just a wrapper).                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL           = /bin/sh

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile

#*---------------------------------------------------------------------*/
#*    The directory that compose the examples                          */
#*---------------------------------------------------------------------*/
DIRECTORIES	= Depend Embedded Fib Foreign \
                  Lower Match Object Pragma Process Roman \
                  Runtime Yacc2bigloo Dsssl Socket ClientServer \
                  Lib Repl Fork Jclass Jarray Demangle Dload Jawt \
                  Maze Server Grep Sqltiny DNet \
		  hostsh

#*---------------------------------------------------------------------*/
#*    compile everything                                               */
#*---------------------------------------------------------------------*/
test:
	for d in $(DIRECTORIES); do \
	   (cd $$d; $(MAKE) test) || exit 1; \
        done;

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONE: clean cleanall distclean

clean:
	for d in $(DIRECTORIES); do \
	   (cd $$d; $(MAKE) -s clean); \
        done;

cleanall: clean
distclean: cleanall

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ for d in $(DIRECTORIES); do  \
	     (cd $$d; $(MAKE) -s pop); \
          done;
	@ echo $(POPULATION:%=examples/%)

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/examples ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) clean
