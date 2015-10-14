#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bench/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jun 19 20:48:50 1997                          */
#*    Last change :  Tue Oct 13 21:30:30 2015 (serrano)                */
#*    Copyright   :  1997-2015 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The global bench Makefile (mainly for backuping).                */
#*=====================================================================*/
 
#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
BIGLOO		= $(BOOTBINDIR)/bigloo.sh

TIME		= /usr/bin/time
SHELLDIR	= shell

BENCHS		= pseudoknot \
                  maze \
                  bague \
                  byte \
                  conform \
                  earley \
                  peval \
                  queens \
                  rgc \
                  leval \
                  fft

SCRIPTS		= run.sh

POPULATION	= Makefile Makefile.bench $(SCRIPTS) $(wildcard RUN.*)

RUN		:= "RUN/RUN.$(RELEASE)"

#*---------------------------------------------------------------------*/
#*    all:                                                             */
#*---------------------------------------------------------------------*/
all: all-c all-jvm

all-c: RUN
	@ if [ ! -f $(RUN) ]; then touch $(RUN); fi
	@ echo "===================================================" >> $(RUN)
	@ echo `date` >> $(RUN)
	@ echo `uname -a` >> $(RUN)
	@ echo $(BIGLOO) >> $(RUN)
	@ echo `$(BIGLOO) -version` >> $(RUN)
	@ $(MAKE) -s mesure-c >> $(RUN)
	@ echo "" >> $(RUN)
	@ mv $(RUN) "RUN/C.$(RELEASE)-`date '+%m-%d-%y:%X'`"

all-jvm: RUN
	@ if [ ! -f $(RUN) ]; then touch $(RUN); fi
	@ echo "===================================================" >> $(RUN)
	@ echo `date` >> $(RUN)
	@ echo `uname -a` >> $(RUN)
	@ echo $(BIGLOO) >> $(RUN)
	@ echo `$(BIGLOO) -version` >> $(RUN)
	@ $(MAKE) -s mesure-jvm >> $(RUN)
	@ echo "" >> $(RUN)
	@ mv $(RUN) "RUN/JVM.$(RELEASE)-`date '+%m-%d-%y:%X'`"

#*---------------------------------------------------------------------*/
#*    mesure                                                           */
#*---------------------------------------------------------------------*/
mesure-c: 
	@ for f in $(BENCHS); do \
            echo "---------------------------------------------------";\
            echo ". $$f" 1>&2; \
            (cd $$f; $(MAKE) -f ../Makefile.bench \
                             BENCH=$$f \
                             -s clean compile-c run-c \
                             TIME=$(TIME) \
                             BIGLOO=$(BIGLOO)); \
         done

mesure-jvm: 
	@ for f in $(BENCHS); do \
            echo "---------------------------------------------------";\
            echo ". $$f" 1>&2; \
            (cd $$f; $(MAKE) -f ../Makefile.bench \
                             BENCH=$$f \
                             -s clean compile-jvm run-jvm \
                             TIME=$(TIME) \
                             BIGLOO=$(BIGLOO)); \
         done

#*---------------------------------------------------------------------*/
#*    RUN                                                              */
#*---------------------------------------------------------------------*/
RUN:
	mkdir -p RUN

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ for d in $(BENCHS); do       \
	     (cd $$d; $(MAKE) -f ../Makefile.bench BENCH=$$d -s pop); \
          done
	@ echo $(POPULATION:%=bench/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@ for d in $(BENCHS); do \
	     (cd $$d; $(MAKE) -f ../Makefile.bench BENCH=$$d -s clean;) \
          done;
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/bench ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ cd ..; rm -rf bench
