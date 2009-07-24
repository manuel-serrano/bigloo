#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/fthread/examples/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Feb 13 08:23:29 2002                          */
#*    Last change :  Fri Jul 24 09:47:48 2009 (serrano)                */
#*    Copyright   :  2002-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Thread examples                                                  */
#*=====================================================================*/

EXAMPLES=gas gas2 gas3 srfi18 mtracker prims trepl sticker http

all:
	for p in $(EXAMPLES); do \
           (cd $$p; $(MAKE)); \
        done

pop:
	@for p in $(EXAMPLES); do \
           (cd $$p; $(MAKE) -s pop); \
        done
	@echo "fthread/examples/Makefile fthread/examples/README"

clean:
	for p in $(EXAMPLES); do \
           (cd $$p; $(MAKE) clean); \
        done

distclean: clean
