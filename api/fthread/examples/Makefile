#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/fthread/examples/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Feb 13 08:23:29 2002                          */
#*    Last change :  Wed Aug 16 07:05:43 2006 (serrano)                */
#*    Copyright   :  2002-06 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Thread examples                                                  */
#*=====================================================================*/

EXAMPLES=gas gas2 gas3 srfi18 mtracker prims trepl sticker

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
