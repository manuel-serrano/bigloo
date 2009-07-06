#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdb/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Aug  4 20:11:07 1999                          */
#*    Last change :  Fri Apr 24 08:02:18 2009 (serrano)                */
#*    Copyright   :  1999-2009 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The bdb Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    The directory that compose a version                             */
#*---------------------------------------------------------------------*/
DIRECTORIES	= bdb blib

#*---------------------------------------------------------------------*/
#*    Booting bdb on a bare system                                     */
#*---------------------------------------------------------------------*/
boot:
	@ (cd blib; $(MAKE) all)
	@ (cd bdb; $(MAKE) final)

#*---------------------------------------------------------------------*/
#*    Populating bdb                                                   */
#*---------------------------------------------------------------------*/
pop: 
	@ for d in $(DIRECTORIES) ; do \
             for f in `cd $$d; $(MAKE) -s pop;`; do \
                echo bdb/$$d/$$f; \
             done; \
          done;
	@ echo bdb/Makefile

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@ for d in $(DIRECTORIES) ; do \
             (cd $$d; $(MAKE) ROOT="$(ROOT)" clean); \
          done;

cleanall: 
	@ for d in $(DIRECTORIES) ; do \
             (cd $$d; $(MAKE) ROOT="$(ROOT)" cleanall); \
          done;

distclean: 
	@ for d in $(DIRECTORIES) ; do \
             (cd $$d; $(MAKE) ROOT="$(ROOT)" cleanall); \
          done;


#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare Cigloo for a distribution                                */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/bdb ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ for d in $(DIRECTORIES) ; do \
             (cd $$d; $(MAKE) ude); \
          done;
	@ $(RM) -f bdb/Makefile~

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

include ../Makefile.misc

install:
	if [ $(DESTDIR)$(BINDIR) != $(BOOTBINDIR) ]; then \
           mkdir -p $(DESTDIR)$(BINDIR) && \
	   cp $(BOOTBINDIR)/bdb $(DESTDIR)$(BINDIR) && \
	   chmod $(BMASK) $(DESTDIR)$(BINDIR)/bdb && \
           mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR) && \
	   cp $(BOOTLIBDIR)/bdb.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap && \
           chmod $(BMASK) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap || exit 1; \
        fi
	if [ $(FILDIR) != $(BOOTLIBDIR) ]; then \
           mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR) && \
	   cp $(BOOTLIBDIR)/bdb.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap && \
           chmod $(BMASK) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap || exit 1; \
        fi
	$(MAKE) install-lib LIB=libbigloobdb_s-$(RELEASE)


uninstall:
	-$(RM) -f $(DESTDIR)$(BINDIR)/bdb
	-$(RM) -f $(LIBDIR)/bdb.heap
	-$(MAKE) uninstall-lib LIB=libbigloobdb_s-$(RELEASE)

