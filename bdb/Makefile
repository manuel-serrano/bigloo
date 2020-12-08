#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/bdb/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Aug  4 20:11:07 1999                          */
#*    Last change :  Tue Mar 24 12:22:41 2020 (serrano)                */
#*    Copyright   :  1999-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The bdb Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
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
	@ (cd bdb; $(MAKE) MODE=final mode)

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
#*    Prepare bdb for a distribution                                   */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/bdb ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ for d in $(DIRECTORIES) ; do \
             $(MAKE) -C $$d ude; \
          done;
	@ $(RM) -f bdb/Makefile~

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

include ../Makefile.misc

install:
	if [ $(DESTDIR)$(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/bdb $(DESTDIR)$(BINDIR) && \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bdb; \
        fi
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	   cp $(BOOTLIBDIR)/bdb.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap && \
           chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap || exit 1; \
        fi
	$(MAKE) install-lib LIB=libbigloobdb_s-$(RELEASE)


uninstall:
	-$(RM) -f $(DESTDIR)$(BINDIR)/bdb
	-$(RM) -f $(DESTDIR)$(LIBDIR)/bdb.heap
	-$(MAKE) uninstall-lib LIB=libbigloobdb_s-$(RELEASE)

