#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Mar 24 05:30:44 2005                          */
#*    Last change :  Tue Oct 12 11:43:05 2010 (serrano)                */
#*    Copyright   :  2005-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The API Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    DIRECTORIES                                                      */
#*---------------------------------------------------------------------*/
DIRECTORIES= pthread fthread mail ssl web multimedia sqlite calendar \
             srfi1 pkgcomp pkglib gstreamer text packrat srfi27 crypto \
             openpgp phone

#*---------------------------------------------------------------------*/
#*    Booting on a bare system                                         */
#*---------------------------------------------------------------------*/
boot: bootall

fullbootstrap:
	@ apis="$(APIS)"; \
          for d in $$apis; do \
	    echo "[0m[1;35m$$d:[0m"; \
            $(MAKE) -C $$d clean && $(MAKE) -C $$d boot || exit 1; \
	  done

bootall:
	@ apis="$(APIS)"; \
          for d in $$apis; do \
	    echo "[0m[1;35m$$d:[0m"; \
            $(MAKE) -C $$d boot || exit 1; \
	  done

boot-c:
	@ apis="$(APIS)"; \
          for d in $$apis; do \
	    echo "[0m[1;33m$$d:[0m"; \
            $(MAKE) -C $$d boot-c || exit 1; \
          done

boot-jvm:
	@ apis="$(APIS)"; \
          for d in $$apis; do \
            if [ "$(JVMBACKEND)" = "yes" ]; then \
	      echo "[0m[1;33m$$d:[0m"; \
              $(MAKE) -C $$d boot-jvm || exit 1; \
            fi; \
          done

boot-dotnet:
	@ apis="$(APIS)"; \
          for d in $$apis; do \
            if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	      echo "[0m[1;33m$$d:[0m"; \
              $(MAKE) -C $$d boot-dotnet || exit 1; \
            fi \
          done

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@ for d in $(DIRECTORIES) ; do \
             if [ -d $$d ]; then \
               $(MAKE) -C $$d clean; \
             fi \
          done;

cleanall: 
	@ for d in $(DIRECTORIES) ; do \
             if [ -d $$d ]; then \
               $(MAKE) -C $$d cleanall; \
             fi \
          done;

distclean: 
	@ for d in $(DIRECTORIES) ; do \
             if [ -d $$d ]; then \
               $(MAKE) -C $$d distclean; \
             fi \
          done;

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install

install:
	(cp Makefile.api $(LIBDIR)/$(FILDIR)/Makefile.api && \
         chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/Makefile.api)
	if [ "$(APIS) " != " " ]; then \
          for d in $(APIS) ; do \
            $(MAKE) -C $$d install || exit 1; \
          done \
        fi

uninstall:
	$(RM) -f $(LIBDIR)/$(FILDIR)/Makefile.api
	if [ "$(APIS) " != " " ]; then \
          for d in $(APIS) ; do \
            $(MAKE) -C $$d uninstall; \
          done \
        fi

#*---------------------------------------------------------------------*/
#*    Populating the APIs                                              */
#*---------------------------------------------------------------------*/
pop: 
	@ for d in $(DIRECTORIES) ; do \
             for f in `cd $$d && $(MAKE) -s pop;`; do \
                echo api/$$f; \
             done; \
          done;
	@ echo api/Makefile api/Makefile.api api/README

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare for a distribution                                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/api ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ for d in $(DIRECTORIES); do \
             if [ -d $$d ]; then \
               $(MAKE) -C $$d/src ude; \
             fi; \
          done;
	@ $(RM) -f api/Makefile~

