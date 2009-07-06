#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Mar 24 05:30:44 2005                          */
#*    Last change :  Fri Jun 19 16:22:29 2009 (serrano)                */
#*    Copyright   :  2005-09 Manuel Serrano                            */
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
             srfi1 pkgcomp pkglib gstreamer text packrat srfi27

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
.PHONY: install install-init install-c install-jvm install-dotnet uninstall

install: install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	     $(MAKE) install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
             $(MAKE) install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
             $(MAKE) install-dotnet; \
          fi

install-init:
	@ if [ "$(APIS) " != " " ]; then \
            for d in $(APIS) ; do \
              $(MAKE) -C $$d install-init || exit 1; \
            done \
          fi

install-c:
	@ if [ "$(APIS) " != " " ]; then \
            for d in $(APIS) ; do \
              $(MAKE) -C $$d install-c || exit 1; \
            done \
          fi

install-jvm:
	@ if [ "$(APIS) " != " " ]; then \
            for d in $(APIS) ; do \
              $(MAKE) -C $$d install-jvm || exit 1; \
            done \
          fi

install-dotnet:
	@ if [ "$(APIS) " != " " ]; then \
            for d in $(APIS) ; do \
              $(MAKE) -C $$d install-dotnet || exit 1; \
            done \
          fi

uninstall:
	@ if [ "$(APIS) " != " " ]; then \
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
	@ echo api/Makefile api/README

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
               $(MAKE) -C $$d ude; \
             fi; \
          done;
	@ $(RM) -f api/Makefile~

