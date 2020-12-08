#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Mar 24 05:30:44 2005                          */
#*    Last change :  Tue Mar 24 12:21:18 2020 (serrano)                */
#*    Copyright   :  2005-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The API Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    DIRECTORIES                                                      */
#*---------------------------------------------------------------------*/
DIRECTORIES= pthread srfi18 fthread mail ssl web multimedia sqlite calendar \
             srfi1 pkgcomp pkglib gstreamer text packrat srfi27 crypto \
             openpgp phone alsa mpg123 flac wav avahi csv phidget upnp libuv \
             pulseaudio

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
.PHONY: clean-quick clean cleanall distclean

clean-quick:
	@ for d in $(DIRECTORIES) ; do \
             if [ -d $$d ]; then \
               (cd $$d/src; rm -f .afile); \
             fi \
          done;

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
.PHONY: install install-devel uninstall uninstall-devel

install:
	if [ "$(APIS) " != " " ]; then \
          for d in $(APIS) ; do \
            install=true; \
	    if [ "$(NOAPIS) " != " " ]; then \
              for n in $(NOAPIS) ; do \
                if [ "$$n " = "$$d " ]; then \
                  install=false; \
                  break; \
                fi; \
              done; \
            fi; \
            if [ "$$install " = "true " ]; then \
              $(MAKE) -C $$d install || exit 1; \
            fi; \
          done \
        fi

install-devel:
	(cp Makefile.api $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api && \
         chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api)
	(cp Makefile.api-safe $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api-safe && \
         chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api-safe)

uninstall:
	if [ "$(APIS) " != " " ]; then \
          for d in $(APIS) ; do \
            $(MAKE) -C $$d uninstall; \
          done \
        fi

uninstall-devel:
	$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api
	$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.api-safe

#*---------------------------------------------------------------------*/
#*    Populating the APIs                                              */
#*---------------------------------------------------------------------*/
pop: 
	@ for d in $(DIRECTORIES) ; do \
             for f in `cd $$d && $(MAKE) -s pop;`; do \
                echo api/$$f; \
             done; \
          done;
	@ echo api/Makefile api/Makefile.api api/Makefile.api-safe api/README

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
               $(MAKE) -C $$d distrib; \
             fi; \
          done;
	@ $(RM) -f api/Makefile~

