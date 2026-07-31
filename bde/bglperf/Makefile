#*=====================================================================*/
#*    serrano/prgm/project/bigloo/5.0.x/bde/bglperf/Makefile           */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Apr 20 09:34:24 2003                          */
#*    Last change :  Fri Jul 31 08:41:07 2026 (serrano)                */
#*    Copyright   :  2003-26 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The makefile to build and install bglperf                        */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.buildconfig
include ../../Makefile.config
include ../../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Targets                                                          */
#*---------------------------------------------------------------------*/
.PHONY: lib bglperf 

all: bglperf

bglperf: $(BOOTBINDIR)/bglperf $(BOOTLIBDIR)/bglperf

$(BOOTBINDIR)/bglperf:
	echo "#!/bin/sh" > $@
	echo "exec $(BOOTLIBDIR)/bglperf/bglperf.sh --demangle $(DESTDIR)$(BINDIR)/bgldemangle \"$$""@\"" >> $@
	chmod $(MODEXE) $(BOOTBINDIR)/bglperf

$(BOOTLIBDIR)/bglperf:
	mkdir -p $(BOOTLIBDIR)/bglperf
	cp bglperf.sh $(BOOTLIBDIR)/bglperf \
          && chmod $(MODEXE) $(BOOTLIBDIR)/bglperf/bglperf.sh
	cp flamegraph.pl $(BOOTLIBDIR)/bglperf \
          && chmod $(MODEXE) $(BOOTLIBDIR)/bglperf/flamegraph.pl
	cp stackcollapse-perf.pl $(BOOTLIBDIR)/bglperf \
          && chmod $(MODEXE) $(BOOTLIBDIR)/bglperf/stackcollapse-perf.pl

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
install:
	mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	   cp bglperf.sh $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf \
             && chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf/bglperf.sh; \
	   cp flamegraph.pl $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf \
             && chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf/flamegraph.pl; \
	   cp stackcollapse-perf.pl $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf \
             && chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf/stackcollapse.pl; \
        fi
	echo "#!/bin/sh" > $(DESTDIR)$(BINDIR)/bglperf
	echo "exec $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf/bglperf.sh --demangle $(DESTDIR)$(BINDIR)/bgldemangle \"$$""@\"" >> $(DESTDIR)$(BINDIR)/bglperf
	chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglperf


uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglperf
	- $(RM) -rf $(DESTDIR)$(LIBDIR)/$(FILDIR)/bglperf

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	- $(RM) $(BOOTBINDIR)/bglperf
	- $(RM) -rf $(BOOTLIBDIR)/bglperf
