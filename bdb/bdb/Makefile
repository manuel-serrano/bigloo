#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdb/bdb/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  serrano                                           */
#*    Creation    :  Wed Jul 28 06:59:47 1999                          */
#*    Last change :  Tue May 15 07:49:34 2007 (serrano)                */
#*    -------------------------------------------------------------    */
#*    Automatically generated file:                                    */
#*       bmake                                                         */
#*          -fapplication                                              */
#*          -main                                                      */
#*          Init/main.scm                                              */
#*          -o                                                         */
#*          Makefile                                                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Target and Project                                               */
#*---------------------------------------------------------------------*/
PROJECT		= src
VERSION		= 0
TARGET_DIR      = $(BOOTBINDIR)
TARGET_NAME     = bdb

#*---------------------------------------------------------------------*/
#*    The project mode (either `devel' or `final').                    */
#*---------------------------------------------------------------------*/
MODE		= final

#*---------------------------------------------------------------------*/
#*    Compilers, Flags and Tools ...                                   */
#*---------------------------------------------------------------------*/
# The Bigloo compiler
BIGLOO          = $(BOOTBINDIR)/bigloo
BFLAGS_COMMON   = -afile .afile -no-hello -L $(BOOTLIBDIR)
BFLAGS_G        = $(BFLAGS_COMMON) -g2 -gmodule
BFLAGS_O        = $(BFLAGS_COMMON) -O5 -unsafe
BFLAGS_P        = $(BFLAGS_COMMON) -O5 -unsafe -static-bigloo -p
BFLAGS_P2       = $(BFLAGS_COMMON) -O5 -unsafe -static-bigloo -p2
BFLAGS          = $(BFLAGS_G)
LNFLAGS_G       = -L $(BOOTLIBDIR)
LNFLAGS_O       = -s -L $(BOOTLIBDIR)
LNFLAGS_P       = -L $(BOOTLIBDIR)
LNFLAGS         = $(LNFLAGS_G)
# The mco builder (this could be Bigloo itself or a speciliazed tools)
MCOFLAGS	= -I . -s # when using Bigloo, add the -mco flags here
# The interface builder
BUILDER		= buildoo
BLNFLAGS	= -no-event-loop --generate
# The C compiler
TK_LIB		= $$HOME/prgm/project/bglk/tk
CFLAGS_O	= $(CFLAGS)
# Which shell to use
SHELL           = /bin/sh
# Revision system
PRCS            = prcs
COMPRESS        = gzip
# The Bigloo tools
AFILE		= $(BOOTBINDIR)/$(AFILE_EXE)
JFILE		= $(BOOTBINDIR)/$(JFILE_EXE)
DEPEND		= $(BOOTBINDIR)/$(BDEPEND_EXE)
BTAGS		= $(BOOTBINDIR)/$(BTAGS_EXE)
MCO		= $(BOOTBINDIR)/$(MCO_EXE)
# Profiling
PROF		= PROF
PROFILE_ARGS	= 

#*---------------------------------------------------------------------*/
#*    Suffixes ...						       */
#*---------------------------------------------------------------------*/
.SUFFIXES:

#*---------------------------------------------------------------------*/
#*    Objects and sources ...					       */
#*---------------------------------------------------------------------*/
OBJECTS = Command/command.mco Command/explore.mco Init/setrc.mco        \
      Command/parse.mco Gdb/gread.mco Gdb/tools.mco Engine/engine.mco   \
      Command/set.mco Bee/etags.mco Tools/error.mco                     \
      Command/backtrace.mco Patient/value.mco Gdb/annotate.mco          \
      Gdb/autoconf.mco Engine/param.mco Tools/version.mco               \
      Command/display.mco Engine/repl.mco Gdb/ginvoke.mco               \
      Patient/server.mco Command/run.mco Tools/speek.mco                \
      Command/all_cmd.mco Tools/date.mco Init/parse_args.mco            \
      Tools/io.mco Command/help.mco Command/print.mco                   \
      Patient/mangling.mco Command/whatis.mco Tools/tools.mco           \
      Patient/pinvoke.mco Command/file.mco Command/break.mco            \
      Command/mangle.mco Gdb/proc.mco Command/expr.mco Command/info.mco \
      Bee/binvoke.mco Command/quit.mco Init/main.mco Command/command.o  \
      Command/explore.o Init/setrc.o Command/parse.o Gdb/gread.o        \
      Gdb/tools.o Engine/engine.o Command/set.o Bee/etags.o             \
      Tools/error.o Command/backtrace.o Patient/value.o Gdb/annotate.o  \
      Gdb/autoconf.o Engine/param.o Tools/version.o Command/display.o   \
      Engine/repl.o Gdb/ginvoke.o Patient/server.o Command/run.o        \
      Tools/speek.o Command/all_cmd.o Tools/date.o Init/parse_args.o    \
      Tools/io.o Command/help.o Command/print.o Patient/mangling.o      \
      Command/whatis.o Tools/tools.o Patient/pinvoke.o Command/file.o   \
      Command/break.o Command/mangle.o Gdb/proc.o Command/expr.o        \
      Command/info.o Bee/binvoke.o Command/quit.o Init/main.o           \
      Tools/tty.o 
GUI_OBJECTS = 
OBJECTS_P = Command/command_p.mco Command/explore_p.mco                 \
      Init/setrc_p.mco Command/parse_p.mco Gdb/gread_p.mco              \
      Gdb/tools_p.mco Engine/engine_p.mco Command/set_p.mco             \
      Bee/etags_p.mco Tools/error_p.mco Command/backtrace_p.mco         \
      Patient/value_p.mco Gdb/annotate_p.mco Gdb/autoconf_p.mco         \
      Engine/param_p.mco Tools/version_p.mco Command/display_p.mco      \
      Engine/repl_p.mco Gdb/ginvoke_p.mco Patient/server_p.mco          \
      Command/run_p.mco Tools/speek_p.mco Command/all_cmd_p.mco         \
      Tools/date_p.mco Init/parse_args_p.mco Tools/io_p.mco             \
      Command/help_p.mco Command/print_p.mco Patient/mangling_p.mco     \
      Command/whatis_p.mco Tools/tools_p.mco Patient/pinvoke_p.mco      \
      Command/file_p.mco Command/break_p.mco Command/mangle_p.mco       \
      Gdb/proc_p.mco Command/expr_p.mco Command/info_p.mco              \
      Bee/binvoke_p.mco Command/quit_p.mco Init/main_p.mco              \
      Command/command_p.o Command/explore_p.o Init/setrc_p.o            \
      Command/parse_p.o Gdb/gread_p.o Gdb/tools_p.o Engine/engine_p.o   \
      Command/set_p.o Bee/etags_p.o Tools/error_p.o                     \
      Command/backtrace_p.o Patient/value_p.o Gdb/annotate_p.o          \
      Gdb/autoconf_p.o Engine/param_p.o Tools/version_p.o               \
      Command/display_p.o Engine/repl_p.o Gdb/ginvoke_p.o               \
      Patient/server_p.o Command/run_p.o Tools/speek_p.o                \
      Command/all_cmd_p.o Tools/date_p.o Init/parse_args_p.o            \
      Tools/io_p.o Command/help_p.o Command/print_p.o                   \
      Patient/mangling_p.o Command/whatis_p.o Tools/tools_p.o           \
      Patient/pinvoke_p.o Command/file_p.o Command/break_p.o            \
      Command/mangle_p.o Gdb/proc_p.o Command/expr_p.o Command/info_p.o \
      Bee/binvoke_p.o Command/quit_p.o Init/main_p.o Tools/tty_p.o 
SOURCES = Tools/tty.c Command/command.scm Command/explore.scm           \
      Init/setrc.scm Command/parse.scm Gdb/gread.scm Gdb/tools.scm      \
      Engine/engine.scm Command/set.scm Bee/etags.scm Tools/error.scm   \
      Command/backtrace.scm Patient/value.scm Gdb/annotate.scm          \
      Gdb/autoconf.scm Engine/param.scm Tools/version.scm               \
      Command/display.scm Engine/repl.scm Gdb/ginvoke.scm               \
      Patient/server.scm Command/run.scm Tools/speek.scm                \
      Command/all_cmd.scm Tools/date.scm Init/parse_args.scm            \
      Tools/io.scm Command/help.scm Command/print.scm                   \
      Patient/mangling.scm Command/whatis.scm Tools/tools.scm           \
      Patient/pinvoke.scm Command/file.scm Command/break.scm            \
      Command/mangle.scm Gdb/proc.scm Command/expr.scm Command/info.scm \
      Bee/binvoke.scm Command/quit.scm Init/main.scm 
SOURCES_C = Command/command.c Command/explore.c Init/setrc.c            \
      Command/parse.c Gdb/gread.c Gdb/tools.c Engine/engine.c           \
      Command/set.c Bee/etags.c Tools/error.c Command/backtrace.c       \
      Patient/value.c Gdb/annotate.c Gdb/autoconf.c Engine/param.c      \
      Tools/version.c Command/display.c Engine/repl.c Gdb/ginvoke.c     \
      Patient/server.c Command/run.c Tools/speek.c Command/all_cmd.c    \
      Tools/date.c Init/parse_args.c Tools/io.c Command/help.c          \
      Command/print.c Patient/mangling.c Command/whatis.c Tools/tools.c \
      Patient/pinvoke.c Command/file.c Command/break.c Command/mangle.c \
      Gdb/proc.c Command/expr.c Command/info.c Bee/binvoke.c            \
      Command/quit.c Init/main.c 
POPULATION = $(SOURCES) Makefile Engine/hooks.sch

#*---------------------------------------------------------------------*/
#*    trashcan ...						       */
#*---------------------------------------------------------------------*/
All: all
all: mode
mode: .afile
	$(MAKE) $(MODE)

main: $(TARGET_DIR)/$(TARGET_NAME)
$(TARGET_DIR)/$(TARGET_NAME): $(GUI_OBJECTS) $(OBJECTS)
	@ echo "Linking..."
	@ $(BIGLOO) $(EFLAGS) $(LNFLAGS) $(OBJECTS) -o $(TARGET_DIR)/$(TARGET_NAME)
	@ echo "$(TARGET_DIR)/$(TARGET_NAME) done."
	@ echo "-------------------------------"

devel: $(TARGET_DIR)
	@ $(MAKE) -f Makefile main BFLAGS="$(BFLAGS_G)" LNFLAGS="$(LNFLAGS_G)" CFLAGS="$(CFLAGS_G)"

final: $(TARGET_DIR)
	@ $(MAKE) -f Makefile main BFLAGS="$(BFLAGS_O)" LNFLAGS="$(LNFLAGS_O)" CFLAGS="$(CFLAGS_O)"

profile_comp: $(TARGET_DIR) main_p
main_p: forceafile forcetags $(TARGET_DIR)/$(TARGET_NAME)_p
$(TARGET_DIR)/$(TARGET_NAME)_p: $(OBJECTS_P)
	@ echo "Linking..."
	@ $(BIGLOO) $(BFLAGS_P) $(LNFLAGS_P) $(OBJECTS_P) -o $(TARGET_DIR)/$(TARGET_NAME)_p
	@ echo "$(TARGET_DIR)/$(TARGET_NAME)_p done."
	@ echo "-------------------------------"

extra_profile_comp:
	@ $(MAKE) -f Makefile BFLAGS_P="$(BFLAGS_P2)" profile_comp

gmon.out: $(TARGET_DIR)/$(TARGET_NAME)_p
	@- $(TARGET_DIR)/$(TARGET_NAME)_p $(PROFILE_ARGS)

profile_run: 
	@- $(RM) -f gmon.out $(PROF)
	@ $(MAKE) -f Makefile gmon.out
	@ $(BPROF) -b $(TARGET_DIR)/$(TARGET_NAME)_p > $(PROF)

profile_clean: 
	@- $(RM) -f gmon.out bmon.out gmon.sum $(PROF)
	@- $(RM) -f $(OBJECTS_P)
	@- $(RM) -f $(TARGET_DIR)/$(TARGET_NAME)_p

$(TARGET_DIR):
	@ mkdir -p $(TARGET_DIR)

#*---------------------------------------------------------------------*/
#*    includes                                                         */
#*---------------------------------------------------------------------*/
.PHONY: includes

includes:
	mkdir -p Includes
	mkdir -p Includes/generic
	mkdir -p Includes/unix
	mkdir -p Includes/compat
	mkdir -p Includes/Tcl
	$(RM) -f Includes/*.h
	cp $(TK_LIB)/Tk/generic/*.h Includes/generic
	cp $(TK_LIB)/Tk/unix/*.h Includes/unix
	cp $(TK_LIB)/Tk/compat/*.h Includes/compat
	cp $(TK_LIB)/Tcl/*.h Includes/Tcl

#*---------------------------------------------------------------------*/
#*    Cleaning ...						       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall

# The clean entry removes all file that may generated but the binary files
clean: 
	@- $(RM) -f $(OBJECTS)
	@- $(RM) -f $(GUI_OBJECTS)
	@- $(RM) -f $(OBJECTS_P)
	@- $(RM) -f $(SOURCES_C)
	@- $(RM) -f $(PROF)
	@- $(RM) -f gmon.out bmon.out gmon.sum
	@ find . \( -name '*[~%]'                   \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name \*core \)           \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

udeclean:
	@- $(RM) -f .etags

# clean remove also the binary files
cleanall: clean udeclean
	- $(RM) -f $(TARGET_DIR)/$(TARGET_NAME)
	- $(RM) -f $(TARGET_DIR)/$(TARGET_NAME)_p

#*---------------------------------------------------------------------*/
#*    Population and revision control                                  */
#*---------------------------------------------------------------------*/
.PHONY: version pop revision populate checkin checkout infofile checkoutfile

# display the current version
version:
	@ echo $(VERSION)

# just display the population
pop:
	@ echo $(POPULATION)

# make a prcs revision
revision: populate checkin

populate: $(PROJECT).prj
	$(PRCS) populate --force $(PROJECT) $(POPULATION)

checkin: 
	$(PRCS) checkin --force -r$(VERSION).@ $(PROJECT)

# display how to check out a prcs revision
checkout:
	@ echo "$(PRCS) checkout -r$(VERSION).@ $(PROJECT)"

infofile:
	@ $(PRCS) info --force $(PROJECT) $(FILE)

checkoutfile:
	@ $(PRCS) checkout --force -r$(FILEVERSION) $(PROJECT) $(FILE)

$(PROJECT).prj:
	@ $(PRCS) checkout --force $(PROJECT)
	@ cat $(PROJECT).prj | sed -e s,"(Populate-Ignore ())","(Populate-Ignore (\"\\\\\\\\\\.o\$$\" \"\\\\\\\\\\~$$\" \"\\\\\\\\\\.log\$$\" \"\\\\\\\\\\.ps\$$\" \"\\\\\\\\\\.aux\$$\" \"\\\\\\\\\\.date_of_backup\$$\" \"\\\\\\\\\\.so\$$\" \"\\\\\\\\\\.a\$$\"))", > $(PROJECT).dprj; $(RM) -f $(PROJECT).prj; mv $(PROJECT).dprj $(PROJECT).prj


#*---------------------------------------------------------------------*/
#*    Tar ...							       */
#*---------------------------------------------------------------------*/
tar.gz: $(PROJECT).tar.gz

$(PROJECT).tar.gz: ../$(PROJECT).tar
	@ $(COMPRESS) ../$(PROJECT).tar; mv ../$(PROJECT).tar.gz .

# This entry works only with gnu-make. Change it for other Makefiles.
../$(PROJECT).tar: 
	@ cd ..; \
	  $(TAR) $(TARCOPT) $(PROJECT).tar $(POPULATION:%=$(PROJECT)/%)

#*---------------------------------------------------------------------*/
#*    Bee ...							       */
#*---------------------------------------------------------------------*/
.PHONY: ude forceafile forcetags bpdend updatemakefile

ude:
	@ $(MAKE) -f Makefile .afile
	@ $(MAKE) -f Makefile .etags
	@ $(MAKE) -f Makefile bdepend

forceafile:
	@ if [ ! -f .afile ]; then            \
             $(MAKE) -f Makefile .afile;   \
          fi

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(SOURCES)

forcetags:
	@ if [ ! -f .etags ]; then            \
             $(MAKE) -f Makefile .etags;   \
          fi

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(SOURCES)

bdepend: $(SOURCES)
	@ $(DEPEND) -o Makefile $(SOURCES)

updatemakefile:
	@ $(BMAKE) -o Makefile Init/main.scm
	@ $(MAKE) -f Makefile ude

#*---------------------------------------------------------------------*/
#*    bmake entries ...                                                */
#*    -------------------------------------------------------------    */
#*    These entries are used by bmake to extend the Makefile. Don't    */
#*    use them.                                                        */
#*---------------------------------------------------------------------*/
.PHONY: getobjects getsources getpopulation getbinary getbinary_p getmode

getobjects:
	@ echo $(OBJECTS)

getsources:
	@ echo $(SOURCES)

getpopulation:
	@ echo $(POPULATION)

getbinary:
	@ echo $(TARGET_DIR)/$(TARGET_NAME)

getbinary_p:
	@ echo $(TARGET_DIR)/$(TARGET_NAME)_p

getmode:
	@ echo $(MODE)

# Rules
#*---------------------------------------------------------------------*/
#*    # GUI objects                                                    */
#*---------------------------------------------------------------------*/
#*---------------------------------------------------------------------*/
#*    # Bigloo objects                                                 */
#*---------------------------------------------------------------------*/
Command/command.o: Command/command.scm
	$(BIGLOO) $(BFLAGS) -c Command/command.scm -o Command/command.o
Command/command_p.o: Command/command.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/command.scm -o Command/command_p.o
Command/command.mco: Command/command.scm
	$(MCO) $(MCOFLAGS) Command/command.scm -o Command/command.mco
Command/command_p.mco: Command/command.scm
	$(MCO) $(MCOFLAGS) Command/command.scm -o Command/command_p.mco

Command/explore.o: Command/explore.scm
	$(BIGLOO) $(BFLAGS) -c Command/explore.scm -o Command/explore.o
Command/explore_p.o: Command/explore.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/explore.scm -o Command/explore_p.o
Command/explore.mco: Command/explore.scm
	$(MCO) $(MCOFLAGS) Command/explore.scm -o Command/explore.mco
Command/explore_p.mco: Command/explore.scm
	$(MCO) $(MCOFLAGS) Command/explore.scm -o Command/explore_p.mco

Init/setrc.o: Init/setrc.scm
	$(BIGLOO) $(BFLAGS) -c Init/setrc.scm -o Init/setrc.o
Init/setrc_p.o: Init/setrc.scm
	$(BIGLOO) $(BFLAGS_P) -c Init/setrc.scm -o Init/setrc_p.o
Init/setrc.mco: Init/setrc.scm
	$(MCO) $(MCOFLAGS) Init/setrc.scm -o Init/setrc.mco
Init/setrc_p.mco: Init/setrc.scm
	$(MCO) $(MCOFLAGS) Init/setrc.scm -o Init/setrc_p.mco

Command/parse.o: Command/parse.scm
	$(BIGLOO) $(BFLAGS) -c Command/parse.scm -o Command/parse.o
Command/parse_p.o: Command/parse.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/parse.scm -o Command/parse_p.o
Command/parse.mco: Command/parse.scm
	$(MCO) $(MCOFLAGS) Command/parse.scm -o Command/parse.mco
Command/parse_p.mco: Command/parse.scm
	$(MCO) $(MCOFLAGS) Command/parse.scm -o Command/parse_p.mco

Gdb/gread.o: Gdb/gread.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/gread.scm -o Gdb/gread.o
Gdb/gread_p.o: Gdb/gread.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/gread.scm -o Gdb/gread_p.o
Gdb/gread.mco: Gdb/gread.scm
	$(MCO) $(MCOFLAGS) Gdb/gread.scm -o Gdb/gread.mco
Gdb/gread_p.mco: Gdb/gread.scm
	$(MCO) $(MCOFLAGS) Gdb/gread.scm -o Gdb/gread_p.mco

Gdb/tools.o: Gdb/tools.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/tools.scm -o Gdb/tools.o
Gdb/tools_p.o: Gdb/tools.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/tools.scm -o Gdb/tools_p.o
Gdb/tools.mco: Gdb/tools.scm
	$(MCO) $(MCOFLAGS) Gdb/tools.scm -o Gdb/tools.mco
Gdb/tools_p.mco: Gdb/tools.scm
	$(MCO) $(MCOFLAGS) Gdb/tools.scm -o Gdb/tools_p.mco

Engine/engine.o: Engine/engine.scm
	$(BIGLOO) $(BFLAGS) -c Engine/engine.scm -o Engine/engine.o
Engine/engine_p.o: Engine/engine.scm
	$(BIGLOO) $(BFLAGS_P) -c Engine/engine.scm -o Engine/engine_p.o
Engine/engine.mco: Engine/engine.scm
	$(MCO) $(MCOFLAGS) Engine/engine.scm -o Engine/engine.mco
Engine/engine_p.mco: Engine/engine.scm
	$(MCO) $(MCOFLAGS) Engine/engine.scm -o Engine/engine_p.mco

Command/set.o: Command/set.scm
	$(BIGLOO) $(BFLAGS) -c Command/set.scm -o Command/set.o
Command/set_p.o: Command/set.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/set.scm -o Command/set_p.o
Command/set.mco: Command/set.scm
	$(MCO) $(MCOFLAGS) Command/set.scm -o Command/set.mco
Command/set_p.mco: Command/set.scm
	$(MCO) $(MCOFLAGS) Command/set.scm -o Command/set_p.mco

Bee/etags.o: Bee/etags.scm
	$(BIGLOO) $(BFLAGS) -c Bee/etags.scm -o Bee/etags.o
Bee/etags_p.o: Bee/etags.scm
	$(BIGLOO) $(BFLAGS_P) -c Bee/etags.scm -o Bee/etags_p.o
Bee/etags.mco: Bee/etags.scm
	$(MCO) $(MCOFLAGS) Bee/etags.scm -o Bee/etags.mco
Bee/etags_p.mco: Bee/etags.scm
	$(MCO) $(MCOFLAGS) Bee/etags.scm -o Bee/etags_p.mco

Tools/error.o: Tools/error.scm
	$(BIGLOO) $(BFLAGS) -c Tools/error.scm -o Tools/error.o
Tools/error_p.o: Tools/error.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/error.scm -o Tools/error_p.o
Tools/error.mco: Tools/error.scm
	$(MCO) $(MCOFLAGS) Tools/error.scm -o Tools/error.mco
Tools/error_p.mco: Tools/error.scm
	$(MCO) $(MCOFLAGS) Tools/error.scm -o Tools/error_p.mco

Command/backtrace.o: Command/backtrace.scm
	$(BIGLOO) $(BFLAGS) -c Command/backtrace.scm -o Command/backtrace.o
Command/backtrace_p.o: Command/backtrace.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/backtrace.scm -o Command/backtrace_p.o
Command/backtrace.mco: Command/backtrace.scm
	$(MCO) $(MCOFLAGS) Command/backtrace.scm -o Command/backtrace.mco
Command/backtrace_p.mco: Command/backtrace.scm
	$(MCO) $(MCOFLAGS) Command/backtrace.scm -o Command/backtrace_p.mco

Patient/value.o: Patient/value.scm
	$(BIGLOO) $(BFLAGS) -c Patient/value.scm -o Patient/value.o
Patient/value_p.o: Patient/value.scm
	$(BIGLOO) $(BFLAGS_P) -c Patient/value.scm -o Patient/value_p.o
Patient/value.mco: Patient/value.scm
	$(MCO) $(MCOFLAGS) Patient/value.scm -o Patient/value.mco
Patient/value_p.mco: Patient/value.scm
	$(MCO) $(MCOFLAGS) Patient/value.scm -o Patient/value_p.mco

Gdb/annotate.o: Gdb/annotate.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/annotate.scm -o Gdb/annotate.o
Gdb/annotate_p.o: Gdb/annotate.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/annotate.scm -o Gdb/annotate_p.o
Gdb/annotate.mco: Gdb/annotate.scm
	$(MCO) $(MCOFLAGS) Gdb/annotate.scm -o Gdb/annotate.mco
Gdb/annotate_p.mco: Gdb/annotate.scm
	$(MCO) $(MCOFLAGS) Gdb/annotate.scm -o Gdb/annotate_p.mco

Gdb/autoconf.o: Gdb/autoconf.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/autoconf.scm -o Gdb/autoconf.o
Gdb/autoconf_p.o: Gdb/autoconf.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/autoconf.scm -o Gdb/autoconf_p.o
Gdb/autoconf.mco: Gdb/autoconf.scm
	$(MCO) $(MCOFLAGS) Gdb/autoconf.scm -o Gdb/autoconf.mco
Gdb/autoconf_p.mco: Gdb/autoconf.scm
	$(MCO) $(MCOFLAGS) Gdb/autoconf.scm -o Gdb/autoconf_p.mco

Engine/param.o: Engine/param.scm
	$(BIGLOO) $(BFLAGS) -c Engine/param.scm -o Engine/param.o
Engine/param_p.o: Engine/param.scm
	$(BIGLOO) $(BFLAGS_P) -c Engine/param.scm -o Engine/param_p.o
Engine/param.mco: Engine/param.scm
	$(MCO) $(MCOFLAGS) Engine/param.scm -o Engine/param.mco
Engine/param_p.mco: Engine/param.scm
	$(MCO) $(MCOFLAGS) Engine/param.scm -o Engine/param_p.mco

Tools/version.o: Tools/version.scm
	$(BIGLOO) $(BFLAGS) -c Tools/version.scm -o Tools/version.o
Tools/version_p.o: Tools/version.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/version.scm -o Tools/version_p.o
Tools/version.mco: Tools/version.scm
	$(MCO) $(MCOFLAGS) Tools/version.scm -o Tools/version.mco
Tools/version_p.mco: Tools/version.scm
	$(MCO) $(MCOFLAGS) Tools/version.scm -o Tools/version_p.mco

Command/display.o: Command/display.scm
	$(BIGLOO) $(BFLAGS) -c Command/display.scm -o Command/display.o
Command/display_p.o: Command/display.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/display.scm -o Command/display_p.o
Command/display.mco: Command/display.scm
	$(MCO) $(MCOFLAGS) Command/display.scm -o Command/display.mco
Command/display_p.mco: Command/display.scm
	$(MCO) $(MCOFLAGS) Command/display.scm -o Command/display_p.mco

Engine/repl.o: Engine/repl.scm
	$(BIGLOO) $(BFLAGS) -c Engine/repl.scm -o Engine/repl.o
Engine/repl_p.o: Engine/repl.scm
	$(BIGLOO) $(BFLAGS_P) -c Engine/repl.scm -o Engine/repl_p.o
Engine/repl.mco: Engine/repl.scm
	$(MCO) $(MCOFLAGS) Engine/repl.scm -o Engine/repl.mco
Engine/repl_p.mco: Engine/repl.scm
	$(MCO) $(MCOFLAGS) Engine/repl.scm -o Engine/repl_p.mco

Gdb/ginvoke.o: Gdb/ginvoke.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/ginvoke.scm -o Gdb/ginvoke.o
Gdb/ginvoke_p.o: Gdb/ginvoke.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/ginvoke.scm -o Gdb/ginvoke_p.o
Gdb/ginvoke.mco: Gdb/ginvoke.scm
	$(MCO) $(MCOFLAGS) Gdb/ginvoke.scm -o Gdb/ginvoke.mco
Gdb/ginvoke_p.mco: Gdb/ginvoke.scm
	$(MCO) $(MCOFLAGS) Gdb/ginvoke.scm -o Gdb/ginvoke_p.mco

Patient/server.o: Patient/server.scm
	$(BIGLOO) $(BFLAGS) -c Patient/server.scm -o Patient/server.o
Patient/server_p.o: Patient/server.scm
	$(BIGLOO) $(BFLAGS_P) -c Patient/server.scm -o Patient/server_p.o
Patient/server.mco: Patient/server.scm
	$(MCO) $(MCOFLAGS) Patient/server.scm -o Patient/server.mco
Patient/server_p.mco: Patient/server.scm
	$(MCO) $(MCOFLAGS) Patient/server.scm -o Patient/server_p.mco

Command/run.o: Command/run.scm
	$(BIGLOO) $(BFLAGS) -c Command/run.scm -o Command/run.o
Command/run_p.o: Command/run.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/run.scm -o Command/run_p.o
Command/run.mco: Command/run.scm
	$(MCO) $(MCOFLAGS) Command/run.scm -o Command/run.mco
Command/run_p.mco: Command/run.scm
	$(MCO) $(MCOFLAGS) Command/run.scm -o Command/run_p.mco

Tools/speek.o: Tools/speek.scm
	$(BIGLOO) $(BFLAGS) -c Tools/speek.scm -o Tools/speek.o
Tools/speek_p.o: Tools/speek.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/speek.scm -o Tools/speek_p.o
Tools/speek.mco: Tools/speek.scm
	$(MCO) $(MCOFLAGS) Tools/speek.scm -o Tools/speek.mco
Tools/speek_p.mco: Tools/speek.scm
	$(MCO) $(MCOFLAGS) Tools/speek.scm -o Tools/speek_p.mco

Command/all_cmd.o: Command/all_cmd.scm
	$(BIGLOO) $(BFLAGS) -c Command/all_cmd.scm -o Command/all_cmd.o
Command/all_cmd_p.o: Command/all_cmd.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/all_cmd.scm -o Command/all_cmd_p.o
Command/all_cmd.mco: Command/all_cmd.scm
	$(MCO) $(MCOFLAGS) Command/all_cmd.scm -o Command/all_cmd.mco
Command/all_cmd_p.mco: Command/all_cmd.scm
	$(MCO) $(MCOFLAGS) Command/all_cmd.scm -o Command/all_cmd_p.mco

Tools/date.o: Tools/date.scm
	$(BIGLOO) $(BFLAGS) -c Tools/date.scm -o Tools/date.o
Tools/date_p.o: Tools/date.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/date.scm -o Tools/date_p.o
Tools/date.mco: Tools/date.scm
	$(MCO) $(MCOFLAGS) Tools/date.scm -o Tools/date.mco
Tools/date_p.mco: Tools/date.scm
	$(MCO) $(MCOFLAGS) Tools/date.scm -o Tools/date_p.mco

Init/parse_args.o: Init/parse_args.scm
	$(BIGLOO) $(BFLAGS) -c Init/parse_args.scm -o Init/parse_args.o
Init/parse_args_p.o: Init/parse_args.scm
	$(BIGLOO) $(BFLAGS_P) -c Init/parse_args.scm -o Init/parse_args_p.o
Init/parse_args.mco: Init/parse_args.scm
	$(MCO) $(MCOFLAGS) Init/parse_args.scm -o Init/parse_args.mco
Init/parse_args_p.mco: Init/parse_args.scm
	$(MCO) $(MCOFLAGS) Init/parse_args.scm -o Init/parse_args_p.mco

Tools/io.o: Tools/io.scm
	$(BIGLOO) $(BFLAGS) -c Tools/io.scm -o Tools/io.o
Tools/io_p.o: Tools/io.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/io.scm -o Tools/io_p.o
Tools/io.mco: Tools/io.scm
	$(MCO) $(MCOFLAGS) Tools/io.scm -o Tools/io.mco
Tools/io_p.mco: Tools/io.scm
	$(MCO) $(MCOFLAGS) Tools/io.scm -o Tools/io_p.mco

Command/help.o: Command/help.scm
	$(BIGLOO) $(BFLAGS) -c Command/help.scm -o Command/help.o
Command/help_p.o: Command/help.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/help.scm -o Command/help_p.o
Command/help.mco: Command/help.scm
	$(MCO) $(MCOFLAGS) Command/help.scm -o Command/help.mco
Command/help_p.mco: Command/help.scm
	$(MCO) $(MCOFLAGS) Command/help.scm -o Command/help_p.mco

Command/print.o: Command/print.scm
	$(BIGLOO) $(BFLAGS) -c Command/print.scm -o Command/print.o
Command/print_p.o: Command/print.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/print.scm -o Command/print_p.o
Command/print.mco: Command/print.scm
	$(MCO) $(MCOFLAGS) Command/print.scm -o Command/print.mco
Command/print_p.mco: Command/print.scm
	$(MCO) $(MCOFLAGS) Command/print.scm -o Command/print_p.mco

Patient/mangling.o: Patient/mangling.scm
	$(BIGLOO) $(BFLAGS) -c Patient/mangling.scm -o Patient/mangling.o
Patient/mangling_p.o: Patient/mangling.scm
	$(BIGLOO) $(BFLAGS_P) -c Patient/mangling.scm -o Patient/mangling_p.o
Patient/mangling.mco: Patient/mangling.scm
	$(MCO) $(MCOFLAGS) Patient/mangling.scm -o Patient/mangling.mco
Patient/mangling_p.mco: Patient/mangling.scm
	$(MCO) $(MCOFLAGS) Patient/mangling.scm -o Patient/mangling_p.mco

Command/whatis.o: Command/whatis.scm
	$(BIGLOO) $(BFLAGS) -c Command/whatis.scm -o Command/whatis.o
Command/whatis_p.o: Command/whatis.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/whatis.scm -o Command/whatis_p.o
Command/whatis.mco: Command/whatis.scm
	$(MCO) $(MCOFLAGS) Command/whatis.scm -o Command/whatis.mco
Command/whatis_p.mco: Command/whatis.scm
	$(MCO) $(MCOFLAGS) Command/whatis.scm -o Command/whatis_p.mco

Tools/tools.o: Tools/tools.scm
	$(BIGLOO) $(BFLAGS) -c Tools/tools.scm -o Tools/tools.o
Tools/tools_p.o: Tools/tools.scm
	$(BIGLOO) $(BFLAGS_P) -c Tools/tools.scm -o Tools/tools_p.o
Tools/tools.mco: Tools/tools.scm
	$(MCO) $(MCOFLAGS) Tools/tools.scm -o Tools/tools.mco
Tools/tools_p.mco: Tools/tools.scm
	$(MCO) $(MCOFLAGS) Tools/tools.scm -o Tools/tools_p.mco

Patient/pinvoke.o: Patient/pinvoke.scm
	$(BIGLOO) $(BFLAGS) -c Patient/pinvoke.scm -o Patient/pinvoke.o
Patient/pinvoke_p.o: Patient/pinvoke.scm
	$(BIGLOO) $(BFLAGS_P) -c Patient/pinvoke.scm -o Patient/pinvoke_p.o
Patient/pinvoke.mco: Patient/pinvoke.scm
	$(MCO) $(MCOFLAGS) Patient/pinvoke.scm -o Patient/pinvoke.mco
Patient/pinvoke_p.mco: Patient/pinvoke.scm
	$(MCO) $(MCOFLAGS) Patient/pinvoke.scm -o Patient/pinvoke_p.mco

Command/file.o: Command/file.scm
	$(BIGLOO) $(BFLAGS) -c Command/file.scm -o Command/file.o
Command/file_p.o: Command/file.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/file.scm -o Command/file_p.o
Command/file.mco: Command/file.scm
	$(MCO) $(MCOFLAGS) Command/file.scm -o Command/file.mco
Command/file_p.mco: Command/file.scm
	$(MCO) $(MCOFLAGS) Command/file.scm -o Command/file_p.mco

Command/break.o: Command/break.scm
	$(BIGLOO) $(BFLAGS) -c Command/break.scm -o Command/break.o
Command/break_p.o: Command/break.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/break.scm -o Command/break_p.o
Command/break.mco: Command/break.scm
	$(MCO) $(MCOFLAGS) Command/break.scm -o Command/break.mco
Command/break_p.mco: Command/break.scm
	$(MCO) $(MCOFLAGS) Command/break.scm -o Command/break_p.mco

Command/mangle.o: Command/mangle.scm
	$(BIGLOO) $(BFLAGS) -c Command/mangle.scm -o Command/mangle.o
Command/mangle_p.o: Command/mangle.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/mangle.scm -o Command/mangle_p.o
Command/mangle.mco: Command/mangle.scm
	$(MCO) $(MCOFLAGS) Command/mangle.scm -o Command/mangle.mco
Command/mangle_p.mco: Command/mangle.scm
	$(MCO) $(MCOFLAGS) Command/mangle.scm -o Command/mangle_p.mco

Gdb/proc.o: Gdb/proc.scm
	$(BIGLOO) $(BFLAGS) -c Gdb/proc.scm -o Gdb/proc.o
Gdb/proc_p.o: Gdb/proc.scm
	$(BIGLOO) $(BFLAGS_P) -c Gdb/proc.scm -o Gdb/proc_p.o
Gdb/proc.mco: Gdb/proc.scm
	$(MCO) $(MCOFLAGS) Gdb/proc.scm -o Gdb/proc.mco
Gdb/proc_p.mco: Gdb/proc.scm
	$(MCO) $(MCOFLAGS) Gdb/proc.scm -o Gdb/proc_p.mco

Command/expr.o: Command/expr.scm
	$(BIGLOO) $(BFLAGS) -c Command/expr.scm -o Command/expr.o
Command/expr_p.o: Command/expr.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/expr.scm -o Command/expr_p.o
Command/expr.mco: Command/expr.scm
	$(MCO) $(MCOFLAGS) Command/expr.scm -o Command/expr.mco
Command/expr_p.mco: Command/expr.scm
	$(MCO) $(MCOFLAGS) Command/expr.scm -o Command/expr_p.mco

Command/info.o: Command/info.scm
	$(BIGLOO) $(BFLAGS) -c Command/info.scm -o Command/info.o
Command/info_p.o: Command/info.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/info.scm -o Command/info_p.o
Command/info.mco: Command/info.scm
	$(MCO) $(MCOFLAGS) Command/info.scm -o Command/info.mco
Command/info_p.mco: Command/info.scm
	$(MCO) $(MCOFLAGS) Command/info.scm -o Command/info_p.mco

Bee/binvoke.o: Bee/binvoke.scm
	$(BIGLOO) $(BFLAGS) -c Bee/binvoke.scm -o Bee/binvoke.o
Bee/binvoke_p.o: Bee/binvoke.scm
	$(BIGLOO) $(BFLAGS_P) -c Bee/binvoke.scm -o Bee/binvoke_p.o
Bee/binvoke.mco: Bee/binvoke.scm
	$(MCO) $(MCOFLAGS) Bee/binvoke.scm -o Bee/binvoke.mco
Bee/binvoke_p.mco: Bee/binvoke.scm
	$(MCO) $(MCOFLAGS) Bee/binvoke.scm -o Bee/binvoke_p.mco

Command/quit.o: Command/quit.scm
	$(BIGLOO) $(BFLAGS) -c Command/quit.scm -o Command/quit.o
Command/quit_p.o: Command/quit.scm
	$(BIGLOO) $(BFLAGS_P) -c Command/quit.scm -o Command/quit_p.o
Command/quit.mco: Command/quit.scm
	$(MCO) $(MCOFLAGS) Command/quit.scm -o Command/quit.mco
Command/quit_p.mco: Command/quit.scm
	$(MCO) $(MCOFLAGS) Command/quit.scm -o Command/quit_p.mco

Init/main.o: Init/main.scm
	$(BIGLOO) $(BFLAGS) -c Init/main.scm -o Init/main.o
Init/main_p.o: Init/main.scm
	$(BIGLOO) $(BFLAGS_P) -c Init/main.scm -o Init/main_p.o
Init/main.mco: Init/main.scm
	$(MCO) $(MCOFLAGS) Init/main.scm -o Init/main.mco
Init/main_p.mco: Init/main.scm
	$(MCO) $(MCOFLAGS) Init/main.scm -o Init/main_p.mco

#*---------------------------------------------------------------------*/
#*    # C objects                                                      */
#*---------------------------------------------------------------------*/
Tools/tty.o: Tools/tty.c
	echo Tools/tty.c:
	$(CC) $(CFLAGS) -c Tools/tty.c -o Tools/tty.o
Tools/tty_p.o: Tools/tty.c
	echo Tools/tty.c:
	$(CC) $(CFLAGS_P) -c Tools/tty.c -o Tools/tty.o; mv Tools/tty.o Tools/tty_p.o

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
./Command/all_cmd.o ./Command/all_cmd.class: ./Command/set.mco           \
      ./Command/whatis.mco ./Command/explore.mco ./Command/display.mco   \
      ./Command/print.mco ./Command/backtrace.mco ./Command/info.mco     \
      ./Command/mangle.mco ./Command/break.mco ./Command/file.mco        \
      ./Command/run.mco ./Command/quit.mco ./Command/help.mco 
Command/command.o Command/command.class: ./Command/parse.mco 
./Gdb/gread.o ./Gdb/gread.class: ./Tools/io.mco ./Gdb/proc.mco           \
      ./Tools/tools.mco ./Engine/param.mco ./Tools/speek.mco 
./Tools/version.o ./Tools/version.class: ./Tools/io.mco                  \
      ./Tools/speek.mco ./Engine/param.mco 
./Gdb/tools.o ./Gdb/tools.class: ./Gdb/ginvoke.mco ./Tools/tools.mco 
./Command/help.o ./Command/help.class: ./Tools/io.mco ./Engine/param.mco \
      ./Gdb/ginvoke.mco ./Command/command.mco 
./Bee/etags.o ./Bee/etags.class: ./Engine/repl.mco ./Tools/io.mco        \
      ./Tools/error.mco ./Tools/speek.mco ./Engine/param.mco 
./Command/whatis.o ./Command/whatis.class: ./Command/command.mco         \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
./Init/parse_args.o ./Init/parse_args.class: ./Tools/version.mco         \
      ./Engine/param.mco 
Command/explore.o Command/explore.class: ./Command/command.mco           \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
Init/setrc.o Init/setrc.class: ./Engine/param.mco 
Command/parse.o Command/parse.class: ./Gdb/annotate.mco                  \
      ./Gdb/ginvoke.mco ./Command/command.mco ./Tools/io.mco             \
      ./Tools/speek.mco ./Tools/error.mco ./Engine/param.mco 
./Command/mangle.o ./Command/mangle.class: ./Engine/param.mco            \
      ./Gdb/annotate.mco ./Gdb/ginvoke.mco ./Command/parse.mco           \
      ./Command/command.mco ./Patient/pinvoke.mco ./Patient/mangling.mco \
      ./Tools/io.mco ./Tools/error.mco 
Gdb/gread.o Gdb/gread.class: ./Tools/io.mco ./Gdb/proc.mco               \
      ./Tools/tools.mco ./Engine/param.mco ./Tools/speek.mco 
./Engine/repl.o ./Engine/repl.class: ./Tools/speek.mco                   \
      ./Tools/version.mco ./Tools/io.mco ./Patient/mangling.mco          \
      ./Gdb/proc.mco ./Gdb/annotate.mco ./Command/parse.mco              \
      ./Engine/param.mco ./Engine/hooks.sch 
Gdb/tools.o Gdb/tools.class: ./Gdb/ginvoke.mco ./Tools/tools.mco 
./Gdb/ginvoke.o ./Gdb/ginvoke.class: ./Tools/error.mco ./Engine/repl.mco \
      ./Engine/param.mco ./Tools/io.mco ./Tools/tools.mco                \
      ./Tools/speek.mco ./Gdb/gread.mco ./Gdb/annotate.mco               \
      ./Gdb/proc.mco 
Engine/engine.o Engine/engine.class: ./Tools/speek.mco                   \
      ./Patient/pinvoke.mco ./Command/backtrace.mco ./Command/run.mco    \
      ./Bee/binvoke.mco ./Bee/etags.mco ./Patient/server.mco             \
      ./Gdb/annotate.mco ./Gdb/ginvoke.mco ./Gdb/proc.mco                \
      ./Tools/error.mco ./Engine/repl.mco ./Engine/param.mco             \
      ./Engine/hooks.sch 
Command/set.o Command/set.class: ./Tools/error.mco ./Gdb/ginvoke.mco     \
      ./Patient/server.mco ./Tools/io.mco ./Engine/engine.mco            \
      ./Engine/param.mco ./Command/parse.mco ./Command/command.mco       \
      ./Engine/hooks.sch 
./Command/run.o ./Command/run.class: ./Gdb/ginvoke.mco                   \
      ./Patient/server.mco ./Tools/io.mco ./Engine/engine.mco            \
      ./Engine/param.mco ./Command/parse.mco ./Command/command.mco       \
      ./Engine/hooks.sch 
Bee/etags.o Bee/etags.class: ./Engine/repl.mco ./Tools/io.mco            \
      ./Tools/error.mco ./Tools/speek.mco ./Engine/param.mco 
./Tools/speek.o ./Tools/speek.class: ./Engine/param.mco 
./Command/file.o ./Command/file.class: ./Bee/etags.mco ./Tools/io.mco    \
      ./Patient/mangling.mco ./Engine/param.mco ./Gdb/annotate.mco       \
      ./Gdb/ginvoke.mco ./Command/expr.mco ./Command/command.mco 
Tools/error.o Tools/error.class: ./Tools/io.mco ./Engine/repl.mco        \
      ./Engine/param.mco 
Command/backtrace.o Command/backtrace.class: ./Command/command.mco       \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco 
./Tools/io.o ./Tools/io.class: ./Engine/param.mco 
./Patient/pinvoke.o ./Patient/pinvoke.class: ./Gdb/annotate.mco          \
      ./Gdb/ginvoke.mco ./Patient/server.mco ./Engine/param.mco 
./Command/expr.o ./Command/expr.class: ./Tools/error.mco                 \
      ./Patient/mangling.mco ./Engine/param.mco ./Tools/speek.mco        \
      ./Tools/tools.mco 
Patient/value.o Patient/value.class: ./Engine/param.mco                  \
      ./Patient/pinvoke.mco 
./Command/print.o ./Command/print.class: ./Command/command.mco           \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
./Command/info.o ./Command/info.class: ./Tools/speek.mco                 \
      ./Engine/param.mco ./Gdb/tools.mco ./Gdb/annotate.mco              \
      ./Gdb/ginvoke.mco ./Command/whatis.mco ./Command/break.mco         \
      ./Command/backtrace.mco ./Command/parse.mco ./Command/command.mco  \
      ./Patient/value.mco ./Patient/pinvoke.mco ./Patient/mangling.mco   \
      ./Tools/io.mco ./Tools/tools.mco ./Tools/error.mco 
./Command/quit.o ./Command/quit.class: ./Gdb/annotate.mco                \
      ./Gdb/ginvoke.mco ./Tools/io.mco ./Engine/engine.mco               \
      ./Engine/param.mco ./Command/parse.mco ./Command/command.mco 
Gdb/annotate.o Gdb/annotate.class: ./Patient/mangling.mco                \
      ./Command/expr.mco ./Command/explore.mco ./Command/display.mco     \
      ./Command/print.mco ./Command/backtrace.mco ./Engine/param.mco     \
      ./Tools/io.mco ./Tools/speek.mco ./Tools/tools.mco ./Gdb/gread.mco \
      ./Gdb/proc.mco ./Gdb/ginvoke.mco 
Gdb/autoconf.o Gdb/autoconf.class: ./Engine/param.mco 
Tools/version.o Tools/version.class: ./Tools/io.mco ./Tools/speek.mco    \
      ./Engine/param.mco 
Engine/param.o Engine/param.class: ./Tools/date.mco 
Command/display.o Command/display.class: ./Command/print.mco             \
      ./Command/command.mco ./Engine/param.mco ./Gdb/annotate.mco        \
      ./Gdb/ginvoke.mco ./Gdb/tools.mco ./Patient/value.mco              \
      ./Patient/pinvoke.mco ./Patient/mangling.mco ./Tools/io.mco        \
      ./Tools/tools.mco ./Tools/error.mco ./Command/expr.mco 
./Command/command.o ./Command/command.class: ./Command/parse.mco 
./Command/break.o ./Command/break.class: ./Gdb/annotate.mco              \
      ./Gdb/ginvoke.mco ./Engine/param.mco ./Command/expr.mco            \
      ./Command/parse.mco ./Command/command.mco ./Bee/etags.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco 
Engine/repl.o Engine/repl.class: ./Tools/speek.mco ./Tools/version.mco   \
      ./Tools/io.mco ./Patient/mangling.mco ./Gdb/proc.mco               \
      ./Gdb/annotate.mco ./Command/parse.mco ./Engine/param.mco          \
      ./Engine/hooks.sch 
Gdb/ginvoke.o Gdb/ginvoke.class: ./Tools/error.mco ./Engine/repl.mco     \
      ./Engine/param.mco ./Tools/io.mco ./Tools/tools.mco                \
      ./Tools/speek.mco ./Gdb/gread.mco ./Gdb/annotate.mco               \
      ./Gdb/proc.mco 
Command/run.o Command/run.class: ./Gdb/ginvoke.mco ./Patient/server.mco  \
      ./Tools/io.mco ./Engine/engine.mco ./Engine/param.mco              \
      ./Command/parse.mco ./Command/command.mco ./Engine/hooks.sch 
Patient/server.o Patient/server.class: ./Gdb/ginvoke.mco                 \
      ./Tools/speek.mco ./Tools/error.mco 
Command/all_cmd.o Command/all_cmd.class: ./Command/set.mco               \
      ./Command/whatis.mco ./Command/explore.mco ./Command/display.mco   \
      ./Command/print.mco ./Command/backtrace.mco ./Command/info.mco     \
      ./Command/mangle.mco ./Command/break.mco ./Command/file.mco        \
      ./Command/run.mco ./Command/quit.mco ./Command/help.mco 
Tools/speek.o Tools/speek.class: ./Engine/param.mco 
./Tools/tools.o ./Tools/tools.class: ./Engine/param.mco 
./Gdb/proc.o ./Gdb/proc.class: ./Patient/mangling.mco ./Gdb/autoconf.mco \
      ./Gdb/ginvoke.mco ./Tools/io.mco ./Tools/speek.mco                 \
      ./Tools/error.mco ./Command/file.mco ./Engine/param.mco            \
      ./Engine/hooks.sch 
./Command/explore.o ./Command/explore.class: ./Command/command.mco       \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
./Command/backtrace.o ./Command/backtrace.class: ./Command/command.mco   \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco 
./Init/setrc.o ./Init/setrc.class: ./Engine/param.mco 
Tools/io.o Tools/io.class: ./Engine/param.mco 
Init/parse_args.o Init/parse_args.class: ./Tools/version.mco             \
      ./Engine/param.mco 
Command/help.o Command/help.class: ./Tools/io.mco ./Engine/param.mco     \
      ./Gdb/ginvoke.mco ./Command/command.mco 
./Bee/binvoke.o ./Bee/binvoke.class: ./Engine/param.mco 
./Command/parse.o ./Command/parse.class: ./Gdb/annotate.mco              \
      ./Gdb/ginvoke.mco ./Command/command.mco ./Tools/io.mco             \
      ./Tools/speek.mco ./Tools/error.mco ./Engine/param.mco 
./Gdb/annotate.o ./Gdb/annotate.class: ./Patient/mangling.mco            \
      ./Command/expr.mco ./Command/explore.mco ./Command/display.mco     \
      ./Command/print.mco ./Command/backtrace.mco ./Engine/param.mco     \
      ./Tools/io.mco ./Tools/speek.mco ./Tools/tools.mco ./Gdb/gread.mco \
      ./Gdb/proc.mco ./Gdb/ginvoke.mco 
Command/print.o Command/print.class: ./Command/command.mco               \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
Patient/mangling.o Patient/mangling.class: ./Tools/speek.mco             \
      ./Gdb/annotate.mco ./Command/expr.mco ./Bee/binvoke.mco            \
      ./Patient/pinvoke.mco ./Engine/param.mco ./Tools/tools.mco 
./Patient/server.o ./Patient/server.class: ./Gdb/ginvoke.mco             \
      ./Tools/speek.mco ./Tools/error.mco 
./Gdb/autoconf.o ./Gdb/autoconf.class: ./Engine/param.mco 
./Engine/engine.o ./Engine/engine.class: ./Tools/speek.mco               \
      ./Patient/pinvoke.mco ./Command/backtrace.mco ./Command/run.mco    \
      ./Bee/binvoke.mco ./Bee/etags.mco ./Patient/server.mco             \
      ./Gdb/annotate.mco ./Gdb/ginvoke.mco ./Gdb/proc.mco                \
      ./Tools/error.mco ./Engine/repl.mco ./Engine/param.mco             \
      ./Engine/hooks.sch 
Command/whatis.o Command/whatis.class: ./Command/command.mco             \
      ./Engine/param.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco            \
      ./Gdb/tools.mco ./Patient/value.mco ./Patient/pinvoke.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco ./Command/expr.mco 
./Engine/param.o ./Engine/param.class: ./Tools/date.mco 
Tools/tools.o Tools/tools.class: ./Engine/param.mco 
Patient/pinvoke.o Patient/pinvoke.class: ./Gdb/annotate.mco              \
      ./Gdb/ginvoke.mco ./Patient/server.mco ./Engine/param.mco 
Command/file.o Command/file.class: ./Bee/etags.mco ./Tools/io.mco        \
      ./Patient/mangling.mco ./Engine/param.mco ./Gdb/annotate.mco       \
      ./Gdb/ginvoke.mco ./Command/expr.mco ./Command/command.mco 
Command/break.o Command/break.class: ./Gdb/annotate.mco                  \
      ./Gdb/ginvoke.mco ./Engine/param.mco ./Command/expr.mco            \
      ./Command/parse.mco ./Command/command.mco ./Bee/etags.mco          \
      ./Patient/mangling.mco ./Tools/io.mco ./Tools/tools.mco            \
      ./Tools/error.mco 
Command/mangle.o Command/mangle.class: ./Engine/param.mco                \
      ./Gdb/annotate.mco ./Gdb/ginvoke.mco ./Command/parse.mco           \
      ./Command/command.mco ./Patient/pinvoke.mco ./Patient/mangling.mco \
      ./Tools/io.mco ./Tools/error.mco 
./Command/display.o ./Command/display.class: ./Command/print.mco         \
      ./Command/command.mco ./Engine/param.mco ./Gdb/annotate.mco        \
      ./Gdb/ginvoke.mco ./Gdb/tools.mco ./Patient/value.mco              \
      ./Patient/pinvoke.mco ./Patient/mangling.mco ./Tools/io.mco        \
      ./Tools/tools.mco ./Tools/error.mco ./Command/expr.mco 
Gdb/proc.o Gdb/proc.class: ./Patient/mangling.mco ./Gdb/autoconf.mco     \
      ./Gdb/ginvoke.mco ./Tools/io.mco ./Tools/speek.mco                 \
      ./Tools/error.mco ./Command/file.mco ./Engine/param.mco            \
      ./Engine/hooks.sch 
./Command/set.o ./Command/set.class: ./Tools/error.mco ./Gdb/ginvoke.mco \
      ./Patient/server.mco ./Tools/io.mco ./Engine/engine.mco            \
      ./Engine/param.mco ./Command/parse.mco ./Command/command.mco       \
      ./Engine/hooks.sch 
./Patient/mangling.o ./Patient/mangling.class: ./Tools/speek.mco         \
      ./Gdb/annotate.mco ./Command/expr.mco ./Bee/binvoke.mco            \
      ./Patient/pinvoke.mco ./Engine/param.mco ./Tools/tools.mco 
./Patient/value.o ./Patient/value.class: ./Engine/param.mco              \
      ./Patient/pinvoke.mco 
Command/expr.o Command/expr.class: ./Tools/error.mco                     \
      ./Patient/mangling.mco ./Engine/param.mco ./Tools/speek.mco        \
      ./Tools/tools.mco 
Command/info.o Command/info.class: ./Tools/speek.mco ./Engine/param.mco  \
      ./Gdb/tools.mco ./Gdb/annotate.mco ./Gdb/ginvoke.mco               \
      ./Command/whatis.mco ./Command/break.mco ./Command/backtrace.mco   \
      ./Command/parse.mco ./Command/command.mco ./Patient/value.mco      \
      ./Patient/pinvoke.mco ./Patient/mangling.mco ./Tools/io.mco        \
      ./Tools/tools.mco ./Tools/error.mco 
Command/quit.o Command/quit.class: ./Gdb/annotate.mco ./Gdb/ginvoke.mco  \
      ./Tools/io.mco ./Engine/engine.mco ./Engine/param.mco              \
      ./Command/parse.mco ./Command/command.mco 
Bee/binvoke.o Bee/binvoke.class: ./Engine/param.mco 
Init/main.o Init/main.class: ./Engine/repl.mco ./Engine/engine.mco       \
      ./Command/all_cmd.mco ./Init/parse_args.mco ./Init/setrc.mco 
./Tools/error.o ./Tools/error.class: ./Tools/io.mco ./Engine/repl.mco    \
      ./Engine/param.mco 

#bdepend stop
