#*=====================================================================*/
#*    serrano/prgm/project/bigloo/cigloo/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 14:46:31 1998                          */
#*    Last change :  Sun Jul  9 10:22:24 2017 (serrano)                */
#*    Copyright   :  1998-2017 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build Cigloo. This makefile has to be processed  */
#*    either by Gnu make or Sun make.                                  */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL           = /bin/sh
# the produced executable
CIGLOO_DEST	= $(BOOTBINDIR)/cigloo$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    Compilation flags                                                */
#*---------------------------------------------------------------------*/
BFLAGS		= -export-exports -lib-dir $(BOOTLIBDIR) -afile .afile -g3 -O2 -fsharing -unsafe $(SHRD_BDE_OPT)
EFLAGS		=

#*---------------------------------------------------------------------*/
#*    Les sources                                                      */
#*---------------------------------------------------------------------*/
#*--- init ------------------------------------------------------------*/
INIT_FILE	= main setrc parse_args

INIT_OBJ	= $(INIT_FILE:%=Init/%.o)

#*--- engine ----------------------------------------------------------*/
ENGINE_FILE	= engine param translate

ENGINE_OBJ	= $(ENGINE_FILE:%=Engine/%.o)

#*--- translate -------------------------------------------------------*/
TRANSLATE_FILE	= function declaration type decl tspec expr eval ident

TRANSLATE_OBJ	= $(TRANSLATE_FILE:%=Translate/%.o)


#*--- tools -----------------------------------------------------------*/
TOOLS_FILE	= speek error

TOOLS_OBJ	= $(TOOLS_FILE:%=Tools/%.o)

#*--- write -----------------------------------------------------------*/
WRITE_FILE	= version

WRITE_OBJ	= $(WRITE_FILE:%=Write/%.o)

#*--- parser ---------------------------------------------------------*/
PARSER_FILE	= parser lexer tools cpp

PARSER_OBJ	= $(PARSER_FILE:%=Parser/%.o)

#*---------------------------------------------------------------------*/
#*    Tout le code ...                                                 */
#*---------------------------------------------------------------------*/
O_OBJECTS       = $(INIT_OBJ) $(ENGINE_OBJ) $(PARSE_OBJ) $(TOOLS_OBJ) \
                  $(WRITE_OBJ) $(PARSER_OBJ) $(TRANSLATE_OBJ)

SOURCE_FILES	= $(O_OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCE_FILES) \
                  Makefile \
                  Parser/coord.sch \
                  Translate/ast.sch \
                  Translate/type.sch \
                  Translate/function.sch \
                  Tools/union.sch \
                  Example/Makefile \
	 	  Example/b_file.scm \
	          Example/c_file.h \
	          Example/c_file.c \
                  README \
                  .afile

#*---------------------------------------------------------------------*/
#*     Les suffixes ...                                                */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .c .o

#*---------------------------------------------------------------------*/
#*    L'executable `cigloo'                                            */
#*---------------------------------------------------------------------*/
cigloo:
	@ $(MAKE) $(CIGLOO_DEST)

#*---------------------------------------------------------------------*/
#*    $(CIGLOO_DEST)                                                   */
#*---------------------------------------------------------------------*/
$(CIGLOO_DEST): $(O_OBJECTS)
	@ echo "Linking:"
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) $(O_OBJECTS) -o $(CIGLOO_DEST)
	@- $(STRIP) $(CIGLOO_DEST)
	@ echo "Cigloo Done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	if [ $(DESTDIR)$(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/cigloo$(EXE_SUFFIX) $(DESTDIR)$(BINDIR); \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/cigloo$(EXE_SUFFIX); \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/cigloo$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    afile                                                            */
#*---------------------------------------------------------------------*/
afile: 
	@ echo "   - " .afile
	@ $(BGLAFILE) -o .afile $(SOURCE_FILES)
	@ echo "   - dep"
	@ $(MAKE) dep
	@ echo "   - .etags"
	@ $(MAKE) tags

#*---------------------------------------------------------------------*/
#*    tags                                                             */
#*---------------------------------------------------------------------*/
tags:
	@ $(BGLTAGS) -o .etags $(SOURCE_FILES) 

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
dep.old:
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -n `expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(BDEPEND_EXE) -fno-mco $(SOURCE_FILES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

dep:
	@ $(BGLDEPEND) -fno-mco $(SOURCE_FILES) -o Makefile

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOODIR)/cigloo ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) cleanall
	@ $(MAKE) afile
	$(RM) -f .afile~ Makefile~

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f $(O_OBJECTS)
	@- $(RM) -f $(O_OBJECTS:%.o=%.c)
	@- $(RM) -f .etags
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	@ (cd Example; $(MAKE) clean)
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(CIGLOO_DEST)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
touchall:
	@ touch $(SOURCE_FILES)
	@ echo "touchall done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    link ...                                                         */
#*---------------------------------------------------------------------*/
link:
	@ echo "Linking:"
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) -o $(CIGLOO_DEST) Init/main.o $(O_OBJECTS) -lm
	@ echo "link done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=cigloo/%)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
.scm.o:
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .c                                                     */
#*---------------------------------------------------------------------*/
.scm.c:
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) -C $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*    Parser/parser.o:                                                 */
#*---------------------------------------------------------------------*/
Parser/parser.o: Parser/parser.scm 
	@ $(BIGLOO) $(BOPTFLAGS) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*    Parser/cpp.o:                                                    */
#*---------------------------------------------------------------------*/
Parser/cpp.o: Parser/cpp.scm 
	@ $(BIGLOO) $(BOPTFLAGS) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*    Parser/lexer.o:                                                  */
#*---------------------------------------------------------------------*/
Parser/lexer.o: Parser/lexer.scm 
	@ $(BIGLOO) $(BOPTFLAGS) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
Translate/ast.sch: Tools/union.sch 
./Parser/parser.o ./Parser/parser.class: Translate/ast.sch 
Translate/decl.o Translate/decl.class: Parser/coord.sch                  \
      Translate/ast.sch Translate/type.sch 
Translate/tspec.o Translate/tspec.class: Translate/ast.sch               \
      Translate/type.sch 
./Translate/type.o ./Translate/type.class: Translate/ast.sch             \
      Translate/type.sch 
./Parser/cpp.o ./Parser/cpp.class: Parser/coord.sch 
Parser/tools.o Parser/tools.class: Parser/coord.sch 
Tools/error.o Tools/error.class: Translate/ast.sch Parser/coord.sch 
Translate/function.o Translate/function.class: Parser/coord.sch          \
      Translate/ast.sch Translate/type.sch Translate/function.sch 
./Translate/decl.o ./Translate/decl.class: Parser/coord.sch              \
      Translate/ast.sch Translate/type.sch 
./Translate/function.o ./Translate/function.class: Parser/coord.sch      \
      Translate/ast.sch Translate/type.sch Translate/function.sch 
Parser/parser.o Parser/parser.class: Translate/ast.sch 
Translate/declaration.o Translate/declaration.class: Translate/ast.sch   \
      Translate/type.sch 
Engine/translate.o Engine/translate.class: Parser/coord.sch              \
      Translate/ast.sch 
./Translate/declaration.o ./Translate/declaration.class:                 \
      Translate/ast.sch Translate/type.sch 
./Translate/tspec.o ./Translate/tspec.class: Translate/ast.sch           \
      Translate/type.sch 
./Engine/translate.o ./Engine/translate.class: Parser/coord.sch          \
      Translate/ast.sch 
Translate/expr.o Translate/expr.class: Translate/ast.sch                 \
      Translate/type.sch 
./Parser/tools.o ./Parser/tools.class: Parser/coord.sch 
Parser/cpp.o Parser/cpp.class: Parser/coord.sch 
./Translate/expr.o ./Translate/expr.class: Translate/ast.sch             \
      Translate/type.sch 
Translate/type.o Translate/type.class: Translate/ast.sch                 \
      Translate/type.sch 
./Tools/error.o ./Tools/error.class: Translate/ast.sch Parser/coord.sch 

#bdepend stop
