# Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

SRCS = watib.scm Misc/let-if.scm Opt/optimise.scm Val/validate.scm Asm/binary.scm Opt/TestBr/walk.scm Opt/UnCast/walk.scm Opt/Unreachable/walk.scm Opt/Const/walk.scm Env/env.scm Ast/node.scm Misc/list.scm Type/type.scm Type/match.scm Misc/parse.scm  Asm/leb128.scm

OBJS = $(SRCS:.scm=.o)

FLAGS = -O2 -g
#FLAGS = -O3 -unsafe

all: watib

watib: $(OBJS)
	bigloo $(FLAGS) $(OBJS) -o watib

%.o : %.scm
	bigloo -srfi multijob -c $(FLAGS) $< -o $@

report.pdf: report/report.tex
	latexmk -pdf report/report.tex

clean:
	latexmk -C report/report.tex
	rm -f report-blx.bib
	rm -f $(OBJS) *.o
	rm -f watib
