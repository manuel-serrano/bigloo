BIGLOO = bigloo

BCFLAGS =
BWFLAGS = -wasm
BJFLAGS = -jvm

DEP_TEST_BGH =

.SUFFIXES:
.SUFFIXES: .bgl .o .class .wasm

all:
	if [ "grep backends ../package.sexp | awk '{print $$2}' | grep c" -a "`$(BIGLOO) -config backend-c`" = "#t" ]; then $(MAKE) c; fi
	if [ "grep backends ../package.sexp | grep jvm" -a "`$(BIGLOO) -config backend-jvm`" = "#t" ]; then $(MAKE) jvm; fi
	if [ "grep backends ../package.sexp | grep wasm" -a "`$(BIGLOO) -config backend-wasm`" = "#t" ]; then $(MAKE) wasm; fi
	  
c: check_c
jvm: check_jvm
wasm: check_wasm

test_c: test.bgl $(DEP_TEST_BGH)
	$(BIGLOO) $(BCFLAGS) test.bgl -o $@
check_c: test_c
	./test_c

test_jvm: test.bgl $(DEP_TEST_BGH)
	$(BIGLOO) $(BJFLAGS) test.bgl -o $@
check_jvm: test_jvm
	./test_jvm

test_wasm: test.bgl $(DEP_TEST_BGH)
	$(BIGLOO) $(BWFLAGS) test.bgl -o $@
check_wasm: test_wasm
	./test_wasm
clean:
	rm -f test.o test.class test.wasm
	rm -f test_c test_jvm test_wasm
