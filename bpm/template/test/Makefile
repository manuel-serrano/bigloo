BIGLOO = bigloo

BCFLAGS =
BWFLAGS = -wasm
BJFLAGS = -jvm

.SUFFIXES:
.SUFFIXES: .bgl .o .class .wasm

all:
	if [ "grep backends ../package.sexp | awk '{print $$2}' | grep c" -a "`$(BIGLOO) -config backend-c`" = "#t" ]; then $(MAKE) c; fi
	if [ "grep backends ../package.sexp | grep jvm" -a "`$(BIGLOO) -config backend-jvm`" = "#t" ]; then $(MAKE) jvm; fi
	if [ "grep backends ../package.sexp | grep wasm" -a "`$(BIGLOO) -config backend-wasm`" = "#t" ]; then $(MAKE) wasm; fi
	  
c: check_c
jvm: check_jvm
wasm: check_wasm

test_c: test.bgl 
	$(BIGLOO) $(BCFLAGS) $+ -o $@
check_c: test_c
	./test_c

test_jvm: test.bgl 
	$(BIGLOO) $(BJFLAGS) $+ -o $@
check_jvm: test_jvm
	./test_jvm

test_wasm: test.bgl
	$(BIGLOO) $(BWFLAGS) $+ -o $@
check_wasm: test_wasm
	./test_wasm
clean:
	rm -f *.o *.class *.wat 2> /dev/null
	rm -f test_c test_jvm test_wasm

%.o: ../src/%.bgl
	$(BIGLOO) -c $(BCFLAGS) $< -o $@

%.class: ../src/%.bgl
	$(BIGLOO) -c $(BJFLAGS) $< -o $@

%.wat: ../src/%.bgl
	$(BIGLOO) -c $(BWFLAGS) $< -o $@
