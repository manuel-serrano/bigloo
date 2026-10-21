BIGLOO=bigloo
BFLAGS=-I src
BJFLAGS=-jvm
BWFLAGS=-wasm

-include pkg/Makefile.pkg

c: @PKG@
jvm: @PKG@.jvm
wasm: @PKG@.wasm

@PKG@: obj/@PKG@.o $(PKG_OBJECTS)
        $(BIGLOO) $(BFLAGS) $+ -o $@

obj/@PKG@.o: src/@PKG@.bgl
        $(BIGLOO) -c $(BFLAGS) $< -o $@

@PKG@.jvm: obj/@PKG@.class $(PKG_JOBJECTS)
        $(BIGLOO) -jvm-mainclass @PKG@ -jvm-classpath obj $(PKG_CLASSPATH) $(BFLAGS) $(BJFLAGS) $+ -o $@

obj/@PKG@.class: src/@PKG@.bgl
        $(BIGLOO) -c $(BFLAGS) $(BJFLAGS) $< -o $@

@PKG@.wasm: obj/@PKG@.wat $(PKG_WOBJECTS)
        $(BIGLOO) $(BFLAGS) $(BWFLAGS) $+ -o $@

obj/@PKG@.wat: src/@PKG@.bgl
        $(BIGLOO) -c $(BFLAGS) $(BWFLAGS) $< -o $@

clean:
        rm -f obj/@PKG@.o @PKG@ $(PKG_OBJECTS)
        rm -f obj/@PKG@.class @PKG@.jvm $(PKG_JOBJECTS)
        rm -f obj/@PKG@.obj @PKG@.wasm $(PKG_WOBJECTS)
