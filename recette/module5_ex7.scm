(module ex7
   (extern "c"
      (include "stdio.h")
      (macro $printf::long (::string ::long) "printf"))
   (extern "wasm"
      ($printf "$foo")))
