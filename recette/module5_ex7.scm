(module ex7
   (extern "C"
      (include "stdio.h")
      (macro $printf::long (::string ::long) "printf"))
   (extern "wasm"
      ($printf "$foo")))
