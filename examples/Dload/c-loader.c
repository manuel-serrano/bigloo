#include <stdio.h>
#include <dlfcn.h>

main(int argc, char *argv[]) {
   void *h;
   void (*s)();

   printf("loading: %s\n", argv[1]);
   h = dlopen(argv[1], RTLD_LAZY | RTLD_GLOBAL);
   if (h == 0)
   {
      fprintf(stderr, "dlopen failed: %s\n", dlerror());
      exit(-1);
   }
   printf("loaded...\n");
   exit( 0 );
}

bigloo_abort() {
}
