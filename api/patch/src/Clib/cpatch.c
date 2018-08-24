/* patch.c */

#include "cpatch.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

/*---------------------------------------------------------------------------*/

#define PATCH_DEBUG
#undef PATCH_DEBUG

void bgl_patch_init(void *__start, void *__etext) {
#ifdef linux
  void *start = (void*)(((uint64_t)__start) & ~4095);
  void *end = (void*)((4095+(uint64_t)__etext) & ~4095);
  int len = (uint64_t)end - (uint64_t)start;
#else
  extern uint8_t _mh_execute_header;
  void *start = (void*)(((uint64_t)__start) & ~4095);
  void *end = (void*)((50*4096+4095+(uint64_t)__start) & ~4095);
  int len = (uint64_t)end - (uint64_t)start;
#endif

  if (mprotect(start, len, PROT_READ|PROT_EXEC|PROT_WRITE)) {
    printf("mprotect failed\n");
    exit(1);
  }
  fprintf( stderr, "MPROTECT s=%p e=%p\n", __start, __etext );
}

void patch_32(patch_descr *patch, patch_32_type val_32) {
#if( defined( PATCH_DEBUG ) )  
  //fprintf( stderr, ">>> patching...%p\n", patch );
   long n = 0;
#endif

  while (patch->addr != NULL) {

    patch_32_type *addr = (patch_32_type*)patch->addr;
#if( defined( PATCH_DEBUG ) )  
    n++;
#endif
    
    if ((patch->kind & 0x100) == 0) {

      switch (patch->kind & 0xff) {
      case 10:
        {
          /* patch a function call */

          patch_32_type val = val_32 - ((patch_32_type)addr + 4);

#ifdef PATCH_DEBUG
          fprintf(stderr, "patch_32 patching function %p at address %p\n", (void*)val_32, addr);
#endif

          *addr = val;

          break;
        }


      case 1:
	{
          patch_32_type val = val_32 * (patch_32_type)patch->mult + (patch_32_type)patch->offs;
	  *addr = val;
	  break;
	}
	
      default:
        {
          /* patch a 32 bit value */

          patch_32_type val = val_32 * (patch_32_type)patch->mult + (patch_32_type)patch->offs;

#ifdef PATCH_DEBUG
          fprintf(stderr, "patch_32 patching value %p->%p address=%p kind=%d offset=%d mult=%d\n", *addr, val, patch->addr, patch->kind, patch->offs, patch->mult );
#endif

	  *addr = val;

          break;
        }
      }
    } else {

      /* patch comparison to constant */

      if ((val_32 == (patch_32_type)patch->mult) ==
          ((patch->kind & 0x200) == 0)) {

        /* must generate JMP */

#ifdef PATCH_DEBUG
        fprintf(stderr, "patch_32 patching JMP at address %p\n", ((int8_t*)addr)-1);
#endif

        /* 0xE9 0xXX 0xXX 0xXX 0xXX = JMP .+5+XXX */

        ((int8_t*)addr)[-1] = 0xE9;
        *addr = (patch_32_type)patch->offs; /* write jump distance */

      } else {

        /* must generate NOP */

#ifdef PATCH_DEBUG
        fprintf(stderr, "patch_32 patching NOP at address %p\n", ((int8_t*)addr)-1);
#endif

        /* 0x0F 0x1F 0x44 0x00 0x00 = NOP DWORD ptr [EAX + EAX*1 + 00H] */

        ((int8_t*)addr)[-1] = 0x0F;
        *addr = 0x0000441F;

      }
    }

    patch++;
  }
#if( defined( PATCH_DEBUG ) )
  if( n > 0 ) fprintf( stderr, "<<< patching...n=%d\n",n );
#endif
}

void patch_64(patch_descr *patch, patch_64_type val_64) {
  printf("patch_64 not implemented yet");
  exit(1);
}

void patch_fn_call(patch_descr *patch, void *val_fn) {
  printf("patch_fn_call not implemented yet");
  exit(1);
}

void patch_fn_ptr(patch_descr *patch, void *val_fn) {
  printf("patch_fn_ptr not implemented yet");
  exit(1);
}

/*---------------------------------------------------------------------------*/

