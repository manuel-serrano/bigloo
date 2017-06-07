/* self-mod.c */

#include "self-mod.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

/*---------------------------------------------------------------------------*/

#define NB_PATCHABLE_CONSTANT_FNS 100

void PATCHABLE_CONSTANT_FN0(void) { }
void PATCHABLE_CONSTANT_FN1(void) { }
void PATCHABLE_CONSTANT_FN2(void) { }
void PATCHABLE_CONSTANT_FN3(void) { }
void PATCHABLE_CONSTANT_FN4(void) { }
void PATCHABLE_CONSTANT_FN5(void) { }
void PATCHABLE_CONSTANT_FN6(void) { }
void PATCHABLE_CONSTANT_FN7(void) { }
void PATCHABLE_CONSTANT_FN8(void) { }
void PATCHABLE_CONSTANT_FN9(void) { }
void PATCHABLE_CONSTANT_FN10(void) { }
void PATCHABLE_CONSTANT_FN11(void) { }
void PATCHABLE_CONSTANT_FN12(void) { }
void PATCHABLE_CONSTANT_FN13(void) { }
void PATCHABLE_CONSTANT_FN14(void) { }
void PATCHABLE_CONSTANT_FN15(void) { }
void PATCHABLE_CONSTANT_FN16(void) { }
void PATCHABLE_CONSTANT_FN17(void) { }
void PATCHABLE_CONSTANT_FN18(void) { }
void PATCHABLE_CONSTANT_FN19(void) { }
void PATCHABLE_CONSTANT_FN20(void) { }
void PATCHABLE_CONSTANT_FN21(void) { }
void PATCHABLE_CONSTANT_FN22(void) { }
void PATCHABLE_CONSTANT_FN23(void) { }
void PATCHABLE_CONSTANT_FN24(void) { }
void PATCHABLE_CONSTANT_FN25(void) { }
void PATCHABLE_CONSTANT_FN26(void) { }
void PATCHABLE_CONSTANT_FN27(void) { }
void PATCHABLE_CONSTANT_FN28(void) { }
void PATCHABLE_CONSTANT_FN29(void) { }
void PATCHABLE_CONSTANT_FN30(void) { }
void PATCHABLE_CONSTANT_FN31(void) { }
void PATCHABLE_CONSTANT_FN32(void) { }
void PATCHABLE_CONSTANT_FN33(void) { }
void PATCHABLE_CONSTANT_FN34(void) { }
void PATCHABLE_CONSTANT_FN35(void) { }
void PATCHABLE_CONSTANT_FN36(void) { }
void PATCHABLE_CONSTANT_FN37(void) { }
void PATCHABLE_CONSTANT_FN38(void) { }
void PATCHABLE_CONSTANT_FN39(void) { }
void PATCHABLE_CONSTANT_FN40(void) { }
void PATCHABLE_CONSTANT_FN41(void) { }
void PATCHABLE_CONSTANT_FN42(void) { }
void PATCHABLE_CONSTANT_FN43(void) { }
void PATCHABLE_CONSTANT_FN44(void) { }
void PATCHABLE_CONSTANT_FN45(void) { }
void PATCHABLE_CONSTANT_FN46(void) { }
void PATCHABLE_CONSTANT_FN47(void) { }
void PATCHABLE_CONSTANT_FN48(void) { }
void PATCHABLE_CONSTANT_FN49(void) { }
void PATCHABLE_CONSTANT_FN50(void) { }
void PATCHABLE_CONSTANT_FN51(void) { }
void PATCHABLE_CONSTANT_FN52(void) { }
void PATCHABLE_CONSTANT_FN53(void) { }
void PATCHABLE_CONSTANT_FN54(void) { }
void PATCHABLE_CONSTANT_FN55(void) { }
void PATCHABLE_CONSTANT_FN56(void) { }
void PATCHABLE_CONSTANT_FN57(void) { }
void PATCHABLE_CONSTANT_FN58(void) { }
void PATCHABLE_CONSTANT_FN59(void) { }
void PATCHABLE_CONSTANT_FN60(void) { }
void PATCHABLE_CONSTANT_FN61(void) { }
void PATCHABLE_CONSTANT_FN62(void) { }
void PATCHABLE_CONSTANT_FN63(void) { }
void PATCHABLE_CONSTANT_FN64(void) { }
void PATCHABLE_CONSTANT_FN65(void) { }
void PATCHABLE_CONSTANT_FN66(void) { }
void PATCHABLE_CONSTANT_FN67(void) { }
void PATCHABLE_CONSTANT_FN68(void) { }
void PATCHABLE_CONSTANT_FN69(void) { }
void PATCHABLE_CONSTANT_FN70(void) { }
void PATCHABLE_CONSTANT_FN71(void) { }
void PATCHABLE_CONSTANT_FN72(void) { }
void PATCHABLE_CONSTANT_FN73(void) { }
void PATCHABLE_CONSTANT_FN74(void) { }
void PATCHABLE_CONSTANT_FN75(void) { }
void PATCHABLE_CONSTANT_FN76(void) { }
void PATCHABLE_CONSTANT_FN77(void) { }
void PATCHABLE_CONSTANT_FN78(void) { }
void PATCHABLE_CONSTANT_FN79(void) { }
void PATCHABLE_CONSTANT_FN80(void) { }
void PATCHABLE_CONSTANT_FN81(void) { }
void PATCHABLE_CONSTANT_FN82(void) { }
void PATCHABLE_CONSTANT_FN83(void) { }
void PATCHABLE_CONSTANT_FN84(void) { }
void PATCHABLE_CONSTANT_FN85(void) { }
void PATCHABLE_CONSTANT_FN86(void) { }
void PATCHABLE_CONSTANT_FN87(void) { }
void PATCHABLE_CONSTANT_FN88(void) { }
void PATCHABLE_CONSTANT_FN89(void) { }
void PATCHABLE_CONSTANT_FN90(void) { }
void PATCHABLE_CONSTANT_FN91(void) { }
void PATCHABLE_CONSTANT_FN92(void) { }
void PATCHABLE_CONSTANT_FN93(void) { }
void PATCHABLE_CONSTANT_FN94(void) { }
void PATCHABLE_CONSTANT_FN95(void) { }
void PATCHABLE_CONSTANT_FN96(void) { }
void PATCHABLE_CONSTANT_FN97(void) { }
void PATCHABLE_CONSTANT_FN98(void) { }
void PATCHABLE_CONSTANT_FN99(void) { }

typedef void (*void_fn_ptr)(void);

void_fn_ptr PATCHABLE_CONSTANT_FNS[NB_PATCHABLE_CONSTANT_FNS] = {
PATCHABLE_CONSTANT_FN0,
PATCHABLE_CONSTANT_FN1,
PATCHABLE_CONSTANT_FN2,
PATCHABLE_CONSTANT_FN3,
PATCHABLE_CONSTANT_FN4,
PATCHABLE_CONSTANT_FN5,
PATCHABLE_CONSTANT_FN6,
PATCHABLE_CONSTANT_FN7,
PATCHABLE_CONSTANT_FN8,
PATCHABLE_CONSTANT_FN9,
PATCHABLE_CONSTANT_FN10,
PATCHABLE_CONSTANT_FN11,
PATCHABLE_CONSTANT_FN12,
PATCHABLE_CONSTANT_FN13,
PATCHABLE_CONSTANT_FN14,
PATCHABLE_CONSTANT_FN15,
PATCHABLE_CONSTANT_FN16,
PATCHABLE_CONSTANT_FN17,
PATCHABLE_CONSTANT_FN18,
PATCHABLE_CONSTANT_FN19,
PATCHABLE_CONSTANT_FN20,
PATCHABLE_CONSTANT_FN21,
PATCHABLE_CONSTANT_FN22,
PATCHABLE_CONSTANT_FN23,
PATCHABLE_CONSTANT_FN24,
PATCHABLE_CONSTANT_FN25,
PATCHABLE_CONSTANT_FN26,
PATCHABLE_CONSTANT_FN27,
PATCHABLE_CONSTANT_FN28,
PATCHABLE_CONSTANT_FN29,
PATCHABLE_CONSTANT_FN30,
PATCHABLE_CONSTANT_FN31,
PATCHABLE_CONSTANT_FN32,
PATCHABLE_CONSTANT_FN33,
PATCHABLE_CONSTANT_FN34,
PATCHABLE_CONSTANT_FN35,
PATCHABLE_CONSTANT_FN36,
PATCHABLE_CONSTANT_FN37,
PATCHABLE_CONSTANT_FN38,
PATCHABLE_CONSTANT_FN39,
PATCHABLE_CONSTANT_FN40,
PATCHABLE_CONSTANT_FN41,
PATCHABLE_CONSTANT_FN42,
PATCHABLE_CONSTANT_FN43,
PATCHABLE_CONSTANT_FN44,
PATCHABLE_CONSTANT_FN45,
PATCHABLE_CONSTANT_FN46,
PATCHABLE_CONSTANT_FN47,
PATCHABLE_CONSTANT_FN48,
PATCHABLE_CONSTANT_FN49,
PATCHABLE_CONSTANT_FN50,
PATCHABLE_CONSTANT_FN51,
PATCHABLE_CONSTANT_FN52,
PATCHABLE_CONSTANT_FN53,
PATCHABLE_CONSTANT_FN54,
PATCHABLE_CONSTANT_FN55,
PATCHABLE_CONSTANT_FN56,
PATCHABLE_CONSTANT_FN57,
PATCHABLE_CONSTANT_FN58,
PATCHABLE_CONSTANT_FN59,
PATCHABLE_CONSTANT_FN60,
PATCHABLE_CONSTANT_FN61,
PATCHABLE_CONSTANT_FN62,
PATCHABLE_CONSTANT_FN63,
PATCHABLE_CONSTANT_FN64,
PATCHABLE_CONSTANT_FN65,
PATCHABLE_CONSTANT_FN66,
PATCHABLE_CONSTANT_FN67,
PATCHABLE_CONSTANT_FN68,
PATCHABLE_CONSTANT_FN69,
PATCHABLE_CONSTANT_FN70,
PATCHABLE_CONSTANT_FN71,
PATCHABLE_CONSTANT_FN72,
PATCHABLE_CONSTANT_FN73,
PATCHABLE_CONSTANT_FN74,
PATCHABLE_CONSTANT_FN75,
PATCHABLE_CONSTANT_FN76,
PATCHABLE_CONSTANT_FN77,
PATCHABLE_CONSTANT_FN78,
PATCHABLE_CONSTANT_FN79,
PATCHABLE_CONSTANT_FN80,
PATCHABLE_CONSTANT_FN81,
PATCHABLE_CONSTANT_FN82,
PATCHABLE_CONSTANT_FN83,
PATCHABLE_CONSTANT_FN84,
PATCHABLE_CONSTANT_FN85,
PATCHABLE_CONSTANT_FN86,
PATCHABLE_CONSTANT_FN87,
PATCHABLE_CONSTANT_FN88,
PATCHABLE_CONSTANT_FN89,
PATCHABLE_CONSTANT_FN90,
PATCHABLE_CONSTANT_FN91,
PATCHABLE_CONSTANT_FN92,
PATCHABLE_CONSTANT_FN93,
PATCHABLE_CONSTANT_FN94,
PATCHABLE_CONSTANT_FN95,
PATCHABLE_CONSTANT_FN96,
PATCHABLE_CONSTANT_FN97,
PATCHABLE_CONSTANT_FN98,
PATCHABLE_CONSTANT_FN99
};

uint64_t min_fn_addr;
uint64_t max_fn_addr;

/*---------------------------------------------------------------------------*/

void init_self_mod(void *__start, void *__etext) {
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

  {
    /* preprocess PATCHABLE_CONSTANT_FNS to find min and max */
    int id;
    min_fn_addr = max_fn_addr = (uint64_t)PATCHABLE_CONSTANT_FNS[0];
    for (id=1; id<NB_PATCHABLE_CONSTANT_FNS; id++) {
      uint64_t addr = (uint64_t)PATCHABLE_CONSTANT_FNS[id];
      if (addr < min_fn_addr) {
        min_fn_addr = addr;
      } else if (addr > max_fn_addr) {
        max_fn_addr = addr;
      }
    }
  }
}

/*---------------------------------------------------------------------------*/

#define OFFS_MIN -50
#define OFFS_MAX 50

void init_patch_32(void *fn, size_t len, patch_descr *patch_tab) {

  int8_t *probe = (int8_t*)fn;
  int i;

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    patch->addr = NULL;
  }

  while (*(int64_t*)probe != END_OF_PATCHABLE_FUNCTION_MARKER) {
    patch_32_type val = *(patch_32_type*)probe;
    int32_t mult = 0;
    while (++mult <= 10) {
      int32_t offs = (val % (mult<<(32-BITS_FOR_ID))) -
                     (mult*(PATCHABLE_MARKER_32&((1<<(32-BITS_FOR_ID))-1)));
      if (offs >= OFFS_MIN && offs <= OFFS_MAX) {
        uint64_t id = ((uint64_t)val - offs) / (mult<<(32-BITS_FOR_ID));
        if (id >= len) {
          printf("int32_t constant #%d is out of range\n", id);
          exit(1);
        } else {
          patch_descr *patch = &patch_tab[id];
          if (patch->addr != NULL) {
            printf("int32_t constant #%d appears more than once in machine code\n", id);
            exit(1);
          }
          printf("found int32_t constant #%d at %p with mult=%d and offset=%d\n", id, probe, mult, offs, mult);
          patch->addr = (void*)probe;
          patch->mult = mult;
          patch->offs = offs;
          probe += sizeof (val);
          goto done;
        }
      }
    }
    probe++;
  done:;
  }

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    if (patch->addr == NULL) {
      printf("int32_t constant #%d was not found in the machine code\n", i);
      exit(1);
    }
  }
}

void init_patch_64(void *fn, size_t len, patch_descr *patch_tab) {

  int8_t *probe = (int8_t*)fn;
  int i;

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    patch->addr = NULL;
  }

  while (*(int64_t*)probe != END_OF_PATCHABLE_FUNCTION_MARKER) {
    patch_64_type val = *(patch_64_type*)probe;
    int32_t mult = 0;
    while (++mult <= 10) {
      int32_t offs = (val % ((uint64_t)mult<<(64-BITS_FOR_ID))) -
                     (mult*(PATCHABLE_MARKER_64&((1LL<<(64-BITS_FOR_ID))-1)));
      if (offs >= OFFS_MIN && offs <= OFFS_MAX) {
        uint64_t id = ((uint64_t)val - offs) / ((uint64_t)mult<<(64-BITS_FOR_ID));
        if (id >= len) {
          printf("int64_t constant #%d is out of range\n", id);
          exit(1);
        } else {
          patch_descr *patch = &patch_tab[id];
          if (patch->addr != NULL) {
            printf("int64_t constant #%d appears more than once in machine code\n", id);
            exit(1);
          }
          printf("found int64_t constant #%d at %p with mult=%d and offset=%d\n", id, probe, mult, offs, mult);
          patch->addr = (void*)probe;
          patch->mult = mult;
          patch->offs = offs;
          probe += sizeof (val);
          goto done;
        }
      }
    }
    probe++;
  done:;
  }

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    if (patch->addr == NULL) {
      printf("int64_t constant #%d was not found in the machine code\n", i);
      exit(1);
    }
  }
}

void init_patch_fn_call(void *fn, size_t len, patch_descr *patch_tab) {

  int8_t *probe = (int8_t*)fn;
  int i;

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    patch->addr = NULL;
  }

  while (*(int64_t*)probe != END_OF_PATCHABLE_FUNCTION_MARKER) {

    int8_t code = *probe++;

    if ((uint8_t)code == 0xe8 || (uint8_t)code == 0xe9) {

      int32_t val = *(int32_t*)probe;
      uint64_t dest = (uint64_t)probe + sizeof (val) + val;

      if (dest >= min_fn_addr && dest <= max_fn_addr) {

        uint64_t id;

        for (id=0; id<NB_PATCHABLE_CONSTANT_FNS; id++) {
          int64_t dist = dest - (int64_t)PATCHABLE_CONSTANT_FNS[id];
          if (dist == 0) {
            if (id >= len) {
              printf("constant function call #%d is out of range\n", id);
              exit(1);
            } else {
              patch_descr *patch = &patch_tab[id];
              if (patch->addr != NULL) {
                printf("constant function call #%d appears more than once in machine code\n", id);
                exit(1);
              }
              printf("found constant function call #%d at %p (%s instruction)\n", id, probe, (uint8_t)code == 0xe8 ? "call" : "jmp");
              patch->addr = (void*)probe;
              patch->mult = 1;
              probe += sizeof (val);
              patch->offs = -(int32_t)(int64_t)probe;
              break;
            }
          }
        }
      }
    }
  }

  for (i=0; i<len; i++) {
    patch_descr *patch = &patch_tab[i];
    if (patch->addr == NULL) {
      printf("constant function call #%d was not found in the machine code\n", i);
      exit(1);
    }
  }
}

void init_patch_fn_ptr(void *fn, size_t len, patch_descr *patch_tab) {
  printf("init_patch_fn_ptr not implemented yet");
  exit(1);
}

/*---------------------------------------------------------------------------*/

void patch_32(patch_descr *patch, patch_32_type val_32) {
  *(patch_32_type*)patch->addr = val_32 * (patch_32_type)patch->mult + (patch_32_type)patch->offs;
}

void patch_64(patch_descr *patch, patch_64_type val_64) {
  *(patch_64_type*)patch->addr = val_64 * (patch_64_type)patch->mult + (patch_64_type)patch->offs;
}

void patch_fn_call(patch_descr *patch, void *val_fn) {

  /* call and jmp instructions are encoded using a 32 bit relative address */

  *(int32_t*)patch->addr = (patch_fn_type)val_fn + (patch_fn_type)patch->offs;
}

void patch_fn_ptr(patch_descr *patch, void *val_fn) {
  printf("patch_fn_ptr not implemented yet");
  exit(1);
}

/*---------------------------------------------------------------------------*/

