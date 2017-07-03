/* patch.h */

#ifndef _PATCH_H
#define _PATCH_H

#include <stdint.h>
#include <stdlib.h>

typedef int32_t patch_32_type;

typedef int64_t patch_64_type;

#ifndef __LP64__
#ifndef __ILP32__
#define __ILP32__
#endif
#endif

#ifdef __ILP32__

typedef int32_t patch_fn_type;

#else

#ifdef __LP64__

typedef int64_t patch_fn_type;

#else

#error "either __ILP32__ or __LP64__ must be defined"

#endif

#endif

/* patch descriptor */

typedef struct patch_descr {
  void *addr;
  int32_t offs;
  int32_t mult;
  int32_t kind;
} patch_descr;

extern void patch_32(patch_descr *patch, patch_32_type val_32);
extern void patch_64(patch_descr *patch, patch_64_type val_64);
extern void patch_fn_call(patch_descr *patch, void *val_fn);
extern void patch_fn_ptr(patch_descr *patch, void *val_fn);

extern void PATCHABLE_CONSTANT_FN0(void);
extern void PATCHABLE_CONSTANT_FN1(void);
extern void PATCHABLE_CONSTANT_FN2(void);
extern void PATCHABLE_CONSTANT_FN3(void);
extern void PATCHABLE_CONSTANT_FN4(void);
extern void PATCHABLE_CONSTANT_FN5(void);
extern void PATCHABLE_CONSTANT_FN6(void);
extern void PATCHABLE_CONSTANT_FN7(void);
extern void PATCHABLE_CONSTANT_FN8(void);
extern void PATCHABLE_CONSTANT_FN9(void);
extern void PATCHABLE_CONSTANT_FN10(void);
extern void PATCHABLE_CONSTANT_FN11(void);
extern void PATCHABLE_CONSTANT_FN12(void);
extern void PATCHABLE_CONSTANT_FN13(void);
extern void PATCHABLE_CONSTANT_FN14(void);
extern void PATCHABLE_CONSTANT_FN15(void);
extern void PATCHABLE_CONSTANT_FN16(void);
extern void PATCHABLE_CONSTANT_FN17(void);
extern void PATCHABLE_CONSTANT_FN18(void);
extern void PATCHABLE_CONSTANT_FN19(void);
extern void PATCHABLE_CONSTANT_FN20(void);
extern void PATCHABLE_CONSTANT_FN21(void);
extern void PATCHABLE_CONSTANT_FN22(void);
extern void PATCHABLE_CONSTANT_FN23(void);
extern void PATCHABLE_CONSTANT_FN24(void);
extern void PATCHABLE_CONSTANT_FN25(void);
extern void PATCHABLE_CONSTANT_FN26(void);
extern void PATCHABLE_CONSTANT_FN27(void);
extern void PATCHABLE_CONSTANT_FN28(void);
extern void PATCHABLE_CONSTANT_FN29(void);
extern void PATCHABLE_CONSTANT_FN30(void);
extern void PATCHABLE_CONSTANT_FN31(void);
extern void PATCHABLE_CONSTANT_FN32(void);
extern void PATCHABLE_CONSTANT_FN33(void);
extern void PATCHABLE_CONSTANT_FN34(void);
extern void PATCHABLE_CONSTANT_FN35(void);
extern void PATCHABLE_CONSTANT_FN36(void);
extern void PATCHABLE_CONSTANT_FN37(void);
extern void PATCHABLE_CONSTANT_FN38(void);
extern void PATCHABLE_CONSTANT_FN39(void);
extern void PATCHABLE_CONSTANT_FN40(void);
extern void PATCHABLE_CONSTANT_FN41(void);
extern void PATCHABLE_CONSTANT_FN42(void);
extern void PATCHABLE_CONSTANT_FN43(void);
extern void PATCHABLE_CONSTANT_FN44(void);
extern void PATCHABLE_CONSTANT_FN45(void);
extern void PATCHABLE_CONSTANT_FN46(void);
extern void PATCHABLE_CONSTANT_FN47(void);
extern void PATCHABLE_CONSTANT_FN48(void);
extern void PATCHABLE_CONSTANT_FN49(void);
extern void PATCHABLE_CONSTANT_FN50(void);
extern void PATCHABLE_CONSTANT_FN51(void);
extern void PATCHABLE_CONSTANT_FN52(void);
extern void PATCHABLE_CONSTANT_FN53(void);
extern void PATCHABLE_CONSTANT_FN54(void);
extern void PATCHABLE_CONSTANT_FN55(void);
extern void PATCHABLE_CONSTANT_FN56(void);
extern void PATCHABLE_CONSTANT_FN57(void);
extern void PATCHABLE_CONSTANT_FN58(void);
extern void PATCHABLE_CONSTANT_FN59(void);
extern void PATCHABLE_CONSTANT_FN60(void);
extern void PATCHABLE_CONSTANT_FN61(void);
extern void PATCHABLE_CONSTANT_FN62(void);
extern void PATCHABLE_CONSTANT_FN63(void);
extern void PATCHABLE_CONSTANT_FN64(void);
extern void PATCHABLE_CONSTANT_FN65(void);
extern void PATCHABLE_CONSTANT_FN66(void);
extern void PATCHABLE_CONSTANT_FN67(void);
extern void PATCHABLE_CONSTANT_FN68(void);
extern void PATCHABLE_CONSTANT_FN69(void);
extern void PATCHABLE_CONSTANT_FN70(void);
extern void PATCHABLE_CONSTANT_FN71(void);
extern void PATCHABLE_CONSTANT_FN72(void);
extern void PATCHABLE_CONSTANT_FN73(void);
extern void PATCHABLE_CONSTANT_FN74(void);
extern void PATCHABLE_CONSTANT_FN75(void);
extern void PATCHABLE_CONSTANT_FN76(void);
extern void PATCHABLE_CONSTANT_FN77(void);
extern void PATCHABLE_CONSTANT_FN78(void);
extern void PATCHABLE_CONSTANT_FN79(void);
extern void PATCHABLE_CONSTANT_FN80(void);
extern void PATCHABLE_CONSTANT_FN81(void);
extern void PATCHABLE_CONSTANT_FN82(void);
extern void PATCHABLE_CONSTANT_FN83(void);
extern void PATCHABLE_CONSTANT_FN84(void);
extern void PATCHABLE_CONSTANT_FN85(void);
extern void PATCHABLE_CONSTANT_FN86(void);
extern void PATCHABLE_CONSTANT_FN87(void);
extern void PATCHABLE_CONSTANT_FN88(void);
extern void PATCHABLE_CONSTANT_FN89(void);
extern void PATCHABLE_CONSTANT_FN90(void);
extern void PATCHABLE_CONSTANT_FN91(void);
extern void PATCHABLE_CONSTANT_FN92(void);
extern void PATCHABLE_CONSTANT_FN93(void);
extern void PATCHABLE_CONSTANT_FN94(void);
extern void PATCHABLE_CONSTANT_FN95(void);
extern void PATCHABLE_CONSTANT_FN96(void);
extern void PATCHABLE_CONSTANT_FN97(void);
extern void PATCHABLE_CONSTANT_FN98(void);
extern void PATCHABLE_CONSTANT_FN99(void);

#endif
