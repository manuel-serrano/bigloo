/*===========================================================================*/
/*   (Llib/avahi.scm)                                                        */
/*   Bigloo (4.2a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Wed Apr 8 11:07:39 CEST 2015        */
/*===========================================================================*/
/* COMPILATION: (/home/serrano/prgm/project/bigloo/bin/bigloo -O3 -fcfa-arithmetic -q -I Llib -lib-dir /home/serrano/prgm/project/bigloo/lib/bigloo/4.2a -unsafe -safee -srfi avahi -copt -D_REENTRANT -copt -fPIC -copt -IClib -copt  Llib/avahi.scm -o Clib/bglavahi.h -hgen)*/

/* Object type definitions */
typedef struct BgL_z62avahizd2errorzb0_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_fnamez00;
   obj_t BgL_locationz00;
   obj_t BgL_stackz00;
   obj_t BgL_procz00;
   obj_t BgL_msgz00;
   obj_t BgL_objz00;
   int BgL_errnoz00;
} *BgL_z62avahizd2errorzb0_bglt;

typedef struct BgL_z62avahizd2collisionzd2errorz62_bgl {
   header_t header;
   obj_t widening;
   obj_t BgL_fnamez00;
   obj_t BgL_locationz00;
   obj_t BgL_stackz00;
   obj_t BgL_procz00;
   obj_t BgL_msgz00;
   obj_t BgL_objz00;
   int BgL_errnoz00;
} *BgL_z62avahizd2collisionzd2errorz62_bglt;

typedef struct BgL_avahizd2objectzd2_bgl {
   header_t header;
   obj_t widening;
} *BgL_avahizd2objectzd2_bglt;

typedef struct BgL_avahizd2pollzd2_bgl {
   header_t header;
   obj_t widening;
   int BgL_z42ctypez42;
   obj_t BgL_z52procsz52;
} *BgL_avahizd2pollzd2_bglt;

typedef struct BgL_avahizd2simplezd2pollz00_bgl {
   header_t header;
   obj_t widening;
   int BgL_z42ctypez42;
   obj_t BgL_z52procsz52;
   AvahiSimplePoll * BgL_z42builtinz42;
} *BgL_avahizd2simplezd2pollz00_bglt;

typedef struct BgL_avahizd2threadedzd2pollz00_bgl {
   header_t header;
   obj_t widening;
   int BgL_z42ctypez42;
   obj_t BgL_z52procsz52;
   AvahiThreadedPoll * BgL_z42builtinz42;
} *BgL_avahizd2threadedzd2pollz00_bglt;

typedef struct BgL_avahizd2clientzd2_bgl {
   header_t header;
   obj_t widening;
   AvahiClient * BgL_z42builtinz42;
   struct BgL_avahizd2pollzd2_bgl * BgL_pollz00;
   obj_t BgL_flagsz00;
   obj_t BgL_procz00;
   obj_t BgL_z52groupsz52;
   obj_t BgL_z52browsersz52;
   obj_t BgL_resolversz00;
} *BgL_avahizd2clientzd2_bglt;

typedef struct BgL_avahizd2entryzd2groupz00_bgl {
   header_t header;
   obj_t widening;
   AvahiEntryGroup * BgL_z42builtinz42;
   struct BgL_avahizd2clientzd2_bgl * BgL_clientz00;
   obj_t BgL_procz00;
} *BgL_avahizd2entryzd2groupz00_bglt;

typedef struct BgL_avahizd2servicezd2browserz00_bgl {
   header_t header;
   obj_t widening;
   AvahiServiceBrowser * BgL_z42builtinz42;
   struct BgL_avahizd2clientzd2_bgl * BgL_clientz00;
   obj_t BgL_procz00;
   obj_t BgL_typez00;
   obj_t BgL_domainz00;
} *BgL_avahizd2servicezd2browserz00_bglt;

typedef struct BgL_avahizd2servicezd2typezd2browserzd2_bgl {
   header_t header;
   obj_t widening;
   AvahiServiceTypeBrowser * BgL_z42builtinz42;
   struct BgL_avahizd2clientzd2_bgl * BgL_clientz00;
   obj_t BgL_procz00;
   obj_t BgL_domainz00;
} *BgL_avahizd2servicezd2typezd2browserzd2_bglt;

typedef struct BgL_avahizd2domainzd2browserz00_bgl {
   header_t header;
   obj_t widening;
   AvahiDomainBrowser * BgL_z42builtinz42;
   struct BgL_avahizd2clientzd2_bgl * BgL_clientz00;
   obj_t BgL_procz00;
   obj_t BgL_btypez00;
   obj_t BgL_domainz00;
} *BgL_avahizd2domainzd2browserz00_bglt;

typedef struct BgL_avahizd2servicezd2resolverz00_bgl {
   header_t header;
   obj_t widening;
   AvahiServiceResolver * BgL_z42builtinz42;
   struct BgL_avahizd2clientzd2_bgl * BgL_clientz00;
   obj_t BgL_procz00;
   AvahiIfIndex BgL_interfacez00;
   obj_t BgL_protocolz00;
   obj_t BgL_namez00;
   obj_t BgL_typez00;
   obj_t BgL_domainz00;
} *BgL_avahizd2servicezd2resolverz00_bglt;


