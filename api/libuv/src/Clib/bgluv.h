/*===========================================================================*/
/*   (Llib/uvtypes.scm)                                                      */
/*   Bigloo (4.2a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Wed Jul 23 12:52:44 CEST 2014       */
/*===========================================================================*/
/* COMPILATION: (/users/serrano/prgm/project/bigloo/bin/bigloo -O3 -fcfa-arithmetic -q -I Llib -lib-dir /users/serrano/prgm/project/bigloo/lib/4.2a -unsafe -safee -srfi libuv -copt -I/users/serrano/prgm/project/bigloo/libuv/libuv-master-18jul2014/include -copt -fPIC -copt -IClib -copt  Llib/uvtypes.scm -o Clib/bgluv.h -hgen)*/

/* Object type definitions */
typedef struct BgL_z52uvz52_bgl {
   header_t header;
   obj_t widening;
} *BgL_z52uvz52_bglt;

typedef struct BgL_uvhandlez00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
} *BgL_uvhandlez00_bglt;

typedef struct BgL_uvloopz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   obj_t BgL_z52mutexz52;
   obj_t BgL_z52gcmarksz52;
} *BgL_uvloopz00_bglt;

typedef struct BgL_uvwatcherz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   obj_t BgL_cbz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
} *BgL_uvwatcherz00_bglt;

typedef struct BgL_uvtimerz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   obj_t BgL_cbz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
} *BgL_uvtimerz00_bglt;

typedef struct BgL_uvasyncz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   obj_t BgL_cbz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
} *BgL_uvasyncz00_bglt;

typedef struct BgL_uvfilez00_bgl {
   header_t header;
   obj_t widening;
   int BgL_fdz00;
   obj_t BgL_pathz00;
   void * BgL_z52readreqz52;
   char * BgL_z52bufz52;
   int BgL_z52buflenz52;
} *BgL_uvfilez00_bglt;


