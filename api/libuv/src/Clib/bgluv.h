/*===========================================================================*/
/*   (Llib/uvtypes.scm)                                                      */
/*   Bigloo (4.2a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Mon Sep 1 13:26:57 CEST 2014        */
/*===========================================================================*/
/* COMPILATION: (/home/serrano/prgm/project/bigloo/bin/bigloo -O3 -fcfa-arithmetic -q -I Llib -lib-dir /home/serrano/prgm/project/bigloo/lib/bigloo/4.2a -unsafe -safee -srfi libuv -copt -I/home/serrano/prgm/project/bigloo/libuv/libuv-master-18jul2014/include -copt -fPIC -copt -IClib -copt  Llib/uvtypes.scm -o Clib/bgluv.h -hgen)*/

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
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
} *BgL_uvwatcherz00_bglt;

typedef struct BgL_uvstreamz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52readreqz52;
   char * BgL_z52rbufz52;
   int BgL_z52rbuflenz52;
   void * BgL_z52writereqz52;
   char * BgL_z52wbufz52;
   int BgL_z52wbuflenz52;
   obj_t BgL_z52allocz52;
   obj_t BgL_z52offsetz52;
   obj_t BgL_z52procaz52;
   obj_t BgL_z52proccz52;
} *BgL_uvstreamz00_bglt;

typedef struct BgL_uvtcpz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52readreqz52;
   char * BgL_z52rbufz52;
   int BgL_z52rbuflenz52;
   void * BgL_z52writereqz52;
   char * BgL_z52wbufz52;
   int BgL_z52wbuflenz52;
   obj_t BgL_z52allocz52;
   obj_t BgL_z52offsetz52;
   obj_t BgL_z52procaz52;
   obj_t BgL_z52proccz52;
} *BgL_uvtcpz00_bglt;

typedef struct BgL_uvtimerz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
} *BgL_uvtimerz00_bglt;

typedef struct BgL_uvidlez00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
} *BgL_uvidlez00_bglt;

typedef struct BgL_uvasyncz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_onclosez00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
} *BgL_uvasyncz00_bglt;

typedef struct BgL_uvfilez00_bgl {
   header_t header;
   obj_t widening;
   int BgL_fdz00;
   obj_t BgL_pathz00;
   void * BgL_z52readreqz52;
   char * BgL_z52rbufz52;
   int BgL_z52rbuflenz52;
   char * BgL_z52wbufz52;
   int BgL_z52wbuflenz52;
} *BgL_uvfilez00_bglt;


