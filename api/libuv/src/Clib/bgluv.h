/*===========================================================================*/
/*   (Llib/uvtypes.scm)                                                      */
/*   Bigloo (4.7a)                                                           */
/*   Inria -- Sophia Antipolis (c)       Sun Dec 22 03:57:13 PM CET 2024     */
/*===========================================================================*/
/* COMPILATION: (/home/serrano/prgm/project/bigloo/wasm/bin/bigloo -O3 -fcfa-arithmetic -q -lib-dir /home/serrano/prgm/project/bigloo/wasm/lib/bigloo/4.6a -I Llib -lib-dir /home/serrano/prgm/project/bigloo/wasm/lib/bigloo/4.6a -srfi libuv -copt -I/home/serrano/prgm/project/bigloo/wasm/libuv/libuv-v1.48.0/include -DBGL_DUMMY -safee -unsafe -copt -fPIC -copt -IClib -copt  Llib/uvtypes.scm -o Clib/bgluv.h -hgen) */

#ifndef __BGL___LIBUV_TYPES_H
#define __BGL___LIBUV_TYPES_H

/* object type definitions */
typedef struct BgL_z52uvz52_bgl {
   header_t header;
   obj_t widening;
} *BgL_z52uvz52_bglt;

typedef struct BgL_uvhandlez00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
} *BgL_uvhandlez00_bglt;

typedef struct BgL_uvloopz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   obj_t BgL_z52mutexz52;
} *BgL_uvloopz00_bglt;

typedef struct BgL_uvwatcherz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvwatcherz00_bglt;

typedef struct BgL_uvstreamz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52dataz52;
} *BgL_uvstreamz00_bglt;

typedef struct BgL_uvtcpz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52dataz52;
} *BgL_uvtcpz00_bglt;

typedef struct BgL_uvudpz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52dataz52;
   obj_t BgL_z52procmz52;
} *BgL_uvudpz00_bglt;

typedef struct BgL_uvpipez00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52dataz52;
   bool_t BgL_ipcz00;
} *BgL_uvpipez00_bglt;

typedef struct BgL_uvttyz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   void * BgL_z52dataz52;
   int BgL_fdz00;
   bool_t BgL_readablez00;
} *BgL_uvttyz00_bglt;

typedef struct BgL_uvtimerz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
   uint64_t BgL_repeatz00;
   bool_t BgL_refz00;
} *BgL_uvtimerz00_bglt;

typedef struct BgL_uvidlez00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvidlez00_bglt;

typedef struct BgL_uvasyncz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvasyncz00_bglt;

typedef struct BgL_uvpollz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
   int BgL_fdz00;
} *BgL_uvpollz00_bglt;

typedef struct BgL_uvfilez00_bgl {
   header_t header;
   obj_t widening;
   int BgL_fdz00;
   obj_t BgL_pathz00;
} *BgL_uvfilez00_bglt;

typedef struct BgL_uvfseventz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvfseventz00_bglt;

typedef struct BgL_uvfspollz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvfspollz00_bglt;

typedef struct BgL_uvcheckz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   struct BgL_uvloopz00_bgl * BgL_loopz00;
   obj_t BgL_cbz00;
   void * BgL_z52dataz52;
} *BgL_uvcheckz00_bglt;

typedef struct BgL_uvprocessz00_bgl {
   header_t header;
   obj_t widening;
   uv_handle_t * BgL_z42builtinz42;
   obj_t BgL_z52onclosez52;
   obj_t BgL_z52gcmarksheadz52;
   obj_t BgL_z52gcmarkstailz52;
   obj_t BgL_z52gcmarkz52;
   bool_t BgL_closedz00;
   obj_t BgL_z42onexitz42;
} *BgL_uvprocessz00_bglt;

typedef struct BgL_uvprocessoptionsz00_bgl {
   header_t header;
   obj_t widening;
   uv_process_options_t * BgL_z42builtinz42;
} *BgL_uvprocessoptionsz00_bglt;

typedef struct BgL_uvworkz00_bgl {
   header_t header;
   obj_t widening;
   uv_work_t * BgL_z42builtinz42;
   obj_t BgL_z52workzd2cbz80;
   obj_t BgL_z52afterzd2cbz80;
} *BgL_uvworkz00_bglt;



#endif // __BGL___LIBUV_TYPES_H
