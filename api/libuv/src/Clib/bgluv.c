/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/libuv/src/Clib/bgluv.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May  6 13:53:14 2014                          */
/*    Last change :  Fri Dec  8 19:23:22 2023 (serrano)                */
/*    Copyright   :  2014-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    LIBUV Bigloo C binding                                           */
/*=====================================================================*/
#include <bigloo.h>
#include <uv.h>
#include "bgluv.h"

/*---------------------------------------------------------------------*/
/*    Thread local storage declarations                                */
/*---------------------------------------------------------------------*/
#define ROOTS_INCREMENT 64

#if BGL_HAS_THREAD_LOCALSTORAGE
#  define UV_HAS_THREAD_LOCALSTORAGE 1
#  define UV_TLS_DECL BGL_THREAD_DECL
#  define UV_MUTEX_LOCK(m)
#  define UV_MUTEX_UNLOCK(m)
#else
#  define UV_HAS_THREAD_LOCALSTORAGE 0
#  define UV_TLS_DECL static
#  define UV_MUTEX_LOCK(m) BGL_MUTEX_LOCK(m)
#  define UV_MUTEX_UNLOCK(m) BGL_MUTEX_UNLOCK(m)
#endif

static obj_t GC_roots = BNIL;

// #define DBG
//#define DBG_TRACE 
//#define DBG_CNT
#define DBG_TRACE_FREQ 100

/*---------------------------------------------------------------------*/
/*    request pools                                                    */
/*---------------------------------------------------------------------*/
#define UV_STREAM_POOL_INDEX 0
#define UV_WATCHER_POOL_INDEX 1
#define UV_FS_POOL_INDEX 2
#define UV_WRITE_POOL_INDEX 3
#define UV_SHUTDOWN_POOL_INDEX 4

#define REQ_POOL_SIZE (UV_SHUTDOWN_POOL_INDEX + 1)

static obj_t _roots = BNIL;

/*---------------------------------------------------------------------*/
/*    bgl_uv_mutex                                                     */
/*---------------------------------------------------------------------*/
extern obj_t bgl_uv_mutex;
extern obj_t bgl_make_input_port(obj_t, FILE *, obj_t, obj_t);
extern obj_t bgl_uv_new_file(int, obj_t);

/*---------------------------------------------------------------------*/
/*    ABORT ...                                                        */
/*---------------------------------------------------------------------*/
#if defined(DBG)
#define ABORT() \
   fprintf(stderr, "*** ABORT: %s:%d\n", __FILE__, __LINE__); \
   exit(1/0)
#else
#define ABORT()
#endif

/*---------------------------------------------------------------------*/
/*    TRACES                                                           */
/*---------------------------------------------------------------------*/
#if defined(DBG_TRACE)
static long tcnt = 0, trw = 0 , trw2 = 0, trw3 = 0, tclose = 0, tidle = 0, tconn = 0, tread = 0, trecv = 0, tstream = 0, ttimer = 0;

#  define TRACECB(_c) \
   (--_c,		   \
      (tcnt++ % DBG_TRACE_FREQ == 0) \
      ? fprintf(stderr, "- %4d: %07d rw=%d rw2=%d rw3=%d clo=%d idl=%d con=%d rs=%d rcv=%d stm=%d tmt=%d\n", \
		__LINE__, tcnt, trw, trw2, trw3, tclose, tidle, tconn, tread, trecv, tstream, ttimer) \
      : 0)

#  define TRACECA(_c) \
   (++_c,		   \
      (tcnt++ % DBG_TRACE_FREQ == 0) \
      ? fprintf(stderr, "+ %4d: %07d rw=%d rw2=%d rw3=%d clo=%d idl=%d con=%d rs=%d rcv=%d stm=%d tmt=%d\n", \
		__LINE__, tcnt, trw, trw2, trw3, tclose, tidle, tconn, tread, trecv, tstream, ttimer) \
      : 0)
#else
#  define TRACECB(_c) 
#  define TRACECA(_c)
#endif

#if defined(DBG_CNT)
static long kcnt = 0, klisten = 0, kshutdown = 0, kopen = 0, kclose = 0;
#  define TRACECNT(_c) \
      (++_c, \
	 (kcnt++ % DBG_TRACE_FREQ == 0) \
	 ? fprintf(stderr, "# %4d: %07d ltw=%d sh=%d op=%d cl=%d\n", \
		   __LINE__, kcnt, klisten, kshutdown, kopen, kclose)	\
	  : 0)
#else
#  define TRACECNT(_c) 0
#endif

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    gc_replace ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
gc_replace(obj_t roots, void *old, void *new) {
   if (NULLP(roots)) {
      return MAKE_PAIR(new, BNIL);
   } else {
      obj_t aux = roots;

      while (!NULLP(aux)) {
	 if (CAR(aux) == old) {
	    SET_CAR(aux, new);
	    return roots;
	 } else {
	    aux = CDR(aux);
	 }
      }
      return MAKE_PAIR(new, roots);
   }
}

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef BgL_uvloopz00_bglt bgl_uv_loop_t;
typedef BgL_uvfilez00_bglt bgl_uv_file_t;
typedef BgL_uvhandlez00_bglt bgl_uv_handle_t;
typedef BgL_uvstreamz00_bglt bgl_uv_stream_t;
typedef BgL_uvudpz00_bglt bgl_uv_udp_t;
typedef BgL_uvwatcherz00_bglt bgl_uv_watcher_t;
typedef BgL_uvasyncz00_bglt bgl_uv_async_t;
typedef BgL_uvfseventz00_bglt bgl_uv_fs_event_t;
typedef BgL_uvprocessz00_bglt bgl_uv_process_t;
typedef BgL_uvprocessoptionsz00_bglt bgl_uv_process_options_t;
typedef BgL_uvworkz00_bglt bgl_uv_work_t;
typedef BgL_uvfspollz00_bglt bgl_uv_fs_poll_t;
typedef BgL_uvpollz00_bglt bgl_uv_poll_t;

extern obj_t bgl_uv_handle_type_symbol(int);
extern obj_t bgl_uv_events_to_list(int);

extern obj_t bgl_uv_pop_gcmark(bgl_uv_handle_t, obj_t);

/*---------------------------------------------------------------------*/
/*    Accessors                                                        */
/*---------------------------------------------------------------------*/
#define LOOP_BUILTIN(o) \
   ((uv_loop_t *)(((bgl_uv_loop_t)(COBJECT((obj_t)o)))->BgL_z42builtinz42))
#define STREAM_BUILTIN(o) \
   ((uv_stream_t *)(((bgl_uv_stream_t)(COBJECT((obj_t)o)))->BgL_z42builtinz42))
#define STREAM_DATA(o) \
   ((bgl_uv_stream_t)(COBJECT((obj_t)o)))->BgL_z52dataz52
#define WATCHER_BUILTIN(o) \
   ((uv_watcher_t *)(((bgl_uv_watcher_t)(COBJECT((obj_t)o)))->BgL_z42builtinz42))
#define WATCHER_DATA(o) \
   ((bgl_uv_watcher_t)(COBJECT((obj_t)o)))->BgL_z52dataz52
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_marks ...                                                     */
/*---------------------------------------------------------------------*/
UV_TLS_DECL obj_t GC_marks = BNIL;

static obj_t bgl_uv_fstat(uv_stat_t);

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    gc_mark ...                                                      */
/*---------------------------------------------------------------------*/
static void
gc_mark(obj_t obj) {
   UV_MUTEX_LOCK(bgl_uv_mutex);
   GC_marks = MAKE_PAIR(obj, GC_marks);
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gc_unmark ...                                                    */
/*---------------------------------------------------------------------*/
void
gc_unmark(obj_t obj) {
   UV_MUTEX_LOCK(bgl_uv_mutex);
   GC_marks = bgl_remq_bang(obj, GC_marks);
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    UV_EXTEND ...                                                    */
/*---------------------------------------------------------------------*/
#define UV_EXTEND_POOL(pool, type) {					\
      if (!pool##_size) {						\
	 pool##_size = ROOTS_INCREMENT;					\
	 pool##_roots = (type **)GC_MALLOC(sizeof(type *) * pool##_size); \
	 pool = malloc(sizeof(type *) * pool##_size);			\
	 GC_roots = MAKE_PAIR((obj_t)pool##_roots, GC_roots);		\
      } else {								\
	 pool##_size *= 2;						\
	 type **new_roots = (type **)GC_REALLOC((void *)pool##_roots, sizeof(type *) * pool##_size); \
	 pool = realloc(pool, sizeof(type *) * pool##_size);		\
	 if (new_roots != pool##_roots) {				\
	    GC_roots = gc_replace(GC_roots, pool##_roots, new_roots);	\
	    pool##_roots = new_roots;					\
	 }								\
      }									\
      for (long i = pool##_idx; i < pool##_size; i++) {			\
	 pool[i] = pool##_roots[i] = (type *)GC_MALLOC_UNCOLLECTABLE(sizeof(type)); \
      }									\
   }

/*---------------------------------------------------------------------*/
/*    uv_stream_pool ...                                               */
/*---------------------------------------------------------------------*/
typedef enum uv_stream_state {
   FREE = 0, LOCKED = 1, READING = 2, CLOSING = 3
} uv_stream_state_t;

typedef struct uv_stream_data {
   obj_t obj;
   obj_t proc;
   obj_t alloc;
   obj_t offset;
   obj_t allocobj;
   obj_t close;
   obj_t listen;
   long index;
   uv_stream_state_t state;
} uv_stream_data_t;

UV_TLS_DECL uv_stream_data_t **uv_stream_data_pool_roots = 0L;
UV_TLS_DECL uv_stream_data_t **uv_stream_data_pool = 0L;
UV_TLS_DECL long uv_stream_data_pool_idx = 0;
UV_TLS_DECL long uv_stream_data_pool_size = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    assert_stream_data ...                                           */
/*---------------------------------------------------------------------*/
void
assert_stream_data(obj_t obj) {
   if (STREAM_DATA(obj)) {
      uv_stream_data_t *data = (uv_stream_data_t *)STREAM_DATA(obj);
      
      if (data->index < 0) {
	 fprintf(stderr, "assert_stream_data: bad uv_stream_data_t index: %p %ld\n", data, data->index);
	 ABORT();
      }

      if (data->proc && !PROCEDUREP(data->proc)) {
	 fprintf(stderr, "assert_stream_data: bad uv_stream_data_t procedure: data=%p (idx=%ld:%d) proc=%p\n", data, data->index, data->state, data->proc);
	 ABORT();
      }
	 
      if (data->alloc && !PROCEDUREP(data->alloc)) {
	 fprintf(stderr, "assert_stream_data: bad uv_stream_data_t alloc: %p %p\n", data, data->alloc);
	 ABORT();
      }
	 
      if (data != STREAM_DATA(data->obj)) {
	 fprintf(stderr, "assert_stream_data: bad uv_stream_data_t data->obj: idx=%ld data=%p data->obj=%p\n", data->index, data, data->obj);
	 ABORT();
      }
      
      if (((uv_stream_data_t *)STREAM_DATA(data->obj))->obj != data->obj) {
	 fprintf(stderr, "assert_stream_dataL bad uv_stream_data_t obj->data: idx=%ld data=%p data->obj=%p\n", data->index, data, data->obj);
	 ABORT();
      }

      if (data->state == FREE) {
	 fprintf(stderr, "!!! %s:%d Bad stream_data state (%d)!\n",
		 __FILE__, __LINE__, data->state);
	 ABORT();
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static uv_stream_data_t *                                        */
/*    alloc_stream_data ...                                            */
/*---------------------------------------------------------------------*/
static uv_stream_data_t *
alloc_stream_data() {
   uv_stream_data_t *data;

   UV_MUTEX_LOCK(bgl_uv_mutex);

#if defined(DBG)
   fprintf(stderr, "+++ alloc_stream_data idx=%d/%d\n", uv_stream_pool_idx, uv_stream_pool_size);
#endif

   if (uv_stream_data_pool_idx == uv_stream_data_pool_size) {
      UV_EXTEND_POOL(uv_stream_data_pool, uv_stream_data_t);
   }

   data = uv_stream_data_pool[uv_stream_data_pool_idx++];
   
   UV_MUTEX_UNLOCK(bgl_uv_mutex);

   return data;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_stream_data ...                                             */
/*---------------------------------------------------------------------*/
static void
free_stream_data(uv_stream_data_t *data) {
   assert_stream_data(data->obj);
   STREAM_DATA(data->obj) = 0L;

   TRACECB(tstream);
#if defined(DBG)
   fprintf(stderr, "!!! free_stream_data data=%p idx=%d:%d\n", data, data->index, data->state);
#endif

   data->obj = 0L;
   data->proc = 0L;
   data->alloc = 0L;
   data->offset = BINT(-1);
   data->allocobj = BUNSPEC;
   data->close = 0L;
   data->listen = 0L;
   data->state = FREE;

   UV_MUTEX_LOCK(bgl_uv_mutex);
   uv_stream_data_pool[--uv_stream_data_pool_idx] = data;
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    static uv_stream_data_t *                                        */
/*    get_stream_data ...                                              */
/*---------------------------------------------------------------------*/
static uv_stream_data_t *
get_stream_data(obj_t obj) {
   if (!STREAM_DATA(obj)) {
      uv_stream_data_t *data = alloc_stream_data();
      STREAM_DATA(obj) = data;
      data->obj = obj;

      TRACECA(tstream);
#if defined(DBG)
      if (data->state != FREE) {
	 fprintf(stderr, "!!! get_stream_data: bad stream_data state (%d)!\n",
		 data->state);
	 ABORT();
      }
#endif
      
      data->state = LOCKED;
      return data;
   } else {
#if defined(DBG)
      assert_stream_data(obj);
#endif
      return STREAM_DATA(obj);
   }
}

/*---------------------------------------------------------------------*/
/*    uv_watcher_pool ...                                              */
/*---------------------------------------------------------------------*/
typedef struct uv_watcher_data {
   obj_t obj;
   obj_t proc;
   obj_t close;
} uv_watcher_data_t;

UV_TLS_DECL uv_watcher_data_t **uv_watcher_data_pool_roots = 0L;
UV_TLS_DECL uv_watcher_data_t **uv_watcher_data_pool = 0L;
UV_TLS_DECL long uv_watcher_data_pool_idx = 0;
UV_TLS_DECL long uv_watcher_data_pool_size = 0;

/*---------------------------------------------------------------------*/
/*    static uv_watcher_data_t *                                       */
/*    alloc_watcher_data ...                                           */
/*---------------------------------------------------------------------*/
static uv_watcher_data_t *
alloc_watcher_data() {
   uv_watcher_data_t *data;

   UV_MUTEX_LOCK(bgl_uv_mutex);

#if defined(DBG)
   fprintf(stderr, "+++ alloc_watcher_data_t idx=%d/%d\n", uv_watcher_pool_idx, uv_watcher_pool_size);
#endif

   if (uv_watcher_data_pool_idx == uv_watcher_data_pool_size) {
      UV_EXTEND_POOL(uv_watcher_data_pool, uv_watcher_data_t);
   }

   data = uv_watcher_data_pool[uv_watcher_data_pool_idx++];
   
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
   return data;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_watcher_data ...                                            */
/*---------------------------------------------------------------------*/
static void
free_watcher_data(uv_watcher_data_t *data) {
   WATCHER_DATA(data->obj) = 0L;

#if defined(DBG)
   fprintf(stderr, "!!! free_watcher_data data=%p idx=%d:%d\n", data, data->index, data->state);
#endif
   data->obj = 0L;
   data->proc = 0L;

   UV_MUTEX_LOCK(bgl_uv_mutex);
   uv_watcher_data_pool[--uv_watcher_data_pool_idx] = data;
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    static uv_watcher_data_t *                                       */
/*    get_watcher_data ...                                             */
/*---------------------------------------------------------------------*/
static uv_watcher_data_t *
get_watcher_data(obj_t obj) {
   if (!WATCHER_DATA(obj)) {
      uv_watcher_data_t *data = alloc_watcher_data();
      WATCHER_DATA(obj) = data;
      data->obj = obj;

#if defined(DBG)
      if (data->state != FREE) {
	 fprintf(stderr, "!!! get_watcher_data: bad watcher_data state (%d)!\n",
		 data->state);
	 ABORT();
      }
#endif
      
      return data;
   } else {
      return WATCHER_DATA(obj);
   }
}

/*---------------------------------------------------------------------*/
/*    uv_fs_pool ...                                                   */
/*---------------------------------------------------------------------*/
typedef struct uv_fs_data {
   obj_t proc;
   obj_t arg[5];
} uv_fs_data_t;

UV_TLS_DECL uv_fs_t **uv_fs_pool = 0L;
UV_TLS_DECL uv_fs_data_t **uv_fs_data_pool_roots = 0L;
UV_TLS_DECL uv_fs_data_t **uv_fs_data_pool = 0L;
UV_TLS_DECL long uv_fs_data_pool_idx = 0;
UV_TLS_DECL long uv_fs_data_pool_size = 0;

/*---------------------------------------------------------------------*/
/*    uv_fs_t *                                                        */
/*    alloc_uv_fs ...                                                  */
/*---------------------------------------------------------------------*/
static uv_fs_t *
alloc_uv_fs() {
   uv_fs_t *req;

   UV_MUTEX_LOCK(bgl_uv_mutex);
   
   if (uv_fs_data_pool_idx == uv_fs_data_pool_size) {
      long pre_size = uv_fs_data_pool_size;
      
      UV_EXTEND_POOL(uv_fs_data_pool, uv_fs_data_t);
      uv_fs_pool = realloc(uv_fs_pool, sizeof(uv_fs_t *) * uv_fs_data_pool_size);

      for (; pre_size < uv_fs_data_pool_size; pre_size++) {
	 uv_fs_pool[pre_size] = malloc(sizeof(uv_fs_t));
	 uv_fs_pool[pre_size]->data = uv_fs_data_pool[pre_size];
      }
   }      

   req = uv_fs_pool[uv_fs_data_pool_idx++];
   
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
   
   return req;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_uv_fs ...                                                   */
/*---------------------------------------------------------------------*/
static void
free_uv_fs(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)(req->data);

   data->proc = BUNSPEC;
   data->arg[0] = BUNSPEC;
   data->arg[1] = BUNSPEC;
   data->arg[2] = BUNSPEC;
   data->arg[3] = BUNSPEC;
   data->arg[4] = BUNSPEC;
   
   uv_fs_req_cleanup(req);
   req->data = data;
   
   UV_MUTEX_LOCK(bgl_uv_mutex);
   uv_fs_pool[--uv_fs_data_pool_idx] = req;
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    uv_write_pools ...                                               */
/*---------------------------------------------------------------------*/
typedef struct uv_write_data {
   obj_t proc;
   obj_t arg[5];
} uv_write_data_t;

UV_TLS_DECL uv_write_t **uv_write_pool = 0L;
UV_TLS_DECL uv_write_data_t **uv_write_data_pool_roots = 0L;
UV_TLS_DECL uv_write_data_t **uv_write_data_pool = 0L;
UV_TLS_DECL long uv_write_data_pool_idx = 0;
UV_TLS_DECL long uv_write_data_pool_size = 0;

/*---------------------------------------------------------------------*/
/*    uv_write_t *                                                     */
/*    alloc_uv_write ...                                               */
/*---------------------------------------------------------------------*/
static uv_write_t *
alloc_uv_write() {
   uv_write_t *req;

   UV_MUTEX_LOCK(bgl_uv_mutex);
   
   if (uv_write_data_pool_idx == uv_write_data_pool_size) {
      long pre_size = uv_write_data_pool_size;
      
      UV_EXTEND_POOL(uv_write_data_pool, uv_write_data_t);
      uv_write_pool = realloc(uv_write_pool, sizeof(uv_write_t *) * uv_write_data_pool_size);

      for (; pre_size < uv_write_data_pool_size; pre_size++) {
	 uv_write_pool[pre_size] = malloc(sizeof(uv_write_t));
	 uv_write_pool[pre_size]->data = uv_write_data_pool[pre_size];
      }
   }

   req = uv_write_pool[uv_write_data_pool_idx++];
   
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
   
   return req;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_uv_write ...                                                */
/*---------------------------------------------------------------------*/
static void
free_uv_write(uv_write_t *req) {
   uv_write_data_t *data = (uv_write_data_t *)(req->data);

   data->proc = BUNSPEC;
   data->arg[0] = BUNSPEC;
   data->arg[1] = BUNSPEC;
   data->arg[2] = BUNSPEC;
   data->arg[3] = BUNSPEC;
   data->arg[4] = BUNSPEC;
   
   UV_MUTEX_LOCK(bgl_uv_mutex);
   uv_write_pool[--uv_write_data_pool_idx] = req;
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    uv_shutdown_pools ...                                            */
/*---------------------------------------------------------------------*/
typedef struct uv_shutdown_data {
   obj_t proc;
   obj_t obj;
} uv_shutdown_data_t;

UV_TLS_DECL uv_shutdown_t **uv_shutdown_pool = 0L;
UV_TLS_DECL uv_shutdown_data_t **uv_shutdown_data_pool_roots = 0L;
UV_TLS_DECL uv_shutdown_data_t **uv_shutdown_data_pool = 0L;
UV_TLS_DECL long uv_shutdown_data_pool_idx = 0;
UV_TLS_DECL long uv_shutdown_data_pool_size = 0;

/*---------------------------------------------------------------------*/
/*    uv_shutdown_t *                                                  */
/*    alloc_uv_shutdown ...                                            */
/*---------------------------------------------------------------------*/
static uv_shutdown_t *
alloc_uv_shutdown() {
   uv_shutdown_t *req;

   UV_MUTEX_LOCK(bgl_uv_mutex);
   
   if (uv_shutdown_data_pool_idx == uv_shutdown_data_pool_size) {
      long pre_size = uv_shutdown_data_pool_size;
      
      UV_EXTEND_POOL(uv_shutdown_data_pool, uv_shutdown_data_t);
      uv_shutdown_pool = realloc(uv_shutdown_pool, sizeof(uv_shutdown_t *) * uv_shutdown_data_pool_size);

      for (; pre_size < uv_shutdown_data_pool_size; pre_size++) {
	 uv_shutdown_pool[pre_size] = malloc(sizeof(uv_shutdown_t));
	 uv_shutdown_pool[pre_size]->data = uv_shutdown_data_pool[pre_size];
      }
   }

   req = uv_shutdown_pool[uv_shutdown_data_pool_idx++];

   UV_MUTEX_UNLOCK(bgl_uv_mutex);
   return req;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_uv_shutdown ...                                             */
/*---------------------------------------------------------------------*/
static void
free_uv_shutdown(uv_shutdown_t *req) {
   uv_shutdown_data_t *data = (uv_shutdown_data_t *)(req->data);
   
   data->proc = BUNSPEC;
   data->obj = BUNSPEC;
   
   UV_MUTEX_LOCK(bgl_uv_mutex);
   uv_shutdown_pool[--uv_shutdown_data_pool_idx] = req;
   UV_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_loop_init ...                                             */
/*---------------------------------------------------------------------*/
void
bgl_uv_loop_init(obj_t o) {
   static int init = 0;
   
   BGL_MUTEX_LOCK(bgl_uv_mutex);

   if (!init) {
      init = 1;
      GC_add_roots(&GC_roots, &GC_roots + 1);
#if !UV_HAS_THREAD_LOCALSTORAGE
      GC_add_roots(&GC_marks, &GC_marks + 1);
#endif      
   }
   
#if UV_HAS_THREAD_LOCALSTORAGE
   GC_add_roots(&GC_marks, &GC_marks + 1);
#endif
   
   BGL_MUTEX_UNLOCK(bgl_uv_mutex);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_process_title_init ...                                    */
/*---------------------------------------------------------------------*/
void
bgl_uv_process_title_init() {
   extern char *executable_name;
   uv_setup_args(1, &executable_name);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_close_cb ...                                              */
/*    -------------------------------------------------------------    */
/*    The data argument can either be a handle, or a pair when         */
/*    uv_close is automatically on an active handle, as those          */
/*    involved in a uv_listen action.                                  */
/*---------------------------------------------------------------------*/
void
bgl_uv_close_cb(uv_handle_t *handle) {
   obj_t o = (obj_t)(handle->data);
   bgl_uv_handle_t h = (bgl_uv_handle_t)(PAIRP(o) ? CAR(o) : o);
   obj_t p = ((bgl_uv_handle_t)COBJECT(h))->BgL_z52onclosez52;

   if (PROCEDUREP(p)) BGL_PROCEDURE_CALL0(p);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_stream_close_cb ...                                       */
/*    -------------------------------------------------------------    */
/*    The data argument can either be a handle, or a pair when         */
/*    uv_close is automatically on an active handle, as those          */
/*    involved in a uv_listen action.                                  */
/*---------------------------------------------------------------------*/
void
bgl_uv_stream_close_cb(uv_handle_t *hdl) {
   obj_t obj = hdl->data;
   uv_stream_data_t *data = STREAM_DATA(obj);

#if defined(DBG)
   fprintf(stderr, "<<< bgl_uv_stream_close_cb: data=%p index=%d:%d\n", data, data ? data->index : -1, data? data->state : -1);
#endif

   if (data) {
      obj_t p = data->close;

      if (data->state == LOCKED) {
	 if (p) BGL_PROCEDURE_CALL0(p);
	 free_stream_data(data);
      } else {
	 data->state = CLOSING;
	 if (p) BGL_PROCEDURE_CALL0(p);
	 if (data->state != FREE) {
	    free_stream_data(data);
	 }
      }
   }
   TRACECB(tclose);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_stream_close ...                                          */
/*---------------------------------------------------------------------*/
void
bgl_uv_stream_close(obj_t obj, obj_t proc) {
   bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
   uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
   uv_stream_data_t *data = get_stream_data(obj);

   data->proc = 0L;

#if defined(DBG)
   fprintf(stderr, ">>> bgl_uv_stream_close data=%p idx=%d:%d o=%p proc=%p\n",
	   data, data ? data->index : -1, data ? data->state : -1, obj, proc);
#endif

   if (PROCEDUREP(proc)) {
      if (PROCEDURE_CORRECT_ARITYP(proc, 0)) {
	 data->close = proc;
	 TRACECA(tclose);
	 uv_close((uv_handle_t *)s, bgl_uv_stream_close_cb);
      } else {
	 C_SYSTEM_FAILURE(BGL_ERROR, "bgl_uv_stream_close",
			  "wrong callback arity", proc);
      }
   } else {
      data->close = 0L;
      TRACECA(tclose);
      uv_close((uv_handle_t *)s, bgl_uv_stream_close_cb);
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_handle_cb ...                                             */
/*---------------------------------------------------------------------*/
void
bgl_uv_handle_cb(uv_handle_t *handle, int status) {
   bgl_uv_watcher_t o = (bgl_uv_watcher_t)handle->data;
   obj_t p = ((bgl_uv_watcher_t)COBJECT(o))->BgL_cbz00;

   if (PROCEDUREP(p)) BGL_PROCEDURE_CALL2(p, (obj_t)o, BINT(status));
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_idle_cb ...                                               */
/*---------------------------------------------------------------------*/
static void
bgl_uv_idle_cb(uv_idle_t *handle) {
   obj_t o = handle->data;
   uv_watcher_data_t *data = WATCHER_DATA(o);
   obj_t p = data->proc;
   
   if (PROCEDUREP(p)) BGL_PROCEDURE_CALL1(p, o);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_idle_start ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_uv_idle_start(obj_t obj, obj_t proc) {
   bgl_uv_watcher_t watcher = (bgl_uv_watcher_t)COBJECT(obj);
   uv_idle_t *w = (uv_idle_t *)(watcher->BgL_z42builtinz42);
   uv_watcher_data_t *data = get_watcher_data(obj);

   data->proc = proc;

   TRACECA(tidle);
   return uv_idle_start(w, bgl_uv_idle_cb);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_idle_stop ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_idle_stop(obj_t obj) {
   bgl_uv_watcher_t watcher = (bgl_uv_watcher_t)COBJECT(obj);
   uv_idle_t *w = (uv_idle_t *)(watcher->BgL_z42builtinz42);
   uv_watcher_data_t *data = get_watcher_data(obj);

   free_watcher_data(data);
   
   TRACECB(tidle);
   return uv_idle_stop(w);
}

/*---------------------------------------------------------------------*/
/*    uv_timer_t *                                                     */
/*    bgl_uv_timer_new ...                                             */
/*---------------------------------------------------------------------*/
uv_timer_t *
bgl_uv_timer_new(BgL_uvtimerz00_bglt o, bgl_uv_loop_t loop) {
   uv_timer_t *new = (uv_timer_t *)GC_MALLOC(sizeof(uv_timer_t));
   new->data = o;

   uv_timer_init(LOOP_BUILTIN(loop), new);
   return new;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_timer_cb ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_uv_timer_cb(uv_timer_t *handle) {
   obj_t o = handle->data;
   bgl_uv_watcher_t watcher = (bgl_uv_watcher_t)COBJECT(o);
   uv_watcher_data_t *data = WATCHER_DATA(o);

   if (data) {
      obj_t p = data->proc;
      if (uv_timer_get_repeat((uv_timer_t *)(watcher->BgL_z42builtinz42)) == 0) {
	 free_watcher_data(data);
	 TRACECB(ttimer);
      }

      if (PROCEDUREP(p)) BGL_PROCEDURE_CALL1(p, o);
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_timer_start ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_uv_timer_start(obj_t obj, obj_t proc, uint64_t timeout, uint64_t repeat) {
   bgl_uv_watcher_t watcher = (bgl_uv_watcher_t)COBJECT(obj);
   uv_timer_t *w = (uv_timer_t *)(watcher->BgL_z42builtinz42);
   uv_watcher_data_t *data = get_watcher_data(obj);

   data->proc = proc;

   TRACECA(ttimer);
   return uv_timer_start(w, bgl_uv_timer_cb, timeout, repeat);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_timer_stop ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_uv_timer_stop(obj_t obj) {
   bgl_uv_watcher_t watcher = (bgl_uv_watcher_t)COBJECT(obj);
   uv_timer_t *w = (uv_timer_t *)(watcher->BgL_z42builtinz42);
   uv_watcher_data_t *data = get_watcher_data(obj);

   free_watcher_data(data);
   
   TRACECB(ttimer);
   return uv_timer_stop(w);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_fs_event_cb ...                                           */
/*---------------------------------------------------------------------*/
void
bgl_uv_fs_event_cb(uv_handle_t *handle, char *path, int events, int status) {
   bgl_uv_watcher_t o = (bgl_uv_watcher_t)handle->data;
   obj_t p = ((bgl_uv_watcher_t)COBJECT(o))->BgL_cbz00;

   if (PROCEDUREP(p)) {
      BGL_PROCEDURE_CALL4(p, (obj_t)o, string_to_bstring(path), BINT(events), BINT(status));
   }
}

/*---------------------------------------------------------------------*/
/*    uv_fs_event_t *                                                  */
/*    bgl_uv_fs_event_new ...                                          */
/*---------------------------------------------------------------------*/
uv_fs_event_t *
bgl_uv_fs_event_new(BgL_uvtimerz00_bglt o, bgl_uv_loop_t loop) {
   uv_fs_event_t *new = (uv_fs_event_t *)GC_MALLOC(sizeof(uv_fs_event_t));
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_fs_event_init(LOOP_BUILTIN(loop), new);
   return new;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_fs_poll_cb ...                                            */
/*---------------------------------------------------------------------*/
void
bgl_uv_fs_poll_cb(uv_handle_t *handle, int status, const uv_stat_t* prev, const uv_stat_t* curr) {
   bgl_uv_fs_poll_t o = (bgl_uv_fs_poll_t)handle->data;
   obj_t p = ((bgl_uv_fs_poll_t)COBJECT(o))->BgL_cbz00;

   /* some libuv versions uses -2 instead of -1 for error, fix this! */
   if (status < 0) { status = -1; }
   
   if (PROCEDUREP(p)) {
      BGL_PROCEDURE_CALL4(p, (obj_t)o, BINT(status), bgl_uv_fstat(*prev), bgl_uv_fstat(*curr));
   }
}

/*---------------------------------------------------------------------*/
/*    uv_fs_poll_t *                                                   */
/*    bgl_uv_fs_poll_new ...                                           */
/*---------------------------------------------------------------------*/
uv_fs_poll_t *
bgl_uv_fs_poll_new(bgl_uv_fs_poll_t o, bgl_uv_loop_t loop) {
   uv_fs_poll_t *new = (uv_fs_poll_t *)GC_MALLOC(sizeof(uv_fs_poll_t));
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_fs_poll_init(LOOP_BUILTIN(loop), new);
   return new;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_poll_getpath ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_poll_getpath(uv_fs_poll_t *o) {
   obj_t buf = make_string_sans_fill(256);
   size_t size = STRING_LENGTH(buf);
   int len = uv_fs_poll_getpath(o, BSTRING_TO_STRING(buf), &size);

   if (len == UV_ENOBUFS) {
      obj_t buf = make_string_sans_fill(size + 1);
      uv_fs_poll_getpath(o, BSTRING_TO_STRING(buf), &size);
   }

   return buf;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_poll_cb ...                                               */
/*---------------------------------------------------------------------*/
void
bgl_uv_poll_cb(uv_handle_t *handle, int status, int state) {
   bgl_uv_poll_t o = (bgl_uv_poll_t)handle->data;
   obj_t p = ((bgl_uv_poll_t)COBJECT(o))->BgL_cbz00;

   /* some libuv versions uses -2 instead of -1 for error, fix this! */
   if (status < 0) { status = -1; }
   
   if (PROCEDUREP(p)) {
      BGL_PROCEDURE_CALL3(p, (obj_t)o, BINT(status), bgl_uv_events_to_list(state));
   }
}


/*---------------------------------------------------------------------*/
/*    uv_poll_t *                                                      */
/*    bgl_uv_poll_new ...                                              */
/*---------------------------------------------------------------------*/
uv_poll_t *
bgl_uv_poll_new(obj_t o, bgl_uv_loop_t loop) {
   uv_poll_t *new = (uv_poll_t *)GC_MALLOC(sizeof(uv_poll_t));
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_poll_init(LOOP_BUILTIN(loop), new,
		 ((bgl_uv_poll_t)COBJECT(o))->BgL_fdz00);
   return new;
}

/*---------------------------------------------------------------------*/
/*    uv_idle_t *                                                      */
/*    bgl_uv_idle_new ...                                              */
/*---------------------------------------------------------------------*/
uv_idle_t *
bgl_uv_idle_new(BgL_uvidlez00_bglt o, bgl_uv_loop_t loop) {
   uv_idle_t *new = (uv_idle_t *)GC_MALLOC(sizeof(uv_idle_t));
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_idle_init(LOOP_BUILTIN(loop), new);
   return new;
}

/*---------------------------------------------------------------------*/
/*    uv_check_t *                                                     */
/*    bgl_uv_check_new ...                                             */
/*---------------------------------------------------------------------*/
uv_check_t *
bgl_uv_check_new(BgL_uvcheckz00_bglt o, bgl_uv_loop_t loop) {
   uv_check_t *new = (uv_check_t *)GC_MALLOC(sizeof(uv_check_t));
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_check_init(LOOP_BUILTIN(loop), new);
   return new;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_async_cb ...                                              */
/*---------------------------------------------------------------------*/
static void
bgl_uv_async_cb(uv_async_t *handle) {
   bgl_uv_watcher_t o = (bgl_uv_watcher_t)handle->data;
   obj_t p = ((bgl_uv_watcher_t)COBJECT(o))->BgL_cbz00;

   if (PROCEDUREP(p)) {
      BGL_PROCEDURE_CALL1(p, (obj_t)o);
   }
}

/*---------------------------------------------------------------------*/
/*    uv_async_t *                                                     */
/*    bgl_uv_async_new ...                                             */
/*---------------------------------------------------------------------*/
uv_async_t *
bgl_uv_async_new(bgl_uv_async_t o, bgl_uv_loop_t loop) {
   uv_async_t *new = (uv_async_t *)GC_MALLOC(sizeof(uv_async_t));
   new->data = o;

   uv_async_init(LOOP_BUILTIN(loop), new, &bgl_uv_async_cb);
   return new;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    _irq ...                                                         */
/*---------------------------------------------------------------------*/
static obj_t _irq = BUNSPEC, _idle, _sys, _nice, _user, _times, _speed, _model;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_init_cpus ...                                                 */
/*---------------------------------------------------------------------*/
static void
uv_init_cpus() {

   if (_irq == BUNSPEC) {
      _irq = string_to_symbol("irq");
      _idle = string_to_symbol("idle");
      _sys = string_to_symbol("sys");
      _nice = string_to_symbol("nice");
      _user = string_to_symbol("user");
      _times = string_to_symbol("times");
      _speed = string_to_symbol("speed");
      _model = string_to_symbol("model");
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_cpu ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
bgl_uv_cpu(uv_cpu_info_t cpu) {
   obj_t res = BNIL;
   obj_t times = BNIL;

   times = MAKE_PAIR(
      MAKE_PAIR(_irq, BGL_INT64_TO_BINT64(cpu.cpu_times.irq)),
      times);
   times = MAKE_PAIR(
      MAKE_PAIR(_idle, BGL_INT64_TO_BINT64(cpu.cpu_times.idle)),
      times);
   times = MAKE_PAIR(
      MAKE_PAIR(_sys, BGL_INT64_TO_BINT64(cpu.cpu_times.sys)),
      times);
   times = MAKE_PAIR(
      MAKE_PAIR(_nice, BGL_INT64_TO_BINT64(cpu.cpu_times.nice)),
      times);
   times = MAKE_PAIR(
      MAKE_PAIR(_user, BGL_INT64_TO_BINT64(cpu.cpu_times.user)),
      times);

   res = MAKE_PAIR(MAKE_PAIR(_times, times), res);
   res = MAKE_PAIR(MAKE_PAIR(_speed, BINT(cpu.speed)), res);
   res = MAKE_PAIR(MAKE_PAIR(_model, string_to_bstring(cpu.model)), res);

   return res;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_uv_resident_memory ...                                       */
/*---------------------------------------------------------------------*/
long
bgl_uv_resident_memory() {
   size_t rss;

  if (uv_resident_set_memory(&rss) != 0) {
     return 0;
  } else {
     return rss;
  }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_cpus ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_cpus() {
   int count;
   uv_cpu_info_t *cpus;

   if (uv_cpu_info(&cpus, &count)) {
      return create_vector(0);
   } else {
      obj_t vec = create_vector(count);
      int i;

      uv_init_cpus();
      
      for (i = 0; i < count; i++) {
	 VECTOR_SET(vec, i, bgl_uv_cpu(cpus[i]));
      }

      uv_free_cpu_info(cpus, count);
      return vec;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_exepath ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_exepath() {
   char buffer[1024];
   size_t n = 1024;

   uv_exepath(buffer, &n);

   return string_to_bstring_len(buffer, n);
}

/*---------------------------------------------------------------------*/
/*    BGL_UV_FS_WRAPPER ...                                            */
/*---------------------------------------------------------------------*/
#define BGL_UV_FS_WRAPPER0(name, obj) { \
   uv_loop_t *loop = LOOP_BUILTIN(bloop); \
   int r; \
   if (bgl_check_fs_cb(proc, 1, #name)) { \
      uv_fs_t *req = alloc_uv_fs(); \
      ((uv_fs_data_t *)(req->data))->proc = proc; \
      if (!(r = name(loop, req, obj, &bgl_uv_fs_cb) >= 0)) { \
        free_uv_fs(req); \
      } \
      return r; \
   } else { \
      uv_fs_t req; \
      if ((r = name(loop, &req, obj, 0L)) >= 0) { \
         r = req.result; \
      } \
      uv_fs_req_cleanup(&req); \
      return r; \
   } \
}

#define BGL_UV_FS_WRAPPER1(name, obj, arg) { \
   uv_loop_t *loop = LOOP_BUILTIN(bloop); \
   int r; \
   if (bgl_check_fs_cb(proc, 1, #name)) { \
      uv_fs_t *req = alloc_uv_fs(); \
      ((uv_fs_data_t *)(req->data))->proc = proc; \
      if (!(r = name(loop, req, obj, arg, &bgl_uv_fs_cb) >= 0)) { \
        free_uv_fs(req); \
      } \
      return r; \
   } else { \
      uv_fs_t req; \
      if ((r = name(loop, &req, obj, arg, 0L)) >= 0) { \
        r = req.result; \
      } \
      uv_fs_req_cleanup(&req); \
      return r; \
   } \
}

#define BGL_UV_FS_WRAPPER2(name, obj, arg0, arg1) {	\
   uv_loop_t *loop = LOOP_BUILTIN(bloop); \
   int r; \
   if (bgl_check_fs_cb(proc, 1, #name)) { \
      uv_fs_t *req = alloc_uv_fs(); \
      ((uv_fs_data_t *)(req->data))->proc = proc; \
      if (!(r = name(loop, req, obj, arg0, arg1, &bgl_uv_fs_cb) >= 0)) { \
        free_uv_fs(req); \
      } \
      return r; \
   } else { \
      uv_fs_t req; \
      if ((r = name(loop, &req, obj, arg0, arg1, 0L)) >= 0) { \
        r = req.result; \
      } \
      uv_fs_req_cleanup(&req); \
      return r; \
   } \
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_cb ...                                                 */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_cb(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   obj_t proc = data->proc;

   BGL_PROCEDURE_CALL1(proc, BINT(req->result));
   free_uv_fs(req);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_rw_cb ...                                              */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_rw_cb(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   obj_t proc = data->proc;

   BGL_PROCEDURE_CALL1(proc, BINT(req->result));
   free_uv_fs(req);
   TRACECB(trw);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_rw2_cb ...                                             */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_rw2_cb(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   
   obj_t proc = data->proc;
   obj_t arg0 = data->arg[0];
   obj_t arg1 = data->arg[1];
   
   BGL_PROCEDURE_CALL3(proc, BINT(req->result), arg0, arg1);
   free_uv_fs(req);
   TRACECB(trw2);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_rw3_cb ...                                             */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_rw3_cb(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   
   obj_t proc = data->proc;
   obj_t arg0 = data->arg[0];
   obj_t arg1 = data->arg[1];
   obj_t arg2 = data->arg[2];
   
   BGL_PROCEDURE_CALL4(proc, BINT(req->result), arg0, arg1, arg2);
   free_uv_fs(req);
   TRACECB(trw3);
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_check_fs_cb ...                                              */
/*---------------------------------------------------------------------*/
static int
bgl_check_fs_cb(obj_t proc, int arity, char *fun) {
   if (PROCEDUREP(proc)) {
      if (PROCEDURE_CORRECT_ARITYP(proc, arity)) {
	 return 1;
      } else {
	 C_SYSTEM_FAILURE(BGL_ERROR, fun,
			   "wrong callback arity", proc);
	 return -1;
      }
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_rename ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_rename(char *oldp, char *newp, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER1(uv_fs_rename, oldp, newp)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_ftruncate ...                                          */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_ftruncate(obj_t obj, int64_t offset, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;

   BGL_UV_FS_WRAPPER1(uv_fs_ftruncate, fd, offset)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_fchown ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_fchown(obj_t obj, int uid, int gid, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;
   
   BGL_UV_FS_WRAPPER2(uv_fs_fchown, fd, uid, gid)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_lchown ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_lchown(char *path, int uid, int gid, obj_t proc, bgl_uv_loop_t bloop) {
   C_SYSTEM_FAILURE(BGL_IO_PORT_ERROR, "uv_fs_lchown",
		     "Not implemented", proc);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_chown ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_chown(char *path, int uid, int gid, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER2(uv_fs_chown, path, uid, gid)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_fchmod ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_fchmod(obj_t obj, int mod, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;
   
   BGL_UV_FS_WRAPPER1(uv_fs_fchmod, fd, mod)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_chmod ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_chmod(char *path, int mod, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER1(uv_fs_chmod, path, mod)
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_open_cb ...                                            */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_open_cb(uv_fs_t* req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   obj_t proc = data->proc;
   obj_t obj;
   
   if (req->result <= 0) {
      obj = BINT(req->result);
   } else {
      obj_t name = string_to_bstring((char *)req->path);
      obj = bgl_uv_new_file(req->result, name);
   }
   
   BGL_PROCEDURE_CALL1(proc, obj);
   free_uv_fs(req);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_open ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_open(obj_t bpath, int flags, int mode, obj_t proc, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   char *path = BSTRING_TO_STRING(bpath);

   if (bgl_check_fs_cb(proc, 1, "uv-fs-open")) {
      uv_fs_t *req = alloc_uv_fs();
      uv_fs_data_t *data = (uv_fs_data_t *)(req->data);
      
      data->proc = proc;

      if (uv_fs_open(loop, req, path, flags, mode, bgl_uv_fs_open_cb) < 0) {
	 free_uv_fs(req);
      } else {
	 TRACECNT(kopen);
      }

      return BUNSPEC;
   } else {
      uv_fs_t req;
      obj_t res;

      if (uv_fs_open(loop, &req, path, flags, mode, 0L) >= 0) {
	 TRACECNT(kopen);
      }

      if (req.result <= 0) {
	 res = BINT(req.result);
      } else {
	 res = bgl_uv_new_file(req.result, bpath);
      }

      uv_fs_req_cleanup(&req);
      
      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_open4_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_open4_cb(uv_fs_t* req) {
   obj_t obj;
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;

   obj_t proc = data->proc;
   obj_t arg0 = data->arg[0];
   obj_t arg1 = data->arg[1];
   obj_t arg2 = data->arg[2];
   obj_t arg3 = data->arg[3];
   obj_t name = data->arg[4];
   
   if (req->result <= 0) {
      obj = BINT(req->result);
   } else {
      obj = bgl_uv_new_file(req->result, name);
   }
   
   BGL_PROCEDURE_CALL5(proc, obj, arg0, arg1, arg2, arg3);
   free_uv_fs(req);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_open4 ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_open4(obj_t bpath, int flags, int mode, obj_t proc, obj_t arg0, obj_t arg1, obj_t arg2, obj_t arg3, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   char *path = BSTRING_TO_STRING(bpath);

   if (bgl_check_fs_cb(proc, 5, "uv-fs-open4")) {
      uv_fs_t *req = alloc_uv_fs();
      uv_fs_data_t *data = (uv_fs_data_t *)req->data;

      data->proc = proc;
      data->arg[0] = arg0;
      data->arg[1] = arg1;
      data->arg[2] = arg2;
      data->arg[3] = arg3;
      data->arg[4] = bpath;
      
      if (uv_fs_open(loop, req, path, flags, mode, bgl_uv_fs_open4_cb) < 0) {
	 free_uv_fs(req);
      } else {
	 TRACECNT(kopen);
      }

      return BUNSPEC;
   } else {
      uv_fs_t req;
      obj_t res;

      uv_fs_open(loop, &req, path, flags, mode, 0L);

      if (req.result <= 0) {
	 res = BINT(req.result);
      } else {
	 res = bgl_uv_new_file(req.result, bpath);
      }

      uv_fs_req_cleanup(&req);
      
      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_close ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_close(obj_t port, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   BGL_UV_FS_WRAPPER0(uv_fs_close, fd);
   TRACECNT(kclose);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_close2 ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_close2(obj_t port, obj_t proc, obj_t arg0, obj_t arg1, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   uv_loop_t *loop = LOOP_BUILTIN(bloop); 
   int r; 
   if (bgl_check_fs_cb(proc, 3, "uv-fs-close2")) { 
      uv_fs_t *req = alloc_uv_fs(); 
      uv_fs_data_t *data = (uv_fs_data_t *)req->data;

      data->proc = proc;
      data->arg[0] = arg0;
      data->arg[1] = arg1;

      if (!(r = uv_fs_close(loop, req, fd, &bgl_uv_fs_rw2_cb) >= 0)) { 
	 free_uv_fs(req);
      } else {
	 TRACECA(trw2);
	 TRACECNT(kclose);
      }
      
      return r; 
   } else { 
      uv_fs_t req; 
      if ((r = uv_fs_close(loop, &req, fd, 0L)) >= 0) { 
         r = req.result; 
      } 
      uv_fs_req_cleanup(&req);
      
      return r; 
   } 
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_copyfile ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_copyfile(obj_t bpath, obj_t bnewpath, int flags, obj_t proc, bgl_uv_loop_t bloop) {
   const char *path = BSTRING_TO_STRING(bpath);
   const char *newpath = BSTRING_TO_STRING(bnewpath);
   
   BGL_UV_FS_WRAPPER2(uv_fs_copyfile, path, newpath, flags);
}

/*---------------------------------------------------------------------*/
/*    statbuf ...                                                      */
/*---------------------------------------------------------------------*/
static obj_t _dev = BUNSPEC, _mode, _nlink, _uid, _gid, _rdev, _ino, _size;
static obj_t _blksize, _blocks, _flags, _gen, _atime, _mtime, _ctime, _btime;
static obj_t _atimeNS, _mtimeNS, _ctimeNS, _btimeNS;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_init_stat ...                                                 */
/*---------------------------------------------------------------------*/
static void
uv_init_stat() {
   if (_dev == BUNSPEC) {
      _dev = string_to_symbol("dev");
      _mode = string_to_symbol("mode");
      _nlink = string_to_symbol("nlink");
      _uid = string_to_symbol("uid");
      _gid = string_to_symbol("gid");
      _rdev = string_to_symbol("rdev");
      _ino = string_to_symbol("ino");
      _size = string_to_symbol("size");
      _blksize = string_to_symbol("blksize");
      _blocks = string_to_symbol("blocks");
      _flags = string_to_symbol("flags");
      _gen = string_to_symbol("gen");
      _atime = string_to_symbol("atime");
      _mtime = string_to_symbol("mtime");
      _ctime = string_to_symbol("ctime");
      _btime = string_to_symbol("btime");
      _atimeNS = string_to_symbol("atime-ns");
      _mtimeNS = string_to_symbol("mtime-ns");
      _ctimeNS = string_to_symbol("ctime-ns");
      _btimeNS = string_to_symbol("btime-ns");
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fstat ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fstat(uv_stat_t buf) {
   obj_t res = BNIL;

   uv_init_stat();

   res = MAKE_PAIR(
      MAKE_PAIR(_ctime, ELONG_TO_BELONG(buf.st_ctim.tv_sec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_mtime, ELONG_TO_BELONG(buf.st_mtim.tv_sec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_atime, ELONG_TO_BELONG(buf.st_atim.tv_sec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_btime, ELONG_TO_BELONG(buf.st_birthtim.tv_sec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_gen, BGL_INT64_TO_BINT64(buf.st_gen)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_flags, BGL_INT64_TO_BINT64(buf.st_flags)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_blocks, BGL_INT64_TO_BINT64(buf.st_blocks)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_blksize, BGL_INT64_TO_BINT64(buf.st_blksize)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_size, BGL_INT64_TO_BINT64(buf.st_size)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_ino, BGL_INT64_TO_BINT64(buf.st_ino)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_rdev, BGL_INT64_TO_BINT64(buf.st_rdev)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_gid, BGL_INT64_TO_BINT64(buf.st_gid)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_uid, BGL_INT64_TO_BINT64(buf.st_uid)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_nlink, BGL_INT64_TO_BINT64(buf.st_nlink)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_mode, BGL_INT64_TO_BINT64(buf.st_mode)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_dev, BGL_INT64_TO_BINT64(buf.st_dev)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_ctimeNS, ELONG_TO_BELONG(buf.st_ctim.tv_nsec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_mtimeNS, ELONG_TO_BELONG(buf.st_mtim.tv_nsec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_atimeNS, ELONG_TO_BELONG(buf.st_atim.tv_nsec)),
      res);
   res = MAKE_PAIR(
      MAKE_PAIR(_btimeNS, ELONG_TO_BELONG(buf.st_birthtim.tv_nsec)),
      res);

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fstat_vec ...                                             */
/*    -------------------------------------------------------------    */
/*    Fill the vector passed as argument with the stat values. The     */
/*    vector must be _at least_ as large to store all values.          */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fstat_vec(uv_stat_t buf, obj_t vec) {
#if (PTR_ALIGNMENT >= 3)   
   VECTOR_SET(vec, 0, BINT(buf.st_ctim.tv_sec)); // ctime
   VECTOR_SET(vec, 1, BINT(buf.st_mtim.tv_sec)); // mtime
   VECTOR_SET(vec, 2, BINT(buf.st_atim.tv_sec)); // atime
   VECTOR_SET(vec, 3, BINT(buf.st_birthtim.tv_sec)); // btime
   VECTOR_SET(vec, 4, BINT(buf.st_gen)); // gen
   VECTOR_SET(vec, 5, BINT(buf.st_flags)); // flags
   VECTOR_SET(vec, 6, BINT(buf.st_blocks)); // blocks
   VECTOR_SET(vec, 7, BINT(buf.st_blksize)); // blksize
   VECTOR_SET(vec, 8, BINT(buf.st_size)); // size
   VECTOR_SET(vec, 9, BINT(buf.st_ino)); // ino
   VECTOR_SET(vec, 10, BINT(buf.st_rdev)); // rdev
   VECTOR_SET(vec, 11, BINT(buf.st_gid)); // gid
   VECTOR_SET(vec, 12, BINT(buf.st_uid)); // uid
   VECTOR_SET(vec, 13, BINT(buf.st_nlink)); // nlink
   VECTOR_SET(vec, 14, BINT(buf.st_mode)); // mode
   VECTOR_SET(vec, 15, BINT(buf.st_dev)); // dev
   VECTOR_SET(vec, 16, BINT(buf.st_ctim.tv_nsec)); // ctimeNS
   VECTOR_SET(vec, 17, BINT(buf.st_mtim.tv_nsec)); // mtimeNS
   VECTOR_SET(vec, 18, BINT(buf.st_atim.tv_nsec)); // atimeNS
   VECTOR_SET(vec, 19, BINT(buf.st_birthtim.tv_nsec)); // birthtimeNS
#else
   VECTOR_SET(vec, 0, ELONG_TO_BELONG(buf.st_ctim.tv_sec)); // ctime
   VECTOR_SET(vec, 1, ELONG_TO_BELONG(buf.st_mtim.tv_sec)); // mtime
   VECTOR_SET(vec, 2, ELONG_TO_BELONG(buf.st_atim.tv_sec)); // atime
   VECTOR_SET(vec, 3, ELONG_TO_BELONG(buf.st_birthtim.tv_sec)); // btime
   VECTOR_SET(vec, 4, BGL_INT64_TO_BINT64(buf.st_gen)); // gen
   VECTOR_SET(vec, 5, BGL_INT64_TO_BINT64(buf.st_flags)); // flags
   VECTOR_SET(vec, 6, BGL_INT64_TO_BINT64(buf.st_blocks)); // blocks
   VECTOR_SET(vec, 7, BGL_INT64_TO_BINT64(buf.st_blksize)); // blksize
   VECTOR_SET(vec, 8, BGL_INT64_TO_BINT64(buf.st_size)); // size
   VECTOR_SET(vec, 9, BGL_INT64_TO_BINT64(buf.st_ino)); // ino
   VECTOR_SET(vec, 10, BGL_INT64_TO_BINT64(buf.st_rdev)); // rdev
   VECTOR_SET(vec, 11, BGL_INT64_TO_BINT64(buf.st_gid)); // gid
   VECTOR_SET(vec, 12, BGL_INT64_TO_BINT64(buf.st_uid)); // uid
   VECTOR_SET(vec, 13, BGL_INT64_TO_BINT64(buf.st_nlink)); // nlink
   VECTOR_SET(vec, 14, BGL_INT64_TO_BINT64(buf.st_mode)); // mode
   VECTOR_SET(vec, 15, BGL_INT64_TO_BINT64(buf.st_dev)); // dev
   VECTOR_SET(vec, 16, ELONG_TO_BELONG(buf.st_ctim.tv_nsec)); // ctimeNS
   VECTOR_SET(vec, 17, ELONG_TO_BELONG(buf.st_mtim.tv_nsec)); // mtimeNS
   VECTOR_SET(vec, 18, ELONG_TO_BELONG(buf.st_atim.tv_nsec)); // atimeNS
   VECTOR_SET(vec, 19, ELONG_TO_BELONG(buf.st_birthtim.tv_nsec)); // birthtimeNS
#endif
   return vec;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_fstat_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_fstat_cb(uv_fs_t *req) {
   obj_t p = (obj_t)req->data;

   gc_unmark(p);

   if (req->result < 0) {
      BGL_PROCEDURE_CALL1(p, BINT(req->result));
   } else {
      BGL_PROCEDURE_CALL1(p, bgl_uv_fstat(req->statbuf));
   }

   uv_fs_req_cleanup(req);
   
   free(req);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_fstat_vec_cb ...                                       */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_fstat_vec_cb(uv_fs_t *req) {
   uv_fs_data_t *data = (uv_fs_data_t *)req->data;
   obj_t p = data->proc;
   obj_t v = data->arg[0];

   if (req->result >= 0) {
      bgl_uv_fstat_vec(req->statbuf, v);
   }
      
   BGL_PROCEDURE_CALL2(p, BINT(req->result), v);
   free_uv_fs(req);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_fstat ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_fstat(obj_t port, obj_t proc, obj_t vec, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   if (PROCEDUREP(proc)) {
      if (PROCEDURE_CORRECT_ARITYP(proc, 2)) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = vec;
       
	 uv_fs_fstat(loop, req, fd, &bgl_uv_fs_fstat_vec_cb);
      
	 return BUNSPEC;
      } else if (PROCEDURE_CORRECT_ARITYP(proc, 1)) {
	 uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));
	 req->data = proc;
	 gc_mark(proc);
	 
	 uv_fs_fstat(loop, req, fd, &bgl_uv_fs_fstat_cb);
	 
	 return BUNSPEC;
      } else {
	 C_SYSTEM_FAILURE(BGL_ERROR, "bgl_uv_fs_fstat",
			  "wrong callback arity", proc);
	 return BUNSPEC;
      }
   } else {
      uv_fs_t req;

      if (uv_fs_fstat(loop, &req, fd, 0L) < 0) {
	 uv_fs_req_cleanup(&req);
	 return BINT(req.result);
      } else if (VECTORP(vec)) {
	 bgl_uv_fstat_vec(req.statbuf, vec);
	 uv_fs_req_cleanup(&req);

	 return BUNSPEC;
      } else {
	 obj_t res = bgl_uv_fstat(req.statbuf);

	 uv_fs_req_cleanup(&req);

	 return res;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_lstat ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_lstat(char *path, obj_t proc, obj_t vec, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);

   if (PROCEDUREP(proc)) {
      if (PROCEDURE_CORRECT_ARITYP(proc, 2)) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = vec;
      
	 uv_fs_lstat(loop, req, path, &bgl_uv_fs_fstat_vec_cb);
      
	 return BUNSPEC;
      } else if (PROCEDURE_CORRECT_ARITYP(proc, 1)) {
	 uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));
	 req->data = proc;
	 gc_mark(proc);

	 uv_fs_lstat(loop, req, path, &bgl_uv_fs_fstat_cb);
	 
	 return BUNSPEC;
      } else {
	 C_SYSTEM_FAILURE(BGL_ERROR, "bgl_uv_fs_lstat",
			  "wrong callback arity", proc);
	 return BUNSPEC;
      }
   } else {
      uv_fs_t req;

      if (uv_fs_lstat(loop, &req, path, 0L) < 0) {
	 uv_fs_req_cleanup(&req);
	 return BINT(req.result);
      } else if (VECTORP(vec)) {
	 bgl_uv_fstat_vec(req.statbuf, vec);
	 uv_fs_req_cleanup(&req);

	 return BUNSPEC;
      } else {
	 obj_t res = bgl_uv_fstat(req.statbuf);

	 uv_fs_req_cleanup(&req);

	 return BUNSPEC;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_stat ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_stat(char *path, obj_t proc, obj_t vec, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);

   if (PROCEDUREP(proc)) {
      if (PROCEDURE_CORRECT_ARITYP(proc, 2)) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = vec;

	 uv_fs_stat(loop, req, path, &bgl_uv_fs_fstat_vec_cb);
      
	 return BUNSPEC;
      } else if (PROCEDURE_CORRECT_ARITYP(proc, 1)) {
	 uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));
	 req->data = proc;
	 gc_mark(proc);
      
	 uv_fs_stat(loop, req, path, &bgl_uv_fs_fstat_cb);
      
	 return BUNSPEC;
      } else {C_SYSTEM_FAILURE(BGL_ERROR, "bgl_uv_fs_stat",
			       "wrong callback arity", proc);
	 return BUNSPEC;
      }
   } else {
      uv_fs_t req;

      if (uv_fs_stat(loop, &req, path, 0L) < 0) {
	 uv_fs_req_cleanup(&req);
	 return BINT(req.result);
      } else if (VECTORP(vec)) {
	 bgl_uv_fstat_vec(req.statbuf, vec);
	 uv_fs_req_cleanup(&req);

	 return BUNSPEC;
      } else {
	 obj_t res = bgl_uv_fstat(req.statbuf);

	 uv_fs_req_cleanup(&req);

	 return res;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_link ...                                               */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_link(char *oldp, char *newp, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER1(uv_fs_link, oldp, newp)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_symlink ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_symlink(char *oldp, char *newp, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER2(uv_fs_symlink, oldp, newp, 0)
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_readlink_cb ...                                        */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_readlink_cb(uv_fs_t *req) {
   obj_t p = (obj_t)req->data;

   gc_unmark(p);

   if (req->result < 0) {
      BGL_PROCEDURE_CALL1(p, BINT(req->result));
   } else {
      BGL_PROCEDURE_CALL1(p, string_to_bstring((char *)req->ptr));
   }

   uv_fs_req_cleanup(req);
   free(req);
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_readlink ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_readlink(char *path, obj_t proc, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);

   if (bgl_check_fs_cb(proc, 1, "uv_fs_readlink")) {
      uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));
      req->data = proc;
      gc_mark(proc);
      
      uv_fs_readlink(loop, req, path, &bgl_uv_fs_readlink_cb);
      
      return BUNSPEC;
   } else {
      uv_fs_t req;

      if (uv_fs_readlink(loop, &req, path, 0L) < 0) {
	 uv_fs_req_cleanup(&req);

	 return BINT(req.result);
      } else {
	 obj_t res = string_to_bstring((char *)req.ptr);

	 uv_fs_req_cleanup(&req);

	 return res;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_unlink ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_unlink(char *path, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER0(uv_fs_unlink, path)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_rmdir ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_rmdir(char *path, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER0(uv_fs_rmdir, path)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_mkdir ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_mkdir(char *path, int mod, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER1(uv_fs_mkdir, path, mod)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_fsync ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_fsync(obj_t port, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   BGL_UV_FS_WRAPPER0(uv_fs_fsync, fd)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_fdatasync ...                                          */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_fdatasync(obj_t port, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   BGL_UV_FS_WRAPPER0(uv_fs_fdatasync, fd)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_futime ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_futime(obj_t port, double atime, double mtime, obj_t proc, bgl_uv_loop_t bloop) {
   int fd = ((bgl_uv_file_t)COBJECT(port))->BgL_fdz00;

   BGL_UV_FS_WRAPPER2(uv_fs_futime, fd, atime, mtime)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_utime ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_utime(char *path, double atime, double mtime, obj_t proc, bgl_uv_loop_t bloop) {
   BGL_UV_FS_WRAPPER2(uv_fs_utime, path, atime, mtime)
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_write ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_write(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-write",
			"offset+length out of buffer range",
			BINT(STRING_LENGTH(buffer)));
   } else {
      uv_buf_t iov;

      iov = uv_buf_init((char *)&(STRING_REF(buffer, offset)), length);
      
      if (bgl_check_fs_cb(proc, 1, "uv-fs-write")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;

	 TRACECA(trw);
	 uv_fs_write(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw_cb);
      } else {
	 uv_fs_t req;
	 int r;

	 r = uv_fs_write(loop, &req, fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);
	 
	 return r;
      }
   }
}
      
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_write2 ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_write2(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, obj_t arg0, obj_t arg1, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-write2",
			"offset+length out of buffer range",
			BINT(STRING_LENGTH(buffer)));
   } else {
      uv_buf_t iov;

      iov = uv_buf_init((char *)&(STRING_REF(buffer, offset)), length);
      
      if (bgl_check_fs_cb(proc, 3, "uv-fs-write2")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = arg0;
	 data->arg[1] = arg1;

	 TRACECA(trw2);
	 uv_fs_write(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw2_cb);
      } else {
	 uv_fs_t req;
	 int r;

	 r = uv_fs_write(loop, &req, fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);
	 
	 return r;
      }
   }
}
      
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_write3 ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_write3(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, obj_t arg0, obj_t arg1, obj_t arg2, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-write3",
			"offset+length out of buffer range",
			BINT(STRING_LENGTH(buffer)));
   } else {
      uv_buf_t iov;

      iov = uv_buf_init((char *)&(STRING_REF(buffer, offset)), length);
      
      if (bgl_check_fs_cb(proc, 4, "uv-fs-write3")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = arg0;
	 data->arg[1] = arg1;
	 data->arg[2] = arg2;

	 TRACECA(trw3);
	 uv_fs_write(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw3_cb);
      } else {
	 uv_fs_t req;
	 int r;

	 r = uv_fs_write(loop, &req, fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);
	 
	 return r;
      }
   }
}
      
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_read ...                                               */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_read(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;
   int len = 0;
   int r;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-read",
			"offset+length out of buffer range",
			BINT(len));
   } else {
      uv_buf_t iov;
      iov = uv_buf_init((void *)&(STRING_REF(buffer, offset)), length);

      if (bgl_check_fs_cb(proc, 1, "uv-fs-read")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;

	 r = uv_fs_read(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw_cb);
	 if (r == -1) {
	    free_uv_fs(req);
	 } else {
	    TRACECA(trw);
	 }
	 return r;
      } else {
	 uv_fs_t req;

	 r = uv_fs_read(loop, &req, fd < 0 ? 0 : fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);

	 return r;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_read2 ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_read2(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, obj_t arg0, obj_t arg1, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;
   int len = 0;
   int r;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-read",
			"offset+length out of buffer range",
			BINT(len));
   } else {
      uv_buf_t iov;
      iov = uv_buf_init((void *)&(STRING_REF(buffer, offset)), length);

      if (bgl_check_fs_cb(proc, 3, "uv-fs-read2")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;
	 
	 data->proc = proc;
	 data->arg[0] = arg0;
	 data->arg[1] = arg1;

	 r = uv_fs_read(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw2_cb);
	 if (r == -1) {
	    free_uv_fs(req);
	 } else {
	    TRACECA(trw2);
	 }
	 return r;
      } else {
	 uv_fs_t req;

	 r = uv_fs_read(loop, &req, fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);

	 return r;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_read3 ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_read3(obj_t obj, obj_t buffer, long offset, long length, int64_t position, obj_t proc, obj_t arg0, obj_t arg1, obj_t arg2, bgl_uv_loop_t bloop) {
   uv_loop_t *loop = LOOP_BUILTIN(bloop);
   int fd = ((bgl_uv_file_t)COBJECT(obj))->BgL_fdz00;
   int len = 0;
   int r;

   if (length + offset > STRING_LENGTH(buffer)) {
      C_SYSTEM_FAILURE(BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-read",
			"offset+length out of buffer range",
			BINT(len));
   } else {
      uv_buf_t iov;
      iov = uv_buf_init((void *)&(STRING_REF(buffer, offset)), length);

      if (bgl_check_fs_cb(proc, 4, "uv-fs-read3")) {
	 uv_fs_t *req = alloc_uv_fs();
	 uv_fs_data_t *data = (uv_fs_data_t *)req->data;

	 data->proc = proc;
	 data->arg[0] = arg0;
	 data->arg[1] = arg1;
	 data->arg[2] = arg2;

	 r = uv_fs_read(loop, req, fd, &iov, 1, position, &bgl_uv_fs_rw3_cb);
	 if (r == -1) {
	    free_uv_fs(req);
	 } else {
	    TRACECA(trw3);
	 }

	 return r;
      } else {
	 uv_fs_t req;

	 r = uv_fs_read(loop, &req, fd, &iov, 1, position, 0L);
	 uv_fs_req_cleanup(&req);

	 return r;
      }
   }
}

/* {*---------------------------------------------------------------------*} */
/* {*    static int                                                       *} */
/* {*    bgl_uv_close_file ...                                            *} */
/* {*---------------------------------------------------------------------*} */
/* static int                                                          */
/* bgl_uv_close_file(int file) {                                     */
/*    uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));           */
/*    uv_loop_t *loop = uv_default_loop();                             */
/*    int res = uv_fs_close(loop, req, (uv_file)file, 0L);           */
/*                                                                     */
/*    uv_fs_req_cleanup(req);                                        */
/*    free(req);                                                     */
/*    return res;                                                      */
/* }                                                                   */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    static long                                                      *} */
/* {*    bgl_uv_sync_read ...                                             *} */
/* {*---------------------------------------------------------------------*} */
/* static long                                                         */
/* bgl_uv_sync_read(obj_t port, void *ptr, size_t num) {             */
/*    int fd = (long)PORT_FILE(port);                                */
/*    long n;                                                          */
/*                                                                     */
/*  loop:                                                              */
/*    if ((n = read(fd, ptr, num)) <= 0) {                         */
/*       if (n == 0) {                                                */
/* 	 INPUT_PORT(port).eof = 1;                                   */
/*       } else if (errno == EINTR) {                                 */
/* 	 goto loop;                                                    */
/*       }                                                             */
/*    }                                                                */
/*                                                                     */
/*    return n;                                                        */
/* }                                                                   */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    static void                                                      *} */
/* {*    bgl_uv_file_seek ...                                             *} */
/* {*---------------------------------------------------------------------*} */
/* static void                                                         */
/* bgl_uv_file_seek(obj_t port, long pos) {                          */
/*    int fd = (long)PORT_FILE(port);                                */
/*                                                                     */
/*    if (lseek(fd, pos, SEEK_SET) == -1) {                         */
/*       C_SYSTEM_FAILURE(BGL_IO_PORT_ERROR, "set-input-port-position!", */
/* 			strerror(errno),                             */
/* 			port);                                        */
/*    }                                                                */
/*                                                                     */
/*    INPUT_PORT(port).filepos = pos;                                */
/*    INPUT_PORT(port).eof = 0;                                      */
/*    INPUT_PORT(port).matchstart = 0;                               */
/*    INPUT_PORT(port).matchstop = 0;                                */
/*    INPUT_PORT(port).forward = 0;                                  */
/*    INPUT_PORT(port).bufpos = 0;                                   */
/*    INPUT_PORT(port).lastchar = '\n';                              */
/*                                                                     */
/*    RGC_BUFFER_SET(port, 0, '\0');                                 */
/* }                                                                   */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    static void                                                      *} */
/* {*    bgl_uv_fs_open_input_file_cb ...                                 *} */
/* {*---------------------------------------------------------------------*} */
/* static void                                                         */
/* bgl_uv_fs_open_input_file_cb(uv_fs_t* req) {                      */
/*    obj_t proc = CAR((obj_t)req->data);                            */
/*    obj_t buffer = CDR((obj_t)req->data);                          */
/*    obj_t port;                                                      */
/*                                                                     */
/*    gc_unmark(req->data);                                          */
/*                                                                     */
/*    if (req->result == -1) {                                        */
/*       port = BINT(req->result);                                   */
/*    } else {                                                         */
/*       port = bgl_make_input_port(                                  */
/* 	 string_to_bstring((char*)(req->path)), (FILE *)req->result, KINDOF_FILE, buffer); */
/*                                                                     */
/*       INPUT_PORT(port).port.userdata = GC_MALLOC(sizeof(uv_fs_t)); */
/*       INPUT_PORT(port).sysread = &bgl_uv_sync_read;               */
/*       INPUT_PORT(port).sysseek = &bgl_uv_file_seek;               */
/*       INPUT_PORT(port).port.sysclose = &bgl_uv_close_file;        */
/*       BGL_INPUT_PORT_LENGTH_SET(port, bgl_file_size((char *)req->path)); */
/*    }                                                                */
/*                                                                     */
/*    uv_fs_req_cleanup(req);                                        */
/*    free(req);                                                     */
/*                                                                     */
/*    PROCEDURE_ENTRY(proc)(proc, port, BEOA);                     */
/* }                                                                   */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    obj_t                                                            *} */
/* {*    bgl_uv_open_input_file ...                                       *} */
/* {*---------------------------------------------------------------------*} */
/* obj_t                                                               */
/* bgl_uv_open_input_file(obj_t name, obj_t buffer, obj_t proc) {    */
/*    uv_fs_t *req = (uv_fs_t *)malloc(sizeof(uv_fs_t));           */
/*    char *path = BSTRING_TO_STRING(name);                          */
/*    uv_loop_t *loop = uv_default_loop();                             */
/*    obj_t res;                                                       */
/*    int r;                                                           */
/*                                                                     */
/*    req->data = MAKE_PAIR(proc, buffer);                           */
/*    gc_mark(req->data);                                            */
/*                                                                     */
/*    if (r = uv_fs_open(loop, req, path, O_RDONLY, 0,                */
/* 		       PROCEDUREP(proc) ? bgl_uv_fs_open_input_file_cb : 0L) < 0) { */
/*       uv_fs_req_cleanup(req);                                     */
/*       free(req);                                                  */
/*       C_SYSTEM_FAILURE(BGL_IO_PORT_ERROR, "uv-open-input-file",    */
/* 			(char *)uv_strerror(req->result),            */
/* 			name);                                        */
/*    } else {                                                         */
/*       obj_t port = bgl_make_input_port(name, (FILE *)req->result, KINDOF_FILE, buffer); */
/*                                                                     */
/*       if (proc == BFALSE) {                                        */
/* 	 uv_fs_req_cleanup(req);                                     */
/* 	 free(req);                                                  */
/*       }                                                             */
/*                                                                     */
/*       INPUT_PORT(port).port.userdata = GC_MALLOC(sizeof(uv_fs_t)); */
/*       INPUT_PORT(port).sysread = &bgl_uv_sync_read;               */
/*       INPUT_PORT(port).sysseek = &bgl_uv_file_seek;               */
/*       INPUT_PORT(port).port.sysclose = &bgl_uv_close_file;        */
/*                                                                     */
/*       return port;                                                  */
/*    }                                                                */
/* }                                                                   */

/*---------------------------------------------------------------------*/
/*    MAX_IP_LEN ...                                                   */
/*---------------------------------------------------------------------*/
#define MAX_IP_LEN \
      (INET6_ADDRSTRLEN > INET_ADDRSTRLEN ? INET6_ADDRSTRLEN : INET_ADDRSTRLEN)

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    itof ...                                                         */
/*---------------------------------------------------------------------*/
static int
itof(int family) {
   switch(family) {
      case 6:
	 return AF_INET6;
	 break;

      case 4:
	 return AF_INET;

      default:
	 return AF_UNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_getaddrinfo_cb ...                                        */
/*---------------------------------------------------------------------*/
static void
bgl_uv_getaddrinfo_cb(uv_getaddrinfo_t *req, int status, struct addrinfo *res) {
   obj_t p = (obj_t)req->data;
   gc_unmark(p);

   if (status) {
      free(req);
      BGL_PROCEDURE_CALL1(p, BINT(status));
   } else {
      char *addr;
      obj_t acc = BNIL;
      struct addrinfo *tmp;
      
      char ip[MAX_IP_LEN];

      // iterate over the IPv6 addresses 
      for (tmp = res; tmp; tmp = tmp->ai_next) {
	 if (tmp->ai_family == AF_INET6) {
	    addr = (char*)&((struct sockaddr_in6 *)tmp->ai_addr)->sin6_addr;
	       int err = uv_inet_ntop(tmp->ai_family, addr,
				       ip, INET6_ADDRSTRLEN);
	       if (err != 0) {
		  continue;
	       } else {
		  acc = MAKE_PAIR(string_to_bstring(ip), acc);
	       }
	 }
      }
      
      // iterate over the IPv4 addresses 
      for (tmp = res; tmp; tmp = tmp->ai_next) {
	 if (tmp->ai_family == AF_INET) {
	    // ipv4 addtmps
	    addr = (char *)&((struct sockaddr_in *)tmp->ai_addr)->sin_addr;
	    int err = uv_inet_ntop(tmp->ai_family, addr,
				    ip, INET_ADDRSTRLEN);
	    if (err != 0) {
	       continue;
	    } else {
	       acc = MAKE_PAIR(string_to_bstring(ip), acc);
	    }
	 }
      }

      uv_freeaddrinfo(res);
      free(req);

      BGL_PROCEDURE_CALL1(p, acc);
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_getaddrinfo ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_uv_getaddrinfo(char *node, char *service, int family, obj_t proc, bgl_uv_loop_t bloop) {
   if (!(PROCEDUREP(proc) && (PROCEDURE_CORRECT_ARITYP(proc, 1)))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-getaddrinfo",
			"wrong callback", proc);
   } else {
      uv_loop_t *loop = LOOP_BUILTIN(bloop);
      uv_getaddrinfo_t *resolver =
 	 (uv_getaddrinfo_t *)malloc(sizeof(uv_getaddrinfo_t));
      struct addrinfo hints;
      int fam = itof(family);
      int r;

      resolver->data = proc;

      memset(&hints, 0, sizeof(struct addrinfo));
      hints.ai_family = fam;
      hints.ai_socktype = SOCK_STREAM;
   
      if ((r = uv_getaddrinfo(loop, resolver, bgl_uv_getaddrinfo_cb,
			       node, service, &hints)) < 0) {
	 free(resolver);
      } else {
	 gc_mark(proc);
      }

      return r;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_inet_pton ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_inet_pton(char *addr, int family) {
   char buf[MAX_IP_LEN];
   int fam = itof(family);
   int res = uv_inet_pton(fam, addr, &buf);

   if (res == 0) {
      return string_to_bstring(buf);
   } else {
      return BFALSE;
   }
}
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_write_cb ...                                              */
/*---------------------------------------------------------------------*/
static void
bgl_uv_write_cb(uv_write_t *req, int status) {
   uv_write_data_t *data = (uv_write_data_t *)(req->data);
   obj_t p = data->proc;

   switch (PROCEDURE_ARITY(p)) {
      case 1:
      case -2:
	 BGL_PROCEDURE_CALL1(p, BINT(status));
	 break;
      case 2:
      case -3:
	 BGL_PROCEDURE_CALL2(p, BINT(status), data->arg[0]);
	 break;
      case 3:
      case -4:
	 BGL_PROCEDURE_CALL3(p, BINT(status), data->arg[0], data->arg[1]);
	 break;
      case 4:
      case -5:
	 BGL_PROCEDURE_CALL4(p, BINT(status), data->arg[0], data->arg[1], data->arg[2]);
	 break;
      case 5:
      case -6:
	 BGL_PROCEDURE_CALL5(p, BINT(status), data->arg[0], data->arg[1], data->arg[2], data->arg[3]);
	 break;
      case 6:
      case -1:
      case -7:
	 BGL_PROCEDURE_CALL6(p, BINT(status), data->arg[0], data->arg[1], data->arg[2], data->arg[3], data->arg[4]);
	 break;
      default:
	 C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-stream-write", "wrong callback", p);
   }

   free_uv_write(req);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_write ...                                                 */
/*---------------------------------------------------------------------*/
int
bgl_uv_write(obj_t obj, char *buffer, long offset, long length, obj_t proc,
	     obj_t arg0, obj_t arg1, obj_t arg2, obj_t arg3, obj_t arg4) {
   if (!(PROCEDUREP(proc))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-stream-write",
		       "wrong callback", proc);
   } else {
      uv_stream_t *handle = STREAM_BUILTIN(obj);
      uv_write_t *req = alloc_uv_write();
      uv_write_data_t *data = (uv_write_data_t *)(req->data);
      uv_buf_t iov;
      int r;

      data->proc = proc;
      data->arg[0] = arg0;
      data->arg[1] = arg1;
      data->arg[2] = arg2;
      data->arg[3] = arg3;
      data->arg[4] = arg4;
      
      iov = uv_buf_init(buffer + offset, length);

      if (r = uv_write(req, handle, &iov, 1, bgl_uv_write_cb)) {
	 free_uv_write(req);
      }

      return r;
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_write2 ...                                                */
/*---------------------------------------------------------------------*/
int
bgl_uv_write2(obj_t obj, char *buffer, long offset, long length, obj_t sendhandle, obj_t proc,
	      obj_t arg0, obj_t arg1, obj_t arg2, obj_t arg3, obj_t arg4) {
   if (!(PROCEDUREP(proc))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-stream-write",
			"wrong callback", proc);
   } else {
      uv_stream_t *handle = STREAM_BUILTIN(obj);
      uv_stream_t *sendhdl =
	 (sendhandle == BFALSE ? 0L : STREAM_BUILTIN(sendhandle));
      uv_write_t *req = alloc_uv_write();
      uv_write_data_t *data = (uv_write_data_t *)(req->data);
      uv_buf_t iov;
      int r;

      data->proc = proc;
      data->arg[0] = arg0;
      data->arg[1] = arg1;
      data->arg[2] = arg2;
      data->arg[3] = arg3;
      data->arg[4] = arg4;

      iov = uv_buf_init(buffer + offset, length);

      if (r = uv_write2(req, handle, &iov, 1, sendhdl, bgl_uv_write_cb)) {
	 free_uv_write(req);
      }

      return r;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_read_cb ...                                               */
/*---------------------------------------------------------------------*/
static void
bgl_uv_read_cb(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
   obj_t obj = stream->data;
   uv_stream_data_t *data = STREAM_DATA(obj);
   obj_t p = data->proc;
   obj_t allocobj = data->allocobj;
   obj_t offset = data->offset;
   int c = 0;
   obj_t pendingsym = BFALSE;
   uv_stream_state_t prev_state = data->state;

#if defined(DBG)
   assert_stream_data(obj);
#endif

   data->allocobj = BUNSPEC;

   if ((stream->type == UV_NAMED_PIPE)) {
      if (uv_pipe_pending_count((uv_pipe_t*)stream) > 0) {
	uv_handle_type pending = uv_pipe_pending_type((uv_pipe_t*)stream);
	pendingsym = bgl_uv_handle_type_symbol((int)pending);
      }
   }

#if defined(DBG)
   fprintf(stderr, "~~~>>> bgl_uv_read_cb idx=%d stream=%p data=%p proc=%p alloc=%p allocobj=%p nread=%d %s\n", data->index, stream, data, p, data->alloc, allocobj, nread, nread < 0 ? uv_strerror(nread) : "");

   if (nread == -105) {
      fprintf(stderr, "alloc error data->obj=%p obj=%p\n", data->obj, obj);
      ABORT();
   }
#endif

   if (p && prev_state != CLOSING) {
      data->state = READING;
      if (nread >= 0) {
	 BGL_PROCEDURE_CALL5(p, BTRUE, allocobj, offset, BINT(nread), pendingsym);
      } else if (nread == UV_EOF) {
	 BGL_PROCEDURE_CALL5(p, BEOF, allocobj, offset, BINT(-1), pendingsym);
      } else {
	 BGL_PROCEDURE_CALL5(p, BFALSE, allocobj, offset, BINT(nread), pendingsym);
      }

      if (data->state == CLOSING) {
	 TRACECB(tread);
	 free_stream_data(data);
      } else if (data->state == FREE) {
	 TRACECB(tread);
      } else {
	 data->state = prev_state;
      }
   }

#if defined(DBG)
   fprintf(stderr, "~~~<<< bgl_uv_read_cb idx=%d stream=%p data=%p proc=%p alloc=%p allocobj=%p nread=%d %s\n", data->index, stream, data, p, data->alloc, allocobj, nread, nread < 0 ? uv_strerror(nread) : "");
   assert_stream_data(obj);
#endif
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_alloc_cb ...                                              */
/*---------------------------------------------------------------------*/
static void
bgl_uv_alloc_cb(uv_handle_t *hdl, size_t ssize, uv_buf_t *buf) {
   obj_t obj = hdl->data;
   uv_stream_data_t *data = STREAM_DATA(obj);

#if defined(DBG)
   fprintf(stderr, "--->>> bgl_uv_alloc_cb idx=%d data=%p hdl=%p alloc=%p proc=%p %d\n", data->index, data, hdl, data->alloc, data->proc, ssize);
   assert_stream_data(obj);
#endif
   
   obj_t p = data->alloc;

   if (p) {
      obj_t allocobj = BGL_PROCEDURE_CALL2(p, obj, BINT(ssize));
      obj_t chunk = BGL_MVALUES_VAL(1);
      obj_t offset = BGL_MVALUES_VAL(2);

      if (!STRINGP(chunk)) {
	 C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-read-start, onalloc", "string", chunk);
      }

      data->allocobj = allocobj;
      data->offset = offset;

      buf->base = (char *)&(STRING_REF(chunk, CINT(offset)));
      buf->len = ssize;

      if ((STRING_LENGTH(chunk) - CINT(offset)) < ssize) {
	 C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-read-start, onalloc", "buffer too small", BINT(ssize));
      }
	 
#if defined(DBG)
      fprintf(stderr, "---<<< bgl_uv_alloc_cb idx=%d ssize=%d\n", data->index, ssize);
#endif
   } else {
      fprintf(stderr, "*** no allocation rountine index=%ld state=%d\n", data->index, data->state);
      C_SYSTEM_FAILURE(BGL_ERROR, "bgl_uv_alloc_cb",
		       "no allocation routine", BUNSPEC);
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_read_start ...                                            */
/*---------------------------------------------------------------------*/
int
bgl_uv_read_start(obj_t obj, obj_t proca, obj_t procc) {
#if defined(DBG)
   assert_stream_data(obj);
#endif
   if (!PROCEDUREP(proca) || (!PROCEDURE_CORRECT_ARITYP(proca, 2))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-read-start",
			"wrong onalloc", proca);
   } else {
      if (!(PROCEDUREP(procc) && (PROCEDURE_CORRECT_ARITYP(procc, 5)))) {
	 C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-read-start",
			   "wrong callback", procc);
      } else {
	 bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
	 uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
#if defined(DBG_TRACE)
	 int need_trace = !STREAM_DATA(obj);
#endif	 
	 uv_stream_data_t *data = get_stream_data(obj);

#if defined(DBG)
	 fprintf(stderr, ">>> read_start data=%p idx=%d:%d hdl=%p obj=%p old-proc=%p procc=%p alloc=%p\n", data, data->index, data->state, s, obj, data->proc, procc, proca);

	 if (data->state == READING) {
	    return;
	 }
	 
	 if (data->state != LOCKED) {
	    fprintf(stderr, "bgl_uv_read_start, bad data state\n");
	    ABORT();
	 }
#endif

	 data->proc = procc;
	 data->alloc = proca;
	 data->offset = BINT(-1);
	 data->obj = obj;
#if defined(DBG)
	 assert_stream_data(obj);
#endif	 
#if defined(DBG_TRACE)
	 if (need_trace) TRACECA(tread);
#endif	 
	 return uv_read_start(s, bgl_uv_alloc_cb, bgl_uv_read_cb);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_read_stop ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_read_stop(obj_t obj) {
   bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
   uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
   uv_stream_data_t *data = get_stream_data(obj);

   free_stream_data(data);
#if defined(DBG)
   fprintf(stderr, ">>> read_stop idx=%d proc=%p\n", data->index, data->proc);
#endif   
   TRACECB(tread);
   return uv_read_stop(s);
}

/*---------------------------------------------------------------------*/
/*    uv_tcp_t *                                                       */
/*    bgl_uv_tcp_create ...                                            */
/*---------------------------------------------------------------------*/
uv_tcp_t *
bgl_uv_tcp_create(uv_loop_t *loop, obj_t obj) {
   uv_tcp_t *tcp = (uv_tcp_t *)GC_MALLOC(sizeof(uv_tcp_t));

   uv_tcp_init(loop, tcp);

   tcp->data = obj;
   STREAM_DATA(obj) = 0L;
   
   return tcp;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bcl_connect_cb ...                                               */
/*---------------------------------------------------------------------*/
static void
bgl_connect_cb(uv_connect_t *req, int status) {
   obj_t p = (obj_t)req->data;
   obj_t handle = req->handle->data;

   gc_unmark(p);

   free(req);

   TRACECB(tconn);
   BGL_PROCEDURE_CALL2(p, BINT(status), handle);
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_uv_tcp_connectX ...                                          */
/*---------------------------------------------------------------------*/
static int
bgl_uv_tcp_connectX(obj_t obj, struct sockaddr *address, obj_t proc, bgl_uv_loop_t bloop) {
   if (!(PROCEDUREP(proc) && (PROCEDURE_CORRECT_ARITYP(proc, 2)))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-tcp-connect",
			"wrong callback", proc);
   } else {
      uv_connect_t *req = malloc(sizeof(uv_connect_t));
      uv_tcp_t *handle =
	 (uv_tcp_t *)(((bgl_uv_handle_t)COBJECT(obj))->BgL_z42builtinz42);
      int r;

      req->data = proc;
      
      gc_mark(proc);

      TRACECA(tconn);
      r = uv_tcp_connect(req, handle, address, bgl_connect_cb);
      if (r != 0) {
	 free(req);
      }

      return r;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_tcp_connect ...                                           */
/*---------------------------------------------------------------------*/
int
bgl_uv_tcp_connect(obj_t obj, char *addr, int port, int family, obj_t proc, bgl_uv_loop_t bloop) {
   union addr {
      struct sockaddr_in ip4;
      struct sockaddr_in6 ip6;
   } address;
   int r;
      
   if (family == 4) {
      r = uv_ip4_addr(addr, port, &(address.ip4));
   } else {
      r = uv_ip6_addr(addr, port, &(address.ip6));
   }

   if (r) {
      return r;
   }

   return bgl_uv_tcp_connectX(obj, (struct sockaddr *)&address, proc, bloop);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_listen_cb ...                                                 */
/*---------------------------------------------------------------------*/
static void
uv_listen_cb(uv_stream_t *handle, int status) {
   obj_t obj = handle->data;
   uv_stream_data_t *data = STREAM_DATA(obj);

   obj_t p = data->listen;

   if (p) {
      BGL_PROCEDURE_CALL2(p, obj, BINT(status));
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_listen ...                                                */
/*---------------------------------------------------------------------*/
int
bgl_uv_listen(obj_t obj, int backlog, obj_t proc, bgl_uv_loop_t bloop) {
   if (!(PROCEDUREP(proc) && (PROCEDURE_CORRECT_ARITYP(proc, 2)))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-tcp-listen",
			"wrong callback", proc);
   } else {
      bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
      uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
      uv_stream_data_t *data = get_stream_data(obj);
      int r;

      data->listen = proc;

      r = uv_listen(s, backlog, uv_listen_cb);

      if (r < 0) {
	 fprintf(stderr, "LISTEN ERROR...\n");
	 TRACECNT(klisten);
	 free_stream_data(data);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_tcp_bind ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_tcp_bind(uv_tcp_t *handle, char *addr, int port, int family) {
   union addr {
      struct sockaddr_in ip4;
      struct sockaddr_in6 ip6;
   } address;
   int r;
      
   if (family == 4) {
      r = uv_ip4_addr(addr, port, &(address.ip4));
   } else {
      r = uv_ip6_addr(addr, port, &(address.ip6));
   }

   if (r) {
      return r;
   }

   return uv_tcp_bind(handle, (struct sockaddr *)&address, 0);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_tcp_bind6 ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_tcp_bind6(uv_tcp_t *handle, char *addr, int port) {
   struct sockaddr_in6 address;
   int r = uv_ip6_addr(addr, port, &address);

   if (r) {
      return r;
   }

   return uv_tcp_bind(handle, (struct sockaddr *)&address, UV_TCP_IPV6ONLY);
}

/*---------------------------------------------------------------------*/
/*    address symbols                                                  */
/*---------------------------------------------------------------------*/
static obj_t _address = BUNSPEC, _family, _port, _IPv4, _IPv6;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_init_address ...                                              */
/*---------------------------------------------------------------------*/
static void
uv_init_address() {
   if (_address == BUNSPEC) {
      _address = string_to_symbol("address");
      _family = string_to_symbol("family");
      _port = string_to_symbol("port");
      _IPv4 = string_to_symbol("IPv4");
      _IPv6 = string_to_symbol("IPv6");
   }
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_address ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
bgl_address(struct sockaddr *addr) {
   obj_t res = BNIL;
   char ip[MAX_IP_LEN];

   uv_init_address();
   
   switch(addr->sa_family) {
      case AF_INET6: {
	 struct sockaddr_in6 *a6 = (struct sockaddr_in6 *)addr;
	 uv_inet_ntop(AF_INET6, &a6->sin6_addr, ip, sizeof ip);
	 
	 res = MAKE_PAIR(MAKE_PAIR(_port, BINT(ntohs(a6->sin6_port))),
			  res);
	 res = MAKE_PAIR(MAKE_PAIR(_family, _IPv6), res);
	 res = MAKE_PAIR(MAKE_PAIR(_address, string_to_bstring(ip)), res);
	 break;
      }

      case AF_INET: {
	 struct sockaddr_in *a4 = (struct sockaddr_in *)addr;
	 uv_inet_ntop(AF_INET, &a4->sin_addr, ip, sizeof ip);
	 
	 res = MAKE_PAIR(MAKE_PAIR(_port, BINT(ntohs(a4->sin_port))), res);
	 res = MAKE_PAIR(MAKE_PAIR(_family, _IPv4), res);
	 res = MAKE_PAIR(MAKE_PAIR(_address, string_to_bstring(ip)), res);

	 break;
      }
   }
	 
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_tcp_getsockname ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_tcp_getsockname(uv_tcp_t *handle) {
   struct sockaddr_storage address;
   int addrlen = sizeof(address);
   int r = uv_tcp_getsockname(handle, (struct sockaddr *)&address, &addrlen);

   if (r) { 
      return BINT(r);
   } else {
      return bgl_address((struct sockaddr *)&address);
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_tcp_getpeername ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_tcp_getpeername(uv_tcp_t *handle) {
   struct sockaddr_storage address;
   int addrlen = sizeof(address);
   int r = uv_tcp_getpeername(handle, (struct sockaddr *)&address, &addrlen);

   if (r) { 
      return BINT(r);
   } else {
      return bgl_address((struct sockaddr *)&address);
   }
}

/*---------------------------------------------------------------------*/
/*    uv_tty_t *                                                       */
/*    bgl_uv_tty_create ...                                            */
/*---------------------------------------------------------------------*/
uv_tty_t *
bgl_uv_tty_create(uv_loop_t *loop, obj_t obj, int fd, bool_t readable) {
   uv_tty_t *tty = (uv_tty_t *)GC_MALLOC(sizeof(uv_tty_t));

   uv_tty_init(loop, tty, fd, readable);

   tty->data = obj;
   STREAM_DATA(obj) = 0L;
   
   return tty;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_tty_get_winsize ...                                       */
/*---------------------------------------------------------------------*/
obj_t bgl_uv_tty_get_winsize(uv_tty_t *handle) {
   int width, height;
   obj_t vec = create_vector(2);

   uv_tty_get_winsize(handle, &width, &height);
   VECTOR_SET(vec, 0, BINT(width));
   VECTOR_SET(vec, 1, BINT(height));
   return vec;
}
    

/*---------------------------------------------------------------------*/
/*    uv_udp_t *                                                       */
/*    bgl_uv_udp_create ...                                            */
/*---------------------------------------------------------------------*/
uv_udp_t *
bgl_uv_udp_create(uv_loop_t *loop, obj_t obj) {
   uv_udp_t *udp = (uv_udp_t *)GC_MALLOC(sizeof(uv_udp_t));

   uv_udp_init(loop, udp);

   udp->data = obj;
   STREAM_DATA(obj) = 0L;
   
   return udp;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_udp_bind ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_udp_bind(uv_udp_t *handle, char *addr, int port, int family, int flags) {
   union addr {
      struct sockaddr_in ip4;
      struct sockaddr_in6 ip6;
   } address;
   int r;

   if (family == 4) {
      r = uv_ip4_addr(addr, port, &(address.ip4));
      if (r) return r;
   } else {
      r = uv_ip6_addr(addr, port, &(address.ip6));
      if (r) return r;
   }

   // r = uv_udp_bind(handle, (struct sockaddr *)&address, UV_UDP_REUSEADDR);
   r = uv_udp_bind(handle, (struct sockaddr *)&address, UV_UDP_REUSEADDR | flags);
   
   return r;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_udp_send_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_udp_send_cb(uv_udp_send_t *req, int status) {
   obj_t proc = (obj_t)req->data;
   gc_unmark(proc);
   
   BGL_PROCEDURE_CALL1(proc, BINT(status));

   free(req);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_udp_send ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_udp_send(uv_udp_t *handle, obj_t buffer, long offset, long length,
		 long port, char *addr,
		 int family, obj_t proc, bgl_uv_loop_t bloop) {
   union addr {
      struct sockaddr_in ip4;
      struct sockaddr_in6 ip6;
   } address;
   int r;
   uv_udp_send_t *req = malloc(sizeof(uv_udp_send_t));
   uv_buf_t iov;

   req->data = proc;
   iov = uv_buf_init((char *)&(STRING_REF(buffer, offset)), length);

   if (family == 4) {
      uv_ip4_addr(addr, port, &(address.ip4));
   } else {
      uv_ip6_addr(addr, port, &(address.ip6));
   }

   gc_mark(req->data);

   r = uv_udp_send(req, handle, &iov, 1, (struct sockaddr *)&address, &bgl_uv_udp_send_cb);

   if (r) free(req);

   return r;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_udp_recv_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_udp_recv_cb(uv_udp_t *handle, ssize_t nread, const uv_buf_t *buf,
		    const struct sockaddr* addr,
		    unsigned flags) {
   obj_t obj = handle->data;
   uv_stream_data_t *data = STREAM_DATA(obj);
   obj_t p = data->proc;
   obj_t allocobj = data->allocobj;
   obj_t offset = data->offset;

   data->allocobj = BUNSPEC;

   if (PROCEDUREP(p)) {
      if (nread > 0) {
	 BGL_PROCEDURE_CALL5(p, BTRUE, allocobj, offset, BINT(nread), bgl_address((struct sockaddr *)addr));
      } else if (nread == UV_EOF) {
	 BGL_PROCEDURE_CALL5(p, BEOF, allocobj, BINT(-1), BINT(-1), BNIL);
      } else {
	 BGL_PROCEDURE_CALL5(p, BFALSE, allocobj, BINT(-1), BINT(nread), BNIL);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_udp_recv_start ...                                        */
/*---------------------------------------------------------------------*/
int
bgl_uv_udp_recv_start(obj_t obj, obj_t proca, obj_t procc) {
   if (!PROCEDUREP(proca) || (!PROCEDURE_CORRECT_ARITYP(proca, 2))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-udp_recv-start",
			"wrong onalloc", proca);
   } else {
      if (!(PROCEDUREP(procc) && (PROCEDURE_CORRECT_ARITYP(procc, 5)))) {
	 C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-udp_recv-start",
			   "wrong callback", procc);
      } else {
	 bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
	 uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
	 uv_stream_data_t *data = get_stream_data(obj);
	 
	 data->obj = obj;
	 data->proc = procc;
	 data->alloc = proca;

	 TRACECA(trecv);
	 return uv_udp_recv_start((uv_udp_t *)s, bgl_uv_alloc_cb, bgl_uv_udp_recv_cb);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_udp_recv_stop ...                                         */
/*---------------------------------------------------------------------*/
int
bgl_uv_udp_recv_stop(obj_t obj) {
   bgl_uv_stream_t stream = (bgl_uv_stream_t)COBJECT(obj);
   uv_stream_t *s = (uv_stream_t *)(stream->BgL_z42builtinz42);
   uv_stream_data_t *data = get_stream_data(obj);

   TRACECB(trecv);
   free_stream_data(data);
   
   return uv_udp_recv_stop((uv_udp_t *)s);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_udp_getsockname ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_udp_getsockname(uv_udp_t *handle) {
   struct sockaddr_storage address;
   int addrlen = sizeof(address);
   int r = uv_udp_getsockname(handle, (struct sockaddr *)&address, &addrlen);

   if (r) { 
      return BINT(r);
   } else {
      return bgl_address((struct sockaddr *)&address);
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_shutdown_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_shutdown_cb(uv_shutdown_t* req, int status) {
   uv_stream_t *s = req->handle;
   uv_shutdown_data_t *data = (uv_shutdown_data_t *)(req->data);
   obj_t proc = data->proc;
   obj_t obj = data->obj;

   TRACECNT(kshutdown);
   BGL_PROCEDURE_CALL2(proc, BINT(status), obj);
   
   free_uv_shutdown(req);
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_shutdown ...                                              */
/*---------------------------------------------------------------------*/
int
bgl_uv_shutdown(obj_t obj, obj_t proc) {
   if (!(PROCEDUREP(proc) && (PROCEDURE_CORRECT_ARITYP(proc, 2)))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-shutdown",
			"wrong callback", proc);
   } else {
      uv_stream_t *s = STREAM_BUILTIN(obj);
      uv_shutdown_t *req = alloc_uv_shutdown();
      uv_shutdown_data_t *data = (uv_shutdown_data_t *)(req->data);

      data->proc = proc;
      data->obj = obj;

#if defined(DBG)
      fprintf(stderr, "!!! shutdown\n");
#endif      
      if (uv_shutdown(req, s, bgl_uv_shutdown_cb)) {
	 free_uv_shutdown(req);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    string_array_to_vector ...                                       */
/*---------------------------------------------------------------------*/
static obj_t
string_array_to_vector(char *array[]) {
   long len, i;
   char **runner;
   obj_t res;

   for (len = 0, runner = array; *runner; len++, runner++);

   res = create_vector(len);

   for (i = 0, runner = array; i < len; i++, runner++) {
      VECTOR_SET(res, i, string_to_bstring(*runner));
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    static char **                                                   */
/*    vector_to_string_array ...                                       */
/*---------------------------------------------------------------------*/
static char **
vector_to_string_array(obj_t vec) {
   char **res = (char **)GC_MALLOC(sizeof(char *) * VECTOR_LENGTH(vec) + 1);
   long i;

   res[VECTOR_LENGTH(vec)] = 0;
   
   for (i = VECTOR_LENGTH(vec) - 1; i >= 0; i--) {
      res[i] = BSTRING_TO_STRING(VECTOR_REF(vec, i));
   }

   return res;
}
   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_process_options_args_get ...                              */
/*---------------------------------------------------------------------*/
obj_t bgl_uv_process_options_args_get(uv_process_options_t *opt) {
   return string_array_to_vector(opt->args);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_process_options_args_set ...                              */
/*---------------------------------------------------------------------*/
void bgl_uv_process_options_args_set(uv_process_options_t *opt, obj_t vec) {
   opt->args = vector_to_string_array(vec);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_process_options_env_get ...                               */
/*---------------------------------------------------------------------*/
obj_t bgl_uv_process_options_env_get(uv_process_options_t *opt) {
   return string_array_to_vector(opt->env);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_process_options_env_set ...                               */
/*---------------------------------------------------------------------*/
void bgl_uv_process_options_env_set(uv_process_options_t *opt, obj_t vec) {
   opt->env = vector_to_string_array(vec);
}

/*---------------------------------------------------------------------*/
/*    uv_process_t *                                                   */
/*    bgl_uv_process_new ...                                           */
/*---------------------------------------------------------------------*/
uv_process_t *
bgl_uv_process_new(bgl_uv_process_t o) {
   uv_process_t *new = (uv_process_t *)GC_MALLOC(sizeof(uv_process_t));
   new->data = o;
   
   return new;
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    process_exit_cb ...                                              */
/*---------------------------------------------------------------------*/
static void process_exit_cb(uv_process_t *handle, int64_t status, int term) {
   bgl_uv_process_t o = handle->data;
   obj_t p = ((bgl_uv_process_t)COBJECT(o))->BgL_z42onexitz42;

   if (PROCEDUREP(p)) {
      BGL_PROCEDURE_CALL3(p, (obj_t)o, BGL_INT64_TO_BINT64(status), BINT(term));
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_spawn ...                                                 */
/*---------------------------------------------------------------------*/
int bgl_uv_spawn(bgl_uv_loop_t loop,
		  bgl_uv_process_t process,
		  bgl_uv_process_options_t opts,
		  obj_t callback) {
   uv_process_options_t *options =
      ((bgl_uv_process_options_t)COBJECT(opts))->BgL_z42builtinz42;

   if (PROCEDUREP(callback)) {
      bgl_check_fs_cb(callback, 3, "uv_spawn");
      options->exit_cb = &process_exit_cb;
      ((bgl_uv_process_t)COBJECT(process))->BgL_z42onexitz42 = callback;
   }

   ((bgl_uv_process_t)COBJECT(process))->BgL_z42builtinz42->data = process;

   return uv_spawn(LOOP_BUILTIN(loop),
		    (uv_process_t *)(((bgl_uv_process_t)(COBJECT(process)))->BgL_z42builtinz42),
		    options);
}

/*---------------------------------------------------------------------*/
/*    uv_pipe_t *                                                      */
/*    bgl_uv_pipe_create ...                                           */
/*---------------------------------------------------------------------*/
uv_pipe_t *
bgl_uv_pipe_create(uv_loop_t *loop, obj_t obj, bool_t ipc) {
   uv_pipe_t *pipe = (uv_pipe_t *)GC_MALLOC(sizeof(uv_pipe_t));

   uv_pipe_init(loop, pipe, ipc);

   pipe->data = obj;
   STREAM_DATA(obj) = 0L;
   
   return pipe;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_pipe_connect ...                                          */
/*---------------------------------------------------------------------*/
void
bgl_uv_pipe_connect(obj_t obj, char *name, obj_t proc, bgl_uv_loop_t bloop) {
   if (!(PROCEDUREP(proc) && (PROCEDURE_CORRECT_ARITYP(proc, 2)))) {
      C_SYSTEM_FAILURE(BGL_TYPE_ERROR, "uv-pipe-connect",
			"wrong callback", proc);
   } else {
      uv_connect_t *req = malloc(sizeof(uv_connect_t));
      uv_pipe_t *handle =
	 (uv_pipe_t *)(((bgl_uv_handle_t)COBJECT(obj))->BgL_z42builtinz42);
      int r;

      req->data = proc;
      
      gc_mark(proc);

      uv_pipe_connect(req, handle, name, bgl_connect_cb);
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_work_queue_cb ...                                            */
/*---------------------------------------------------------------------*/
static void
bgl_work_queue_cb(uv_work_t *req) {
   bgl_uv_work_t w = (bgl_uv_work_t)req->data;
   obj_t p = ((bgl_uv_work_t)COBJECT(w))->BgL_z52workzd2cbz80;

   BGL_PROCEDURE_CALL0(p);
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_after_queue_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_after_queue_cb(uv_work_t *req, int status) {
   bgl_uv_work_t w = (bgl_uv_work_t)req->data;
   obj_t p = ((bgl_uv_work_t)COBJECT(w))->BgL_z52afterzd2cbz80;

   BGL_PROCEDURE_CALL1(p, BINT(status));
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_queue_work ...                                            */
/*---------------------------------------------------------------------*/
void
bgl_uv_queue_work(bgl_uv_work_t w, bgl_uv_loop_t bloop) {
   fprintf(stderr, "(%s:%d) BROKEN as libuv uses its own threads\n",
	    __FILE__, __LINE__);
   ((bgl_uv_work_t)COBJECT(w))->BgL_z42builtinz42 = (uv_work_t *)GC_MALLOC(sizeof(uv_work_t));
   ((bgl_uv_work_t)COBJECT(w))->BgL_z42builtinz42->data = w;

   uv_queue_work(LOOP_BUILTIN(bloop),
		  ((bgl_uv_work_t)COBJECT(w))->BgL_z42builtinz42,
		  bgl_work_queue_cb,
		  bgl_after_queue_cb);
}
