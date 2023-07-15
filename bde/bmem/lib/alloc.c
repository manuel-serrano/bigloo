/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/alloc.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:42:57 2003                          */
/*    Last change :  Fri Jun 16 15:14:07 2023 (serrano)                */
/*    Copyright   :  2003-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Allocation replacement routines                                  */
/*=====================================================================*/
#define BGL_GC BGL_NO_GC
#include <bigloo.h>
#include <bmem.h>
#include <stdlib.h>
#include <string.h>

/*---------------------------------------------------------------------*/
/*    Global constants ...                                             */
/*---------------------------------------------------------------------*/
#define DUMP_LINE_SIZE_THRESHOLD 1024 * 32
#define DUMP_LINE_COUNT_THRESHOLD 1024
#define SEARCH_TYPE_STACK_DEPTH 5

/*---------------------------------------------------------------------*/
/*    static hashtable_t *                                             */
/*    native_allocators ...                                            */
/*---------------------------------------------------------------------*/
static hashtable_t *native_allocators = 0L;
static hashtable_t *file_allocs = 0L;

static long long alloc_size = 0;

extern void gc_alloc_size_add(int size);
extern long gc_alloc_size();
extern unsigned long gc_number;

static long alloc_typenum = -1;

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bmem_get_alloc_type ...                                          */
/*---------------------------------------------------------------------*/
long
bmem_get_alloc_type() {
   if (bmem_thread) {
      return (long)____pthread_getspecific(bmem_key);
   } else {
      return alloc_typenum;
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_set_alloc_type ...                                          */
/*---------------------------------------------------------------------*/
void
bmem_set_alloc_type(long tnum) {
   if (bmem_thread) {
      ____pthread_setspecific(bmem_key, (void *)tnum);
   } else {
      alloc_typenum = tnum;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_init ...                                                   */
/*---------------------------------------------------------------------*/
void
alloc_init(char **allocs) {

   if (!native_allocators) {
      native_allocators = hashtable_create(128);
      file_allocs = hashtable_create(128);

      while (*allocs) {
	 hashtable_put(native_allocators, *allocs, (void *)1);
	 allocs++;
      }

      bmem_set_alloc_type(NO_TYPE_NUM);
   }
}

/*---------------------------------------------------------------------*/
/*    static pa_pair_t *                                               */
/*    all_functions ...                                                */
/*---------------------------------------------------------------------*/
static pa_pair_t *all_functions = 0;
static int stamp = 1;
static long alloc_types[] = {-1, -1, -1, -1, -1, -1};
static long alloc_type_offsets[] = {0, 0, 0, 0, 0};
static long alloc_index = -1;
static long max_stack_size = 100000;
unsigned long ante_bgl_init_dsz = 0;

/*---------------------------------------------------------------------*/
/*    struct type                                                      */
/*    all_types ...                                                    */
/*---------------------------------------------------------------------*/
struct type {
   char *name;
   long size;
   long cnt;
};
   
static struct type *all_types = 0;
static int types_number = 0;

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    alloc_is_native ...                                              */
/*---------------------------------------------------------------------*/
int
alloc_is_native(const char *fun) {
   return fun && hashtable_get(native_allocators, fun) != 0L;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    declare_type ...                                                 */
/*---------------------------------------------------------------------*/
void
declare_type(int tnum, char *tname, char *tmodule) {
   if (bmem_verbose >= 2) {
      if (tmodule) {
	 fprintf(stderr, "  %s@%s (%d)...\n", tname, tmodule, tnum);
      } else {
	 fprintf(stderr, "  %s (%d)...\n", tname, tnum);
      }
      fflush(stderr);
   }
   
   if (tnum >= types_number) {
      all_types = (struct type *)realloc(all_types, (tnum + 1) * sizeof(struct type));
      memset(&all_types[types_number], 0, (tnum-types_number) * sizeof(struct type));
      types_number = tnum + 1;
   }

   all_types[tnum].name = tname;
   all_types[tnum].cnt = 0;
   all_types[tnum].size = 0;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    line_alloc_add ...                                               */
/*---------------------------------------------------------------------*/
void
line_alloc_add(file_alloc_t *file, long lineno, long sz, long typenum) {
   int i;

   if (lineno >= file->size) {
      if (file->lines) {
	 file->lines = realloc(file->lines, sizeof(line_alloc_t) * (lineno + 1));
	 memset(&(file->lines[file->size]), 0, sizeof(line_alloc_t) * (lineno + 1 - file->size));
      } else {
	 file->lines = calloc(sizeof(line_alloc_t), lineno + 1);
      }
      
      file->size = lineno + 1;
   }

   file->lines[lineno].size += sz;
   file->lines[lineno].count++;
   file->lines[lineno].lineno = lineno;

   // add typenum to the line types
   for (i = file->lines[lineno].typecount - 1; i >= 0; i--) {
      if (file->lines[lineno].typenums[i] == typenum) break;
   }
   if (i < 0) {
      if (file->lines[lineno].typenums) {
	 file->lines[lineno].typenums =
	    realloc(file->lines[lineno].typenums,
		    sizeof(long) * file->lines[lineno].typecount + 1);
      } else {
	 file->lines[lineno].typenums = malloc(sizeof(long) * 1);
      }
      i = file->lines[lineno].typecount++;
   }

   file->lines[lineno].typenums[i] = typenum;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    file_alloc_add ...                                               */
/*---------------------------------------------------------------------*/
void
file_alloc_add(char *filename, long lineno, long sz, long typenum) {
   file_alloc_t *old = hashtable_get(file_allocs, filename);

   if (bmem_thread) pthread_mutex_lock(&bmem_mutex);
   if (!old) {
      old = malloc(sizeof(file_alloc_t));
      old->filename = filename;
      old->size = 0;
      old->lines = 0L;
      hashtable_put(file_allocs, filename, old);
   }

   line_alloc_add(old, lineno, sz, typenum > 0 ? typenum : UNKNOWN_TYPE_NUM);
   if (bmem_thread) pthread_mutex_unlock(&bmem_mutex);
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    typecmp ...                                                      */
/*---------------------------------------------------------------------*/
static int
typecmp(const void *p1, const void *p2) {
   struct type *t1 = (struct type *)p1;
   struct type *t2 = (struct type *)p2;
   return t2->size >= t1->size; 
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    linesizecmp ...                                                  */
/*---------------------------------------------------------------------*/
static int
linesizecmp(const void *p1, const void *p2) {
   line_alloc_t *l1 = (line_alloc_t *)p1;
   line_alloc_t *l2 = (line_alloc_t *)p2;
   return l2->size >= l1->size; 
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    linecountcmp ...                                                 */
/*---------------------------------------------------------------------*/
static int
linecountcmp(const void *p1, const void *p2) {
   line_alloc_t *l1 = (line_alloc_t *)p1;
   line_alloc_t *l2 = (line_alloc_t *)p2;
   return l2->count >= l1->count; 
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    file_dump_typenums ...                                           */
/*---------------------------------------------------------------------*/
static void
file_dump_typenums(file_alloc_t *file, long i) {
   long j;
   
   if (file->lines[i].typecount >= 1) {
      if (file->lines[i].typenums[0] >= 0) {
	 fprintf(stderr, "%s", all_types[file->lines[i].typenums[0]].name);
      }
   }
   for (j = file->lines[i].typecount - 1; j >= 1; j--) {
      if (file->lines[i].typenums[j] >= 0) {
	 fprintf(stderr, ", %s", all_types[file->lines[i].typenums[j]].name);
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    file_dump_alloc_size ...                                         */
/*---------------------------------------------------------------------*/
void
file_dump_alloc_size(const char *filename, void *data) {
   file_alloc_t *file = (file_alloc_t *)data;
   int show = 0;
   long i;

   for (i = 0; i < file->size && !show; i++) {
      if (file->lines[i].size > DUMP_LINE_SIZE_THRESHOLD) show = 1;
   }

   if (show) {
      if (bmem_color) {
	 fprintf(stderr, "[0m[1;31m%s:[0m\n", file->filename);
      } else {
	 fprintf(stderr, "%s:\n", file->filename);
      }
      qsort(file->lines, file->size, sizeof(line_alloc_t), linesizecmp);

      for (i = 0; i < file->size; i++) {
	 if (file->lines[i].size > DUMP_LINE_SIZE_THRESHOLD) {
	    fprintf(stderr, "   %6ld: %8.2fMB %5.2f%% [%8ld] (", file->lines[i].lineno,
		    (double)(file->lines[i].size) / (1024. * 1024.),
		    ((double)(file->lines[i].size) * 100) / alloc_size,
		    file->lines[i].count);
	    file_dump_typenums(file, i);
	    fprintf(stderr, ")\n");
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    file_dump_alloc_count ...                                        */
/*---------------------------------------------------------------------*/
void
file_dump_alloc_count(const char *filename, void *data) {
   file_alloc_t *file = (file_alloc_t *)data;
   int show = 0;
   long i;

   for (i = 0; i < file->size && !show; i++) {
      if (file->lines[i].count > DUMP_LINE_COUNT_THRESHOLD) show = 1;
   }

   if (show) {
      fprintf(stderr, "%s:\n", file->filename);
      qsort(file->lines, file->size, sizeof(line_alloc_t), linecountcmp);

      for (i = 0; i < file->size; i++) {
	 if (file->lines[i].size > DUMP_LINE_COUNT_THRESHOLD) {
	    fprintf(stderr, "   %6ld: %8ld (", file->lines[i].lineno,
		    file->lines[i].count);
	    file_dump_typenums(file, i);
	    fprintf(stderr, ")\n");
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    dump_types_cnt ...                                               */
/*---------------------------------------------------------------------*/
void
dump_types_cnt() {
   long sum;
   long i;

   qsort(all_types, types_number, sizeof(struct type), typecmp);
   
   for (i = 0; i < types_number; i++) {
      sum += all_types[i].cnt;
   }

   fprintf(stderr, "\n---------------------------------------------------\n");
   if (bmem_color) {
      fprintf(stderr, "[0m[1;32mallocation count:[0m %ld\n", sum);
   } else {
      fprintf(stderr, "types: %ld\n", sum);
   }

   for (i = 0; i < types_number; i++) {
      if ((all_types[i].cnt * 100 / sum) >= 1 ||
	  ((double)(all_types[i].size)) / (1024. * 1024.) >= 1) {
	 fprintf(stderr, "   %-20s: %8.2fMB %5.2f%% [%8ld]\n",
		 all_types[i].name,
		 ((double)(all_types[i].size)) / (1024. * 1024.),
		 ((double)(all_types[i].size * 100)) / alloc_size,
		 all_types[i].cnt);
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_dump_statistics ...                                        */
/*---------------------------------------------------------------------*/
void
alloc_dump_statistics() {
   fprintf(stderr, "\n\n===================================================\n");
   if (bmem_color) {
      fprintf(stderr, "[0m[1;32mallocation size:[0m %.2fMB\n", (double)alloc_size / (1024. * 1024.));
   } else {
      fprintf(stderr, "allocation size: %.2fMB\n", (double)alloc_size / (1024. * 1024.));
   }
   fprintf(stderr, "gc count: %lu\n\n", gc_number);
   hashtable_foreach(file_allocs, file_dump_alloc_size);

   dump_types_cnt();
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_dump_type ...                                              */
/*---------------------------------------------------------------------*/
void
alloc_dump_type(pa_pair_t *i, FILE *f) {
   type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR(i);
   
   fprintf(f, "\n          (%ld #l%ld #l%ld)", (long)PA_CAR(i),
	    tai->num, BMEMSIZE(tai->size));
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_unknown ...                                            */
/*---------------------------------------------------------------------*/
obj_t
#if __GNUC__
__attribute__ ((noinline))
#endif
GC_malloc_unknown(obj_t o, size_t lb) {
   // only for gdb bmem debugging
   return o;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_untracked ...                                          */
/*---------------------------------------------------------------------*/
obj_t
#if __GNUC__
__attribute__ ((noinline))
#endif
GC_malloc_untracked(obj_t o, size_t lb) {
   // only for gdb bmem debugging
   return o;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    trace_alloc ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
trace_alloc(obj_t o, size_t lb) {
   alloc_info_t info = { /* typenum: */ bmem_get_alloc_type(),
      /* size: */ lb,
      /* function: */ 0L,
      /* filename: */ 0L,
      /* lineno: */ -1,
      /* depth: */ SEARCH_TYPE_STACK_DEPTH
   };
   
   gc_alloc_size_add(lb);
   alloc_size += lb;

#if BGL_HAVE_BACKTRACE
   if (bmem_backtrace == 0 || bmem_backtrace == info.typenum) {
      backtrace_for_each(backtrace_alloc_cb, 1, &info);
   }
#endif      

   if (info.typenum == IGNORE_TYPE_NUM) {
      return o;
   }

   if (info.typenum == -1) {
      info.typenum = UNKNOWN_TYPE_NUM;
   }
   
   all_types[info.typenum].cnt++;
   all_types[info.typenum].size += lb;
   
   if (info.filename && info.lineno >= 0) {
      file_alloc_add(info.filename, info.lineno, lb, info.typenum);

      if (info.typenum == NO_TYPE_NUM ||
	  info.typenum == UNKNOWN_TYPE_NUM ||
	  info.typenum == UNKNOWN_ATOMIC_TYPE_NUM ||
	  info.typenum == UNKNOWN_REALLOC_TYPE_NUM) {
	 return GC_malloc_unknown(o, lb);
      } else {
	 return o;
      }
   } else {
      file_alloc_add("__untracked", 1, lb, info.typenum);
      
      if (info.typenum == NO_TYPE_NUM ||
	  info.typenum == UNKNOWN_TYPE_NUM ||
	  info.typenum == UNKNOWN_ATOMIC_TYPE_NUM ||
	  info.typenum == UNKNOWN_REALLOC_TYPE_NUM) {
	 return GC_malloc_untracked(GC_malloc_unknown(o, lb), lb);
      } else {
	 return GC_malloc_untracked(o, lb);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc(size_t lb) {
   return trace_alloc(____GC_malloc(lb), lb);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_realloc ...                                                   */
/*---------------------------------------------------------------------*/
obj_t
GC_realloc(obj_t old, size_t lb) {
   return trace_alloc(____GC_realloc(old,lb), lb);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_atomic ...                                             */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc_atomic(size_t lb) {
   return trace_alloc(____GC_malloc_atomic(lb), lb);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_uncollectable ...                                      */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc_uncollectable(size_t lb) {
   return trace_alloc(____GC_malloc_uncollectable(lb), lb);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_local_malloc ...                                              */
/*    -------------------------------------------------------------    */
/*    We have to disable the GC_local_malloc function otherwise        */
/*    we get confused in function such as make_pair in                 */
/*    multithreaded environment.                                       */
/*---------------------------------------------------------------------*/
obj_t
GC_local_malloc(size_t lb) {
   return GC_malloc(lb);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_local_malloc_atomic ...                                       */
/*---------------------------------------------------------------------*/
obj_t
GC_local_malloc_atomic(size_t lb) {
   return GC_malloc_atomic(lb);
}
