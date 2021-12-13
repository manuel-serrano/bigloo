/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem-ng/lib/hash.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  7 11:34:41 2021                          */
/*    Last change :  Tue Oct 19 16:12:02 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    hashtables indexed by strings.                                   */
/*=====================================================================*/
#include <bigloo.h>
#include <bmem.h>

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    hashnumber ...                                                   */
/*---------------------------------------------------------------------*/
static long
hashnumber(const char *string) {
   long result = 5381;
   char *c = (char *)string;

   while (*c++) {
      result += (result << 5) + (long)(*c);
   }

   return result & ((1 << 29) - 1);
}

/*---------------------------------------------------------------------*/
/*    hashtable_t *                                                    */
/*    hashtable_create ...                                             */
/*---------------------------------------------------------------------*/
hashtable_t *
hashtable_create(long size) {
   hashtable_t *table = malloc(sizeof(hashtable_t));
   hashbucketentry_t *buckets = calloc(sizeof(hashbucketentry_t), size);

   table->size = size;
   table->buckets = buckets;

   return table;
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    hashtable_rehash ...                                             */
/*---------------------------------------------------------------------*/
static void
hashtable_rehash(hashtable_t *table) {
   long osize = table->size;
   hashbucketentry_t *obuckets = table->buckets;
   long nsize = 1 + (osize * 2);
   hashbucketentry_t *nbuckets = calloc(nsize, sizeof(hashbucketentry_t));
   long i;

   table->size = nsize;
   table->buckets = nbuckets;

   for (i = 0; i < osize; i++) {
      if (obuckets[i].key) {
	 hashtable_put(table, obuckets[i].key, obuckets[i].data);
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    hashtable_get ...                                                */
/*---------------------------------------------------------------------*/
void *
hashtable_get(hashtable_t *table, const char *key) {
   long size = table->size;
   hashbucketentry_t *buckets = table->buckets;
   long hash = hashnumber(key);
   long off = hash % size;
   long i = 1;

   while (1) {
      if (buckets[off].key) {
	 if (!strcmp(buckets[off].key, key)) {
	    return buckets[off].data;
	 } else {
	    long noff = off + (i * i);
	    off = (noff >= size) ? noff % size : noff;
	    i++;
	 }
      } else {
	 return 0;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    hashtable_put ...                                                */
/*---------------------------------------------------------------------*/
int
hashtable_put(hashtable_t *table, const char *key, void *data) {
   long size = table->size;
   hashbucketentry_t *buckets = table->buckets;
   long hash = hashnumber(key);
   long off = hash % size;
   long i = 1;

   while (1) {
      if (!buckets[off].key) {
	 buckets[off].key = (char *)key;
	 buckets[off].data = data;
	 return 1;
      } else if (!strcmp(buckets[off].key, key)) {
	 buckets[off].key = (char *)key;
	 buckets[off].data = data;
	 return 2;
      } else if (i >= 5) {
	 hashtable_rehash(table);
	 return hashtable_put(table, key, data);
      } else  {
	 long noff = off + (i * i);
	 off = (noff >= size) ? noff % size : noff;
	 i++;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    hashtable_foreach ...                                            */
/*---------------------------------------------------------------------*/
void
hashtable_foreach(hashtable_t *table, void (*proc)(const char *, void *data)) {
   long size = table->size;
   hashbucketentry_t *buckets = table->buckets;
   long i;

   for (i = 0; i < size; i++) {
      if (buckets[i].key) {
	 proc(buckets[i].key, buckets[i].data);
      }
   }
}
