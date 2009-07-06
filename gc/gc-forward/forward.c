/*=====================================================================*/
/*    serrano/prgm/project/bigloo/gc-forward/forward.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Mar 18 14:20:22 1994                          */
/*    Last change :  Wed Dec 16 07:42:25 1998 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Ce code est juste utile pour faire des benchs. C'est un petit    */
/*    allocateur tout en avant (ce n'est pas un GC).                   */
/*=====================================================================*/
#include <stdio.h>
#include <bigloo.h>

#if( THE_GC == NO_GC )
#include <malloc.h>

/*---------------------------------------------------------------------*/
/*    Cette valeur designe la taille des segments supplementaires      */
/*    alloues pendant une exectution si le tas est plein.              */
/*---------------------------------------------------------------------*/
#define ADD_OCTET_SIZE ( 1024 * 100 )

/*---------------------------------------------------------------------*/
/*    Extern importations                                              */
/*---------------------------------------------------------------------*/
extern void *memset();

/*---------------------------------------------------------------------*/
/*    Les variables de controles                                       */
/*---------------------------------------------------------------------*/
char * heap_hd          = 0L;
char * heap_tl          = 0L;
static long nb_octet_allocated = 0;

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    GC_size ...                                                      */
/*---------------------------------------------------------------------*/
int
GC_size( void *ptr )
{
   return 0;
}

#if( defined( MALLOC ) )
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    init_heap ...                                                    */
/*    -------------------------------------------------------------    */
/*    L'argument est le nombre de Mega que doit contenir le tas.       */
/*---------------------------------------------------------------------*/
int
init_heap( octet )
long octet;
{
   nb_octet_allocated = 0;
   return 1;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t alloc ...                                                  */
/*    -------------------------------------------------------------    */
/*    On alloue un block de octet octets                               */
/*---------------------------------------------------------------------*/
obj_t
heap_alloc( octet )
long octet;
{
   char *aux;

   nb_octet_allocated += octet;
   nb_allocations++;
   
   aux = (char *)malloc( octet );
   memset( aux, 0, octet );

   return (obj_t)aux;
}

#else

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    init_heap ...                                                    */
/*    -------------------------------------------------------------    */
/*    L'argument est le nombre de Mega que doit contenir le tas.       */
/*---------------------------------------------------------------------*/
long
init_heap( octet )
long octet;
{
   nb_octet_allocated = 0;

   if( !(heap_hd = malloc( octet )) )
      return 0;
	
   heap_tl = heap_hd + octet - 1;

   memset( heap_hd, 0, octet );

   return 1;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t alloc ...                                                  */
/*    -------------------------------------------------------------    */
/*    On alloue un block de octet octets                               */
/*---------------------------------------------------------------------*/
obj_t
heap_alloc( octet )
long octet;
{
   char *aux;

   aux = heap_hd;

   heap_hd += octet;

   /* We always enforce a 3 bit pointers alignement */
   /*(even for 32 bits such as the Sparc).          */
   heap_hd = (char *)(((long)heap_hd + 7) &~7);
	
   if( heap_hd > heap_tl )   /* il faut re-allouer un nouveau segment */
   {
      long size = (octet < ADD_OCTET_SIZE) ? ADD_OCTET_SIZE + 16 : octet * 2;
		
      heap_hd = malloc( size );
      heap_tl = heap_hd + size - 1;

      memset( heap_hd, 0, size );
      
      return heap_alloc( octet );
   }
	
#if( defined( DEBUG ) )
   nb_octet_allocated += octet;
   nb_allocations++;

   /* on verifie que la zone qu'on a alloue'e est pleine de 0 */
   {
      char *aux_bis ;
      
      for( aux_bis = aux + octet; (aux_bis >= aux) && !*aux; aux_bis-- );
      
      if( aux_bis > aux )
      {
	 puts( "*** ERROR:nogc:zone non vide" );
	 exit( -1000 );
      }
   }
#endif
	
   return (obj_t)aux;
}

#endif

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_heap ...                                                    */
/*---------------------------------------------------------------------*/
void
free_heap( ptr )
char * ptr;
{
#if( defined( DEBUG ) )   
   printf( "NOGC statistics: %d bytes  %d K  %d Mo\n",
	   nb_octet_allocated,
	   nb_octet_allocated / 1024,
	   nb_octet_allocated / (1024 * 1024) );
   printf( "Number of allocations: %d\n", nb_allocations );
#else
   ;
#endif
}

#endif
