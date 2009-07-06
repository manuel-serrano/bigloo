/*---------------------------------------------------------------------*/
/*    serrano/prgm/project/bigloo/recette/c-file.c                     */
/*                                                                     */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 19 15:37:20 1992                          */
/*    Last change :  Sat May  5 07:47:37 2007 (serrano)                */
/*                                                                     */
/*    Le fichier C du foreign-test                                     */
/*---------------------------------------------------------------------*/
#include "c-file.h"
#include <string.h>
#include <stdlib.h>

/*---------------------------------------------------------------------*/
/*    Une variable globale                                             */
/*---------------------------------------------------------------------*/
int var = 9;

/*---------------------------------------------------------------------*/
/*    Un pointeur vers une fonction                                    */
/*---------------------------------------------------------------------*/
static int lolo( int x )
{
   return 4 + x;
}

int (*f1)(int) = &lolo;

/*---------------------------------------------------------------------*/
/*    bar ...                                                          */
/*---------------------------------------------------------------------*/
int bar( int x )
{
   return x+1;
}

/*---------------------------------------------------------------------*/
/*    hux ...                                                          */
/*---------------------------------------------------------------------*/
char *
hux( char *s )
{
   static char string[ 500 ];

   strcpy( string, "TOTO EST CONTENT" );

   return string;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    sum_el ...                                                       */
/*---------------------------------------------------------------------*/
int
sum_el( struct el *head )
{
	int val = 0;

	while( head )
	   val += head->key, head = head->next;

	return val;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    sum_tab ...                                                      */
/*---------------------------------------------------------------------*/
double
sum_tab( int tab[], int max )
{
	double res;
	int    i;

	for( i = 0, res = 0.0; i< max; res = res + (double)(tab[ i ]), i++ )
	   ;
	
	return res;
}

/*---------------------------------------------------------------------*/
/*    struct el*                                                       */
/*    define_el ...                                                    */
/*---------------------------------------------------------------------*/
struct el*
define_el( int key )
{
	struct el* res;
	
	res = (struct el *)malloc( sizeof( struct el) );

	res->key = key;

	return res;
}

/*---------------------------------------------------------------------*/
/*    make_dummy_el ...                                                */
/*---------------------------------------------------------------------*/
struct el *
make_dummy_el()
{
	static struct el *_truc_ = 0L;
	
	if( _truc_ )
	   return _truc_;
	else
	{
		_truc_ = (struct el *)malloc( sizeof( struct el ) );

		_truc_->next = 0L;
		_truc_->key  = -2;

		return _truc_;
	}
}


	
