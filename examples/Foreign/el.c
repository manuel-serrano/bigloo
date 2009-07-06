#include "el.h"
#include <strings.h>

int var = 9;

bar( x )
int x;
{
   return fib( x );
}

char *
hux( s )
char *s;
{
   static char string[ 500 ];

   strcpy( string, "TOTO IS HAPPY" );
   return string;
}

int 
sum_el( head )
struct el *head;
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
sum_tab( tab, max )
int tab[];
int max;
{
	double res;
	int    i;

	for( i = 0, res = 0.0; i< max; res = res + (double)(tab[ i ]), i++ )
	   ;
	
	return res;
}

