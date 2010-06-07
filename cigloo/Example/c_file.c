/*=====================================================================*/
/*    serrano/prgm/project/bigloo/cigloo/Example/c_file.c              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec  7 13:08:17 1995                          */
/*    Last change :  Mon Jun  7 17:26:27 2010 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The C part                                                       */
/*=====================================================================*/
#include "c_file.h"

double
vect_norm( Vect *vect )
{
   double d1 = (vect->p1.x - vect->p2.x);
   double d2 = (vect->p1.x - vect->p2.y);
   
   return sqrt( (d1 * d1) + (d2 * d2) );
}

Vect *
vect_neg( Vect *v )
{
   Vect *new;

   new = malloc( sizeof( struct vect ) );

   new->p1.x = -v->p1.x;
   new->p1.y = -v->p1.y;
   new->p2.x = -v->p2.x;
   new->p2.y = -v->p2.y;
   
   return new;
}

Vect *
vect_add( Vect *v1, Vect *v2 )
{
   Vect *new;

   new = malloc( sizeof( struct vect ) );

   new->p1.x = (v1->p1.x + v2->p1.x);
   new->p1.y = (v1->p1.y + v2->p1.y);
   new->p2.x = (v1->p2.x + v2->p2.x);
   new->p2.y = (v1->p2.y + v2->p2.y);
   
   return new;
}
