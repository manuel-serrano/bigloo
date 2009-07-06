#include <stdio.h>
#include "point.h"

int print_point_2d( struct point_2d *pt ) {
   printf( "<point-2d: %g, %g>", pt->x, pt->y );
}
