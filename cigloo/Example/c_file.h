/*=====================================================================*/
/*    serrano/prgm/project/bigloo/cigloo0.0/Example/c-file.h           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec  7 13:09:01 1995                          */
/*    Last change :  Sun Dec 10 14:23:46 1995 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The structure definition                                         */
/*=====================================================================*/

#define ORIG_X 0.0
#define ORIG_Y 0.0

typedef struct point {
   double x, y;
} Point;

typedef struct vect {
   struct point p1, p2;
} Vect;
