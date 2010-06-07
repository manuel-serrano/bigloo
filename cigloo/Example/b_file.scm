;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Example/b_file.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  7 13:08:10 1995                          */
;*    Last change :  Mon Jun  7 17:26:52 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Scheme part                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module example
   (extern  (include "c_file.h"))
   (include "b_file.sch"))

(let* ((pt1  (s-point* ORIG_X ORIG_Y))
       (pt2  (s-point* 1.0 1.0))
       (v1   (s-vect* pt1 pt2)))
   (print (vect_norm v1))
   (print (vect_norm (vect_add v1 (vect_neg v1))))) 



