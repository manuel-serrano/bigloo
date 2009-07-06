;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/big-file.scm                 */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 19 15:38:09 1992                          */
;*    Last change :  Mon Dec  9 13:55:27 2002 (serrano)                */
;*                                                                     */
;*    Le fichier ou l'on recupere les c-functions                      */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module big-file
   (foreign (int extern-bar (int) "bar"))
   (java    (abstract-class extern
	       (method static bar::int (::int) "bar")
	       "bar"))
   (export  (inline bis y)))

;*---------------------------------------------------------------------*/
;*    bis                                                              */
;*---------------------------------------------------------------------*/
(define-inline (bis y)
   (extern-bar y))
