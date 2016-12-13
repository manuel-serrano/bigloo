;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/kk.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 14 17:30:55 1995                          */
;*    Last change :  Fri Dec 16 08:46:58 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of K and K* properties.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_kk
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    integrate_info
	    integrate_a)
   (export  (K!  <a-set> ::global)
	    (K*! <a-set>)))

;*---------------------------------------------------------------------*/
;*    K! ...                                                           */
;*    -------------------------------------------------------------    */
;*    La facon de calculer la propriete K est la suivante:             */
;*       1- On parcours la liste des A pour trouver tous               */
;*          les appels non-terminaux.                                  */
;*       2- On re-parcours A pour s'occuper des terminaux sachant      */
;*          que l'application de la troisieme regle declenche          */
;*          le re-ajustage de K pour une et une seule fonction.        */
;*    -------------------------------------------------------------    */
;*    Les trois regles sont (Seniak, page 101):                        */
;*       1.  E(f)                 ==> K(f, bottom)                     */
;*       2.  A(f,g,k)             ==> k(g,k)                           */
;*       3.  A(f,g,tail) ^ k(f,k) ==> k(g,k)                           */
;*    -------------------------------------------------------------    */
;*    En fait, on calcule en meme temps que la propriete K, la         */
;*    premiere regle de la propriete K*                                */
;*---------------------------------------------------------------------*/
(define (K! A var)
   (trace (integrate 4) "K!..." (shape var) #\Newline)
;*---------------------------------------------------------------------*/
;*    1.  on sait qu'une seule fonction est E car toutes les autres    */
;*    ont deja ete globalisees (par la globaliation).                  */
;*---------------------------------------------------------------------*/
   (let ((ifun (global-value var)))
      (sfun/Iinfo-K-set!  ifun (list 'bottom))
      (sfun/Iinfo-K*-set! ifun (list 'bottom))
      (K-2! A (K-1! A '()))))

;*---------------------------------------------------------------------*/
;*    K-1! ...                                                         */
;*---------------------------------------------------------------------*/
(define (K-1! A A-tail)
   (trace (integrate 4) "K-1!..." #\Newline)
   (if (null? A)
       A-tail
       (let* ((pr (car A))
	      (f  (car pr))
	      (g  (cadr pr))
	      (k  (caddr pr)))
	  (cond
	     ((eq? k 'tail)
	      (if (eq? f g)
		  (K-1! (cdr A) A-tail)
		  (K-1! (cdr A) (cons pr A-tail))))
	     (else
	      (let ((ifun (variable-value g)))
		 (if (memq k (sfun/Iinfo-K ifun))
		     (K-1! (cdr A) A-tail)
		     (begin
			(sfun/Iinfo-K-set!  ifun (cons k (sfun/Iinfo-K ifun)))
			(sfun/Iinfo-K*-set! ifun (cons k (sfun/Iinfo-K* ifun)))
			(K-1! (cdr A) A-tail)))))))))

;*---------------------------------------------------------------------*/
;*    K-2! ...                                                         */
;*---------------------------------------------------------------------*/
(define (K-2! A A-tail)
   (trace (integrate 4) "K-2!..." #\Newline)
   (let loop ((continue #t))
      (if (not continue)
	  A-tail
	  (let liip ((At A-tail)
		     (continue #f))
	     (trace (integrate 5) " K-2 liip: At="
		    (if (pair? At) (shape (car At)) '())
		    "  continue=" continue #\Newline)
	     (if (null? At)
		 (loop continue)
		 (let ((ifun (variable-value (car (car At)))))
		    (if (null? (sfun/Iinfo-K ifun))
			(liip (cdr At) continue)
			(let* ((g (cadr (car At)))
			       (gifun (variable-value g)))
			   (let laap ((Ks (sfun/Iinfo-K ifun))
				      (continue continue))
			      (trace (integrate 5) "K-2  laap: Ks="
				     (if (pair? Ks) (shape (car Ks)) '())
				     " continue=" continue #\Newline)
			      (if (null? Ks)
				  (liip (cdr At) continue)
				  (let ((k (car Ks)))
				     (if (memq k (sfun/Iinfo-K gifun))
					 (laap (cdr Ks) continue)
					 (begin
					    (sfun/Iinfo-K-set!
					     gifun
					     (cons k (sfun/Iinfo-K gifun)))
					    (sfun/Iinfo-K*-set!
					     gifun
					     (cons k (sfun/Iinfo-K* gifun)))
					    (laap (cdr Ks) #t))))))))))))))


;*---------------------------------------------------------------------*/
;*    K*! ...                                                          */
;*    -------------------------------------------------------------    */
;*    Les regles qui definissent la propriete K* sont:                 */
;*       1. K(f,g)                ==> K*(f,g)                          */
;*       2. A(f,g,tail) ^ K*(g,k) ==> K*(f,k)                          */
;*    Pour trouver des informations plus precises, se referer a la     */
;*    these Nitsan Seniak, page 102.                                   */
;*    -------------------------------------------------------------    */
;*    On a ete un tout petit peu malin car la premiere regle a deja    */
;*    ete traite lors du calcul de K.                                  */
;*---------------------------------------------------------------------*/
(define (K*! A-tail)
   (trace (integrate 4) "K*!..." #\Newline)
   (let loop ((continue #t))
      (if (not continue)
	  (trace-K)
	  (let liip ((At       A-tail)
		     (continue #f))
	     (if (null? At)
		 (loop continue)
		 (let ((ifun (variable-value (cadr (car At)))))
		    (if (null? (sfun/Iinfo-K* ifun))
			(loop continue)
			(let laap ((Ks       (sfun/Iinfo-K* ifun))
				   (continue continue))
			   (if (null? Ks)
			       (liip (cdr At) continue)
			       (let* ((f      (car (car At)))
				      (fifun  (variable-value f))
				      (k      (car Ks)))
				  (if (memq k (sfun/Iinfo-K* fifun))
				      (laap (cdr Ks) continue)
				      (begin
					 (sfun/Iinfo-K*-set!
					  fifun
					  (cons k (sfun/Iinfo-K* fifun)))
					 (laap (cdr Ks) #t)))))))))))))

;*---------------------------------------------------------------------*/
;*    trace-K ...                                                      */
;*---------------------------------------------------------------------*/
(define (trace-K)
   (trace (integrate 2)
	  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	  (begin
	     (for-each (lambda (p)
			  (let ((ifun (variable-value p)))
			     (fprint *trace-port*
				     " --> " (shape p) #\: #\Newline
				     "   K : " (shape (sfun/Iinfo-K ifun))
				     #\Newline
				     "   K*: " (shape (sfun/Iinfo-K* ifun))
				     #\Newline)))
		       *phi*)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	  #\Newline))


		       
		    

	     
	
