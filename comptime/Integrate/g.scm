;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/g.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 15 14:53:50 1995                          */
;*    Last change :  Thu Nov 17 05:41:06 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Compute the set of globalized functions.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_g
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    integrate_info
	    integrate_a)
   (export  (G! locals)))
 
;*---------------------------------------------------------------------*/
;*    G! ...                                                           */
;*    -------------------------------------------------------------    */
;*    The globalized function due to Cn property have already          */
;*    been computed during the Cn computation. Now, we just            */
;*    perform a fix-point iteration with the Ct property.              */
;*---------------------------------------------------------------------*/
(define (G! G/cn)
   (trace integrate "G!, G/cn: " (shape G/cn) #\Newline)
   (let loop ((stop? #f)
	      (stamp 0)
	      (Gs    G/cn))
      (if stop?
	  (begin
	     ;; we erase every marks because DISPLAY-LET-FUN uses
	     ;; them for another purpose and with a fresh counter.
	     (for-each (lambda (f)
			  (if (local? f)
			      (sfun/Iinfo-istamp-set! (local-value f) -1)))
		       *phi*)
	     (integrate-remaining-local-functions!)
	     (trace integrate "G: " (shape Gs) #\Newline)
	     (trace (integrate 2) "   " stamp #" iteration(s) to fix point\n")
	     Gs)
	  (let liip ((phi   *phi*)
		     (stop? #t) 
		     (Gs    Gs))
	     (if (null? phi)
		 (loop stop? (+fx stamp 1) Gs)
		 (let* ((f   (car phi))
			(fif (variable-value f)))
		    (let laap ((Ct    (sfun/Iinfo-Ct fif))
			       (stop? stop?)
			       (Gs    Gs))
		       (if (null? Ct)
			   (liip (cdr phi) stop? Gs)
			   (let* ((g   (car Ct))
				  (gif (local-value g)))
			      (trace (integrate 4)
				     " Ct( " (shape f) "[G?:"
				     (sfun/Iinfo-G? fif)
				     "], " (shape g) "[G?: "
				     (sfun/Iinfo-G? gif)
				     "] )"
				     #\Newline)
			      (cond
				 ((eq? f g)
				  (laap (cdr Ct) stop? Gs))
				 ((sfun/Iinfo-G? gif)
				  (laap (cdr Ct) stop? Gs))
				 ((sfun/Iinfo-G? fif)
				  (cond
				     ((not (variable? (sfun/Iinfo-L gif)))
				      (trace (integrate 4)
					     "   trying L.1( "
					     (shape f) ", " (shape g) " )"
					     #\Newline)
				      (sfun/Iinfo-L-set! gif f)
				      (laap (cdr Ct) #f Gs))
				     ((eq? (sfun/Iinfo-L gif) f)
				      (laap (cdr Ct) stop? Gs))
				     (else
				      (sfun/Iinfo-G?-set! gif #t)
				      (trace (integrate 4)
					     "   G.1( " (shape g) " )"
					     #\Newline)
				      (laap (cdr Ct) #f (cons g Gs)))))
				 ((not (variable? (sfun/Iinfo-L gif)))
				  (cond
				     ((variable? (sfun/Iinfo-L fif))
				      (sfun/Iinfo-L-set! gif (sfun/Iinfo-L fif))
				      (trace (integrate 4)
					     "   trying L.2( "
					     (shape (sfun/Iinfo-L fif)) ", "
					     (shape g) " )"
					     #\Newline)
				      (laap (cdr Ct) #f Gs))
				     (else
				      (let ((stop? (and
						    stop?
						    (fixnum? (sfun/Iinfo-istamp
							       fif))
						    (<=fx (sfun/Iinfo-istamp
							   fif)
							  stamp))))
					 (trace (integrate 4)
						"  ** G!.bind-fun!("
						(shape f) ", " stamp
						")" #\Newline)
					 (sfun/Iinfo-istamp-set! fif stamp)
					 (laap (cdr Ct) stop? Gs)))))
				 ((not (variable? (sfun/Iinfo-L fif)))
				  (trace (integrate 4)
					 "   trying L.3( "
					 (shape (sfun/Iinfo-L gif)) ", "
					 (shape f)
					 " )" #\Newline)
				  (sfun/Iinfo-L-set! fif (sfun/Iinfo-L gif))
				  (laap (cdr Ct) #f Gs))
				 ((eq? (sfun/Iinfo-L fif) (sfun/Iinfo-L gif))
				  (trace (integrate 4)
					 "   (eq? (sfun/Iinfo-L fif) (sfun/Iinfo-L gif))"
					 #\Newline)
				  (laap (cdr Ct) stop? Gs))
				 (else
				  (sfun/Iinfo-G?-set! gif #t)
				  (trace (integrate 4)
					 "   G.3( " (shape g) " )" #\Newline)
				  (laap (cdr Ct) #f (cons g Gs)))))))))))))
		       
      
;*---------------------------------------------------------------------*/
;*    integrate-remaining-local-functions! ...                         */
;*    -------------------------------------------------------------    */
;*    We scan each used functions. If a function is local, not         */
;*    globalized and not integrated we, we declare it as integrated    */
;*    in its owner (in the definition that binds it).                  */
;*---------------------------------------------------------------------*/
(define (integrate-remaining-local-functions!)
   (for-each (lambda (f)
		(if (and (local? f)
			 (not (sfun/Iinfo-G? (local-value f)))
			 (not (variable? (sfun/Iinfo-L (local-value f)))))
		    (begin
		       (trace (integrate 3)
			      " *** owner integration: " (shape f) " in "
			      (shape (sfun/Iinfo-owner (local-value f)))
			      #\Newline)
		       (sfun/Iinfo-L-set! (local-value f)
					  (sfun/Iinfo-owner (local-value f))))))
	     *phi*))
   
