;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Ieee/pairlist.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 09:58:09 1995                          */
;*    Last change :  Sat Mar 12 14:58:45 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.3. Pairs and Lists (page 15, r4)                               */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Pairs And Lists@                                        */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_pairs_and_lists_6_3
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5
	    __r4_strings_6_7
	    __r4_symbols_6_4
	    __r4_booleans_6_1
	    __r5_control_features_6_4
	    __r4_control_features_6_9
	    
	    __evenv)
   
   (extern  (macro $pair?::bool (::obj) "PAIRP")
	    (macro $epair?::bool (::obj) "EPAIRP")
	    (macro $cons::pair (::obj ::obj) "MAKE_YOUNG_PAIR")
	    (macro $acons::pair (::obj ::obj) "MAKE_STACK_PAIR")
	    (macro $econs::epair (::obj ::obj ::obj) "MAKE_YOUNG_EPAIR")
	    (macro $car::obj (::pair) "CAR")
	    (macro $cdr::obj (::pair) "CDR")
	    (macro $cer::obj (::epair) "CER")
	    (macro $set-car!::obj (::pair ::obj) "SET_CAR")
	    (macro $set-cdr!::obj (::pair ::obj) "SET_CDR")
	    (macro $set-cer!::obj (::epair ::obj) "SET_CER")
	    (macro $null?::bool (::obj) "NULLP")
	    
	    (export length "bgl_list_length")
	    (export list-ref "bgl_list_ref")
	    (export append-2 "bgl_append2")
	    (export reverse "bgl_reverse")
	    (export reverse! "bgl_reverse_bang")
	    (export remq "bgl_remq")
	    (export remq! "bgl_remq_bang"))
   
   (java    (class foreign
	       (method static $pair?::bool (::obj)
		       "PAIRP")
	       (method static $epair?::bool (::obj)
		       "EPAIRP")
	       (method static $cons::pair (::obj ::obj)
		       "MAKE_PAIR")
	       (method static $acons::pair (::obj ::obj)
		       "MAKE_STACK_PAIR")
	       (method static $econs::epair (::obj ::obj ::obj)
		       "MAKE_EPAIR")
	       (method static $car::obj (::pair)
		       "CAR")
	       (method static $cdr::obj (::pair)
		       "CDR")
	       (method static $cer::obj (::epair)
		       "CER")
	       (method static $set-car!::obj (::pair ::obj)
		       "SET_CAR")
	       (method static $set-cdr!::obj (::pair ::obj)
		       "SET_CDR")
	       (method static $set-cer!::obj (::epair ::obj)
		       "SET_CER")
	       (method static $null?::bool (::obj)
		       "NULLP")))
   
   (export  (inline pair?::bool ::obj)
	    (inline epair?::bool ::obj)
	    (inline pair-or-null?::bool ::obj)
	    (inline cons::pair ::obj ::obj)
	    (inline econs::pair ::obj ::obj ::obj)
	    (inline car ::pair)
	    (inline cdr ::pair)
	    (inline cer ::epair)
	    (inline caar ::pair)
	    (inline cadr ::pair) 
	    (inline cdar ::pair)
	    (inline cddr ::pair)
	    (inline caaar ::pair)
	    (inline caadr ::pair)
	    (inline cadar ::pair)
	    (inline caddr ::pair)
	    (inline cdaar ::pair)
	    (inline cddar ::pair)
	    (inline cdadr ::pair)
	    (inline cdddr ::pair)
	    (inline caaaar ::pair)
	    (inline caaadr ::pair)
	    (inline caadar ::pair)
	    (inline cadaar ::pair)
	    (inline cdaaar ::pair)
	    (inline caaddr ::pair)
	    (inline caddar ::pair)
	    (inline cadadr ::pair)
	    (inline cadddr ::pair)
	    (inline cdaadr ::pair)
	    (inline cdaddr ::pair)
	    (inline cddaar ::pair)
	    (inline cdadar ::pair)
	    (inline cddadr ::pair)
	    (inline cdddar ::pair)
	    (inline cddddr ::pair)
	    (inline set-car! ::pair ::obj)
	    (inline set-cdr! ::pair ::obj)
	    (inline set-cer! ::epair ::obj)
	    (inline null?::bool ::obj)
	    (list?::bool ::obj)
	    (inline list::pair-nil . obj)
	    (length::long ::pair-nil)
	    (append-2 ::pair-nil ::obj)
	    (eappend-2 ::pair-nil ::obj)
	    (append . obj)
	    (eappend . obj)
	    (append!::pair-nil . obj)
	    (append-2!::pair-nil ::pair-nil ::pair-nil)
	    (reverse::pair-nil ::pair-nil)
	    (ereverse::pair-nil ::pair-nil)
	    (take::pair-nil ::pair-nil ::long)
	    (drop::pair-nil ::pair-nil ::long)
	    (list-tail::pair-nil ::pair-nil ::long)
	    (list-ref ::pair-nil ::long)
	    (list-set! ::pair-nil ::long ::obj)
	    (last-pair::pair ::pair)
	    (memq ::obj ::pair-nil)
	    (memv ::obj ::pair-nil)
	    (member ::obj ::pair-nil)
	    (assq ::obj ::pair-nil)
	    (assv ::obj ::pair-nil)
	    (assoc ::obj ::pair-nil)
	    (remq::pair-nil ::obj ::pair-nil)
	    (remq!::pair-nil ::obj ::pair-nil)
	    (delete::pair-nil ::obj ::pair-nil #!optional (eq equal?))
	    (delete!::pair-nil ::obj ::pair-nil #!optional (eq equal?))
	    (reverse!::pair-nil ::pair-nil)
	    (cons* ::obj . obj)
	    (every::obj ::procedure . obj)
	    (any::obj ::procedure . obj)
	    (find ::procedure ::pair-nil)
	    (find-tail::obj ::procedure ::pair-nil)
	    (reduce::obj ::procedure ::obj ::pair-nil)
	    (make-list::pair-nil ::int . obj)
	    (list-tabulate::pair-nil ::int ::procedure)
	    (list-split ::pair-nil ::int . obj)
	    (list-split! ::pair-nil ::int . obj)
	    (iota::pair-nil ::int . obj)
	    (list-copy::pair-nil ::pair-nil)
	    (tree-copy::obj ::obj)
	    (delete-duplicates::pair-nil ::pair-nil #!optional (eq equal?))
	    (delete-duplicates!::pair-nil ::pair-nil #!optional (eq equal?)))
   
   (pragma  ($cons args-safe fail-safe)
	    ($null? no-alloc (predicate-of nil) no-cfa-top nesting args-safe fail-safe (effect))
	    (null? no-alloc (predicate-of nil) no-cfa-top nesting fail-safe)
	    (pair-or-null? no-alloc (predicate-of pair-nil) no-cfa-top nesting (effect) fail-safe)
	    ($pair? no-alloc (predicate-of pair) no-cfa-top nesting fail-safe (effect))
	    (pair? no-alloc (predicate-of pair) no-cfa-top nesting fail-safe)
	    ($car no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe
		   (effect (read (car))))
	    ($set-car! no-alloc fail-safe (effect (write (car))))
	    (car no-alloc side-effect-free no-cfa-top nesting)
	    ($cdr no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe
		   (effect (read (cdr))))
	    ($set-cdr! no-alloc fail-safe (effect (write (cdr))))
	    (cdr no-alloc side-effect-free no-cfa-top nesting)
	    ($cer no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe
		   (effect (read (cer))))
	    ($set-cer! no-alloc fail-safe (effect (write (cer))))
	    (cer no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (length no-alloc side-effect-free no-cfa-top nesting
		    (effect (read (car cdr))))
	    (append-2 side-effect-free nesting fail-safe)
	    (eappend-2 side-effect-free nesting fail-safe)
	    (append side-effect-free nesting fail-safe)
	    (eappend side-effect-free nesting fail-safe)
	    (reverse side-effect-free nesting)
	    (ereverse side-effect-free nesting)
	    (take side-effect-free nesting)
	    (drop side-effect-free nesting)
	    (list-tail side-effect-free nesting)
	    (list-ref side-effect-free nesting)
	    (last-pair side-effect-free nesting)
	    (memq side-effect-free nesting)
	    (memv side-effect-free nesting)
	    (member side-effect-free nesting)
	    (assq side-effect-free nesting)
	    (assv side-effect-free nesting)
	    (assoc side-effect-free nesting)
	    (remq side-effect-free nesting)))

;*---------------------------------------------------------------------*/
;*    pair? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (pair? obj)
   ($pair? obj))

;*---------------------------------------------------------------------*/
;*    epair? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (epair? obj)
   ($epair? obj))

;*---------------------------------------------------------------------*/
;*    @deffn pair-or-null?@ ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (pair-or-null? obj)
   (if (pair? obj)
       #t
       (null? obj)))
   
;*---------------------------------------------------------------------*/
;*    cons ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (cons obj1 obj2)
   ($cons obj1 obj2))

;*---------------------------------------------------------------------*/
;*    econs ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (econs obj1 obj2 obj3)
   ($econs obj1 obj2 obj3))

;*---------------------------------------------------------------------*/
;*    car ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (car pair)
   ($car pair))

;*---------------------------------------------------------------------*/
;*    cdr ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (cdr pair)
   ($cdr pair))

;*---------------------------------------------------------------------*/
;*    cer ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (cer obj)
   ($cer obj))

;*---------------------------------------------------------------------*/
;*    caar ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (caar pair)
   (car (car pair)))

;*---------------------------------------------------------------------*/
;*    cadr ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (cadr pair)
   (car (cdr pair)))

;*---------------------------------------------------------------------*/
;*    cdar ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (cdar pair)
   (cdr (car pair)))

;*---------------------------------------------------------------------*/
;*    cddr ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (cddr pair)
   (cdr (cdr pair)))

;*---------------------------------------------------------------------*/
;*    caaar ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (caaar pair)
   (car (car (car pair))))

;*---------------------------------------------------------------------*/
;*    caadr ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (caadr pair)
   (car (car (cdr pair))))

;*---------------------------------------------------------------------*/
;*    cadar ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cadar pair)
   (car (cdr (car pair))))

;*---------------------------------------------------------------------*/
;*    caddr ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (caddr pair)
   (car (cdr (cdr pair))))

;*---------------------------------------------------------------------*/
;*    cdaar ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cdaar pair)
   (cdr (car (car pair))))

;*---------------------------------------------------------------------*/
;*    cddar ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cddar pair)
   (cdr (cdr (car pair))))

;*---------------------------------------------------------------------*/
;*    cdadr ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cdadr pair)
   (cdr (car (cdr pair))))

;*---------------------------------------------------------------------*/
;*    cdddr ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cdddr pair)
   (cdr (cdr (cdr pair))))

;*---------------------------------------------------------------------*/
;*    caaaar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (caaaar pair)
   (car (car (car (car pair)))))

;*---------------------------------------------------------------------*/
;*    caaadr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (caaadr pair)
   (car (car (car (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    caadar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (caadar pair)
   (car (car (cdr (car pair)))))

;*---------------------------------------------------------------------*/
;*    cadaar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cadaar pair)
   (car (cdr (car (car pair)))))

;*---------------------------------------------------------------------*/
;*    cdaaar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cdaaar pair)
   (cdr (car (car (car pair)))))

;*---------------------------------------------------------------------*/
;*    caaddr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (caaddr pair)
   (car (car (cdr (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    caddar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (caddar pair)
   (car (cdr (cdr (car pair)))))

;*---------------------------------------------------------------------*/
;*    cadadr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cadadr pair)
   (car (cdr (car (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    cadddr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cadddr pair)
   (car (cdr (cdr (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    cdaadr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cdaadr pair)
   (cdr (car (car (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    cdaddr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cdaddr pair)
   (cdr (car (cdr (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    cddaar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cddaar pair)
   (cdr (cdr (car (car pair)))))

;*---------------------------------------------------------------------*/
;*    cddadr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cddadr pair)
   (cdr (cdr (car (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    cdadar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cdadar pair)
   (cdr (car (cdr (car pair)))))

;*---------------------------------------------------------------------*/
;*    cdddar ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cdddar pair)
   (cdr (cdr (cdr (car pair)))))


;*---------------------------------------------------------------------*/
;*    cddddr ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (cddddr pair)
   (cdr (cdr (cdr (cdr pair)))))

;*---------------------------------------------------------------------*/
;*    set-car! ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (set-car! pair obj)
   ($set-car! pair obj))

;*---------------------------------------------------------------------*/
;*    set-cdr! ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (set-cdr! pair obj)
   ($set-cdr! pair obj))

;*---------------------------------------------------------------------*/
;*    set-cer! ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (set-cer! epair obj)
   ($set-cer! epair obj)) 

;*---------------------------------------------------------------------*/
;*    null? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (null? obj)
   ($null? obj))

;*---------------------------------------------------------------------*/
;*    list ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (list::pair-nil . l)
   l)

;*---------------------------------------------------------------------*/
;*    lists? ...                                                       */
;*---------------------------------------------------------------------*/
(define (list? x)
   (labels ((l1 (x prev)
		(cond ((null? x)
		       #t)
		      ((pair? x)
		       (if (eq? x prev)
			   #f
			   (l2 (cdr x) prev)))
		      (else #f)))
	    (l2 (x prev)
		(cond ((null? x)
		       #t)
		      ((pair? x)
		       (if (eq? x prev)
			   #f
			   (l1 (cdr x) (cdr prev))))
		      (else #f))))
      (cond ((null? x)
	     #t)
	    ((pair? x)
	     (l1 (cdr x) x))
	    (else
	     #f))))

;*---------------------------------------------------------------------*/
;*    append-2                                                         */
;*---------------------------------------------------------------------*/
(define (append-2 l1 l2)
   (let ((head (cons '() l2)))
      (labels ((loop (prev tail)
		     (if (null? tail)
			 '()
			 (let ((new-prev (cons (car tail) l2)))
			    (set-cdr! prev new-prev)
			    (loop new-prev (cdr tail))))))
	 (loop head l1)
	 (cdr head))))

;*---------------------------------------------------------------------*/
;*    eappend-2                                                        */
;*---------------------------------------------------------------------*/
(define (eappend-2 l1 l2)
   (let ((head (if (epair? l2)
		   (econs '() l2 (cer l2))
		   (cons '() l2))))
      (labels ((loop (prev tail)
		     (if (null? tail)
			 '()
			 (let ((new-prev (if (epair? tail)
					     (econs (car tail) l2 (cer tail))
					     (cons (car tail) l2))))
			    (set-cdr! prev new-prev)
			    (loop new-prev (cdr tail))))))
	 (loop head l1)
	 (cdr head))))

;*---------------------------------------------------------------------*/
;*    append ...                                                       */
;*---------------------------------------------------------------------*/
(define (append . l)
   (labels ((append-list (l)
			 (let ((len (length l)))
			    (if (=fx len 0)
				'()
				(if (=fx len 1)
				    (car l)
				    (if (=fx len 2)
					(append-2 (car l)
						  (car (cdr l)))
					(append-2 (car l)
						  (append-list (cdr l)))))))))
      (append-list l)))

;*---------------------------------------------------------------------*/
;*    eappend ...                                                      */
;*---------------------------------------------------------------------*/
(define (eappend . l)
   (labels ((append-list (l)
			 (let ((len (length l)))
			    (if (=fx len 0)
				'()
				(if (=fx len 1)
				    (car l)
				    (if (=fx len 2)
					(eappend-2 (car l)
						   (car (cdr l)))
					(eappend-2 (car l)
						   (append-list (cdr l)))))))))
      (append-list l)))

;*---------------------------------------------------------------------*/
;*    append! ...                                                      */
;*---------------------------------------------------------------------*/
(define (append! . l)
   (labels ((append-list (l)
			 (let ((len (length l)))
			    (if (=fx len 0)
				'()
				(if (=fx len 1)
				    (car l)
				    (if (=fx len 2)
					(append-2! (car l)
						   (car (cdr l)))
					(append-2! (car l)
						   (append-list (cdr l)))))))))
      (append-list l)))

;*---------------------------------------------------------------------*/
;*    append-2! ...                                                    */
;*---------------------------------------------------------------------*/
(define (append-2! x y)
  (if (null? x)
      y
      (do ((a x b)
           (b (cdr x) (cdr b)))
          ((null? b)
           (set-cdr! a y)
           x))))

;*---------------------------------------------------------------------*/
;*    length ...                                                       */
;*---------------------------------------------------------------------*/
(define (length list)
   (let loop ((l       list)
	      (res::long 0))
      (cond
	 ((null? l)
	  res)
	 (else
	  (loop (cdr l) (+fx 1 res))))))
 
;*---------------------------------------------------------------------*/
;*    reverse ...                                                      */
;*---------------------------------------------------------------------*/
(define (reverse l)
   (let loop ((l   l)
	      (acc '()))
      (if (null? l)
	  acc
	  (loop (cdr l) (cons (car l) acc)))))

;*---------------------------------------------------------------------*/
;*    ereverse ...                                                     */
;*---------------------------------------------------------------------*/
(define (ereverse l)
   (let loop ((l   l)
	      (acc '()))
      (if (null? l)
	  acc
	  (loop (cdr l)
		(if (epair? l)
		    (econs (car l) acc (cer l))
		    (cons (car l) acc))))))

;*---------------------------------------------------------------------*/
;*    take ...                                                         */
;*---------------------------------------------------------------------*/
(define (take list k)
   (let loop ((list list)
	      (k k)
	      (res '()))
      (if (zerofx? k)
	  (reverse! res)
	  (loop (cdr list) (-fx k 1) (cons (car list) res)))))

;*---------------------------------------------------------------------*/
;*    drop ...                                                         */
;*---------------------------------------------------------------------*/
(define (drop list k)
   (if (zerofx? k)
       list
       (drop (cdr list) (-fx k 1))))

;*---------------------------------------------------------------------*/
;*    list-tail ...                                                    */
;*---------------------------------------------------------------------*/
(define (list-tail list k)
   (drop list k))

;*---------------------------------------------------------------------*/
;*    list-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (list-ref list k)
   (if (zerofx? k)
       (car list)
       (list-ref (cdr list) (-fx k 1))))

;*---------------------------------------------------------------------*/
;*    list-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (list-set! list k val)
   (if (zerofx? k)
       (set-car! list val)
       (list-set! (cdr list) (-fx k 1) val)))

;*---------------------------------------------------------------------*/
;*    last-pair ...                                                    */
;*---------------------------------------------------------------------*/
(define (last-pair x)
   (if (pair? (cdr x))
       (last-pair (cdr x))
       x))

;*---------------------------------------------------------------------*/
;*    memq ...                                                         */
;*---------------------------------------------------------------------*/
(define (memq obj list)
   (let loop ((list list))
      (if (pair? list)
	  (if (eq? (car list) obj)
	      list
	      (loop (cdr list)))
	  #f)))

;*---------------------------------------------------------------------*/
;*    memv ...                                                         */
;*---------------------------------------------------------------------*/
(define (memv obj list)
   (let loop ((list list))
      (if (pair? list)
	  (if (eqv? (car list) obj)
	      list
	      (loop (cdr list)))
	  #f)))

;*---------------------------------------------------------------------*/
;*    member ...                                                       */
;*---------------------------------------------------------------------*/
(define (member obj list)
   (let loop ((list list))
      (cond
	 ((not (pair? list)) #f)
	 ((equal? obj (car list)) list)
	 (else (loop (cdr list))))))
 
;*---------------------------------------------------------------------*/
;*    assq ...                                                         */
;*---------------------------------------------------------------------*/
(define (assq obj alist)
   (let loop ((alist alist))
      (if (pair? alist)
          (if (eq? (car (car alist)) obj)
              (car alist)
              (loop (cdr alist)))
          #f)))

;*---------------------------------------------------------------------*/
;*    assv ...                                                         */
;*---------------------------------------------------------------------*/
(define (assv obj alist)
   (let loop ((alist alist))
      (if (pair? alist)
          (if (eqv? (car (car alist)) obj)
              (car alist)
              (loop (cdr alist)))
	  #f)))

;*---------------------------------------------------------------------*/
;*    assoc ...                                                        */
;*---------------------------------------------------------------------*/
(define (assoc obj alist)
   (let loop ((alist alist))
      (if (pair? alist)
          (if (equal? (car (car alist)) obj)
              (car alist)
              (loop (cdr alist)))
	  #f)))

;*---------------------------------------------------------------------*/
;*    The following functions are not in the IEEE (nor r4).            */
;*---------------------------------------------------------------------*/
;*---------------------------------------------------------------------*/
;*    remq ...                                                         */
;*---------------------------------------------------------------------*/
(define (remq x y)
   (cond
      ((null? y) y)
      ((eq? x (car y)) (remq x (cdr y)))
      (else (cons (car y) (remq x (cdr y))))))

;*---------------------------------------------------------------------*/
;*    remq! ...                                                        */
;*---------------------------------------------------------------------*/
(define (remq! x y)
   (cond
      ((null? y) y)
      ((eq? x (car y)) (remq! x (cdr y)))
      (else (let loop ((prev y))
               (cond ((null? (cdr prev))
                      y)
                     ((eq? (cadr prev) x)
                      (set-cdr! prev (cddr prev))
                      (loop prev))
                     (else (loop (cdr prev))))))))

;*---------------------------------------------------------------------*/
;*    delete ...                                                       */
;*---------------------------------------------------------------------*/
(define (delete x y #!optional (eq equal?))
   (let loop ((x x)
	      (y y))
      (cond
	 ((null? y) y)
	 ((eq x (car y)) (loop x (cdr y)))
	 (else (cons (car y) (loop x (cdr y)))))))

;*---------------------------------------------------------------------*/
;*    delete! ...                                                      */
;*---------------------------------------------------------------------*/
(define (delete! x y #!optional (eq equal?))
   (let laap ((x x)
	      (y y))
      (cond
	 ((null? y) y)
	 ((eq x (car y)) (laap x (cdr y)))
	 (else (let loop ((prev y))
		  (cond ((null? (cdr prev))
			 y)
			((eq (cadr prev) x)
			 (set-cdr! prev (cddr prev))
			 (loop prev))
			(else (loop (cdr prev)))))))))

;*---------------------------------------------------------------------*/
;*    cons* ...                                                        */
;*---------------------------------------------------------------------*/
(define (cons* x . y)
   (labels ((cons*1 (x) (cond ((null? (cdr x))
			       (car x))
			      (else
			       (cons (car x) (cons*1 (cdr x)))))))
      (if (null? y)
	  x
          (cons x (cons*1 y)))))

;*---------------------------------------------------------------------*/
;*    reverse! ...                                                     */
;*---------------------------------------------------------------------*/
(define (reverse! l)
   (if (pair? l)
       (let nr ((l l)
		(r '()))
          (if (null? (cdr l))
              (begin
                 (set-cdr! l r)
                 l)
	      (let ((cdrl (cdr l)))
		 (nr cdrl
		     (begin (set-cdr! l r) l)))))
       l))

;*---------------------------------------------------------------------*/
;*    every ...                                                        */
;*---------------------------------------------------------------------*/
(define (every pred . l)
   (cond
      ((null? l)
       #t)
      ((null? (cdr l))
       (let loop ((l (car l)))
	  (or (null? l)
	      (and (pred (car l)) (loop (cdr l))))))
      (else
       (let loop ((l l))
	  (or (null? (car l))
	      (and (apply pred (map car l)) (loop (map cdr l))))))))

;*---------------------------------------------------------------------*/
;*    any ...                                                          */
;*---------------------------------------------------------------------*/
(define (any pred . l)
   (cond
      ((null? l)
       #f)
      ((null? (cdr l))
       (let loop ((l (car l)))
	  (and (pair? l)
	       (or (pred (car l)) (loop (cdr l))))))
      (else
       (let loop ((l l))
	  (and (pair? (car l))
	       (or (apply pred (map car l)) (loop (map cdr l))))))))

;*---------------------------------------------------------------------*/
;*    find ...                                                         */
;*---------------------------------------------------------------------*/
(define (find pred list)
   (cond ((find-tail pred list) => car)
	 (else #f)))

;*---------------------------------------------------------------------*/
;*    find-tail ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-tail pred list)
   (let lp ((list list))
      (when (pair? list)
	 (if (pred (car list))
	     list
	     (lp (cdr list))))))

;*---------------------------------------------------------------------*/
;*    reduce ...                                                       */
;*---------------------------------------------------------------------*/
(define (reduce f ridentify list)
   (if (null? list)
       ridentify
       (let loop ((list (cdr list))
		  (ans (car list)))
	  (if (not (pair? list))
	      ans
	      (loop (cdr list) (f (car list) ans))))))

;*---------------------------------------------------------------------*/
;*    make-list ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-list n . o)
   (let ((fill (if (pair? o) (car o) #unspecified)))
      (let walk ((i n) (r '()))
	 (if (<=fx i 0)
	     r
	     (walk (-fx i 1) (cons fill r))))))
 
;*---------------------------------------------------------------------*/
;*    list-tabulate ...                                                */
;*---------------------------------------------------------------------*/
(define (list-tabulate n init-proc)
   (let walk ((i (-fx n 1)) (r '()))
      (if (<fx i 0)
	  r
	  (walk (-fx i 1) (cons (init-proc i) r)))))

;*---------------------------------------------------------------------*/
;*    list-split ...                                                   */
;*---------------------------------------------------------------------*/
(define (list-split l num . fill)
   (let loop ((l l)
	      (i 0)
	      (acc '())
	      (res '()))
      (cond
	 ((null? l)
	  (reverse! (cons (if (or (null? fill) (=fx i num) (=fx i 0))
			      (reverse! acc)
			      (append! (reverse! acc)
				       (make-list (-fx num i) (car fill))))
			  res)))
	 ((=fx i num)
	  (loop l 0 '() (cons (reverse! acc) res)))
	 (else
	  (loop (cdr l) (+fx i 1) (cons (car l) acc) res)))))

;*---------------------------------------------------------------------*/
;*    list-split! ...                                                  */
;*---------------------------------------------------------------------*/
(define (list-split! l num . fill)
   (let loop ((l l)
	      (i 0)
	      (last #f)
	      (acc l)
	      (rows '()))
      (cond
	 ((null? l)
	  (let ((lrow (if (or (null? fill) (=fx i num) (=fx i 0))
			  acc
			  (begin
			     (set-cdr! last
				       (make-list (-fx num i) (car fill)))
			     acc))))
	     (reverse! (cons lrow rows))))
	 ((=fx i num)
	  (set-cdr! last '())
	  (loop l 0 l l (cons acc rows)))
	 (else
	  (loop (cdr l) (+fx i 1) l acc rows)))))

;*---------------------------------------------------------------------*/
;*    iota ...                                                         */
;*---------------------------------------------------------------------*/
(define (iota count . rest)
   (let ((start 0)
	 (step 1))
      (if (pair? rest)
	  (begin
	     (set! start (car rest))
	     (if (pair? (cdr rest))
		 (set! step (cadr rest)))))
      (let walk ((i count) (v (+ start (* (- count 1) step))) (r '()))
	 (if (<=fx i 0)
	     r
	     (walk (-fx i 1) (- v step) (cons v r))))))
 
;*---------------------------------------------------------------------*/
;*    list-copy ...                                                    */
;*---------------------------------------------------------------------*/
(define (list-copy l)
   (if (pair? l)
       (cons (car l) (list-copy (cdr l)))
       l))

;*---------------------------------------------------------------------*/
;*    tree-copy ...                                                    */
;*---------------------------------------------------------------------*/
(define (tree-copy l)
   (cond
      ((epair? l)
       (econs (tree-copy (car l)) (tree-copy (cdr l)) (tree-copy (cer l))))
      ((pair? l)
       (cons (tree-copy (car l)) (tree-copy (cdr l))))
      (else
       l)))

;*---------------------------------------------------------------------*/
;*    delete-duplicates ...                                            */
;*---------------------------------------------------------------------*/
(define (delete-duplicates lis #!optional (eq equal?))
   (delete-duplicates! (list-copy lis) eq))

;*---------------------------------------------------------------------*/
;*    delete-duplicates! ...                                           */
;*---------------------------------------------------------------------*/
(define (delete-duplicates! lis #!optional (eq equal?))
   (unless (procedure? eq)
      (bigloo-type-error 'delete-duplicates! "procedure" eq))
   (let recur ((lis lis))
      (if (pair? lis)
	  (let* ((x (car lis))
		 (tail (cdr lis))
		 (new-tail (recur (delete! x tail eq))))
	     (if (eq? tail new-tail)
		 lis
		 (cons x new-tail)))
	  lis)))
