;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-range.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Tue Jun 18 17:51:34 2024 (serrano)                */
;*    Copyright   :  2022-24 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    BBV range abstraction                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-range
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    saw_defs
	    saw_regset
	    saw_bbv-debug
	    saw_bbv-config
	    saw_bbv-cache
	    saw_bbv-types)
   
   (export (class bbv-range
	      (lo read-only)
	      (up read-only))

	   (class bbv-vlen
	      (vec read-only)
	      (offset::int read-only))

	   (inline bbv-vlen?::bool ::obj)
	   (inline bbv-vlen-vec::obj ::bbv-vlen)
	   (inline bbv-vlen-offset::int ::bbv-vlen)
	   (inline bbv-max-fixnum::long)
	   (inline bbv-min-fixnum::long)
	   (inline bbv-range?::bool ::obj)
	   (=rv::obj ::obj ::obj)
	   (minrv::obj ::obj ::obj #!optional (unspecified #unspecified))
	   (maxrv::obj ::obj ::obj #!optional (unspecified #unspecified))
	   (bbv-range-fixnum?::bool ::bbv-range)
	   (empty-range?::bool ::bbv-range)
	   (fixnum-range::bbv-range)
	   (fixnum->range::bbv-range ::long)
	   (vlen-range::bbv-range)
	   (rtl-range::obj ::obj ::bbv-ctx)
	   (range-type?::bool ::obj)
	   (bbv-singleton?::bool ::obj)
	   (bbv-range<::obj ::bbv-range ::bbv-range)
	   (bbv-range<=::obj ::bbv-range ::bbv-range)
	   (bbv-range>::obj ::bbv-range ::bbv-range)
	   (bbv-range>=::obj ::bbv-range ::bbv-range)
	   (bbv-range=::obj ::bbv-range ::bbv-range)
	   (bbv-range-lt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-lte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-eq::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-neq::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-add::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-sub::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-mul::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-union::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-intersection::bbv-range ::bbv-range ::bbv-range)))

;*---------------------------------------------------------------------*/
;*    object-print ::bbv-vlen ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-print o::bbv-vlen p proc)
   (let ((str (format "#(~a ~a)"
		 (shape (bbv-vlen-vec o))
		 (bbv-vlen-offset o))))
      (display str p)))

;*---------------------------------------------------------------------*/
;*    bbv-vlen? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (bbv-vlen? o)
   (isa? o bbv-vlen))

(define-inline (bbv-vlen-vec o)
   (with-access::bbv-vlen o (vec) vec))

(define-inline (bbv-vlen-offset o)
   (with-access::bbv-vlen o (offset) offset))

(define-inline (bbv-vlen-vec-eq? x y)
   (and (bbv-vlen? x) (bbv-vlen? y) (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))))

;*---------------------------------------------------------------------*/
;*    =rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (=rv x y)
   (cond
      ((and (number? x) (number? y))
       (= x y))
      ((bbv-vlen-vec-eq? x y)
       (=fx (bbv-vlen-offset x) (bbv-vlen-offset y)))
      ((and (fixnum? x) (bbv-vlen? y))
       (if (<fx x (bbv-vlen-offset y)) #f #unspecified))
      ((and (bbv-vlen? x) (fixnum? y))
       (if (>fx (bbv-vlen-offset x) y) #f #unspecified))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    >rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (>rv x y)
   (cond
      ((and (number? x) (number? y))
       (> x y))
      ((bbv-vlen-vec-eq? x y)
       (>fx (bbv-vlen-offset x) (bbv-vlen-offset y)))
      ((and (bbv-vlen? x) (fixnum? y))
       (if (>fx (bbv-vlen-offset x) y)
	   #t
	   (if (<= (->range (+fx/ov (bbv-max-fixnum) (bbv-vlen-offset x))) y)
	       #f
	       #unspecified)))
      ((and (fixnum? x) (bbv-vlen? y))
       (if (> x (->range (+fx (bbv-max-fixnum) (bbv-vlen-offset y))))
	   #t
	   (if (<=fx x (bbv-vlen-offset y))
	       #f
	       #unspecified)))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    >=rv ...                                                         */
;*---------------------------------------------------------------------*/
(define (>=rv x y)
   (cond
      ((and (number? x) (number? y))
       (>= x y))
      ((bbv-vlen-vec-eq? x y)
       (>=fx (bbv-vlen-offset x) (bbv-vlen-offset y)))
      ((and (bbv-vlen? x) (fixnum? y))
       (if (>=fx (bbv-vlen-offset x) y)
	   #t
	   (if (< (->range (+fx/ov (bbv-max-fixnum) (bbv-vlen-offset x))) y)
	       #f
	       #unspecified)))
      ((and (fixnum? x) (bbv-vlen? y))
       (if (> x (->range (+fx (bbv-max-fixnum) (bbv-vlen-offset y))))
	   #t
	   (if (<fx x (bbv-vlen-offset y))
	       #f
	       #unspecified)))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    <rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (<rv x y)
   (>rv y x))

;*---------------------------------------------------------------------*/
;*    <=rv ...                                                         */
;*---------------------------------------------------------------------*/
(define (<=rv x y)
   (>=rv y x))

;*---------------------------------------------------------------------*/
;*    minrv ...                                                        */
;*---------------------------------------------------------------------*/
(define (minrv x y #!optional (unspecified #unspecified))
   (case (<=rv x y)
      ((#t) x)
      ((#f) y)
      (else unspecified)))

;*---------------------------------------------------------------------*/
;*    minrv-up ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return a conservative approximation of the min of X and Y. The   */
;*    result could be (bbv-max-fixnum).                                */
;*---------------------------------------------------------------------*/
(define (minrv-up x y #!optional (unspecified #unspecified))
   (let ((m (minrv x y)))
      (if (not (eq? m #unspecified))
	  m
	  (let loop ((x x)
		     (y y))
	     (cond
		((and (bbv-vlen? x) (fixnum? y))
		 (duplicate::bbv-vlen x
		    (offset (minfx (bbv-vlen-offset x) y))))
		((and (bbv-vlen? x) (bbv-vlen? y))
		 (if (<fx (bbv-vlen-offset x) (bbv-vlen-offset y)) x y))
		 ;;(minfx (bbv-vlen-offset x) (bbv-vlen-offset y)))
		((bbv-vlen? y)
		 (loop y x))
		(else
		 unspecified))))))

;*---------------------------------------------------------------------*/
;*    maxrv ...                                                        */
;*---------------------------------------------------------------------*/
(define (maxrv x y #!optional (unspecified #unspecified))
   (case (>=rv x y)
      ((#t) x)
      ((#f) y)
      (else unspecified)))

;*---------------------------------------------------------------------*/
;*    maxrv-lo ...                                                     */
;*    -------------------------------------------------------------    */
;*    Return a conservative approximation of the max of X and Y. The   */
;*    result could be (bbv-min-fixnum).                                */
;*---------------------------------------------------------------------*/
(define (maxrv-lo x y #!optional (unspecified #unspecified))
   (let ((m (maxrv x y)))
      (if (not (eq? m #unspecified))
	  m
	  (let loop ((x x)
		     (y y))
	     (cond
		((and (bbv-vlen? x) (fixnum? y))
		 (duplicate::bbv-vlen x
		    (offset (maxfx (bbv-vlen-offset x) y))))
		((and (bbv-vlen? x) (bbv-vlen? y))
		 (if (>fx (bbv-vlen-offset x) (bbv-vlen-offset y)) x y))
;* 		 (maxfx (bbv-max-fixnum)                               */
;* 		    (maxfx (bbv-vlen-offset x) (bbv-vlen-offset y))))  */
		((bbv-vlen? y)
		 (loop y x))
		(else
		 unspecified))))))

;*---------------------------------------------------------------------*/
;*    integer boundaries ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (bbv-max-fixnum)
   (bit-lsh 1 (-fx (bigloo-config 'int-size) 2)))
(define-inline (bbv-min-fixnum)
   (-fx (negfx (bbv-max-fixnum)) 1))
(define-inline (bbv-max-vlen)
   (bbv-max-fixnum))

(define (++ i)
   (cond
      ((eq? i bbv-max-fixnum)
       bbv-max-fixnum)
      ((bbv-vlen? i)
       (duplicate::bbv-vlen i
	  (offset (+fx (bbv-vlen-offset i) 1))))
      (else
       (+ i 1))))

(define (-- i)
   (cond
      ((eq? i bbv-min-fixnum)
       bbv-min-fixnum)
      ((bbv-vlen? i)
       (duplicate::bbv-vlen i
	  (offset (-fx (bbv-vlen-offset i) 1))))
      (else
       (- i 1))))

;*---------------------------------------------------------------------*/
;*    bbv-range? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range? o)
   (isa? o bbv-range))

;*---------------------------------------------------------------------*/
;*    bbv-range-fixnum? ...                                            */
;*---------------------------------------------------------------------*/
(define (bbv-range-fixnum? o)
   (with-access::bbv-range o (lo up)
      (and (or (bbv-vlen? lo) (and (fixnum? lo) (>=fx lo (bbv-min-fixnum))))
	   (or (bbv-vlen? up) (and (fixnum? up) (<=fx up (bbv-max-fixnum)))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lo ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lo o)
   (with-access::bbv-range o (lo)
      lo))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-lonum ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lonum o)
   (with-access::bbv-range o (lo)
      (if (bbv-vlen? lo)
	  (bbv-vlen-offset lo)
	  lo)))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-up ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-up o)
   (with-access::bbv-range o (up)
      up))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-upnum ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-upnum o)
   (with-access::bbv-range o (up)
      (if (bbv-vlen? o)
	  (bbv-max-fixnum)
	  up)))
   
;*---------------------------------------------------------------------*/
;*    shape ::bbv-range ...                                            */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-range)

   (define (bbv-vlen-str n)
      (format "#(~a ~a)"
	 (shape (bbv-vlen-vec n))
	 (bbv-vlen-offset n)))
   
   (define (str n)
      (cond
	 ((eq? n (bbv-max-fixnum)) "upfx")
	 ((eq? n (-fx (bbv-max-fixnum) 1)) "upfx-1")
	 ((and (fixnum? n) (>fx n 0) (<fx (-fx (bbv-max-fixnum) n) 20))
	  (format "upfx-~a" (-fx (bbv-max-fixnum) n)))
	 ((eq? n (bbv-min-fixnum)) "lofx")
	 ((eq? n (+fx (bbv-min-fixnum) 1)) "lofx+1")
	 ((eq? n (+fx (bbv-min-fixnum) 2)) "lofx+2")
	 ((and (fixnum? n) (<fx n 0) (>fx (+fx (bbv-min-fixnum) n) 20))
	  (format "lofx+~a" (+fx (bbv-min-fixnum) n)))
	 ((fixnum? n) n)
	 ((bbv-vlen? n) (bbv-vlen-str n))
	 ((not (flonum? n)) (shape n))
	 ((infinitefl? n) (if (<fl n 0.0) "-inf" "+inf"))
	 (else n)))
   
   (if (empty-range? e)
       "[empty]"
       (with-access::bbv-range e (lo up)
	  (format "[~a..~a]" (str lo) (str up)))))

;*---------------------------------------------------------------------*/
;*    empty-range? ...                                                 */
;*---------------------------------------------------------------------*/
(define (empty-range?::bool r)
   (when (and (number? (bbv-range-lo r)) (number? (bbv-range-up r)))
      (> (bbv-range-lo r) (bbv-range-up r))))

;*---------------------------------------------------------------------*/
;*    *fixnum-range* ...                                               */
;*---------------------------------------------------------------------*/
(define *fixnum-range*
   (instantiate::bbv-range
      (lo (bbv-min-fixnum))
      (up (bbv-max-fixnum))))

;*---------------------------------------------------------------------*/
;*    fixnum-range ...                                                 */
;*---------------------------------------------------------------------*/
(define (fixnum-range)
   *fixnum-range*)

;*---------------------------------------------------------------------*/
;*    fixnum->range ...                                                */
;*---------------------------------------------------------------------*/
(define (fixnum->range n)
   (instantiate::bbv-range
      (lo n)
      (up n)))

;*---------------------------------------------------------------------*/
;*    *vlen-range* ...                                                 */
;*---------------------------------------------------------------------*/
(define *vlen-range*
   (instantiate::bbv-range
      (lo 0)
      (up (bbv-max-fixnum))))

;*---------------------------------------------------------------------*/
;*    vlen-range ...                                                   */
;*---------------------------------------------------------------------*/
(define (vlen-range)
   *vlen-range*)

;*---------------------------------------------------------------------*/
;*    vlen->range ...                                                  */
;*---------------------------------------------------------------------*/
(define (vlen->range reg ctx)
   
   (define (vlen-low reg ctx)
      (with-access::bbv-ctx ctx (entries)
	 (let loop ((low 0)
		    (entries entries))
	    (if (null? entries)
		low
		(with-access::bbv-ctxentry (car entries) (value)
		   (if (isa? value bbv-range)
		       (with-access::bbv-range value (lo up)
			  (if (bbv-vlen? up)
			      (with-access::bbv-vlen up (vec offset)
				 (if (eq? vec reg)
				     (let ((minlen (-fx lo offset)))
					(loop (maxfx low minlen) (cdr entries)))
				     (loop low (cdr entries))))
			      (loop low (cdr entries))))
		       (loop low (cdr entries))))))))
	 
   (if *bbv-optim-vlength*
       (let ((val (instantiate::bbv-vlen
		     (vec reg)
		     (offset 0))))
	  (instantiate::bbv-range
	     (lo (vlen-low reg ctx))
	     (up val)))
       *vlen-range*))

;*---------------------------------------------------------------------*/
;*    range-type? ...                                                  */
;*---------------------------------------------------------------------*/
(define (range-type? ty)
   (or (eq? ty *bint*) (eq? ty *long*)))

;*---------------------------------------------------------------------*/
;*    rtl-range ...                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-range i ctx)
   (with-trace 'bbv-range (format "rtl-range [@~a]" (gendebugid))
      (trace-item "ins=" (shape i))
      (trace-item "ctx=" (shape ctx))
      (cond
	 ((isa? i rtl_reg)
	  (trace-item "rtl_reg=" (shape i))
	  (trace-item "ctx=" (shape ctx))
	  (let ((e (bbv-ctx-get ctx i)))
	     (when e
		(with-access::bbv-ctxentry e (types value polarity)
		   (trace-item "value=" (shape value))
		   (trace-item "types=" (shape types))
		   (trace-item "polarity=" polarity)
		   (when (and polarity (any range-type? types))
		      (when (bbv-range? value)
			 value))))))
	 ((rtl_ins-mov? i)
	  (trace-item "rtl_mov")
	  (rtl-range (car (rtl_ins-args i)) ctx))
	 ((rtl_ins-strlen? i)
	  ;; similar to vlen
	  (trace-item "rtl_strlen")
	  (with-access::rtl_ins i (dest args)
	     (cond
		((not (rtl_reg? (car args)))
		 (vlen-range))
		((bbv-ctx-get ctx (car args))
		 =>
		 (lambda (e)
		    (vlen->range (car args) ctx)))
		(else
		 (vlen-range)))))
	 ((rtl_ins-vlen? i)
	  (trace-item "rtl_vlen")
	  (with-access::rtl_ins i (dest args)
	     (cond
		((not (rtl_reg? (car args)))
		 (vlen-range))
		((bbv-ctx-get ctx (car args))
		 =>
		 (lambda (e)
		    (vlen->range (car args) ctx)))
		(else
		 (vlen-range)))))
	 ((rtl_ins-call? i)
	  (trace-item "rtl_call")
	  (with-access::rtl_ins i (fun)
	     (with-access::rtl_call fun (var)
		(cond
		   ((eq? var *int->long*)
		    (rtl-range (car (rtl_ins-args i)) ctx))
		   ((eq? var *long->bint*)
		    (rtl-range (car (rtl_ins-args i)) ctx))
		   ((eq? var *bint->long*)
		    (rtl-range (car (rtl_ins-args i)) ctx))
		   (else
		    #f)))))
	 ((rtl_ins-loadi? i)
	  (trace-item "rtl_loadi")
	  (with-access::rtl_ins i (fun)
	     (with-access::rtl_loadi fun (constant)
		(with-access::atom constant (value)
		   (when (fixnum? value)
		      (fixnum->range value))))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    bbv-singleton? ...                                               */
;*---------------------------------------------------------------------*/
(define (bbv-singleton? rng)
   (and (isa? rng bbv-range)
	(eq? (=rv (bbv-range-up rng) (bbv-range-lo rng)) #t)
	(>rv (bbv-range-up rng) (+ (bbv-min-fixnum) 1))
	(<rv (bbv-range-up rng) (- (bbv-max-fixnum) 1))))

;*---------------------------------------------------------------------*/
;*    bbv-range-up-vlen-offset ...                                     */
;*---------------------------------------------------------------------*/
(define (bbv-range-up-vlen-offset x)
   (bbv-vlen-offset (bbv-range-up x)))

;*---------------------------------------------------------------------*/
;*    bbv-range< ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbv-range<::obj x y)
   
   (define (range< x y)
      (cond
	 ((empty-range? x) (if (empty-range? y) #unspecified #t))
	 ((empty-range? y) #t)
	 ((bbv-vlen-vec-eq? (bbv-range-up x) (bbv-range-up y))
	  (if (<fx (bbv-range-up-vlen-offset x) (bbv-range-up-vlen-offset y))
	      (if (eq? (<=rv (bbv-range-lo x) (bbv-range-lo y)) #t)
		  #t
		  #unspecified)
	      #unspecified))
	 ((eq? (<rv (bbv-range-up x) (bbv-range-lo y)) #t) #t)
	 ((eq? (>=rv (bbv-range-lo x) (bbv-range-up y)) #t) #f)
	 (else #unspecified)))

   (with-trace 'bbv-range (format "bbv-range< [@~a]" (gendebugid))
      (trace-item "x=" (shape x))
      (trace-item "y=" (shape y))
      (trace-item "<- " (range< x y))
      (range< x y)))

;*---------------------------------------------------------------------*/
;*    bbv-range<= ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range<=::obj x y)
   (cond
      ((empty-range? x) (if (empty-range? y) #unspecified #t))
      ((empty-range? y) #t)
      ((bbv-vlen-vec-eq? (bbv-range-up x) (bbv-range-up y))
       (if (<=fx (bbv-range-up-vlen-offset x) (bbv-range-up-vlen-offset y))
	   (if (eq? (<=rv (bbv-range-lo x) (bbv-range-lo y)) #t)
	       #t
	       #unspecified)
	   #unspecified))
      ((eq? (<=rv (bbv-range-up x) (bbv-range-lo y)) #t) #t)
      ((eq? (>rv (bbv-range-lo x) (bbv-range-up y)) #t) #f)
      (else #unspecified)))

;*---------------------------------------------------------------------*/
;*    bbv-range> ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbv-range>::obj x y)
   
   (define (range> x y)
      (cond
	 ((empty-range? x) (if (empty-range? y) #unspecified #t))
	 ((empty-range? y) #t)
	 ((bbv-vlen-vec-eq? (bbv-range-up x) (bbv-range-up y))
	  (if (>fx (bbv-range-up-vlen-offset x) (bbv-range-up-vlen-offset y))
	      (if (eq? (>= (bbv-range-lo x) (bbv-range-lo y)) #t)
		  #t
		  #unspecified)))
	 ((eq? (>rv (bbv-range-lo x) (bbv-range-up y)) #t) #t)
	 ((eq? (<=rv (bbv-range-up x) (bbv-range-lo y)) #t) #f)
	 (else #unspecified)))
   
   (with-trace 'bbv-range (format "bbv-range> [@~a]" (gendebugid))
      (trace-item "x=" (shape x))
      (trace-item "y=" (shape y))
      (trace-item "<- " (range> x y))
      (range> x y)))

;*---------------------------------------------------------------------*/
;*    bbv-range>= ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range>=::obj x y)
   (cond
      ((empty-range? x) (if (empty-range? y) #unspecified #t))
      ((empty-range? y) #t)
      ((bbv-vlen-vec-eq? (bbv-range-up x) (bbv-range-up y))
       (if (>=fx (bbv-range-up-vlen-offset x) (bbv-range-up-vlen-offset y))
	   (if (eq? (>= (bbv-range-lo x) (bbv-range-lo y)) #t)
	       #t
	       #unspecified)))
      ((eq? (>=rv (bbv-range-lo x) (bbv-range-up y)) #t) #t)
      ((eq? (<rv (bbv-range-up x) (bbv-range-lo y)) #t) #f)
      (else #unspecified)))

;*---------------------------------------------------------------------*/
;*    bbv-range= ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbv-range=::obj x y)
   (cond
      ((or (empty-range? x) (empty-range? y))
       #unspecified)
      ((and (eq? (=rv (bbv-range-lo x) (bbv-range-up x)) #t)
	    (eq? (=rv (bbv-range-lo y) (bbv-range-up y)) #t)
	    (eq? (=rv (bbv-range-lo x) (bbv-range-up y)) #t))
       #t)
      ((and (eq? (=rv (bbv-range-lo x) (bbv-range-up x)) #t)
	    (eq? (=rv (bbv-range-lo y) (bbv-range-up y)) #t)
	    (eq? (=rv (bbv-range-lo x) (bbv-range-up y)) #f))
       #f)
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    bbv-range-lt ...                                                 */
;*    -------------------------------------------------------------    */
;*    Narrow X so that it is smaller than Y. The result is             */
;*    smaller or equal to X.                                           */
;*---------------------------------------------------------------------*/
(define (bbv-range-lt x y)
   (with-trace 'bbv-range "bbv-range-lt"
      (let ((xl (bbv-range-lo x))
	    (xu (bbv-range-up x))
	    (yl (bbv-range-lo y))
	    (yu (bbv-range-up y)))
	 ;; [xl..xu] < [yl..yu] => [xl...min(xu, yu-1)]
	 (let ((res (instantiate::bbv-range
		       (lo xl)
		       (up (minrv-up xu (-- yu) xu)))))
	    (trace-item (shape x) " < " (shape y))
	    (trace-item "    xl=" xl)
	    (trace-item "    xu=" xu)
	    (trace-item "    yu=" yu)
	    (trace-item "  --yu=" (-- yu))
	    (trace-item " minrv=" (minrv-up xu (-- yu)))
	    (trace-item "     =>" (shape res))
	    res))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lte ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-lte x y)
   (let ((xl (bbv-range-lo x))
	 (xu (bbv-range-up x))
	 (yl (bbv-range-lo y))
	 (yu (bbv-range-up y)))
      ;; [xl..xu] <= [yl..yu] => [xl...min(xu, yu)]
      (let ((res (instantiate::bbv-range
		    (lo xl)
		    (up (minrv-up xu yu xu)))))
	 res)))

;*---------------------------------------------------------------------*/
;*    bbv-range-gt ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-gt x y)
   (let ((xl (bbv-range-lo x))
	 (xu (bbv-range-up x))
	 (yl (bbv-range-lo y))
	 (yu (bbv-range-up y)))
      ;; [xl..xu] > [yl..yu] => [max(xl, yl+1)..xu]
      (let ((res (instantiate::bbv-range
		    (lo (maxrv-lo xl (++ yl) xl))
		    (up xu))))
	 res)))

;*---------------------------------------------------------------------*/
;*    bbv-range-gte ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-gte x y)
   (with-trace 'bbv-range "bbv-range-gte"
      (let ((xl (bbv-range-lo x))
	    (xu (bbv-range-up x))
	    (yl (bbv-range-lo y))
	    (yu (bbv-range-up y)))
	 ;; [xl..xu] >= [yl..yu] => [max(xl, yl)..xu]
	 (let ((res (instantiate::bbv-range
		       (lo (maxrv-lo xl yl xl))
		       (up xu))))
	    (trace-item (shape x) " >= " (shape y))
	    (trace-item "    xl=" xl)
	    (trace-item "    xu=" xu)
	    (trace-item "    yl=" yl)
	    (trace-item " maxrv=" (maxrv xl yl))
	    (trace-item "     =>" (shape res))
	    res))))

;*---------------------------------------------------------------------*/
;*    bbv-range-eq ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-eq x y)
   (with-trace 'bbv-range "bbv-range-eq"
      (bbv-range-intersection x y)))

;*---------------------------------------------------------------------*/
;*    bbv-range-neq ...                                                */
;*    -------------------------------------------------------------    */
;*    Return a smaller ranger from X if it is not Y.                   */
;*---------------------------------------------------------------------*/
(define (bbv-range-neq x y)
   (with-trace 'bbv-range "bbv-range-neq"
      (cond
	 ((bbv-singleton? x)
	  x)
	 ((bbv-singleton? y)
	  (let ((yv (bbv-range-lo y)))
	     (cond
		((equal? yv (bbv-range-lo x))
		 (duplicate::bbv-range x
		    (lo (++ (bbv-range-lo x)))))
		((equal? yv (bbv-range-up x))
		 (duplicate::bbv-range x
		    (up (-- (bbv-range-up x)))))
		(else
		 x))))
	 (else
	  x))))
   
;*---------------------------------------------------------------------*/
;*    ->range ...                                                      */
;*---------------------------------------------------------------------*/
(define (->range n)
   (cond
      ((fixnum? n)
       (cond
	  ((< n (bbv-min-fixnum)) -inf.0)
	  ((> n (bbv-max-fixnum)) +inf.0)
	  (else n)))
      ((bignum? n)
       (if (> n 0) +inf.0 -inf.0))
      (else
       n)))

;*---------------------------------------------------------------------*/
;*    +rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (+rv x y def)
   (cond
      ((and (number? x) (number? y))
       (->range (+ x y)))
      ((and (bbv-vlen? x) (bbv-vlen? y))
       (if (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	   (let ((no (+fx (bbv-vlen-offset x) (bbv-vlen-offset y) )))
	      (duplicate::bbv-vlen x
		 (offset no)))
	   (bbv-max-fixnum)))
      ((and (bbv-vlen? x) (fixnum? y))
       (let ((no (+fx (bbv-vlen-offset x) y)))
	  (if (<=fx no 0)
	      (duplicate::bbv-vlen x
		 (offset no))
	      (bbv-max-fixnum))))
      ((and (fixnum? x) (bbv-vlen? y))
       (+rv y x def))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    -rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (-rv x y def)
   (cond
      ((and (number? y) (number? y))
       (+rv x (- y) def))
      ((and (bbv-vlen? x) (bbv-vlen? y) )
       (if (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	   (let ((no (-fx (bbv-vlen-offset x) (bbv-vlen-offset y) )))
	      (duplicate::bbv-vlen x
		 (offset no)))
	   (bbv-max-fixnum)))
      ((and (bbv-vlen? x) (fixnum? y))
       (let ((no (-fx (bbv-vlen-offset x) y)))
	  (if (<=fx no 0)
	      (duplicate::bbv-vlen x
		 (offset no))
	      (->range (-fx/ov (bbv-max-fixnum) y)))))
      ((and (fixnum? x) (bbv-vlen? y))
       (let ((no (-fx x (bbv-vlen-offset y))))
	  (->range (-fx/ov x (bbv-max-fixnum)))))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    *rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (*rv x y def)
   (cond
      ((and (number? x) (number? y))
       (->range (* x y)))
      ((and (bbv-vlen? x) (fixnum? y))
       (let ((no (*fx (bbv-vlen-offset x) y)))
	  (if (<fx no 0)
	      (duplicate::bbv-vlen x
		 (offset no))
	      (bbv-max-fixnum))))
      ((and (bbv-vlen? x) (fixnum? y))
       (let ((no (*fx (bbv-vlen-offset x) y)))
	  (if (<fx no 0)
	      (duplicate::bbv-vlen y
		 (offset no))
	      +inf.0)))
      ((and (fixnum? x) (bbv-vlen? y))
       (*rv y x def))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-add ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-add x::bbv-range y::bbv-range)
   (let ((yl (bbv-range-lo y))
	 (yu (bbv-range-up y))
	 (xl (bbv-range-lo x))
	 (xu (bbv-range-up x)))
      (instantiate::bbv-range
	 (lo (+rv xl yl -inf.0))
	 (up (+rv xu yu +inf.0)))))

;*---------------------------------------------------------------------*/
;*    bbv-range-sub ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-sub x::bbv-range y::bbv-range)
   (let ((yl (bbv-range-lo y))
	 (yu (bbv-range-up y))
	 (xl (bbv-range-lo x))
	 (xu (bbv-range-up x)))
      (instantiate::bbv-range
	 (lo (-rv xl yu -inf.0))
	 (up (-rv xu yl +inf.0)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-mul ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-mul x::bbv-range y::bbv-range)
   (let* ((yl (bbv-range-lonum y))
	  (yu (bbv-range-upnum y))
	  (xl (bbv-range-lonum x))
	  (xu (bbv-range-upnum x))
	  (v0 (*rv xl yl #unspecified))
	  (v1 (*rv xl yu #unspecified))
	  (v2 (*rv xu yu #unspecified))
	  (v3 (*rv yu yl #unspecified))
	  (mi0 (minrv v0 v1))
	  (mi1 (minrv v2 v3))
	  (ma0 (maxrv v0 v1))
	  (ma1 (maxrv v2 v3))
	  (nlo (minrv (minrv mi0 mi1) (minrv xl yl)))
	  (nup (maxrv (maxrv ma0 ma1) (maxrv xu yu))))
      (instantiate::bbv-range
	 (lo (if (eq? nlo #unspecified) bbv-min-fixnum nlo))
	 (up (if (eq? nup #unspecified) bbv-max-fixnum nup)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-union ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-union::bbv-range x::bbv-range y::bbv-range)
   (with-trace 'bbv-range "bbv-range-union"
      (let ((res (instantiate::bbv-range
		    (lo (minrv-up (bbv-range-lo x) (bbv-range-lo y) (bbv-min-fixnum)))
		    (up (maxrv-lo (bbv-range-up x) (bbv-range-up y) (bbv-max-fixnum))))))
	 (trace-item "x=" (shape x))
	 (trace-item "y=" (shape y))
	 (trace-item "->" (shape res))
	 res)))

;*---------------------------------------------------------------------*/
;*    bbv-range-intersection ...                                       */
;*---------------------------------------------------------------------*/
(define (bbv-range-intersection::bbv-range x::bbv-range y::bbv-range)
   (with-trace 'bbv-range "bbv-range-intersection"
      (let ((res (instantiate::bbv-range
		    (lo (maxrv-lo (bbv-range-lo x) (bbv-range-lo y) (bbv-min-fixnum)))
		    (up (minrv-up (bbv-range-up x) (bbv-range-up y) (bbv-max-fixnum))))))
	 (trace-item "x=" (shape x))
	 (trace-item "y=" (shape y))
	 (trace-item "->" (shape res))
	 res)))
