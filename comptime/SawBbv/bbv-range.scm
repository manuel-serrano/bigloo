;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-range.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Fri Sep 16 09:17:21 2022 (serrano)                */
;*    Copyright   :  2022 manuel serrano                               */
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
	    saw_bbv-cache
	    saw_bbv-types)
   
   (export (class bbv-range
	      (lo read-only)
	      (up read-only))

	   (inline bbv-max-fixnum::long)
	   (inline bbv-min-fixnum::long)
	   (inline bbv-range?::bool ::obj)
	   (bbv-range-fixnum?::bool ::bbv-range)
	   (fixnum-range::bbv-range)
	   (fixnum->range::bbv-range ::long)
	   (rtl-range::obj ::obj ::bbv-ctx)
	   (range-type?::bool ::obj)
	   (bbv-singleton?::bool ::obj)
	   (bbv-range<? ::bbv-range ::bbv-range)
	   (bbv-range<=? ::bbv-range ::bbv-range)
	   (bbv-range>? ::bbv-range ::bbv-range)
	   (bbv-range>=? ::bbv-range ::bbv-range)
	   (bbv-range=? ::bbv-range ::bbv-range)
	   (bbv-range-lt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-lte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-eq::bool ::bbv-range ::bbv-range)
	   (bbv-range-add::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-sub::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-mul::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-intersection::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-merge::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-widen::bbv-range ::bbv-range ::bbv-range)))

;*---------------------------------------------------------------------*/
;*    integer boundaries ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (bbv-max-fixnum)
   (bit-lsh 1 (-fx (bigloo-config 'int-size) 2)))
(define-inline (bbv-min-fixnum)
   (-fx (negfx (bbv-max-fixnum)) 1))

(define (++ i)
   (if (eq? i bbv-max-fixnum)
       bbv-max-fixnum
       (+ i 1)))

(define (-- i)
   (if (eq? i bbv-min-fixnum)
       bbv-min-fixnum
       (- i 1)))

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
      (and (fixnum? lo) (fixnum? up))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lo ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lo o)
  (with-access::bbv-range o (lo)
      lo))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-up ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-up o)
   (with-access::bbv-range o (up)
      up))
   
;*---------------------------------------------------------------------*/
;*    shape ::bbv-range ...                                            */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-range)
   
   (define (shape n)
      (cond
	 ((eq? n (bbv-max-fixnum)) "upfx")
	 ((eq? n (-fx (bbv-max-fixnum) 1)) "upfx-1")
	 ((eq? n (-fx (bbv-max-fixnum) 2)) "upfx-2")
	 ((eq? n (bbv-min-fixnum)) "lofx")
	 ((eq? n (+fx (bbv-min-fixnum) 1)) "lofx+1")
	 ((eq? n (+fx (bbv-min-fixnum) 2)) "lofx+2")
	 ((fixnum? n) n)
	 ((not (flonum? n)) n)
	 ((infinitefl? n) (if (<fl n 0.0) "-inf" "+inf"))
	 (else n)))
   
   (with-access::bbv-range e (lo up)
      (format "[~a..~a]" (shape lo) (shape up))))

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
;*    range-type? ...                                                  */
;*---------------------------------------------------------------------*/
(define (range-type? ty)
   (or (eq? ty *bint*) (eq? ty *long*)))

;*---------------------------------------------------------------------*/
;*    rtl-range ...                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-range i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (bbv-ctx-get ctx i)))
	  (when e
	     (with-access::bbv-ctxentry e (types value polarity)
		(when (and polarity (any range-type? types))
		   (when (bbv-range? value)
		      value))))))
      ((rtl_ins-mov? i)
       (rtl-range (car (rtl_ins-args i)) ctx))
      ((rtl_ins-call? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_call fun (var)
	     (cond
		((eq? var *int->long*)
		 (rtl-range (car (rtl_ins-args i)) ctx))
		((eq? var *bint->long*)
		 (rtl-range (car (rtl_ins-args i)) ctx))
		(else
		 #f)))))
      ((rtl_ins-loadi? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_loadi fun (constant)
	     (with-access::atom constant (value)
		(when (fixnum? value)
		   (fixnum->range value))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bbv-singleton? ...                                               */
;*---------------------------------------------------------------------*/
(define (bbv-singleton? rng)
   (and (isa? rng bbv-range)
	(=fx (bbv-range-up rng) (bbv-range-lo rng))
	(> (bbv-range-up rng) (+fx (bbv-min-fixnum) 1))
	(< (bbv-range-up rng) (-fx (bbv-max-fixnum) 1))))

;*---------------------------------------------------------------------*/
;*    bbv-range<? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range<? left right)
   (cond
      ((<fx (bbv-range-up left) (bbv-range-lo right)) 'true)
      ((>=fx (bbv-range-lo left) (bbv-range-up right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range<=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range<=? left right)
   (cond
      ((<=fx (bbv-range-up left) (bbv-range-lo right)) 'true)
      ((>fx (bbv-range-lo left) (bbv-range-up right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range>? left right)
   (cond
      ((>fx (bbv-range-lo left) (bbv-range-up right)) 'true)
      ((<=fx (bbv-range-up left) (bbv-range-lo right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range>=? left right)
   (cond
      ((>=fx (bbv-range-lo left) (bbv-range-up right)) 'true)
      ((<fx (bbv-range-up left) (bbv-range-lo right)) 'false)
      (else #f)))
      

;*---------------------------------------------------------------------*/
;*    bbv-range=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range=? left right)
   (cond
      ((and (=fx (bbv-range-lo left) (bbv-range-lo right))
	    (=fx (bbv-range-up left) (bbv-range-up right)))
       'true)
      (else
       'false)))

;* {*---------------------------------------------------------------------*} */
;* {*    bbv-range-compare ...                                            *} */
;* {*---------------------------------------------------------------------*} */
;* (define-expander (bbv-range-compare x e)                            */
;*    (match-case x                                                    */
;*       ((?op ?left ?right                                            */
;* 	  :case0 ?case0                                                */
;* 	  :case1 ?case1                                                */
;* 	  :case2 ?case2                                                */
;* 	  :case3 ?case3                                                */
;* 	  :case4 ?case4                                                */
;*                                                                     */
;* 	  )                                                            */
;*        `(let ((li (bbv-range-lo left))                             */
;* 	      (la (bbv-range-up left))                                */
;* 	      (ri (bbv-range-lo right))                               */
;* 	      (ra (bbv-range-up right)))                              */
;* 	   (cond                                                       */
;* 	      ((and (= li ri) (= la ra))                               */
;* 	       ;; [10..20]                                             */
;* 	       ;; [10..20]                                             */
;* 	       case0)                                                  */
;* 	      ((< la ri)                                               */
;* 	       ;; [10..20]                                             */
;* 	       ;;          [30..40]                                    */
;* 	       case1)                                                  */
;* 	      ((> li ra)                                               */
;* 	       ;;          [30..40]                                    */
;* 	       ;; [10..20]                                             */
;* 	       case2)                                                  */
;* 	      ((and (< li ri) (> la ri) (< la ra))                     */
;* 	       ;; [10....20]                                           */
;* 	       ;;    [15....30]                                        */
;* 	       case3)                                                  */
;* 	      ((and (< li ri) (> la ra))                               */
;* 	       ;; [10.........40]                                      */
;* 	       ;;    [20..30]                                          */
;* 	       case4)                                                  */
;* 	      ((and (< li ri) (= la ra))                               */
;* 	       ;; [10.....30]                                          */
;* 	       ;;    [20..30]                                          */
;* 	       case5)                                                  */
;* 	       ;; => [10..20]                                          */
;* 	      ((and (> li ri) (> la ra))                               */
;* 	       ;;    [15....30]                                        */
;* 	       ;; [10.....20]                                          */
;* 	       case6)                                                  */
;* 	  ((and (> li ri) (= la ra))                                   */
;* 	   ;;    [15..20]                                              */
;* 	   ;; [10.....20]                                              */
;* 	   ;; => [10..20]                                              */
;* 	   right)                                                      */
;* 	  ((> li ra)                                                   */
;* 	   ;;          [30..40]                                        */
;* 	   ;; [10..20]                                                 */
;* 	   ;; => [-inf...+inf] (error)                                 */
;* 	   *fixnum-range*)                                             */
;* 	  ((and (> li ri) (< la ra))                                   */
;* 	   ;;    [20...30]                                             */
;* 	   ;; [10.........40]                                          */
;* 	   ;; => [20...30]                                             */
;* 	   left)                                                       */
;* 	  ((and (> li ri) (= la ra))                                   */
;* 	   ;;    [20...40]                                             */
;* 	   ;; [10......40]                                             */
;* 	   ;; => [10...40]                                             */
;* 	   right)                                                      */
;* 	  (else                                                        */
;* 	   (error op "should not be here"                              */
;* 	      (format "~a/~a" (shape ,left) (shape ,right)))))))       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    bbv-range-lts ...                                                *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Enforce left <= right                                            *} */
;* {*---------------------------------------------------------------------*} */
;* (define (bbv-range-lts left::bbv-range right::bbv-range shift::int) */
;*    (let ((li (bbv-range-lo left))                                  */
;* 	 (la (bbv-range-up left))                                     */
;* 	 (ri (+fx (bbv-range-lo right) shift))                        */
;* 	 (ra (+fx (bbv-range-up right) shift)))                       */
;*       (cond                                                         */
;* 	 ((< la ri)                                                    */
;* 	  ;; [10..20]                                                  */
;* 	  ;;          [30..40]                                         */
;* 	  ;; => [10..20]                                               */
;* 	  left)                                                        */
;* 	 ((and (< li ri) (> la ri) (< la ra))                          */
;* 	  ;; [10....20]                                                */
;* 	  ;;    [15....30]                                             */
;* 	  ;; => [10..15]                                               */
;* 	  (instantiate::bbv-range                                      */
;* 	     (lo li)                                                  */
;* 	     (up ri)))                                                */
;* 	 ((and (< li ri) (> la ra))                                    */
;* 	  ;; [10.........40]                                           */
;* 	  ;;    [20..30]                                               */
;* 	  ;; => [10..20]                                               */
;* 	  (instantiate::bbv-range                                      */
;* 	     (lo li)                                                  */
;* 	     (up ri)))                                                */
;* 	 ((and (< li ri) (= la ra))                                    */
;* 	  ;; [10.....30]                                               */
;* 	  ;;    [20..30]                                               */
;* 	  ;; => [10..20]                                               */
;* 	  (instantiate::bbv-range                                      */
;* 	     (lo li)                                                  */
;* 	     (up ri)))                                                */
;* 	 ((and (= li ri) (= la ra))                                    */
;* 	  ;; [10..20]                                                  */
;* 	  ;; [10..20]                                                  */
;* 	  ;; => [10..20]                                               */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (> la ra))                                    */
;* 	  ;;    [15....30]                                             */
;* 	  ;; [10.....20]                                               */
;* 	  ;; => [10...20]                                              */
;* 	  right)                                                       */
;* 	 ((and (> li ri) (= la ra))                                    */
;* 	  ;;    [15..20]                                               */
;* 	  ;; [10.....20]                                               */
;* 	  ;; => [10..20]                                               */
;* 	  right)                                                       */
;* 	 ((> li ra)                                                    */
;* 	  ;;          [30..40]                                         */
;* 	  ;; [10..20]                                                  */
;* 	  ;; => [-inf...+inf] (error)                                  */
;* 	  *fixnum-range*)                                              */
;* 	 ((and (> li ri) (< la ra))                                    */
;* 	  ;;    [20...30]                                              */
;* 	  ;; [10.........40]                                           */
;* 	  ;; => [20...30]                                              */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (= la ra))                                    */
;* 	  ;;    [20...40]                                              */
;* 	  ;; [10......40]                                              */
;* 	  ;; => [10...40]                                              */
;* 	  right)                                                       */
;* 	 (else                                                         */
;* 	  (error "bbv-range-lts" "should not be here"                  */
;* 	     (format "~a/~a" (shape left) (shape right)))))))          */
       
(define (bbv-range-lt left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      (if (>= li ra)
	  ;; [30..40] < [10..30] => error
	  *fixnum-range*
	  ;; [30..X] < [Y..?] => [31..lo(X, Y-1)]
	  (instantiate::bbv-range
	     (lo li)
	     (up (max li (min la (-- ri))))))))

(define (bbv-range-lte left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      (if (> li ra)
	  ;; [30..40] <= [10..20] => error
	  *fixnum-range*
	  ;; [30..X] <= [Y..?] => [31..lo(X, Y)]
	  (instantiate::bbv-range
	     (lo li)
	     (up (max li (min la ri)))))))

(define (bbv-range-gt left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      (if (<= la ri)
	  ;; [10..20] > [30..40] => error
	  *fixnum-range*
	  ;; [X..20] > [?..Y] => [31..lo(X, Y)]
	  (instantiate::bbv-range
	     (lo (min la (max li (++ ri))))
	     (up la)))))

(define (bbv-range-gte left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      (if (< la ri)
	  ;; [10..20] >= [30..40] => error
	  *fixnum-range*
	  ;; [X..20] >= [?..Y] => [31..lo(X, Y)]
	  (instantiate::bbv-range
	     (lo (min la (max li ri)))
	     (up la)))))

;*    (bbv-range-compare "bbv-range-lt" left right                     */
;*       ;; [10..20] < [10..20] => [10..20]                            */
;*       :case0 left                                                   */
;*       ;; [10..20] < [30..40] => [10..20]                            */
;*       :case1 left                                                   */
;*       ;; [30..40] < [10..20] => error                               */
;*       :case2 *fixnum-range*                                         */
;*       ;; [10..20] < [15..30] => [10..14]                            */
;*       :case3 (instantiate::bbv-range (lo li) (up (-- ra)))        */
;*       ;; [10..40] < [20..30] => [10..19]                            */
;*       :case4 (instantiate::bbv-range (lo li) (up (-- ra)))        */
;*       ;; [10..30] < [20..30] => [10..19]                            */
;*       :case5 (instantiate::bbv-range (lo li) (up (-- ra)))        */
;*       ;; [15..20] < [10..20] => [                                   */
;*       ))                                                            */
;*                                                                     */
;* (define (bbv-range-lte left right)                                  */
;*    (bbv-range-compare "bbv-range-lte" left right                    */
;*       ;; [10..20] <= [10..20] => [10..20]                           */
;*       :case0 left                                                   */
;*       ;; [10..20] <= [30..40] => [10..20]                           */
;*       :case1 left                                                   */
;*       ;; [30..40] <= [10..20] => error                              */
;*       :case2 *fixnum-range*                                         */
;*       ;; [10....20] <= [15....30] => [10..15]                       */
;*       :case3 (instantiate::bbv-range (lo li) (up ra))             */
;*       ;; [10..40] <= [20..30] => [10..20]                           */
;*       :case4 (instantiate::bbv-range (lo li) (up ra))             */
;*       ;; [10..30] <= [20..30] => [10..20]                           */
;*       :case5 (instantiate::bbv-range (lo li) (up ra))             */
;*       ))                                                            */
;*                                                                     */
;* (define (bbv-range-gt left right)                                   */
;*    (bbv-range-compare "bbv-range-gt" left right                     */
;*       ;; [10..20] > [10..20] => [10..20]                            */
;*       :case0 left                                                   */
;*       ;; [10..20] > [30..40] => error                               */
;*       :case1 *fixnum-range*                                         */
;*       ;; [30..40] > [10..20] => [30..40]                            */
;*       :case2 left                                                   */
;*       ;; [10....20] > [15....30] => [16..20]                        */
;*       :case3 (instantiate::bbv-range (lo (++ ri)) (up la))        */
;*       ;; [10..40] > [20..30] => [21..40]                            */
;*       :case4 (instantiate::bbv-range (lo (++ ri)) (up ra))        */
;*       ;; [10..30] > [20..30] => [21..30]                            */
;*       :case5 (instantiate::bbv-range (lo (++ ri)) (up la)         */
;*       ))                                                            */
;*                                                                     */
;* (define (bbv-range-gte left right)                                  */
;*    (bbv-range-compare "bbv-range-gte" left right                    */
;*       ;; [10..20] > [10..20] => [10..20]                            */
;*       :case0 left                                                   */
;*       ;; [10..20] >= [30..40] => error                              */
;*       :case1 *fixnum-range*                                         */
;*       ;; [30..40] >= [10..20] => [30..40]                           */
;*       :case2 left                                                   */
;*       ;; [10....20] >= [15....30] => [15..20]                       */
;*       :case3 (instantiate::bbv-range (lo ri) (up la))             */
;*       ;; [10..40] >= [20..30] => [20..40]                           */
;*       :case4 (instantiate::bbv-range (lo ri) (up ra))             */
;*       ;; [10..30] >= [20..30] => [20..30]                           */
;*       :case5 (instantiate::bbv-range (lo ri) (up la)              */
;*       ))                                                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    bbv-range-gts ...                                                *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Enforce left >= right                                            *} */
;* {*---------------------------------------------------------------------*} */
;* (define (bbv-range-gts left::bbv-range right::bbv-range shift::int) */
;*    (let ((li (bbv-range-lo left))                                  */
;* 	 (la (bbv-range-up left))                                     */
;* 	 (ri (+fx (bbv-range-lo right) shift))                        */
;* 	 (ra (+fx (bbv-range-up right) shift)))                       */
;*       (cond                                                         */
;* 	 ((< la ri)                                                    */
;* 	  ;; [10..20]                                                  */
;* 	  ;;          [30..40]                                         */
;* 	  ;; => [-inf...+inf] (error)                                  */
;* 	  *fixnum-range*)                                              */
;* 	 ((and (< li ri) (> la ri) (< la ra))                          */
;* 	  ;; [10....20]                                                */
;* 	  ;;    [15....30]                                             */
;* 	  ;; => [15..20]                                               */
;* 	  (instantiate::bbv-range                                      */
;* 	     (lo ri)                                                  */
;* 	     (up la)))                                                */
;* 	 ((and (< li ri) (> la ra))                                    */
;* 	  ;; [10.........40]                                           */
;* 	  ;;    [20..30]                                               */
;* 	  ;; => [20..40]                                               */
;* 	  left)                                                        */
;* 	 ((and (< li ri) (= la ra))                                    */
;* 	  ;; [10.....30]                                               */
;* 	  ;;    [20..30]                                               */
;* 	  ;; => [20..30]                                               */
;* 	  (instantiate::bbv-range                                      */
;* 	     (lo ri)                                                  */
;* 	     (up la)))                                                */
;* 	 ((and (= li ri) (= la ra))                                    */
;* 	  ;; [10..20]                                                  */
;* 	  ;; [10..20]                                                  */
;* 	  ;; => [10..20]                                               */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (> la ra))                                    */
;* 	  ;;    [15....30]                                             */
;* 	  ;; [10.....20]                                               */
;* 	  ;; => [15...30]                                              */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (= la ra))                                    */
;* 	  ;;    [15..20]                                               */
;* 	  ;; [10.....20]                                               */
;* 	  ;; => [15..20]                                               */
;* 	  left)                                                        */
;* 	 ((> li ra)                                                    */
;* 	  ;;          [30..40]                                         */
;* 	  ;; [10..20]                                                  */
;* 	  ;; => [30..40]                                               */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (< la ra))                                    */
;* 	  ;;    [20...30]                                              */
;* 	  ;; [10.........40]                                           */
;* 	  ;; => [20...30]                                              */
;* 	  left)                                                        */
;* 	 ((and (> li ri) (= la ra))                                    */
;* 	  ;;    [20...40]                                              */
;* 	  ;; [10......40]                                              */
;* 	  ;; => [20...40]                                              */
;* 	  left)                                                        */
;* 	 (else                                                         */
;* 	  (tprint "BAD " li " " la " " (shape left))                   */
;* 	  (tprint "    " ri " " ra " " (shape right))                  */
;* 	  (error "bbv-range-gts" "should not be here"                  */
;* 	     (format "~a/~a" (shape left) (shape right)))))))          */
;*                                                                     */
;* (define (bbv-range-gt left right)                                   */
;*    (bbv-range-gts left right 1))                                    */
;*                                                                     */
;* (define (bbv-range-gte left right)                                  */
;*    (bbv-range-gts left right 0))                                    */

;*---------------------------------------------------------------------*/
;*    bbv-range-eq ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-eq left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lo right))
	  (ra (bbv-range-up right))
	  (li (bbv-range-lo left))
	  (la (bbv-range-up left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (when (<= oi oa)
	 (instantiate::bbv-range
	    (lo oi)
	    (up oa)))))

;*---------------------------------------------------------------------*/
;*    normalize ...                                                    */
;*---------------------------------------------------------------------*/
(define (normalize n)
   (cond
      ((< n (bbv-min-fixnum)) -inf.0)
      ((> n (bbv-max-fixnum)) +inf.0)
      (else n)))

;*---------------------------------------------------------------------*/
;*    +rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (+rng x y)
   (cond
      ((=fx x (bbv-max-fixnum)) (bbv-max-fixnum))
      ((=fx y (bbv-max-fixnum)) (bbv-max-fixnum))
      ((=fx x (bbv-min-fixnum)) (+fx (bbv-min-fixnum) 1))
      ((=fx x (+fx (bbv-min-fixnum) 1)) x)
      ((=fx y (+fx (bbv-min-fixnum) 1)) y)
      ((and (=fx x (-fx (bbv-max-fixnum) 1)) (>fx y 0)) (bbv-max-fixnum))
      ((and (=fx y (-fx (bbv-max-fixnum) 1)) (>fx x 0)) (bbv-max-fixnum))
      (else (+fx x y))))
      
;*---------------------------------------------------------------------*/
;*    -rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (-rng x y)
   (cond
      ((=fx x (bbv-min-fixnum)) (bbv-min-fixnum))
      ((=fx y (bbv-min-fixnum)) (bbv-min-fixnum))
      ((=fx x (-fx (bbv-max-fixnum) 1)) x)
      ((=fx y (-fx (bbv-max-fixnum) 1)) y)
      ((and (=fx x (-fx (bbv-min-fixnum) 1)) (<fx y 0)) (bbv-min-fixnum))
      ((and (=fx y (-fx (bbv-min-fixnum) 1)) (<fx x 0)) (bbv-min-fixnum))
      (else (-fx x y))))
      
;*---------------------------------------------------------------------*/
;*    *rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (*rng x y)
   (cond
      (else (*fx x y))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-add ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-add left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-lo right))
	 (ra (bbv-range-up right))
	 (li (bbv-range-lo left))
	 (la (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (+rng li ri))
	 (up (+rng la ra)))))

;*---------------------------------------------------------------------*/
;*    bbv-range-sub ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-sub left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-lo right))
	 (ra (bbv-range-up right))
	 (li (bbv-range-lo left))
	 (la (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (-rng li ra))
	 (up (-rng la ri)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-mul ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-mul left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lo right))
	  (ra (bbv-range-up right))
	  (li (bbv-range-lo left))
	  (la (bbv-range-up left))
	  (v0 (*rng li ri))
	  (v1 (*rng li ra))
	  (v2 (*rng la ra))
	  (v3 (*rng ra ri))
	  (mi0 (min v0 v1))
	  (mi1 (min v2 v3))
	  (ma0 (max v0 v1))
	  (ma1 (max v2 v3)))
      (instantiate::bbv-range
	 (lo (min mi0 mi1 li ri))
	 (up (max ma0 ma1 la ra)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-intersection ...                                       */
;*---------------------------------------------------------------------*/
(define (bbv-range-intersection::bbv-range x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (max lox loy))
	    (up (min upx upy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-merge ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-merge x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (min lox loy))
	    (up (max upx upy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-widen ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-widen x::bbv-range y::bbv-range)
   
   (define (widen-lo lox loy)
      (let ((i (min lox loy)))
	 (cond
	    ((>fx i 16) (/fx i 2))
	    ((>=fx i -2) (-fx i 1))
	    ((>=fx i -16) (*fx i 2))
	    ((=fx i (+fx (bbv-min-fixnum) 1)) (bbv-min-fixnum))
	    ((<=fx i -256) (+fx (bbv-min-fixnum) 1))
	    (else (bbv-min-fixnum)))))

   (define (widen-up upx upy)
      (let ((i (max upx upy)))
	 (cond
	    ((<=fx i 2) (+fx i 1))
	    ((<=fx i 256) (*fx i 2))
	    ((=fx i (-fx (bbv-max-fixnum) 1)) (bbv-max-fixnum))
	    ((>=fx i 256) (-fx (bbv-max-fixnum) 1))
	    (else (bbv-max-fixnum)))))
   
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (if (=fx lox loy) lox (widen-lo lox loy)))
	    (up (if (=fx upx upy) upx (widen-up upx upy)))))))
