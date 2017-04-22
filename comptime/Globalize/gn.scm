;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/gn.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 14:54:22 1995                          */
;*    Last change :  Fri Apr 21 18:47:39 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We compute the G0 and G1 properties which is defined as follow:  */
;*                                                                     */
;*    Let  A(f,g)  <=> f is a free function in g, called by g          */
;*         E(f)    <=> f is a function used as value                   */
;*                                                                     */
;*    then G0(f,g) <=> E(f) v (#g, E(f) ^ A(g,f))                      */
;*         G1(f,g) <=> G0(f) ^ !(E(f))                                 */
;*                                                                     */
;*    # = exists                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_gn
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    globalize_ginfo
	    globalize_globalize)
   (export  (Gn! local* ::node ::variable variable*)))

;*---------------------------------------------------------------------*/
;*    Gn! ...                                                          */
;*    -------------------------------------------------------------    */
;*    In order to compute the E property, we first compute the E       */
;*    set, the set of all escaping functions. During this tree         */
;*    walk, we compute the call-graph (using the fun-Ginfo             */
;*    structure).                                                      */
;*---------------------------------------------------------------------*/
(define (Gn! args node caller g)
   (set! *E* (E node caller g))
   (let loop ((G  *E*)
	      (G1 '()))
      (if (null? G)
	  (begin
	     (set! *G0* (append *E* G1))
	     (set! *G1* G1))
	  (let ((new-G (G-from-cto (car G))))
	     (loop (append new-G (cdr G))
		   (append new-G G1))))))

;*---------------------------------------------------------------------*/
;*    E ...                                                            */
;*---------------------------------------------------------------------*/
(define-generic (E node::node caller::variable g))

;*---------------------------------------------------------------------*/
;*    E ::atom ...                                                     */
;*---------------------------------------------------------------------*/
(define-method (E node::atom caller::variable g)
   g)

;*---------------------------------------------------------------------*/
;*    E ::kwote ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (E node::kwote caller::variable g)
   g)

;*---------------------------------------------------------------------*/
;*    E ::var ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (E node::var caller::variable g)
   g)

;*---------------------------------------------------------------------*/
;*    E ::closure ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (E node::closure caller::variable g)
   (let ((var (var-variable node)))
      (save-fun! caller var)
      (if (and (local? var)
	       ;; du to cfa, `fun' may introduce non escaping functions
	       (local/Ginfo-escape? var)
	       (not (sfun/Ginfo-G? (local-value var))))
	  (begin
	     (sfun/Ginfo-G?-set! (local-value var) #t)
	     (cons var g))
	  g)))
   
;*---------------------------------------------------------------------*/
;*    E ::sequence ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (E node::sequence caller g)
   (E* (sequence-nodes node) caller g))

;*---------------------------------------------------------------------*/
;*    E ::sync ...                                                     */
;*---------------------------------------------------------------------*/
(define-method (E node::sync caller g)
   (E (sync-body node) caller
      (E (sync-prelock node) caller
	 (E (sync-mutex node) caller g))))

;*---------------------------------------------------------------------*/
;*    E ::app ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (E node::app caller g)
   (with-access::app node (fun args)
      (save-app! caller (var-variable fun))
      (E* args caller g)))
 
;*---------------------------------------------------------------------*/
;*    E ::app-ly ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (E node::app-ly caller g)
   (with-access::app-ly node (fun arg)
      (E fun caller (E arg caller g))))

;*---------------------------------------------------------------------*/
;*    E ::funcall ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (E node::funcall caller g)
   (with-access::funcall node (fun args)
      (E fun caller (E* args caller g))))

;*---------------------------------------------------------------------*/
;*    E ::extern ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (E node::extern caller g)
   (with-access::extern node (expr*)
      (E* expr* caller g)))

;*---------------------------------------------------------------------*/
;*    E ::cast ...                                                     */
;*---------------------------------------------------------------------*/
(define-method (E node::cast caller g)
   (with-access::cast node (arg)
      (E arg caller g)))

;*---------------------------------------------------------------------*/
;*    E ::setq ...                                                     */
;*---------------------------------------------------------------------*/
(define-method (E node::setq caller g)
   (with-access::setq node (value)
      (E value caller g)))

;*---------------------------------------------------------------------*/
;*    E ::conditional ...                                              */
;*---------------------------------------------------------------------*/
(define-method (E node::conditional caller g)
   (with-access::conditional node (test true false)
      (E test caller (E true caller (E false caller g)))))

;*---------------------------------------------------------------------*/
;*    E ::fail ...                                                     */
;*---------------------------------------------------------------------*/
(define-method (E node::fail caller g)
   (with-access::fail node (proc msg obj)
      (E proc caller (E msg caller (E obj caller g)))))

;*---------------------------------------------------------------------*/
;*    E ::switch ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (E node::switch caller g)
   (with-access::switch node (clauses test)
      (let loop ((clauses clauses)
		 (g       g))
	 (if (null? clauses)
	     (E test caller g)
	     (loop (cdr clauses) (E (cdr (car clauses)) caller g))))))

;*---------------------------------------------------------------------*/
;*    E ::let-fun ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (E node::let-fun caller g)
   (with-access::let-fun node (body locals)
      (let loop ((locals locals)
		 (g      g))
	 (if (null? locals)
	     (E body caller g)
	     (loop (cdr locals)
		   (E (sfun-body (local-value (car locals)))
		      (car locals)
		      g))))))

;*---------------------------------------------------------------------*/
;*    E ::let-var ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (E node::let-var caller g)
   (with-access::let-var node (body bindings)
      (let loop ((bindings bindings)
		 (g        g))
	 (if (null? bindings)
	     (E body caller g)
	     (loop (cdr bindings)
		   (E (cdr (car bindings)) caller g))))))

;*---------------------------------------------------------------------*/
;*    E ::set-ex-it ...                                                */
;*---------------------------------------------------------------------*/
(define-method (E node::set-ex-it caller g)
   (with-access::set-ex-it node (body)
      (E body caller g)))

;*---------------------------------------------------------------------*/
;*    E ::jump-ex-it ...                                               */
;*---------------------------------------------------------------------*/
(define-method (E node::jump-ex-it caller g)
   (with-access::jump-ex-it node (exit value)
      (E exit caller (E value caller g))))

;*---------------------------------------------------------------------*/
;*    E ::make-box ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (E node::make-box caller g)
   (with-access::make-box node (value)
      (E value caller g)))

;*---------------------------------------------------------------------*/
;*    E ::box-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (E node::box-ref caller g)
   (with-access::box-ref node (var)
      (E var caller g)))

;*---------------------------------------------------------------------*/
;*    E ::box-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (E node::box-set! caller g)
   (with-access::box-set! node (var value)
      (E var caller (E value caller g))))

;*---------------------------------------------------------------------*/
;*    E* ...                                                           */
;*---------------------------------------------------------------------*/
(define (E* node* caller g)
   (let loop ((node* node*)
	      (g     g))
      (if (null? node*)
	  g
	  (loop (cdr node*)
		(E (car node*) caller g)))))
		    
;*---------------------------------------------------------------------*/
;*    save-app! ...                                                    */
;*---------------------------------------------------------------------*/
(define (save-app! caller callee)
   (if (global? callee)
       'done
       (let ((callee-info (local-value callee)))
	  (if (not (memq caller (sfun/Ginfo-cfrom callee-info)))
	      (begin
		 (sfun/Ginfo-cfrom-set! callee-info
					(cons caller
					      (sfun/Ginfo-cfrom callee-info)))
		 (let ((caller-info (variable-value caller)))
		    (sfun/Ginfo-cto-set! caller-info
					 (cons callee
					       (sfun/Ginfo-cto
						caller-info))))))
	  'done)))
	  
;*---------------------------------------------------------------------*/
;*    save-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (save-fun! caller callee)
   (if (or (global? caller) (global? callee))
       'done
       (let ((caller-info (local-value caller)))
	  (trace (globalize 3) "save-fun!: "
	     (shape caller) " " (shape callee)
	     #\Newline)
	  (if (not (memq callee (sfun/Ginfo-efunctions caller-info)))
	      (sfun/Ginfo-efunctions-set! caller-info
		 (cons callee (sfun/Ginfo-efunctions caller-info))))
	  'done)))
	  
;*---------------------------------------------------------------------*/
;*    G-from-cto ...                                                   */
;*---------------------------------------------------------------------*/
(define (G-from-cto local)
   (let loop ((cto (sfun/Ginfo-cto (local-value local)))
	      (G   '()))
      (cond
	 ((null? cto)
	  G)
	 ((sfun/Ginfo-G? (local-value (car cto)))
	  (loop (cdr cto) G))
	 (else
	  (sfun/Ginfo-G?-set! (local-value (car cto)) #t)
	  (loop (cdr cto) (cons (car cto) G))))))
	  
