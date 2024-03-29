;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Defuse/sfun.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 10 07:53:36 2013                          */
;*    Last change :  Sat Jun 19 08:23:11 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Def/Use node property.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module defuse_sfun
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    module_module
	    engine_param
	    defuse_types
	    defuse_set)
   (export (defuse-sfun! ::sfun)
	   (generic defuse ::node)))

;*---------------------------------------------------------------------*/
;*    defuse-sfun! ...                                                 */
;*---------------------------------------------------------------------*/
(define (defuse-sfun! value::sfun)
   (with-access::sfun value (args body)
      (for-each (lambda (l)
		   (widen!::local/defuse l))
	 args)
      (defuse body)))

;*---------------------------------------------------------------------*/
;*    defuse ::node ...                                                */
;*    -------------------------------------------------------------    */
;*    Returns two values: def x use                                    */
;*    Does not store anything in the node                              */
;*---------------------------------------------------------------------*/
(define-generic (defuse n::node)
   (values '() '()))

;*---------------------------------------------------------------------*/
;*    defuse ::var ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::var)
   (with-access::var n ((v variable))
      (values '() (if (isa? v local/defuse) (list v) '()))))

;*---------------------------------------------------------------------*/
;*    defuse* ...                                                      */
;*---------------------------------------------------------------------*/
(define (defuse* nodes::pair-nil def::pair-nil use::pair-nil)
   (let loop ((nodes nodes)
	      (def def)
	      (use use))
      (if (null? nodes)
	  (values def use)
	  (multiple-value-bind (d u)
	     (defuse (car nodes))
	     (loop (cdr nodes) (union d def) (union u use))))))

;*---------------------------------------------------------------------*/
;*    defuse-sequence ...                                              */
;*---------------------------------------------------------------------*/
(define (defuse-sequence nodes::pair-nil)
   (let loop ((nodes nodes)
	      (def '())
	      (use '()))
      (if (null? nodes)
	  (values def use)
	  (multiple-value-bind (d u)
	     (defuse (car nodes))
	     (loop (cdr nodes)
		(union def d)
		(union use (disjonction u def)))))))

;*---------------------------------------------------------------------*/
;*    defuse ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::sequence)
   (with-access::sequence n (nodes)
      (multiple-value-bind (def use)
	 (defuse-sequence nodes)
	 (defuse
	    (widen!::sequence/defuse n
	       (def def)
	       (use use))))))

(define-method (defuse n::sequence/defuse)
   (with-access::sequence/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app)
   (with-access::app n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun)
	 (multiple-value-bind (def use)
	    (defuse* args d u)
	    (defuse 
	       (widen!::app/defuse n
		  (def def)
		  (use use)))))))

(define-method (defuse n::app/defuse)
   (with-access::app/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app-ly)
   (with-access::app-ly n (fun arg)
      (multiple-value-bind (deffun usefun)
	 (defuse fun)
	 (multiple-value-bind (defarg usearg)
	    (defuse arg)
	    (defuse 
	       (widen!::app-ly/defuse n
		  (def (union deffun defarg))
		  (use (union usefun usearg))))))))

(define-method (defuse n::app-ly/defuse)
   (with-access::app-ly/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::funcall)
   (with-access::funcall n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun)
	 (multiple-value-bind (def use)
	    (defuse* args d u)
	    (defuse 
	       (widen!::funcall/defuse n
		  (def def)
		  (use use)))))))

(define-method (defuse n::funcall/defuse)
   (with-access::funcall/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::extern)
   (with-access::extern n (expr*)
      (defuse* expr* '() '())))

;*---------------------------------------------------------------------*/
;*    defuse ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::cast)
   (with-access::cast n (arg)
      (defuse arg)))

;*---------------------------------------------------------------------*/
;*    defuse ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::setq)
   (with-access::setq n (var value)
      (multiple-value-bind (defvalue usevalue)
	 (defuse value)
	 (with-access::var var ((v variable))
	    (when (isa? v local/defuse)
	       (set! defvalue (add v defvalue)))
	    (defuse
	       (widen!::setq/defuse n
		  (def defvalue)
		  (use usevalue)))))))

(define-method (defuse n::setq/defuse)
   (with-access::setq/defuse n (def use)
      (values def use)))
   
;*---------------------------------------------------------------------*/
;*    defuse ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (defuse n::conditional)
   (with-access::conditional n (test true false)
      (multiple-value-bind (deftest usetest)
	 (defuse test)
	 (multiple-value-bind (deftrue usetrue)
	    (defuse true)
	    (multiple-value-bind (deffalse usefalse)
	       (defuse false)
	       (defuse
		  (widen!::conditional/defuse n
		     (def (union deftest (intersection deftrue deffalse)))
		     (use (union usetest (disjonction (union usetrue usefalse) deftest))))))))))

(define-method (defuse n::conditional/defuse)
   (with-access::conditional/defuse n (def use)
      (values def use)))
   
;*---------------------------------------------------------------------*/
;*    defuse ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::fail)
   (with-access::fail n (proc msg obj)
      (multiple-value-bind (defproc useproc)
	 (defuse proc)
	 (multiple-value-bind (defmsg usemsg)
	    (defuse msg)
	    (multiple-value-bind (defobj useobj)
	       (defuse obj)
	       (defuse
		  (widen!::fail/defuse n
		     (def (union defproc defmsg defobj))
		     (use (union useproc usemsg useobj)))))))))

(define-method (defuse n::fail/defuse)
   (with-access::fail/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::switch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::switch)
   (with-access::switch n (test clauses)
      (multiple-value-bind (deftest usetest)
	 (defuse test)
	 ;; compute separatly the def use props of all clauses
	 (let ((defs '())
	       (uses '()))
	    (for-each (lambda (clause)
			 (multiple-value-bind (def use)
			    (defuse (cdr clause))
			    (set! defs (cons def defs))
			    (set! uses (cons use uses))))
	       clauses)
	    (defuse
	       (widen!::switch/defuse n
		  (def (union deftest (apply intersection defs)))
		  (use (union usetest (disjonction (apply union uses) deftest)))))))))

(define-method (defuse n::switch/defuse)
   (with-access::switch/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (defuse n::set-ex-it)
   (with-access::set-ex-it n (var body onexit)
      (multiple-value-bind (defvar usevar)
	 (defuse var)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (multiple-value-bind (defonx useonx)
	       (defuse onexit)
	       (defuse
		  (widen!::set-ex-it/defuse n
		     (def (union defvar (intersection defbody defonx)))
		     (use (union usevar (disjonction (union usebody useonx) defvar))))))))))

(define-method (defuse n::set-ex-it/defuse)
   (with-access::set-ex-it/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (defuse n::jump-ex-it)
   (with-access::jump-ex-it n (exit value)
      (multiple-value-bind (defexit useexit)
	 (defuse exit)
	 (multiple-value-bind (defvalue usevalue)
	    (defuse value)
	    (defuse
	       (widen!::jump-ex-it/defuse n
		  (def (union defexit defvalue))
		  (use (union useexit usevalue))))))))

(define-method (defuse n::jump-ex-it/defuse)
   (with-access::jump-ex-it/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::make-box)
   (with-access::make-box n (value)
      (defuse value)))

;*---------------------------------------------------------------------*/
;*    defuse ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::box-ref)
   (with-access::box-ref n (var)
      (defuse var)))

;*---------------------------------------------------------------------*/
;*    defuse ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::box-set!)
   (with-access::box-set! n (var value)
      (multiple-value-bind (defvalue usevalue)
	 (defuse value)
	 (multiple-value-bind (defvar usevar)
	    (defuse var)
	    (defuse
	       (widen!::box-set!/defuse n
		  (def (union defvalue defvar))
		  (use (union usevalue usevar))))))))

(define-method (defuse n::box-set!/defuse)
   (with-access::box-set!/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::sync)
   (with-access::sync n (mutex prelock body)
      (multiple-value-bind (def use)
	 (defuse-sequence (list prelock mutex body))
	 (defuse
	    (widen!::sync/defuse n
	       (def def)
	       (use use))))))

(define-method (defuse n::sync/defuse)
   (with-access::sync/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::let-var)
   (with-access::let-var n (bindings body)
      ;; bindings evaluation in unorder so cannot
      ;; be treated like a sequence
      (let ((defbindings '())
	    (usebindings '()))
	 (for-each (lambda (b)
		      (widen!::local/defuse (car b))
		      (multiple-value-bind (def use)
			 (defuse (cdr b))
			 (set! defbindings (union def defbindings))
			 (set! usebindings (union use usebindings))))
	    bindings)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (defuse
	       (widen!::let-var/defuse n
		  (def (union defbindings defbody))
		  (use (union usebindings (disjonction usebody defbindings)))))))))

(define-method (defuse n::let-var/defuse)
   (with-access::let-var/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::let-fun)
   (with-access::let-fun n (locals body)
      ;; this is a conservative approach, we assume all functions called
      ;; for use but none called for def
      (let ((defbindings '())
	    (usebindings '()))
	 (for-each (lambda (fun)
		      (multiple-value-bind (def use)
			 (with-access::local fun (value)
			    (defuse-sfun! value))
			 (set! defbindings (union def defbindings))
			 (set! usebindings (union use usebindings))))
	    locals)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (defuse
	       (widen!::let-fun/defuse n
		  (def (union defbody defbindings))
		  (use (union usebody usebindings))))))))

(define-method (defuse n::let-fun/defuse)
   (with-access::let-fun/defuse n (def use)
      (values def use)))

