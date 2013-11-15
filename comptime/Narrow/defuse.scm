;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/defuse.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 10 07:53:36 2013                          */
;*    Last change :  Thu Nov 14 21:29:30 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Def/Use node property.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_defuse
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
	    narrow_types)
   (export (defuse-locals ::node ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    defuse-locals ...                                                */
;*---------------------------------------------------------------------*/
(define (defuse-locals n::node locals::pair-nil)
   (for-each (lambda (l)
		(when (isa? l local/narrow)
		   (with-access::local/narrow l (%count)
		      (set! %count 0))))
      locals)
   (defuse n))
   
;*---------------------------------------------------------------------*/
;*    defuse ::node ...                                                */
;*    -------------------------------------------------------------    */
;*    Returns two values: def x use                                    */
;*    Does not store anything in the node                              */
;*    Only consider local/defuse variables                             */
;*---------------------------------------------------------------------*/
(define-generic (defuse n::node)
   (values '() '()))

;*---------------------------------------------------------------------*/
;*    defuse ::var ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::var)
   (with-access::var n (variable)
      (if (isa? variable local/narrow)
	  (values '() (list variable))
	  (values '() '()))))

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
      (defuse-sequence nodes)))

;*---------------------------------------------------------------------*/
;*    defuse ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app)
   (with-access::app n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun)
	 (defuse* args d u))))

;*---------------------------------------------------------------------*/
;*    defuse ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app-ly)
   (with-access::app-ly n (fun arg)
      (multiple-value-bind (deffun usefun)
	 (defuse fun)
	 (multiple-value-bind (defarg usearg)
	    (defuse arg)
	    (values (union deffun defarg) (union usefun usearg))))))

;*---------------------------------------------------------------------*/
;*    defuse ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::funcall)
   (with-access::funcall n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun)
	 (defuse* args d u))))

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
	 (with-access::var var (variable)
	    (if (isa? variable local/narrow)
		(values (cons variable defvalue) usevalue)
		(values defvalue usevalue))))))

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
	       (values (union deftest (intersection deftrue deffalse))
		  (union usetest (disjonction (union usetrue usefalse) deftest))))))))
	 
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
	       (values (union defproc defmsg defobj)
		  (union useproc usemsg useobj)))))))
      
;*---------------------------------------------------------------------*/
;*    defuse ::select ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::select)
   (with-access::select n (test clauses)
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
	    (values (union deftest (apply intersection defs))
	       (union usetest (disjonction (apply union uses) deftest)))))))

;*---------------------------------------------------------------------*/
;*    defuse ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (defuse n::set-ex-it)
   (with-access::set-ex-it n (var body)
      (multiple-value-bind (defvar usevar)
	 (defuse var)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (values (union defvar defbody) (union usevar usebody))))))

;*---------------------------------------------------------------------*/
;*    defuse ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (defuse n::jump-ex-it)
   (with-access::jump-ex-it n (exit value)
      (multiple-value-bind (defexit useexit)
	 (defuse exit)
	 (multiple-value-bind (defvalue usevalue)
	    (defuse value)
	    (values (union defexit defvalue) (union useexit usevalue))))))

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
      (with-access::var var (variable)
	 (if (isa? variable local/narrow)
	     (values '() (list variable))
	     (values '() '())))))

;*---------------------------------------------------------------------*/
;*    defuse ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::box-set!)
   (with-access::box-set! n (var value)
      (multiple-value-bind (defvalue usevalue)
	 (defuse value)
	 (with-access::var var (variable)
	    (if (isa? variable local/narrow)
		(values (cons variable defvalue) usevalue)
		(values defvalue usevalue))))))

;*---------------------------------------------------------------------*/
;*    defuse ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::sync)
   (with-access::sync n (mutex prelock body)
      (defuse-sequence (list prelock mutex body))))

;*---------------------------------------------------------------------*/
;*    defuse ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::let-fun)
   (with-access::let-fun n (locals body)
      ;; this is a conservative approach, we assume all functions called
      ;; for use but none called for def
      (let ((usebindings '()))
	 (for-each (lambda (fun)
		      (with-access::local fun (value)
			 (with-access::sfun value (body)
			    (multiple-value-bind (def use)
			       (defuse body)
			       (set! usebindings (union use usebindings))))))
	    locals)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (values
	       defbody
	       (union usebindings usebody))))))

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
		      (multiple-value-bind (def use)
			 (defuse (cdr b))
			 (set! defbindings (union def defbindings))
			 (set! usebindings (union use usebindings))))
	    bindings)
	 (multiple-value-bind (defbody usebody)
	    (defuse body)
	    (values
	       (union defbindings defbody)
	       (union usebindings (disjonction usebody defbindings)))))))

;*---------------------------------------------------------------------*/
;*    variable-reset! ...                                              */
;*---------------------------------------------------------------------*/
(define (variable-reset! v)
   (with-access::local/narrow v (%count)
      (set! %count 0)))

;*---------------------------------------------------------------------*/
;*    union ...                                                        */
;*    -------------------------------------------------------------    */
;*    union of N lists of local variables                              */
;*---------------------------------------------------------------------*/
(define (union . ls)
   
   (define res '())
   
   (define (variable-mark! v)
      (with-access::local/narrow v (%count)
	 (when (=fx %count 0)
	    (set! %count 1)
	    (set! res (cons v res)))))

   (for-each (lambda (l) (for-each variable-mark! l)) ls)
   (for-each variable-reset! res)
   res)

;*---------------------------------------------------------------------*/
;*    intersection ...                                                 */
;*    -------------------------------------------------------------    */
;*    intersection of N lists of local variables                       */
;*---------------------------------------------------------------------*/
(define (intersection . ls)
   
   (define (variable-mark! v)
      (with-access::local/narrow v (%count)
	 (set! %count (+fx 1 %count))))
   
   (define (mark-list! l)
      (for-each variable-mark! l))
   
   (for-each mark-list! ls)
   (let* ((%count (length ls))
	  (res (filter (lambda (l)
			  (with-access::local/narrow l ((c %count))
			     (=fx c %count)))
		  (car ls))))
      (for-each (lambda (l) (for-each variable-reset! l)) ls)
      res))

;*---------------------------------------------------------------------*/
;*    disjonction ...                                                  */
;*    -------------------------------------------------------------    */
;*    disjonction of 2 lists of local variables                        */
;*---------------------------------------------------------------------*/
(define (disjonction l1 l2)
   
   (define (variable-mark! v)
      (with-access::local/narrow v (%count)
	 (set! %count (+fx 1 %count))))
   
   (for-each variable-mark! l2)
   (let ((res (filter (lambda (v)
			 (with-access::local/narrow v (%count)
			    (=fx %count 0)))
		 l1)))
      (for-each variable-reset! l2)
      res))
   

   
   
   
