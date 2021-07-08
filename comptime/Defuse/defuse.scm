;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Defuse/defuse.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 10 07:53:36 2013                          */
;*    Last change :  Thu Jul  8 11:44:32 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Def/Use node property with fix point iteration.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module defuse_defuse
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
	   (defuse-get ::node)))

;*---------------------------------------------------------------------*/
;*    defuse-sfun! ...                                                 */
;*---------------------------------------------------------------------*/
(define (defuse-sfun! value::sfun)
   (with-access::sfun value (args body)
      (for-each (lambda (l)
		   (widen!::local/defuse l))
	 args)
      (let ((fix (make-cell #f)))
	 (let loop ()
	    (cell-set! fix #t)
	    (multiple-value-bind (def use)
	       (defuse body fix)
	       (if (cell-ref fix)
		   (values def use)
		   (loop)))))))

;*---------------------------------------------------------------------*/
;*    defuse-get ...                                                   */
;*---------------------------------------------------------------------*/
(define (defuse-get node)
   (defuse node (make-cell #t)))

;*---------------------------------------------------------------------*/
;*    defuse-use-add! ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (defuse-use-add! node use fix)
   (error "defuse-use-add!" "should not be here" (shape node)))

;*---------------------------------------------------------------------*/
;*    defuse ::node ...                                                */
;*    -------------------------------------------------------------    */
;*    Returns two values: def x use                                    */
;*    Does not store anything in the node                              */
;*---------------------------------------------------------------------*/
(define-generic (defuse n::node fix::cell)
   (values '() '()))

;*---------------------------------------------------------------------*/
;*    defuse ::literal ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::literal fix)
   (cell-set! fix #f)
   (defuse 
      (widen!::literal/defuse n
	 (def '())
	 (use '()))
      fix))

(define-method (defuse n::literal/defuse fix)
   (with-access::literal/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::ref ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::ref fix)
   (cell-set! fix #f)
   (with-access::ref n (variable)
      (defuse 
	 (widen!::ref/defuse n
	    (def '())
	    (use (if (isa? variable local) (list variable) '())))
	 fix)))

(define-method (defuse n::ref/defuse fix)
   (with-access::ref/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse* ...                                                      */
;*---------------------------------------------------------------------*/
(define (defuse* nodes::pair-nil def::pair-nil use::pair-nil fix)
   (let loop ((nodes nodes)
	      (def def)
	      (use use))
      (if (null? nodes)
	  (values def use)
	  (multiple-value-bind (d u)
	     (defuse (car nodes) fix)
	     (loop (cdr nodes) (union d def) (union u use))))))

;*---------------------------------------------------------------------*/
;*    defuse-sequence ...                                              */
;*---------------------------------------------------------------------*/
(define (defuse-sequence nodes::pair-nil fix)
   (let loop ((n nodes)
	      (def '())
	      (use '()))
      (if (null? n)
	  ;; propagate backwards the def use properties
	  (if (null? (cdr nodes))
	      (values def use)
	      (let loop ((n (reverse nodes)))
		 (if (null? (cdr n))
		     (values def use)
		     (multiple-value-bind (d u)
			(defuse (car n) fix)
			(defuse-use-add! (cdr n) (disjonction u d) fix)
			(loop (cdr n))))))
	  (multiple-value-bind (d u)
	     (defuse (car n) fix)
	     (loop (cdr n)
		(union def d)
		(union use (disjonction u def)))))))

;*---------------------------------------------------------------------*/
;*    defuse ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::sequence fix)
   (cell-set! fix #f)
   (with-access::sequence n (nodes)
      (multiple-value-bind (def use)
	 (defuse-sequence nodes fix)
	 (defuse
	    (widen!::sequence/defuse n
	       (def def)
	       (use use))
	    fix))))

(define-method (defuse n::sequence/defuse fix)
   (with-access::sequence/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app fix)
   (cell-set! fix #f)
   (with-access::app n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun fix)
	 (multiple-value-bind (def use)
	    (defuse* args d u fix)
	    (defuse 
	       (widen!::app/defuse n
		  (def def)
		  (use use))
	       fix)))))

(define-method (defuse n::app/defuse fix)
   (with-access::app/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::app-ly fix)
   (cell-set! fix #f)
   (with-access::app-ly n (fun arg)
      (multiple-value-bind (deffun usefun)
	 (defuse fun fix)
	 (multiple-value-bind (defarg usearg)
	    (defuse arg fix)
	    (defuse 
	       (widen!::app-ly/defuse n
		  (def (union deffun defarg))
		  (use (union usefun usearg)))
	       fix)))))

(define-method (defuse n::app-ly/defuse fix)
   (with-access::app-ly/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::funcall fix)
   (cell-set! fix #f)
   (with-access::funcall n (fun args)
      (multiple-value-bind (d u)
	 (defuse fun fix)
	 (multiple-value-bind (def use)
	    (defuse* args d u fix)
	    (defuse 
	       (widen!::funcall/defuse n
		  (def def)
		  (use use))
	       fix)))))

(define-method (defuse n::funcall/defuse fix)
   (with-access::funcall/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::extern fix)
   ;;(cell-set! fix #f)
   (with-access::extern n (expr*)
      (defuse* expr* '() '() fix)))

;*---------------------------------------------------------------------*/
;*    defuse ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::cast fix)
   ;;(cell-set! fix #f)
   (with-access::cast n (arg)
      (defuse arg fix)))

;*---------------------------------------------------------------------*/
;*    defuse ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::setq fix)
   (cell-set! fix #f)
   (with-access::setq n (var value)
      (multiple-value-bind (defvalue usevalue)
	 (defuse value fix)
	 (with-access::var var ((v variable))
	    (when (isa? v local/defuse)
	       (set! defvalue (add v defvalue)))
	    (defuse
	       (widen!::setq/defuse n
		  (def defvalue)
		  (use usevalue))
	       fix)))))

(define-method (defuse n::setq/defuse fix)
   (with-access::setq/defuse n (def use)
      (values def use)))
   
;*---------------------------------------------------------------------*/
;*    defuse ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (defuse n::conditional fix)
   (cell-set! fix #f)
   (with-access::conditional n (test true false)
      (multiple-value-bind (deftest usetest)
	 (defuse test fix)
	 (multiple-value-bind (deftrue usetrue)
	    (defuse true fix)
	    (multiple-value-bind (deffalse usefalse)
	       (defuse false fix)
	       (defuse
		  (widen!::conditional/defuse n
		     (def (union deftest (intersection deftrue deffalse)))
		     (use (union usetest (disjonction (union usetrue usefalse) deftest))))
		  fix))))))

(define-method (defuse n::conditional/defuse fix)
   (with-access::conditional/defuse n (def use)
      (values def use)))
   
;*---------------------------------------------------------------------*/
;*    defuse ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::fail fix)
   (cell-set! fix #f)
   (with-access::fail n (proc msg obj)
      (multiple-value-bind (defproc useproc)
	 (defuse proc fix)
	 (multiple-value-bind (defmsg usemsg)
	    (defuse msg fix)
	    (multiple-value-bind (defobj useobj)
	       (defuse obj fix)
	       (defuse
		  (widen!::fail/defuse n
		     (def (union defproc defmsg defobj))
		     (use (union useproc usemsg useobj)))
		  fix))))))

(define-method (defuse n::fail/defuse fix)
   (with-access::fail/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::switch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (defuse n::switch fix)
   (cell-set! fix #f)
   (with-access::switch n (test clauses)
      (multiple-value-bind (deftest usetest)
	 (defuse test fix)
	 ;; compute separatly the def use props of all clauses
	 (let ((defs '())
	       (uses '()))
	    (for-each (lambda (clause)
			 (multiple-value-bind (def use)
			    (defuse (cdr clause) fix)
			    (set! defs (cons def defs))
			    (set! uses (cons use uses))))
	       clauses)
	    (defuse
	       (widen!::switch/defuse n
		  (def (union deftest (apply intersection defs)))
		  (use (union usetest (disjonction (apply union uses) deftest))))
	       fix)))))

(define-method (defuse n::switch/defuse fix)
   (with-access::switch/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (defuse n::set-ex-it fix)
   (cell-set! fix #f)
   (with-access::set-ex-it n (var body onexit)
      (multiple-value-bind (defvar usevar)
	 (defuse var fix)
	 (multiple-value-bind (defbody usebody)
	    (defuse body fix)
	    (multiple-value-bind (defonx useonx)
	       (defuse onexit fix)
	       (defuse
		  (widen!::set-ex-it/defuse n
		     (def (union defvar (intersection defbody defonx)))
		     (use (union usevar (disjonction (union usebody useonx) defvar))))
		  fix))))))

(define-method (defuse n::set-ex-it/defuse fix)
   (with-access::set-ex-it/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (defuse n::jump-ex-it fix)
   (cell-set! fix #f)
   (with-access::jump-ex-it n (exit value)
      (multiple-value-bind (defexit useexit)
	 (defuse exit fix)
	 (multiple-value-bind (defvalue usevalue)
	    (defuse value fix)
	    (defuse
	       (widen!::jump-ex-it/defuse n
		  (def (union defexit defvalue))
		  (use (union useexit usevalue)))
	       fix)))))

(define-method (defuse n::jump-ex-it/defuse fix)
   (with-access::jump-ex-it/defuse n (def use)
      (values def use)))
      
;*---------------------------------------------------------------------*/
;*    defuse ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::make-box fix)
   ;;(cell-set! fix #f)
   (with-access::make-box n (value)
      (defuse value fix)))

;*---------------------------------------------------------------------*/
;*    defuse ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::box-ref fix)
   ;;(cell-set! fix #f)
   (with-access::box-ref n (var)
      (defuse var fix)))

;*---------------------------------------------------------------------*/
;*    defuse ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (defuse n::box-set! fix)
   (cell-set! fix #f)
   (with-access::box-set! n (var value)
      (multiple-value-bind (defvalue usevalue)
	 (defuse value fix)
	 (multiple-value-bind (defvar usevar)
	    (defuse var fix)
	    (defuse
	       (widen!::box-set!/defuse n
		  (def (union defvalue defvar))
		  (use (union usevalue usevar)))
	       fix)))))

(define-method (defuse n::box-set!/defuse fix)
   (with-access::box-set!/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (defuse n::sync fix)
   (cell-set! fix #f)
   (with-access::sync n (mutex prelock body)
      (multiple-value-bind (def use)
	 (defuse-sequence (list prelock mutex body) fix)
	 (defuse
	    (widen!::sync/defuse n
	       (def def)
	       (use use))
	    fix))))

(define-method (defuse n::sync/defuse fix)
   (with-access::sync/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::let-var fix)
   (cell-set! fix #f)
   (with-access::let-var n (bindings body)
      ;; bindings evaluation in unorder so cannot
      ;; be treated like a sequence
      (let ((defbindings '())
	    (usebindings '()))
	 (for-each (lambda (b)
		      (widen!::local/defuse (car b))
		      (multiple-value-bind (def use)
			 (defuse (cdr b) fix)
			 (set! defbindings (union def defbindings))
			 (set! usebindings (union use usebindings))))
	    bindings)
	 (multiple-value-bind (defbody usebody)
	    (defuse body fix)
	    (defuse
	       (widen!::let-var/defuse n
		  (def (union defbindings defbody))
		  (use (union usebindings (disjonction usebody defbindings))))
	       fix)))))

(define-method (defuse n::let-var/defuse fix)
   (with-access::let-var/defuse n (def use)
      (values def use)))

;*---------------------------------------------------------------------*/
;*    defuse ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (defuse n::let-fun fix)
   (cell-set! fix #f)
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
	    (defuse body fix)
	    (defuse
	       (widen!::let-fun/defuse n
		  (def (union defbody defbindings))
		  (use (union usebody usebindings)))
	       fix)))))

(define-method (defuse n::let-fun/defuse fix)
   (with-access::let-fun/defuse n (def use)
      (values def use)))

