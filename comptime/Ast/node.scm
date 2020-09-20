;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/node.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:14:41 1996                          */
;*    Last change :  Tue Jun 27 09:10:26 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ast's node class definition                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_node
   
   (import type_type
	   ast_var)

   (include "Ast/nodetype.sch")
   
   (export (class node::object
	      ;; the location 
	      (loc::obj read-only (default #f))
	      ;; the type of node, for var node this has a special meaning.
	      ;; for variables (instances of the class VAR), the type field
	      ;; of the node might differ from the type of the variable
	      ;; because dataflow analysis may have found that the reference
	      ;; at a particular site may has a more precise type than the
	      ;; general variable reference type. For instance, for an
	      ;; expression such as
	      ;; (let ((x::obj EXP))
	      ;;    (if (pair? x) THEN ELSE))
	      ;; in the THEN part, the type of the var node might be ::pair
	      ;; while the type of X is ::obj
	      type::type)

	   (class node/effect::node
	      ;; a side effect field
	      (side-effect (default #unspecified))
	      ;; a key for set manipulation (exclusively the cfa
	      ;; but to avoid wasting time in node-key this field
	      ;; is in the global tree)
	      (key (default -1)))

	   ;; the atom node
	   (abstract-class atom::node
	      ;; the atom value
	      value::obj)

	   ;; the literal node
	   (final-class literal::atom)
	   
	   ;; the patch node (patchable literals)
	   (final-class patch::atom
	      (ref::var read-only)
	      (index::long (default -1))
	      (patchid::obj (default #unspecified)))

	   ;; the variable node
	   (class var::node
	      ;; the variable
	      variable::variable)

	   ;; the functional values
	   (class closure::var)

	   ;; the literal node
	   (final-class kwote::node
	      ;; its value
	      (value::obj read-only))

	   ;; the sequence node
	   (final-class sequence::node/effect
	      (nodes::pair-nil read-only)
	      (unsafe::bool read-only (default #f))
	      (meta::pair-nil read-only (default '())))

	   ;; the application node
	   (final-class app::node/effect
	      ;; the called function
	      fun::var
	      ;; the actuals of the call
	      args
	      ;; can this call be replaced with an equiv. stack allocation
	      (stackable (default #f)))

	   ;; the apply construction node
	   (final-class app-ly::node
	      ;; the called function
	      fun::node
	      ;; and the argument (multiple apply are expanded)
	      arg::node)

	   ;; the computed functional application node
	   (final-class funcall::node
	      ;; the called function
	      fun::node
	      ;; the actuals
	      args
	      ;; the strength (set by the cfa pass)
	      ;; should be ???, LIGHT or ELIGHT
	      (strength::symbol (default '???))
	      ;; the list of potentially called functions
	      (functions::obj (default #unspecified)))
	   
	   ;; the extern class that describes all the primitive constructions
	   (class extern::node/effect
	      ;; the parameters of the extern form
	      (expr*::pair-nil (default '()))
	      ;; the effect of this extern expression
	      (effect (default #unspecified)))
	      
	   ;; the pragma node
	   (final-class pragma::extern
	      (format::bstring read-only))

	   ;; getpatchid node
	   (final-class genpatchid::extern
	      (index::long (default -1))
	      (rindex::long (default -1)))
	   
	   ;; private extern expression
	   (class private::extern
	      (c-format::bstring read-only))
	   
	   ;; the object field read and write
	   (final-class getfield::private
	      ;; the name of the field
	      (fname::bstring read-only)
	      ;; the type of the field
	      ftype::type
	      ;; the type the field belongs to (i.e., the class)
	      otype::type)

	   ;; the object field read and write
	   (final-class setfield::private
	      ;; the name of the field
	      (fname::bstring read-only)
	      ;; the type of the field
	      ftype::type
	      ;; the type the field belongs to (i.e., the class)
	      otype::type)

	   ;; wide chunk access
	   ;; MS CARE 16nov2011 (not used yet)
	   (final-class widening::private
	      otype::type)

	   ;; extern object creation
	   (final-class new::private
	      (args-type::pair-nil (default '())))

	   ;; extern vector object creation
	   (final-class valloc::private
	      ;; the vector fields type
	      ftype::type
	      ;; the type of the dimension
	      otype::type)

	   ;; vector reference
	   (final-class vref::private
	      ;; the vector fields type
	      ftype::type
	      ;; the type of the offset
	      otype::type
	      ;; the type of the vector
	      vtype::type
	      ;; unsafe
	      (unsafe::bool read-only (default #f)))

	   ;; vector mutation
	   (final-class vset!::private
	      ;; the vector fields type
	      ftype::type
	      ;; the type of the offset
	      otype::type
	      ;; the type of the vector
	      vtype::type
	      ;; unsafe
	      (unsafe::bool read-only (default #f)))

	   ;; vector length
	   (final-class vlength::private
	      ;; the type of the vector
	      vtype::type
	      ;; the vector fields type
	      (ftype read-only))
	   
	   ;; extern predicate
	   (final-class instanceof::private
	      class::type)
	      
	   ;; the null node
	   (final-class cast-null::private)

	   ;; the cast node
	   (final-class cast::node
	      ;; the casted node. 
	      arg::node)

	   ;; the affections
	   (final-class setq::node
	      ;; the setted variable
	      var::var
	      ;; and the value
	      value::node)

	   ;; conditionals
	   (final-class conditional::node/effect
	      ;; the test
	      test::node
	      ;; the `then'
	      true::node
	      ;; the `else'
	      false::node)

	   ;; the failure
	   (final-class fail::node
	      ;; the 3 failures arguments
	      proc::node
	      msg::node
	      obj::node)

	   ;; the `case' constructions
	   (final-class switch::node/effect
	      ;; the test of the switch
	      test::node
	      ;; the clauses
	      (clauses::obj read-only)
	      ;; item types
	      item-type::type)

	   ;; the labels constructions
	   (final-class let-fun::node/effect
	      ;; a list of locals variables
	      locals::obj
	      ;; the body of the labels
	      body::node)

	   ;; the let-var constructions
	   (final-class let-var::node/effect
	      ;; a list of bindings
	      bindings::obj
	      ;; the body
	      body::node
	      ;; is this let removable?
	      (removable?::bool (default #t)))

	   ;; the set-exit construction
	   (final-class set-ex-it::node
	      ;; the variable of the set-exit
	      var::var
	      ;; and the body
	      body::node)

	   ;; the jump-exit construction
	   (final-class jump-ex-it::node
	      ;; the variable of the jump-exit
	      exit::node
	      ;; the value
	      value::node)

	   ;;  return retblock
	   (final-class retblock::node
	      body::node)
	   
	   ;; return node
	   (final-class return::node
	      ;; the expression of the return node
	      block::retblock
	      value::node)

	   ;; the make-box construction
	   (final-class make-box::node/effect
	      value::node
	      vtype::type
	      ;; can this box allocated on the stack
	      (stackable (default #f)))

	   ;; the box-ref
	   (final-class box-ref::node/effect
	      var::var
	      vtype::type)

	   ;; the box-set!
	   (final-class box-set!::node
	      var::var
	      value::node
	      vtype::type)

	   ;; synchronization
	   (final-class sync::node
	      mutex::node
	      prelock::node
	      body::node)
	   
	   (strict-node-type::type ::type ::type)
	   (generic node-walk ::node ::procedure)))

;*---------------------------------------------------------------------*/
;*    strict-node-type ...                                             */
;*---------------------------------------------------------------------*/
(define (strict-node-type t1 t2)
   t1)

;*---------------------------------------------------------------------*/
;*    node-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node-walk node::node proc::procedure))

;*---------------------------------------------------------------------*/
;*    node-walk ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::atom proc::procedure)
   (proc node))
 
;*---------------------------------------------------------------------*/
;*    node-walk ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::var proc::procedure)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::kwote proc::procedure)
   (proc node))
       
;*---------------------------------------------------------------------*/
;*    node-walk ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::sequence proc::procedure)
   (node-walk* (sequence-nodes node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::sync proc::procedure)
   (node-walk (sync-mutex node) proc)
   (node-walk (sync-prelock node) proc)
   (node-walk (sync-body node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::app proc::procedure)
   (node-walk (app-fun node) proc)
   (node-walk* (app-args node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::app-ly ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::app-ly proc::procedure)
   (node-walk (app-ly-fun node) proc)
   (node-walk (app-ly-arg node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::funcall proc::procedure)
   (node-walk (funcall-fun node) proc)
   (node-walk* (funcall-args node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::extern ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::extern proc::procedure)
   (node-walk* (extern-expr* node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::cast proc::procedure)
   (node-walk (cast-arg node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::setq proc::procedure)
   (node-walk (setq-value node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::conditional proc::procedure)
   (node-walk (conditional-test node) proc)
   (node-walk (conditional-true node) proc)
   (node-walk (conditional-false node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::fail proc::procedure)
   (node-walk (fail-proc node) proc)
   (node-walk (fail-msg node) proc)
   (node-walk (fail-obj node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::switch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::switch proc::procedure)
   (node-walk (switch-test node) proc)
   (for-each (lambda (clause)
		(node-walk (cdr clause) proc))
	     (switch-clauses node))
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::make-box proc::procedure)
   (node-walk (make-box-value node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::box-ref proc::procedure)
   (node-walk (box-ref-var node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::box-set! proc::procedure)
   (node-walk (box-set!-var node) proc)
   (node-walk (box-set!-value node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::let-fun proc::procedure)
   (with-access::let-fun node (locals body)
      (node-walk body proc)
      (for-each (lambda (local)
		   (let ((sfun (local-value local)))
		      (node-walk (sfun-body sfun) proc)))
		locals)
      (proc node)))

;*---------------------------------------------------------------------*/
;*    node-walk ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::let-var proc::procedure)
   (with-access::let-var node (bindings body)
      (node-walk body proc)
      (for-each (lambda (binding)
		   (node-walk (cdr binding) proc))
		bindings)
      (proc node)))

;*---------------------------------------------------------------------*/
;*    node-walk ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::set-ex-it proc::procedure)
   (node-walk (set-ex-it-body node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-walk node::jump-ex-it proc::procedure)
   (node-walk (jump-ex-it-exit node) proc)
   (node-walk (jump-ex-it-value node) proc)
   (proc node))

;*---------------------------------------------------------------------*/
;*    node-walk* ...                                                   */
;*---------------------------------------------------------------------*/
(define (node-walk* node* proc::procedure)
   (for-each (lambda (node) (node-walk node proc)) node*))

	      

	   
	   
