;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/node.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:14:41 1996                          */
;*    Last change :  Mon Mar  8 18:06:41 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ast's node class definition                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_node
   
   (import type_type
	   ast_var)

   (export (class node::object
	      ;; the location 
	      (loc::obj read-only (default #f))
	      ;; the type of the node (a special type stands for `no-type')
	      ;; don't trust that value, instead, use the typeof function of
	      ;; module type_typeof
	      type::type)

	   (class node/effect::node
	      ;; a side effect field
	      (side-effect? (default #unspecified))
	      ;; a key for set manipulation (exclusively the cfa
	      ;; but to avoid wasting time in node-key this field
	      ;; is in the global tree)
	      (key (default -1)))
	   
	   ;; the atom node
	   (final-class atom::node
	      ;; the atom value
	      value::obj)

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
	      (nodes::obj read-only))

	   ;; the application node
	   (final-class app::node/effect
	      ;; the called function
	      fun::var
	      ;; the actuals of the call
	      args)

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
	      (strength::symbol (default '???)))
	   
	   ;; the extern class that describes all the primitive constructions
	   (class extern::node/effect
	      ;; the parameters of the extern form
	      (expr*::pair-nil (default '()))
	      ;; the effect of this extern expression
	      (effect (default #unspecified)))
	      
	   ;; the pragma node
	   (final-class pragma::extern
	      (format::bstring read-only))

	   ;; the object field read and write
	   (final-class getfield::extern
	      ;; the name of the field
	      (fname::bstring read-only)
	      ;; the type of the field
	      (ftype::type read-only)
	      ;; the type the field belongs to
	      (otype::type read-only))
	   
	   ;; the object field read and write
	   (final-class setfield::extern
	      ;; the name of the field
	      (fname::bstring read-only)
	      ;; the type of the field
	      (ftype::type read-only)
	      ;; the type the field belongs to
	      (otype::type read-only))

	   ;; extern object creation
	   (final-class new::extern
	      (args-type::pair-nil (default '())))

	   ;; extern vector object creation
	   (final-class valloc::extern
	      ;; the format of the extern call to be emitted
	      (c-heap-format::bstring read-only)
	      ;; the vector fields type
	      (ftype::type read-only (default *obj*))
	      ;; the type of the dimension
	      (otype::type read-only))

	   ;; vector reference
	   (final-class vref::extern
	      ;; the format of the extern call to be emitted
	      (c-format::bstring read-only)
	      ;; the vector fields type
	      (ftype::type (default *obj*))
	      ;; the type of the offset
	      (otype::type read-only)
	      ;; the type of the vector
	      (vtype::type read-only))

	   ;; vector mutation
	   (final-class vset!::extern
	      ;; the format of the extern call to be emitted
	      (c-format::bstring read-only)
	      ;; the vector fields type
	      (ftype::type (default *obj*))
	      ;; the type of the offset
	      (otype::type read-only)
	      ;; the type of the vector
	      (vtype::type read-only))

	   ;; vector length
	   (final-class vlength::extern
	      ;; the format of the extern call to be emitted
	      (c-format::bstring read-only)
	      ;; the vector fields type
	      (vtype::type read-only))
	   
	   ;; extern predicate
	   (final-class isa::extern
	      (class::type read-only))
	      
	   ;; the null node
	   (final-class cast-null::extern)

	   ;; the cast node
	   (class cast::node
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
	   (final-class select::node/effect
	      ;; the test of the switch
	      test::node
	      ;; the clauses
	      (clauses::obj read-only)
	      ;; item types
	      (item-type::type read-only))

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

	   ;; the make-box construction
	   (final-class make-box::node/effect
	      value::node)

	   ;; the box-ref
	   (final-class box-ref::node/effect
	      var::var)

	   ;; the box-set!
	   (final-class box-set!::node
	      var::var
	      value::node)))
	      

	   
	   
