;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/var.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 15:12:51 1996                          */
;*    Last change :  Thu Feb 18 08:30:07 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The variable class definition                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_var

   (import engine_param
	   type_type)

   (include "Ast/var.sch")
   
   (export (class value)
	   
	   (class variable::object
	      ;; the variable identifier
	      (id::symbol read-only)
	      ;; the target name
	      (name (default #f))
	      ;; the variable type or the function type result
	      type::type
	      ;; its type
	      value::value
	      ;; its access mode (should be either READ, WRITE
	      ;; CELL-CALLCC CELL-GOBALIZE CELL-INTEGRATE)
	      (access (default 'read))
	      ;; a slot for fast alpha conversion
	      (fast-alpha (default #unspecified))
	      ;; does this variable can be removed
	      (removable (default 'now))
	      ;; the variable number of occurrences
	      (occurrence::long (default 0))
	      ;; the variable number of write occurrences
	      (occurrencew::long (default 0))
	      ;; does this local variable belongs to the user ?
	      (user?::bool (default #f)))
	   
	   (final-class global::variable
	      ;; the global's module. this variable is changed only
	      ;; in one place: the module foreign_library
	      ;;    @ref ../Foreign/library.scm:module change@
	      module::symbol
	      ;; the global's importation (is mutated in make_heap@heap_make)
	      ;; should be STATIC, EXPORT, EVAL or FOREIGN.
	      import
	      ;; is this global can be known by eval?
	      (evaluable?::bool (default #t))
	      ;; is the variable eval exported?
	      (eval?::bool (default #f))
	      ;; The library that defines the variable (or #f)
	      (library (default #f))
	      ;; a user pragma about the variable
	      (pragma::obj (default '()))
	      ;; declaration source
	      src::obj
	      ;; the qualified type the global belongs to
	      ;; (for JVM compilation only)
	      jvm-type-name::bstring
	      ;; is the global always initialized before used? #t means
	      ;; that the variable is always initialized, #f means that
	      ;; it is used before initialized, and #unspecified means that
	      ;; the exact property is unknown
	      (init::obj (default #unspecified))
	      ;; an optional alias which is used for aliased imports
	      (alias (default #f)))

	   (final-class local::variable
	      ;; the local's identification key
	      (key::long read-only))

	   (class fun::value
	      ;; the function arity, for non DSSSL optional functions,
	      ;; a positive value means an exact number of args and a
	      ;; negative value means a var-args with at least (-arity - 1)
	      ;; arguments. (lambda l ...) has arity -1 and
	      ;; (lambda (x . y) ...) has arity -2. DSSSL optional functions
	      ;; have arity > 0 where the arity is the number of required
	      ;; arguments. The optional args are described in the optionals
	      ;; field of the SFUN class 
	      (arity::long read-only)
	      ;; side effect field
	      (side-effect (default #unspecified))
	      ;; if this function is a predicate, the type tested
	      (predicate-of (default #f))
	      ;; an associated stack allocator
	      (stack-allocator (default #f))
	      ;; is this function `top' its arguments (for the cfa)
	      (top?::bool (default #t))
	      ;; the associated closure
	      (the-closure (default #unspecified))
	      ;; the effect of this function
	      (effect (default #unspecified))
	      ;; is this function failsafe (#t=yes, #f=no, else=unknown)
	      (failsafe (default #unspecified))
	      ;; non-escaping arguments: #unspecified, *, or an index list
	      ;; non-escaping arguments can be stack allocated
	      (args-noescape (default #unspecified)))

	   (final-class sfun::fun
	      ;; a property list
	      (property::obj (default '()))
	      ;; the formals parameters types
	      args
	      ;; the formals names
	      (args-name read-only)
	      ;; the body
	      (body (default #unspecified))
	      ;; a class (should be SFUN, SIFUN, SNIFUN, SGFUN or SMFUN)
	      class
	      ;; dsssl keyword arguments encoding
	      (dsssl-keywords (default '()))
	      ;; the location, in the source file, of the function declaration.
	      ;; this field is not read-only because it will be set when
	      ;; encountering the global define which will occur after the
	      ;; global declaration (the module clause).
	      (loc (default #unspecified))
	      ;; optional arguments (see fun-arity)
	      (optionals read-only (default '()))
	      ;; key arguments (see fun-arity)
	      (keys read-only (default '()))
	      ;; closure field pointing to the global function
	      (the-closure-global (default #unspecified))
	      ;; the strength (see funcall node) set by the CFA pass
	      ;; should be ???, LIGHT, or ELIGHT
	      (strength::symbol (default '???))
	      ;; can this closure be stack allocated?: #unspecifed, #t, #f
	      (stackable::obj (default #unspecified)))

	   (final-class cfun::fun
	      ;; the formal parameters' type
	      (args-type read-only)
	      ;; is it a macro function
	      (macro?::bool read-only)
	      ;; is it an infix macro ?
	      (infix?::bool (default #f))
	      ;; when compiling for the JVM, the kind of method this foreign is
	      (method::pair-nil (default '())))

	   (final-class svar::value
	      ;; the location, in the source file, of the variable declaration
	      (loc (default #unspecified)))

	   (final-class scnst::value
	      ;; a possible variable value
	      (node read-only)
	      ;; a class (should be SGFUN, SFUN, SSTRING, SREAL, STVECTOR)
	      class
	      ;; the location, in the source file, of the variable declaration
	      (loc (default #unspecified)))

	   (final-class cvar::value
	      ;; is it a macro variable
	      (macro?::bool read-only))

	   (final-class sexit::value
	      ;; the associated handling function
	      handler::obj
	      ;; is the handler detached (after globalize)
	      (detached?::bool (default #f)))

	   (class feffect
	      (read (default '()))
	      (write (default '())))

	   (global-read-only?::bool ::global)
	   (global-set-read-only! ::global)
	   (global-args-safe?::bool ::global)
	   (fun-optional-arity::int ::fun)
	   (global-optional?::bool ::obj)
	   (sfun-optional?::bool ::obj)
	   (global-key?::bool ::obj)
	   (sfun-key?::bool ::obj)))

;*---------------------------------------------------------------------*/
;*    global-read-only? ...                                            */
;*---------------------------------------------------------------------*/
(define (global-read-only? global::global)
   (with-access::global global (pragma)
      (pair? (memq 'read-only pragma))))

;*---------------------------------------------------------------------*/
;*    global-set-read-only! ...                                        */
;*---------------------------------------------------------------------*/
(define (global-set-read-only! global::global)
   (with-access::global global (pragma)
      (set! pragma (cons 'read-only pragma))))

;*---------------------------------------------------------------------*/
;*    global-args-safe? ...                                            */
;*---------------------------------------------------------------------*/
(define (global-args-safe? global::global)
   (with-access::global global (value pragma)
      (or (not (cfun? value))
	  (not (cfun-macro? value))
	  (memq 'args-safe pragma))))

;*---------------------------------------------------------------------*/
;*    fun-optional-arity ...                                           */
;*---------------------------------------------------------------------*/
(define (fun-optional-arity fun)
   (with-access::fun fun (arity)
      (if (sfun? fun)
	  (with-access::sfun fun (optionals)
	     (if (pair? optionals)
		 (+fx arity 1)
		 arity))
	  arity)))

;*---------------------------------------------------------------------*/
;*    global-optional? ...                                             */
;*---------------------------------------------------------------------*/
(define (global-optional? g)
   (and (global? g) (sfun-optional? (global-value g))))

;*---------------------------------------------------------------------*/
;*    sfun-optional? ...                                               */
;*    -------------------------------------------------------------    */
;*    Is the argument an sfun with optional parameters?                */
;*---------------------------------------------------------------------*/
(define (sfun-optional? fun)
   (and (sfun? fun) (pair? (sfun-optionals fun))))

;*---------------------------------------------------------------------*/
;*    global-key? ...                                                  */
;*---------------------------------------------------------------------*/
(define (global-key? g)
   (and (global? g) (sfun-key? (global-value g))))

;*---------------------------------------------------------------------*/
;*    sfun-key? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is the argument an sfun with key parameters?                     */
;*---------------------------------------------------------------------*/
(define (sfun-key? fun)
   (and (sfun? fun) (pair? (sfun-keys fun))))
