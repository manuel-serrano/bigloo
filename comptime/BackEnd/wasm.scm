
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_wasm
    (include "Engine/pass.sch")
    (import engine_param
        engine_configure
        tools_license
        tools_error
        tools_shape
        backend_backend
        backend_cvm
        
        backend_cplib
        module_module
        type_type
        type_cache
        type_tools
        type_env
        object_class
        object_slots
        ast_var
        ast_node
        ast_env
        ast_ident
        ast_occur
        
        ast_type-occur
        ast_pragma
        cgen_cop
        saw_wasm_compile
        saw_wasm_code
        (emit-bdb-loc cgen_emit-cop)
        type_tools
        cnst_node)
        
    (export (build-wasm-backend)
        *wasm-port*))

;*---------------------------------------------------------------------*/
;*    The backend                                                      */
;*---------------------------------------------------------------------*/
(register-backend! 'wasm build-wasm-backend)

;*---------------------------------------------------------------------*/
;*    build-jvm-backend ...                                            */
;*---------------------------------------------------------------------*/
(define (build-wasm-backend)
   (instantiate::wasm
      (language 'wasm)
      (heap-compatible 'c)
      (trace-support #f)
      (srfi0 'bigloo-wasm)
      (foreign-clause-support '(wasm extern))
      (strict-type-cast #t)
      (pragma-support #f)
      (require-tailc #t)
      ; TODO: maybe remove these two checks
      (bound-check #f)
      (type-check #f)
      (force-register-gc-roots #f)))

(define-method (backend-compile me::wasm)
    (wasm-walk me))

(define-method (backend-link-objects me::wasm sources)
    (tprint sources))

(define (require-prototype? global)
   (and (or (eq? (global-module global) *module*)
        (not (eq? (global-import global) 'static)))
    (or (and (eq? (global-module global) *module*)
         (eq? (global-import global) 'export))
        (>fx (global-occurrence global) 0)
        (eq? (global-removable global) 'never))))

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))

(define (wasm-walk me::wasm)
  (pass-prelude "WebAssembly generation"
    (lambda () (start-emission! ".wat")))

  (fprint *wasm-port* "(module ;; " *module*)

  (for-each-type! (lambda (t) (type-occurrence-set! t 0)))
  (for-each-global!
    (lambda (global)
      (cond
        ((and (eq? (global-module global) *module*)
              (>fx (global-occurrence global) 0))
          (type-increment-global! global))
        ((require-prototype? global)
          (type-increment-global! global)
          (type-occurrence-increment! (global-type global))
          (when (sfun? (global-value global))
            (for-each (lambda (a)
              (cond
                ((type? a)
                  (type-occurrence-increment! a))
                ((local? a)
                  (type-occurrence-increment! (local-type a)))))
            (sfun-args (global-value global))))))))

  (let ((compiled-funcs (backend-compile-functions me)))
  
  (with-output-to-port *wasm-port* (lambda () 
    ;; FIXME: Still required?
    (wasm-pp '(import "__runtime" "generic_va_call" (func $generic_va_call (param (ref $procedure)) (param (ref $vector)) (result eqref))))
    (wasm-pp '(import "__runtime" "BUNSPEC" (global $BUNSPEC (ref $bunspec))))
    (wasm-pp '(import "__runtime" "BOPTIONAL" (global $BOPTIONAL (ref $boptional))))
    (wasm-pp '(import "__runtime" "BKEY" (global $BKEY (ref $bkey))))
    (wasm-pp '(import "__runtime" "BREST" (global $BREST (ref $brest))))
    (for-each wasm-pp (emit-imports))
    (for-each wasm-pp (emit-memory))))

  (for-each (lambda (line) 
    (fprintf *wasm-port* "  ~a\n" line))
  (readlines "Wlib/runtime.types"))

  (let ((fixpoint #f))
  (let loop ()
    (unless fixpoint
      (set! fixpoint #t)
      (for-each (lambda (t::tclass)
        (when (>fx (type-occurrence t) 0)
          (with-access::tclass t (its-super slots)
            (when (and its-super (=fx (type-occurrence its-super) 0))
              (type-occurrence-increment! its-super)
              (set! fixpoint #f))
            (for-each (lambda (t::slot)
              (with-access::slot t (type)
                (when (=fx (type-occurrence type) 0)
                  (type-occurrence-increment! type)
                  (set! fixpoint #f))))
            slots))))
      (get-class-list)))))
  (let ((classes (filter (lambda (t) (>fx (type-occurrence t) 0)) (get-class-list))))
    (with-output-to-port *wasm-port* (lambda () 
      (for-each wasm-pp (emit-class-types classes)))))

  (emit-prototypes)
  
  ;; then we emit the constants values
  (with-output-to-port *wasm-port* (lambda () 
    (for-each wasm-pp (emit-cnsts))))
  
  ; we now emit the code for all the Scheme functions
  (with-output-to-port *wasm-port* (lambda () 
    (for-each wasm-pp compiled-funcs)))

  (fprint *wasm-port* ")"))
  
  (stop-emission!))

(define *wasm-port* #f)

(define (wasm-pp l)
  (define (ppindent depth)
    (unless (=fx depth 0)
        (display "  ")
        (ppindent (-fx depth 1))))

  (letrec ((aux (lambda (l depth)
    (define (pp-args l)
      (for-each (lambda (n)
        (newline)
        (aux n (+fx depth 1))) l))

    (define (pp-arg a)
      (cond
        ((elong? a) (display a))
        ((llong? a) (display a))
        ((bignum? a) (display a))
        (else (write a))))

    (define (pp-0 l)
      (display "(")
      (write (car l))
      (pp-args (cdr l))
      (display ")"))

    (define (pp-1 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (pp-args (cddr l))
      (display ")"))

    (define (pp-2 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (display " ")
      (pp-arg (car (cddr l)))
      (pp-args (cdr (cddr l)))
      (display ")"))

    (define (pp-oneline l)
      (display "(")
      (write (car l))
      (map (lambda (a) (display " ") (pp-arg a)) (cdr l))
      (display ")"))

    (ppindent depth)
    (cond 
      ((pair? l)
        (case (car l)
          ('import (pp-2 l))
          ('func (pp-1 l))
          ('type (pp-1 l))
          ('sub (pp-1 l))
          ('global (pp-1 l))
          ('memory (pp-oneline l))
          ('data (pp-oneline l))
          ('elem (pp-oneline l))
          ('export (pp-oneline l))
          ('param (pp-oneline l))
          ('result (pp-oneline l))
          ('local (pp-oneline l))
          ('field (pp-oneline l))
          ('mut (pp-oneline l))
          ('i32.const (pp-oneline l))
          ('i64.const (pp-oneline l))
          ('f32.const (pp-oneline l))
          ('f64.const (pp-oneline l))
          ('ref (pp-oneline l))
          ('ref.null (pp-oneline l))
          ('local.get (pp-oneline l))
          ('global.get (pp-oneline l))
          ('local.set (pp-1 l))
          ('global.set (pp-1 l))
          ('br (pp-oneline l))
          ('unreachable (pp-oneline l))
          ('block (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('loop (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('call (pp-1 l))
          ('struct.new (pp-1 l))
          ('struct.get (pp-2 l))
          ('array.new (pp-1 l))
          ('array.new_elem (pp-2 l))
          (else (pp-0 l))))
      ((not l) 'nothing)
      (else (pp-arg l))))))
    (aux l 0))
  (newline))

;*---------------------------------------------------------------------*/
;*    start-emission! ...                                              */
;*---------------------------------------------------------------------*/
(define (start-emission! suffix)
   (let ((prefix 
        (when (string? *dest*) 
            (prefix *dest*))))
      (if (or (eq? *dest* '--to-stdout) (not (string? prefix)))
        (set! *wasm-port* (current-output-port))
        (let ((f-name (string-append prefix suffix)))
            (set! *wasm-port* (open-output-file f-name))
            (if (not (output-port? *wasm-port*))
            (error *bigloo-name* "Can't open file for output" f-name)
            #unspecified)))))

;*---------------------------------------------------------------------*/
;*    stop-emission! ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-emission!)
   (cond
      ((not (output-port? *wasm-port*))
       #f)
      ((eq? *wasm-port* (current-output-port))
       #f)
      (else
       (flush-output-port *wasm-port*)
       (close-output-port *wasm-port*)
       (set! *wasm-port* #f))))

(define (class-refs class)
  (filter-map (lambda (slot)
    (with-access::slot slot (type)
      (isa? type tclass))) 
  (tclass-slots class)))

(define (scc-classes class-list)
  (define (splitf-at list f)
    (let helper ((left '())
                (right list))
      (if (and (not (null? right)) (f (car right)))
        (helper (cons (car right) left) (cdr right))
        (values (reverse left) right))))

  (let ((indexes '())
        (index 0)
        (low-links '())
        (on? '())
        (stack '())
        (sccs '()))

  (define (dfs class)
    (set! indexes (cons (cons class index) indexes))
    (set! low-links (cons (cons class index) low-links))
    (set! on? (cons (cons class #t) on?))
    (set! index (+fx index 1))
    (set! stack (cons class stack))

    (for-each (lambda (w) 
      (let ((found-index (assq w indexes))
            (found-low-link (assq w low-links))
            (on-stack? (assq w on?)))
        (cond
          ((not found-index)
            (dfs w)
            (set! low-links (cons (cons class (min (cdr found-low-link))) low-links)))
          ((and on-stack? (cdr on-stack?)) 
            (set! low-links (cons (cons class (min (cdr found-index))) low-links)))))
      ) 
    (class-refs class))

    (let ((found-index (assq class indexes))
          (found-low-link (assq class low-links)))
      (if (and found-index found-low-link (=fx (cdr found-index) (cdr found-low-link)))
        (multiple-value-bind (scc* stack*) (splitf-at stack (lambda (w) (not (eq? w class))))
          (set! stack (cdr stack*))
          (let ((scc (cons (car stack*) scc*)))
            (for-each (lambda (n) (set! on? (cons (cons n #f) on?))) scc)
            (set! sccs (cons scc sccs)))))
    ))
  
  (for-each (lambda (n) (when (not (assq n indexes)) (dfs n))) class-list)
  sccs))

;*---------------------------------------------------------------------*/
;*    emit-class-types ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-types class-list)
  ; Sorts classes such that all classes appear after their super class (if any).
  ; This is required by WASM: struct types must be defined after their supertype.

  ; TODO: emit classes that are mutually dependent in rec groups.
  ;       for that, compute the elementary cycles in the dependencies graph using the Jordan
  ;       algorithm, and generate a rec for each cycle.
  ; (let ((sccs (scc-classes class-list)))
  ;  (tprint (map (lambda (scc) (map (lambda (class) (type-name class)) scc)) sccs)))

  (let ((orders (make-hashtable))
        (current-order 0))
    (define (dfs class)
      (unless (hashtable-contains? orders class)
        (let ((super (tclass-its-super class)))
          (when super (dfs super)))
        (hashtable-put! orders class current-order)
        (set! current-order (+fx current-order 1))))
    (for-each dfs class-list)
    
    (filter-map emit-class-type 
      (sort (lambda (x y) (<fx (hashtable-get orders x) (hashtable-get orders y))) class-list))))

;*---------------------------------------------------------------------*/
;*    emit-class-type ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-type class)
  (define (emit-slot slot)
    (with-access::slot slot (type virtual-num name)
    (let ((cname (slot-type slot)))
      (if (>=fx virtual-num 0)
        #f ; TODO: what to do with virtual-num >= 0
        `(field 
          ,(wasm-sym (slot-name slot)) 
          ; TODO: consider removing WASM mut qualifier for read-only slots
          (mut ,(wasm-type cname)))))))

  (let ((super (tclass-its-super class))
        (name (type-class-name class))
        (struct `(struct
          ; MUST BE the same fields as defined in runtime.types.
          (field $header i64)
          (field $widening eqref)
          ,@(filter-map emit-slot (tclass-slots class)))))
    (if super
      `(type
        ,(wasm-sym name) 
        (sub ,@(if (tclass-final? class) '(final) '()) ,(wasm-sym (type-class-name super)) ,struct))
      #f)))

;*---------------------------------------------------------------------*/
;*    should-import? ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (should-import? variable::variable))

(define-method (should-import? variable::global)
  (case (global-import variable)
    ((import) #t)
    (else #f)))

(define-method (should-import? variable::local) #f)

;*---------------------------------------------------------------------*/
;*    emit-prototypes ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-prototypes)
  ;; set the proper name for bigloo-initialized! that is used
  ;; when a main is produced
  (let ((init (find-global 'bigloo-initialized! '__param)))
    (when init (set-variable-name! init)))

  (with-output-to-port *wasm-port* (lambda ()
  (for-each-global!
    (lambda (global)
      (if (and (require-prototype? global)
               (not (scnst? (global-value global)))
               (not (require-import? global)))
          (let ((prototype (emit-prototype (global-value global) global)))
            (when prototype (wasm-pp prototype)))))))))

(define-generic (emit-prototype value::value variable::variable) #f)

(define-method (emit-prototype value::svar variable)
   (emit-prototype/svar/scnst value variable))

; TODO: maybe remove this function as scnst should never be emitted as prototypes 
;       (see conditions in emit-prototypes).
(define-method (emit-prototype value::scnst variable)
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype/svar/scnst ...                                    */
;*---------------------------------------------------------------------*/
(define (emit-prototype/svar/scnst value variable)
  (with-access::variable variable (type id name pragma)
    (set-variable-name! variable)
    ; TODO: for now, all global variables are mutable...
    `(global 
      ,(wasm-sym name)
      ,@(if (eq? (global-import variable) 'export)
          `((export ,(global-name variable)))
          '())
      (mut ,(wasm-type type)) ,(emit-default-value type))))

;*---------------------------------------------------------------------*/
;*    emit-default-value ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-default-value type)
  (case (type-id type)
    ; TODO: implement types
    ('bool '(i32.const 0))
    ('char '(i32.const 0))
    ('byte '(i32.const 0))
    ('ubyte '(i32.const 0))
    ('int8 '(i32.const 0))
    ('uint8 '(i32.const 0))
    ('int16 '(i32.const 0))
    ('uint16 '(i32.const 0))
    ('int32 '(i32.const 0))
    ('uint32 '(i32.const 0))
    ('int64 '(i64.const 0))
    ('uint64 '(i64.const 0))
    ('int '(i32.const 0))
    ('uint '(i32.const 0))
    ('long '(i64.const 0))
    ('elong '(i64.const 0))
    ('llong '(i64.const 0))
    ('float '(f32.const 0))
    ('double '(f64.const 0))
    (else '(ref.null none))))

;*---------------------------------------------------------------------*/
;*    emit-cnsts ...                                                   */
;*---------------------------------------------------------------------*/
(define (emit-cnsts)
  (with-output-to-port *wasm-port* (lambda ()
  (let ((cnsts '()))
    (for-each-global!
      (lambda (global)
        (if (and (require-prototype? global)
                 (scnst? (global-value global)))
          (let ((cnst (emit-cnst (global-value global) global)))
            (when cnst (set! cnsts (cons cnst cnsts)))))))
    (apply append cnsts)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-cnst value::scnst variable::global)
  (with-trace 'wasm "Emit CNST"
    (with-access::scnst value (class node)
      (trace-item "class=" class)
      (case class
      ((sreal)
        (emit-cnst-real node variable))
      ((selong)
        (emit-cnst-i64 node variable))
      ((sllong)
        (emit-cnst-i64 node variable))
      ((sint32)
        (emit-cnst-i32 node variable))
      ((suint32)
        (emit-cnst-i32 node variable))
      ((sint64)
        (emit-cnst-i64 node variable))
      ((suint64)
        (emit-cnst-i64 node variable))
      ((sstring) 
        (emit-cnst-string node variable))
      ((sfun) 
        (emit-cnst-sfun node variable))
      ((sgfun)
        (emit-cnst-sgfun node variable))
      ; ((selfun)
      ;  (emit-cnst-selfun node variable))
      ; ((slfun)
      ;  (emit-cnst-slfun node variable))
      ; ((stvector)
      ;  (emit-cnst-stvector node variable))
        (else
        (internal-error "backend:emit-cnst"
                (format "Unknown cnst class \"~a\"" class)
                (shape node)))))))

;*---------------------------------------------------------------------*/
;*    allocate-string ...                                              */
;*---------------------------------------------------------------------*/
(define (allocate-string str::bstring)
  ; Allocate space inside WebAssembly module's linear memory for the string
  ; and return its offset.
  ; TODO: maybe do some string interning if not yet done by optimizations?
  (let* ((length (string-length str))
         (offset *string-current-offset*))
    (set! *string-current-offset* (+fx *string-current-offset* length))
    offset))

(define *string-current-offset* 0)

;*---------------------------------------------------------------------*/
;*    emit-cnst-string ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-string ostr global)
  ;; FIXME: Use passive (data) to allocate strings (see bulk memory extension)
  ;;        this will allow to use a single global memory for all modules.
  ;;        If it is done, no need to allocate offsets to strings anymore.
  ;;        Ex:
  ;;          (data $mystring-data "hello")
  ;;          (global $mystring (ref $bstring) (array.new_data $bstring $mystring-data (LENGTH)))
  ;; FIXME: initialize the string. As is it is not possible to init in the 
  ;;        global declaration as WASM requires a constant expression. But
  ;;        array.new_data is not constant (for now).
  (set-variable-name! global)
  (let* ((str (string-for-read ostr))
         (offset (allocate-string ostr)))
    `((data (i32.const ,offset) ,str)
      (global 
        ,(wasm-sym (global-name global))
        ,@(if (eq? (global-import global) 'export)
            `((export ,(global-name global)))
            '())
        (ref null $bstring) 
        (array.new_default $bstring (i32.const ,(string-length ostr)))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-real ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-real real global)
  (set-variable-name! global)
  (let ((value 
          (cond
            ((nanfl? real) 'nan)
            ((and (infinitefl? real) (>fl real 0.0)) 'inf)
            ((infinitefl? real) '-inf)
            (else real))))
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      ; FIXME: remove the mut and null qualifiers
      (mut (ref null $real)) 
      (struct.new $real (f64.const ,value))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i32 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i32 value global)
  (set-variable-name! global)
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      i32 (i32.const ,value))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i64 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i64 value global)
  (set-variable-name! global)
  `((global 
      ,(wasm-sym (global-name global))
      ,@(if (eq? (global-import global) 'export)
          `((export ,(global-name global)))
          '())
      i64 (i64.const ,value))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun sfun global)
  (emit-cnst-sfun/sgfun sfun global))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sgfun sgfun global)
  (emit-cnst-sfun/sgfun sgfun global))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun/sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun/sgfun fun global)
  (with-trace 'wasm "Emit SFUN"
    (trace-item "name=" (global-name global) " import=" (global-import global))
    ; TODO: implement SFUN cnst
    (unless (eq? (global-import global) 'import)
      (let* ((actuals (app-args fun))
              (entry (car actuals))
              (arity (get-node-atom-value (cadr actuals)))
              (vname (set-variable-name! global))
              (name (set-variable-name! (var-variable entry))))
        `((global 
            ,(wasm-sym vname)
            ,@(if (eq? (global-import global) 'export)
                `((export ,vname))
                '())
            ; FIXME: should not be mutable and remove the null qualifier
            (mut (ref null $procedure)) 
            (struct.new $procedure 
              (ref.func ,(wasm-sym name))
              (global.get $BUNSPEC)
              (i32.const ,arity)
              (ref.null none))))))))
  
; We maintain a hashtable of imported globals and functions in WASM as
; we can't import two times the same item (this occurs in the standard
; library).
(define *imported-ids* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    emit-imports ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-imports)
  (let ((imports '()))
    (for-each-global! (lambda (global)
      ;; TODO: remove this log code
      ;; (tprint "GLOBAL " (global-id global) " " (global-name global) " " (global-import global) " " (global-occurrence global))
      (when (and (require-import? global) (not (scnst? global)))
        (let ((import (emit-import (global-value global) global)))
          (when (and import (not (hashtable-contains? *imported-ids* (global-name global))))
            (hashtable-put! *imported-ids* (global-name global) #t)
            (set! imports (cons import imports)))))))
    imports))

;*---------------------------------------------------------------------*/
;*    require-import? ...                                              */
;*---------------------------------------------------------------------*/
(define (require-import? global)
  (let ((value (global-value global))
        (import (global-import global)))
    ;; TODO: remove this log code
    ; (if (and (eq? import 'foreign))
    ;  (tprint "FOREIGN " (global-id global) " " (global-name global) " " (global-jvm-type-name global) " " (global-occurrence global)))
    (and
      (or 
        (eq? import 'import)
        (and 
          (eq? import 'foreign)
          (not (and (isa? value cfun) (not (string-null? (global-jvm-type-name global)))))))
      (>fx (global-occurrence global) 0))))

;*---------------------------------------------------------------------*/
;*    wasm-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (wasm-module variable)
  (let ((name (global-name variable))
        (library (global-library variable))
        (module (global-module variable)))
    ; (multiple-value-bind (id module) (bigloo-demangle name)
      ;; TODO: remove log
      ; (tprint "MODULE " name ": " library " " (global-module variable) " " module " " id)
      (let ((is-macro (isa? variable cfun)))
        (cond
          ; (is-macro "__runtime")
          (library (symbol->string library))
          ((not (eq? module 'foreign)) (symbol->string module))
          ; ((and module (not (eq? module #unspecified))) module)
          (else "__runtime")))))

(define-generic (emit-import value::value variable::variable)
  (set-variable-name! variable)
  (let ((name (global-name variable)))
    `(import ,(wasm-module variable) ,name (global ,(wasm-sym name) (mut ,(wasm-type (global-type variable)))))))

(define-method (emit-import value::cvar variable)
  (set-variable-name! variable)
  `(import 
    ,(wasm-module variable) 
    ,(global-name variable) 
    (global ,(wasm-sym (global-name variable)) ,(wasm-type (global-type variable)))))

(define-method (emit-import value::sfun variable)
  (emit-import/sfun/cfun (map (lambda (arg) (if (local? arg) (local-type arg) arg)) (sfun-args value)) variable))

(define-method (emit-import value::cfun variable)
  (emit-import/sfun/cfun (cfun-args-type value) variable))

;*---------------------------------------------------------------------*/
;*    emit-import/sfun/cfun ...                                        */
;*---------------------------------------------------------------------*/
(define (emit-import/sfun/cfun args-type variable)
  (set-variable-name! variable)
  `(import 
    ,(wasm-module variable)
    ,(global-name variable)
    ,(emit-func-signature args-type variable)))

;*---------------------------------------------------------------------*/
;*    emit-result ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-result t)
  ; TODO: merge this function with the one defined in code.scm (gen-result)
  (if (eq? (type-id t) 'void)
    '()
    `((result ,(wasm-type t)))))

;*---------------------------------------------------------------------*/
;*    emit-func-signature ...                                          */
;*---------------------------------------------------------------------*/
(define (emit-func-signature args-type variable)
  `(func ,(wasm-sym (global-name variable))
    ,@(map (lambda (type)
        `(param ,(wasm-type type)))
      args-type)
    ,@(emit-result (variable-type variable))))

;*---------------------------------------------------------------------*/
;*    emit-memory ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-memory)
  `((memory 1)
    (export "memory" (memory 0))))
