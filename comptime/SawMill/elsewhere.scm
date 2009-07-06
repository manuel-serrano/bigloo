(module saw_elsewhere
   (import type_type ast_var ast_node
	   module_module
	   backend_c_prototype )
   (export (need-function-pointer var::global)) )

(define (need-function-pointer var::global)
   (let ( (val (global-value var)) )
      (and (eq? (global-module var) *module*)
	   (require-prototype? var)
	   (scnst? val)
	   (memq (scnst-class val) '(sfun sgfun)) )))
