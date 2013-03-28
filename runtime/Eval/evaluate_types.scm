;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_types.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:37:05 2011                          */
;*    Last change :  Sun Feb 10 09:52:31 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private types of the lambda-based evaluator                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_types
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __dsssl
	    __bit
	    __param
	    __bexit
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3)

   (export (class ev_expr)
	   
	   (class ev_var::ev_expr
	      name::symbol
	      (eff (default #f))
	      type)
	   (class ev_global::ev_expr
	      (loc read-only)
	      name::symbol
	      mod)
	   
	   (class ev_litt::ev_expr
	      value)
	   (class ev_if::ev_expr
	      p::ev_expr
	      t::ev_expr
	      e::ev_expr)
	   (class ev_list::ev_expr
	      args)
	   (class ev_or::ev_list)
	   (class ev_and::ev_list)
	   (class ev_prog2::ev_expr
	      e1::ev_expr
	      e2::ev_expr)
	   (class ev_hook::ev_expr
	      e::ev_expr)
	   (class ev_trap::ev_hook)
	   (class ev_setlocal::ev_hook
	      v::ev_var)
	   (class ev_setglobal::ev_hook
	      (loc read-only)
	      name::symbol
	      mod)
	   (class ev_defglobal::ev_setglobal)
	   (class ev_bind-exit::ev_expr
	      var::ev_var
	      body::ev_expr)
	   (class ev_unwind-protect::ev_expr
	      e::ev_expr
	      body::ev_expr)
	   (class ev_with-handler::ev_expr
	      handler::ev_expr
	      body::ev_expr)
	   (class ev_synchronize::ev_expr
	      (loc read-only)
	      mutex::ev_expr
	      prelock::ev_expr
	      body::ev_expr)
	   (class ev_binder::ev_expr
	      vars
	      vals
	      body::ev_expr)
	   (class ev_let::ev_binder
	      (boxes (default '())))
	   (class ev_let*::ev_binder
	      (boxes (default '())))
	   (class ev_letrec::ev_binder)
	   (class ev_labels::ev_expr
	      vars
	      vals ; cons (list var) expr
	      (env (default '())) ; list (cons var code)
	      (stk (default '()))
	      body::ev_expr
	      (boxes (default '()))
	      )
	   (class ev_goto::ev_expr
	      (loc read-only)
	      label::ev_var
	      labels::ev_labels
	      args )
	   (class ev_app::ev_expr
	      (loc read-only)
	      fun::ev_expr args tail?)
	   (class ev_abs::ev_expr
	      (loc read-only)
	      (where read-only)
	      (arity read-only)
	      vars
	      body::ev_expr
	      (size::int (default 0))
	      (bind (default '()))
	      (free (default '()))
	      (inner (default '()))
	      (boxes (default '())))))
