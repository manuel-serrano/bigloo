;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/unit.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 13:33:25 1996                          */
;*    Last change :  Thu Sep 18 11:57:44 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The unit structure                                               */
;*=====================================================================*/

(define-struct unit id weight sexp* printable? exported?)
