;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/recette/main4.sch               */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Wed Oct  8 14:12:11 2025                          */
;*    Last change :  Fri Mar 13 09:35:07 2026 (serrano)                */
;*    Copyright   :  2025-26 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    Module4 directives                                               */
;*=====================================================================*/

(directives
   (main recette)
   
   (import utils
	   vital
	   bps
	   hash
	   bool
	   list
	   vector
	   srfi4
	   struct
	   print
	   bchar
	   string
	   kwote
	   case
	   bind-exit
	   vararity
	   apply
	   globalisation
	   glo_cell
	   kapture
	   filtre
	   match
	   rgc-trap
	   rgc-jm
	   rgc-eval
	   rgc-insert
	   rgc
	   lalr
	   input-port
	   mmap
           input-mmap-port
	   read
	   callcc
	   fringe
	   tail
	   sqic
	   reval
	   inline
	   letrec
	   macro
	   flonum
	   number
	   bignum
	   define
	   cse
	   error
	   include
	   0cfa
	   sua
	   alias
	   alias-aux
	   module
	   import1
	   import2
	   object
	   object-sans
	   object5
	   object5-sans
	   hygiene
	   wind
	   dsssl
	   peek
	   unicode
	   optim
	   pregexp
	   system
	   date
           process
           weakptr
	   crypto
	   crc
	   ssr)
   
   (cond-expand
      (bigloo-jvm (import external_jvm)))
   
   (option (bigloo-debug-set! 0)))
