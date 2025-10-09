;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/main-module4.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Wed Oct  8 14:12:11 2025                          */
;*    Last change :  Wed Oct  8 14:12:53 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module4 directives                                               */
;*=====================================================================*/

(directives
   (main recette)
   
   (import vital
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
   
   (export (do-test name thunk good?)
	   (test-module name file)
	   *recette-port*
	   *bigloo-path*
	   *silent*)
   
   (option (bigloo-debug-set! 0)))
