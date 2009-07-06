;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/ast.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 11:02:52 1994                          */
;*    Last change :  Thu Feb 12 09:28:59 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ast-node structure                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives
   (include "Tools/union.sch"))

;*---------------------------------------------------------------------*/
;*    ast node                                                         */
;*---------------------------------------------------------------------*/
(define-union ast
   
   ;; all structures inherit these slots
   (coord)

   ;; id
   (define-node ident
      id)
   
   ;; storage class specifier
   (define-node storage-class-spec
      value)

   ;; type qualifier specifier
   (define-node type-qualifier-spec
      value)

   ;; type
   (define-node type-spec
      class
      c-name
      value)
   
   ;; structure or union specifier
   (define-node struct-spec
      class   ;; `struct' or `union'
      id      ;; struct id
      fields)

   ;; declaration
   (define-node declare
      spec             ;; declaration specifier
      init-decl-list)  ;; init-declarator-list

   ;; pointer
   (define-node c-pointer
      pointer
      value)

   ;; function definition
   (define-node fun-def
      processed?       ;; is the function already processed ?
      decl-spec        ;; declarator specifier
      decl             ;; declarator
      body)            ;; (to get the formal type in std C)

   ;; declarator
   (define-node decl
      a-ptr            ;; is it a ptr on something ?
      a-decl2)         ;; a decl2 ptr

   ;; declarator2
   (define-node decl2
      id
      a-decl
      a-decl2
      array
      parameter-type-list
      parameter-identifier-list)

   ;; abstract declarator
   (define-node adecl
      a-ptr
      a-adecl2)

   ;; abstract declarator 2
   (define-node adecl2
      a-adecl
      a-adecl2
      array
      parameter-type-list)
      
   ;; parameter declaration
   (define-node para-decl
      type-spec-list
      decl
      type-name)

   ;; pointer
   (define-node ptr
      type-spec-list
      pointer)

   ;; enum
   (define-node enum-spec
      id
      enumerator-list)

   ;; type-name
   (define-node t-name
      type-spec-list
      adecl)
   
   )

