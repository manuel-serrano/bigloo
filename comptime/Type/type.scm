;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/type.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:38:54 1996                          */
;*    Last change :  Wed Nov 16 08:17:05 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type class definition                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_type

   (import tools_location)

   (include "Type/type.sch")
   
   (export (final-class type::object
	      ;; the type identifier
	      (id::symbol read-only (default '_))
	      ;; the target type name
	      (name (default #unspecified))
	      ;; the expression used by sizeof
	      (size (default #unspecified))
	      ;; the kind of type
	      (class (default 'bigloo))
	      ;; the coercion-to list
	      (coerce-to::obj (default '()))
	      ;; its parents
	      (parents::obj (default '()))
	      ;; initialized? (a type can be only declared (use-type))
	      init?::bool
	      ;; is this type can be converted into every thing ?
	      (magic?::bool (default #f))
	      ;; is the type name containing a `$'?
	      ($ (default #t))
	      ;; a type than self is aliasing
	      (alias (default #f))
	      ;; a type that points to self
	      (pointed-to-by (default #f))
	      ;; a tvector associated to this type
	      ;; (only used by the cfa but much easier to make it general)
	      (tvector (default #unspecified))
	      ;; location (for the first use of that type)
	      (location (default #unspecified))
	      ;; if that type is imported the source of the import clause
	      (import-location (default #f))
	      ;; occurrence counter, (only used by the C backend)
	      (occurrence::int (default 0)))

	   (get-aliased-type::type ::type)
	   (bigloo-type?::bool ::type)
	   (foreign-type?::bool ::type)
	   (generic type-occurrence-increment! ::type)))

;*---------------------------------------------------------------------*/
;*    get-aliased-type ...                                             */
;*---------------------------------------------------------------------*/
(define (get-aliased-type type)
   (let loop ((type type))
      (if (type? (type-alias type))
	  (loop (type-alias type))
	  type)))

;*---------------------------------------------------------------------*/
;*    bigloo-type? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bigloo-type? type)
   (eq? (type-class type) 'bigloo))

;*---------------------------------------------------------------------*/
;*    foreign-type? ...                                                */
;*---------------------------------------------------------------------*/
(define (foreign-type? type)
   (eq? (type-class type) 'C))

;*---------------------------------------------------------------------*/
;*    type-occurrence-increment! ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (type-occurrence-increment! t)
   (with-access::type t (occurrence)
      (set! occurrence (+fx 1 occurrence))))
