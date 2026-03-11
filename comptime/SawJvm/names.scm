;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/SawJvm/names.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  8 10:40:16 2003                          */
;*    Last change :  Fri Feb 13 08:07:25 2026 (serrano)                */
;*    Copyright   :  2026 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JVM standard names                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_jvm_names
   (import type_type ast_var ast_node
	   ast_env
	   type_env
	   object_class
	   object_slots
	   tvector_tvector
	   foreign_jtype
	   read_jvm
	   backend_backend
	   backend_bvm
	   backend_jvm_class
	   backend_cplib)
   (include "SawJvm/names.sch")
   (export (names-initialization me::jvm)
	   (wide-class jvmbasic::type)))

;*---------------------------------------------------------------------*/
;*    names-initialization ...                                         */
;*---------------------------------------------------------------------*/
(define (names-initialization me::jvm)
   (reset-jvmstd-type!)
   (for-each-global! (get-genv) reset-global!)
   (for-each-type! set-type-names!))

;*---------------------------------------------------------------------*/
;*    reset-jvmstd-type! ...                                           */
;*    -------------------------------------------------------------    */
;*    Create of standard jvm types                                     */
;*---------------------------------------------------------------------*/
(define (reset-jvmstd-type!)
   ;; Basic types with id=name
   (for-each (lambda (x) (type-name-set! (widen!::jvmbasic (find-type x)) x))
      '(void
	short
	int
	float
	double))
   ;; Basic types with specific names
   (for-each (lambda (t)
		(type-name-set! (widen!::jvmbasic (find-type (car t))) (cdr t)))
      '((bool . boolean)
	(char . byte)
	(byte . byte)
	(ubyte . byte)
	(ucs2 . char)
	(ushort . short)
	(long . int)
	(uchar . int)
	(llong . long)
	(ullong . long)
	(elong . long)
	(uelong . long)
	(ulong . int)
	(int8 . byte)
	(uint8 . byte)
	(int16 . short)
	(uint16 . short)
	(int32 . int)
	(uint32 . int)
	(int64 . long)
	(uint64 . long)))
   ;; Upgrade some types to vectors
   (for-each (lambda (t)
		(widen!::tvec (find-type (car t))
		   (item-type (find-type (cdr t)))))
      '((bstring . char)
	(string . char)
	(ucs2string . ucs2)
	(vector . obj)
	(cnst* . obj)
	(procedure-el . obj)))
   ;; Set some names by hand
   (for-each (lambda (s) (type-name-set! (find-type (car s)) (cdr s)))
      '((obj . obj)
	(magic . obj)
	(pair-nil . obj)
	(void* . obj)
	(tvector . obj)
	(class . class)
	(class-field . obj)
	(output-port . output-port)
	(input-port . input-port)
	(binary-port . binary-port)
	(datagram-socket . datagram-socket)
	(regexp . regexp)
	(epair . extended_pair)
	(dynamic-env . bgldynamic)
	(procedure . procedure)
	(procedure-l . procedure)
	(String . string)
	(CharSequence . charsequence))))

;*---------------------------------------------------------------------*/
;*    set-type-names! ...                                              */
;*    -------------------------------------------------------------    */
;*    Associate jvm types to types.                                    */
;*---------------------------------------------------------------------*/
(define (set-type-names! type::type)
   (get-jvmtype type))

;*---------------------------------------------------------------------*/
;*    get-jvmtype ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-jvmtype type::type)
   (let ((name (type-name type)))
      (if (symbol? name)
	  name
	  (let ((jtype (build-type-name type)))
	     (type-name-set! type jtype)
	     jtype))))

;*---------------------------------------------------------------------*/
;*    build-type-name ...                                              */
;*---------------------------------------------------------------------*/
(define (build-type-name type::type)
   (cond
      ((tclass? type)
       (if (eq? (type-id type) 'object)
	   'object
	   (qualified-tclass-name type)))
      ((wclass? type)
       (qualified-wclass-name type))
      ((jclass? type) (qualified-jclass-name type))
      ((tvec? type)
       (get-jvmtype (tvec-item-type type))
       "Zector")
      ((jarray? type)
       (get-jvmtype (jarray-item-type type))
       "Zector")
      (else
       (qualified-type-name type))))

