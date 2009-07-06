;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/bit.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 27 11:06:41 1995                          */
;*    Last change :  Fri May 29 07:09:50 2009 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bit management                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bit
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    
	    __evenv)
   
   (extern  (infix macro c-bitor::long (::long ::long) " | ")
	    (infix macro c-bitorelong::elong (::elong ::elong) " | ")
	    (infix macro c-bitorllong::llong (::llong ::llong) " | ")
	    (infix macro c-bitand::long (::long ::long) " & ")
	    (infix macro c-bitandelong::elong (::elong ::elong) " & ")
	    (infix macro c-bitandllong::llong (::llong ::llong) " & ")
	    (infix macro c-bitxor::long (::long ::long) " ^ ")
	    (infix macro c-bitxorelong::elong (::elong ::elong) " ^ ")
	    (infix macro c-bitxorllong::llong (::llong ::llong) " ^ ")
	    (macro c-bitnot::long (::long) "~")
	    (macro c-bitnotelong::elong (::elong) "~")
	    (macro c-bitnotllong::llong (::llong) "~")
	    (infix macro c-bitrsh::long (::long ::int) " >> ")
	    (infix macro c-bitrshelong::elong (::elong ::int) " >> ")
	    (infix macro c-bitrshllong::llong (::llong ::int) " >> ")
	    (infix macro c-bitursh::ulong (::ulong ::int) " >> ")
	    (infix macro c-biturshelong::uelong (::uelong ::int) " >> ")
	    (infix macro c-biturshllong::ullong (::ullong ::int) " >> ")
	    (infix macro c-bitlsh::long (::long ::int) " << ")
	    (infix macro c-bitlshelong::elong (::elong ::int) " << ")
	    (infix macro c-bitlshllong::llong (::llong ::int) " << "))
   
   (java    (class foreign
	       (method static c-bitor::long (::long ::long)
		       "BITOR")
	       (method static c-bitorelong::elong (::elong ::elong)
		       "BITORELONG")
	       (method static c-bitorllong::llong (::llong ::llong)
		       "BITORLLONG")
	       (method static c-bitand::long (::long ::long)
		       "BITAND")
	       (method static c-bitandelong::elong (::elong ::elong)
		       "BITANDELONG")
	       (method static c-bitandllong::llong (::llong ::llong)
		       "BITANDLLONG")
	       (method static c-bitxor::long (::long ::long)
		       "BITXOR")
	       (method static c-bitxorelong::elong (::elong ::elong)
		       "BITXORELONG")
	       (method static c-bitxorllong::llong (::llong ::llong)
		       "BITXORLLONG")
	       (method static c-bitnot::long (::long)
		       "BITNOT")
	       (method static c-bitnotelong::elong (::elong)
		       "BITNOTELONG")
	       (method static c-bitnotllong::llong (::llong)
		       "BITNOTLLONG")
	       (method static c-bitrsh::long (::long ::int)
		       "BITRSH")
	       (method static c-bitrshelong::elong (::elong ::int)
		       "BITRSHELONG")
	       (method static c-bitrshllong::llong (::llong ::int)
		       "BITRSHLLONG")
	       (method static c-bitursh::ulong (::ulong ::int)
		       "BITURSH")
	       (method static c-biturshelong::uelong (::uelong ::int)
		       "BITURSHELONG")
	       (method static c-biturshllong::ullong (::ullong ::int)
		       "BITURSHLLONG")
	       (method static c-bitlsh::long (::long ::int)
		       "BITLSH")
	       (method static c-bitlshelong::elong (::elong ::int)
		       "BITLSHELONG")
	       (method static c-bitlshllong::llong (::llong ::int)
		       "BITLSHLLONG")))
   
   (export  (inline bit-or::long ::long ::long)
	    (inline bit-orelong::elong ::elong ::elong)
	    (inline bit-orllong::llong ::llong ::llong)
	    (inline bit-and::long ::long ::long)
	    (inline bit-andelong::elong ::elong ::elong)
	    (inline bit-andllong::llong ::llong ::llong)
	    (inline bit-xor::long ::long ::long)
	    (inline bit-xorelong::elong ::elong ::elong)
	    (inline bit-xorllong::llong ::llong ::llong)
	    (inline bit-not::long ::long)
	    (inline bit-notelong::elong ::elong)
	    (inline bit-notllong::llong ::llong)
	    (inline bit-rsh::long ::long ::long)
	    (inline bit-rshelong::elong ::elong ::long)
	    (inline bit-rshllong::llong ::llong ::long)
	    (inline bit-ursh::ulong ::ulong ::long)
	    (inline bit-urshelong::uelong ::uelong ::long)
	    (inline bit-urshllong::ullong ::ullong ::long)
	    (inline bit-lsh::long ::long ::long)
	    (inline bit-lshelong::elong ::elong ::long)
	    (inline bit-lshllong::llong ::llong ::long))
   
   (pragma  (c-bitor side-effect-free no-cfa-top nesting args-safe)
	    (c-bitorelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitorllong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitand side-effect-free no-cfa-top nesting args-safe)
	    (c-bitandelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitandllong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitxor side-effect-free no-cfa-top nesting args-safe)
	    (c-bitxorelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitxorllong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitnot side-effect-free no-cfa-top nesting args-safe)
	    (c-bitnotelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitnotllong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitrsh side-effect-free no-cfa-top nesting args-safe)
	    (c-bitrshelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitrshllong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitursh side-effect-free no-cfa-top nesting args-safe)
	    (c-bitlsh side-effect-free no-cfa-top nesting args-safe)
	    (c-bitlshelong side-effect-free no-cfa-top nesting args-safe)
	    (c-bitlshllong side-effect-free no-cfa-top nesting args-safe)
	    (bit-or side-effect-free no-cfa-top nesting)
	    (bit-orelong side-effect-free no-cfa-top nesting)
	    (bit-orllong side-effect-free no-cfa-top nesting)
	    (bit-and side-effect-free no-cfa-top nesting)
	    (bit-andelong side-effect-free no-cfa-top nesting)
	    (bit-andllong side-effect-free no-cfa-top nesting)
	    (bit-xor side-effect-free no-cfa-top nesting)
	    (bit-xorelong side-effect-free no-cfa-top nesting)
	    (bit-xorllong side-effect-free no-cfa-top nesting)
	    (bit-not side-effect-free no-cfa-top nesting)
	    (bit-notelong side-effect-free no-cfa-top nesting)
	    (bit-notllong side-effect-free no-cfa-top nesting)
	    (bit-rsh side-effect-free no-cfa-top nesting)
	    (bit-rshelong side-effect-free no-cfa-top nesting)
	    (bit-rshllong side-effect-free no-cfa-top nesting)
	    (bit-ursh side-effect-free no-cfa-top nesting)
	    (bit-urshelong side-effect-free no-cfa-top nesting)
	    (bit-urshllong side-effect-free no-cfa-top nesting)
	    (bit-lsh side-effect-free no-cfa-top nesting)
	    (bit-lshelong side-effect-free no-cfa-top nesting)
	    (bit-lshllong side-effect-free no-cfa-top nesting)))

;*---------------------------------------------------------------------*/
;*    bit-or ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (bit-or x y)
   (c-bitor x y))

;*---------------------------------------------------------------------*/
;*    bit-orelong ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (bit-orelong x y)
   (c-bitorelong x y))

;*---------------------------------------------------------------------*/
;*    bit-orllong ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (bit-orllong x y)
   (c-bitorllong x y))

;*---------------------------------------------------------------------*/
;*    bit-and ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bit-and x y)
   (c-bitand x y))

;*---------------------------------------------------------------------*/
;*    bit-andelong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-andelong x y)
   (c-bitandelong x y))

;*---------------------------------------------------------------------*/
;*    bit-andllong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-andllong x y)
   (c-bitandllong x y))

;*---------------------------------------------------------------------*/
;*    bit-xor ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bit-xor x y)
   (c-bitxor x y))

;*---------------------------------------------------------------------*/
;*    bit-xorelong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-xorelong x y)
   (c-bitxorelong x y))

;*---------------------------------------------------------------------*/
;*    bit-xorllong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-xorllong x y)
   (c-bitxorllong x y))

;*---------------------------------------------------------------------*/
;*    bit-not ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bit-not x)
   (c-bitnot x))
   
;*---------------------------------------------------------------------*/
;*    bit-notelong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-notelong x)
   (c-bitnotelong x))
   
;*---------------------------------------------------------------------*/
;*    bit-notllong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-notllong x)
   (c-bitnotllong x))
   
;*---------------------------------------------------------------------*/
;*    bit-rsh ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bit-rsh x y)
   (c-bitrsh x y))
       
;*---------------------------------------------------------------------*/
;*    bit-rshelong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-rshelong x y)
   (c-bitrshelong x y))
       
;*---------------------------------------------------------------------*/
;*    bit-rshllong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-rshllong x y)
   (c-bitrshllong x y))
       
;*---------------------------------------------------------------------*/
;*    bit-ursh ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (bit-ursh x y)
   (c-bitursh x y))
       
;*---------------------------------------------------------------------*/
;*    bit-urshelong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bit-urshelong x y)
   (c-biturshelong x y))
       
;*---------------------------------------------------------------------*/
;*    bit-urshllong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bit-urshllong x y)
   (c-biturshllong x y))
       
;*---------------------------------------------------------------------*/
;*    bit-lsh ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bit-lsh x y)
   (c-bitlsh x y))

;*---------------------------------------------------------------------*/
;*    bit-lshelong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-lshelong x y)
   (c-bitlshelong x y))

;*---------------------------------------------------------------------*/
;*    bit-lshllong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bit-lshllong x y)
   (c-bitlshllong x y))

