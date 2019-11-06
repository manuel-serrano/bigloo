;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/alsa/src/Llib/rawmidi.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar  4 08:15:55 2019                          */
;*    Last change :  Wed Mar 13 08:15:55 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ALSA rawmidi wrapper                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_rawmidi

   (library multimedia)
   
   (include "alsa.sch")

   (import  __alsa_alsa
	    __alsa_control)

   
   (extern (macro $u8vector->bytes::void*
	      (::u8vector) "BGL_SVECTOR_TO_PTR")
	   (macro $bstring->bytes::void*
	      (::bstring) "(void *)BSTRING_TO_STRING"))

   (export (class alsa-snd-rawmidi::alsa-object
	      ($builtin::$snd-rawmidi read-only (default (%$snd-rawmidi-nil))))

	   (%$snd-rawmidi-nil)
	   
	   (alsa-snd-rawmidi-open-output::alsa-snd-rawmidi ::bstring ::symbol)
	   (alsa-snd-rawmidi-close ::alsa-snd-rawmidi)

	   (inline alsa-snd-rawmidi-input?::bool ::alsa-snd-ctl ::int ::int)
	   (inline alsa-snd-rawmidi-output?::bool ::alsa-snd-ctl ::int ::int)
	   
	   (inline alsa-snd-rawmidi-write-byte ::alsa-snd-rawmidi ::uint8)
	   (inline alsa-snd-rawmidi-write-bytes ::alsa-snd-rawmidi ::u8vector)
	   (alsa-snd-rawmidi-write-string ::alsa-snd-rawmidi ::bstring)

	   (inline alsa-snd-rawmidi-drain ::alsa-snd-rawmidi)
	   (inline alsa-snd-rawmidi-drop ::alsa-snd-rawmidi)))

;*---------------------------------------------------------------------*/
;*    %$snd-rawmidi-nil ...                                            */
;*---------------------------------------------------------------------*/
(define (%$snd-rawmidi-nil)
   ($snd-rawmidi-nil))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-open-output ...                                 */
;*---------------------------------------------------------------------*/
(define (alsa-snd-rawmidi-open-output name flag)

   (define (flag->rawmidi-mode flag)
      (case flag
	 ((append) $snd-rawmidi-append)
	 ((nonblock) $snd-rawmidi-nonblock)
	 ((sync) $snd-rawmidi-sync)
	 (else (error "alsa-snd-rawmidi-open-output" "Illegal flag" flag))))
	 
   (let ((o (instantiate::alsa-snd-rawmidi)))
      ($bgl-snd-rawmidi-open-output o name (flag->rawmidi-mode flag))
      o))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-close ...                                       */
;*---------------------------------------------------------------------*/
(define (alsa-snd-rawmidi-close rm::alsa-snd-rawmidi)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-close $builtin)))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-input? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-input?::bool ctl device sub)
   ($bgl-snd-rawmidi-isdir ctl device sub $snd-rawmidi-stream-input))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-output? ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-output?::bool ctl device sub)
   ($bgl-snd-rawmidi-isdir ctl device sub $snd-rawmidi-stream-output))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-drain ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-drain rm::alsa-snd-rawmidi)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-drain $builtin)))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-drop ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-drop rm::alsa-snd-rawmidi)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-drop $builtin)))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-write-byte ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-write-byte rm::alsa-snd-rawmidi byte)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-write $builtin (pragma::void* "&($1)" byte) 1)))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-write-bytes ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-write-bytes rm::alsa-snd-rawmidi bytes)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-write $builtin ($u8vector->bytes bytes)
	 (u8vector-length bytes))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-write-string ...                                */
;*---------------------------------------------------------------------*/
(define (alsa-snd-rawmidi-write-string rm::alsa-snd-rawmidi string)
   (with-access::alsa-snd-rawmidi rm ($builtin)
      ($snd-rawmidi-write $builtin ($bstring->bytes string)
	 (string-length string))))
