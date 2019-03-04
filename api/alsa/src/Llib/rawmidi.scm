;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/alsa/src/Llib/rawmidi.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar  4 08:15:55 2019                          */
;*    Last change :  Mon Mar  4 15:44:58 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ALSA rawmidi wrapper                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_rawmidi

   (include "alsa.sch")

   (import  __alsa_alsa
	    __alsa_control)

   (export (class alsa-snd-rawmidi::alsa-object
	      ($builtin::$snd-rawmidi read-only (default (%$snd-rawmidi-nil))))

	   (%$snd-rawmidi-nil)
	   
	   (alsa-snd-rawmidi-open-output::alsa-snd-rawmidi ::bstring ::symbol)
	   (alsa-snd-rawmidi-close ::alsa-snd-rawmidi)

	   (inline alsa-snd-rawmidi-write-byte ::alsa-snd-rawmidi ::uint8)
	   (inline alsa-snd-rawmidi-write-bytes ::alsa-snd-rawmidi ::u8vector)
	   
	   (inline alsa-snd-rawmidi-input?::bool ::alsa-snd-ctl ::int ::int)
	   (inline alsa-snd-rawmidi-output?::bool ::alsa-snd-ctl ::int ::int)))

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
;*    alsa-snd-rawmidi-input? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-input?::bool ctl device sub)
   ($bgl-snd-rawmidi-isdir ctl device sub $snd-rawmidi-stream-input))

;*---------------------------------------------------------------------*/
;*    alsa-snd-rawmidi-output? ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (alsa-snd-rawmidi-output?::bool ctl device sub)
   ($bgl-snd-rawmidi-isdir ctl device sub $snd-rawmidi-stream-output))

