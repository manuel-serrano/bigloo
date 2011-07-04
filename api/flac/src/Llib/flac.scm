;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Mon Jul  4 08:00:37 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bigloo binding for the flac library                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_flac

   (include "flac.sch")
   
   (extern (export flac-error "bgl_flac_error")
	   (export flac-decoder-read "bgl_flac_decoder_read"))
   
   (export (class flac-decoder
	      (flac-decoder-init)
	      ($builtin::$flac-decoder read-only (default (%$flac-decoder-new)))
	      (%port (default #f))
	      (%flacbuffer::string (default (string-nil)))
	      (md5check::bool read-only (default #f))
	      (seek (default #f))
	      (tell (default #f))
	      (length (default #f))
	      (eof (default #f))
	      (write (default #f))
	      (metadata (default #f))
	      (error (default #f)))

	   (class &flac-error::&error)

	   (%$flac-decoder-new::$flac-decoder)

	   (generic flac-decoder-init ::flac-decoder)
	   (generic flac-decoder-close ::flac-decoder)
	   (generic flac-decoder-read ::flac-decoder ::long)
	   (flac-error::int ::string ::string ::obj)
	   ))

;*  	   (flac-handle-reset! ::flac-handle)                          */
;* 	   (flac-get-format ::flac-handle)                             */
;* 	   (flac-decode ::flac-handle ::bstring ::long ::bstring ::long) */
;* 	   (flac-position::long ::flac-handle ::bstring)               */
;* 	   (flac-info::long ::flac-handle)                             */
;* 	   (flac-seek::long ::flac-handle ::long)                      */
;* 	   (flac-volume-get::obj ::flac-handle)                        */
;* 	   (flac-volume-set! ::flac-handle ::obj)))                    */

;*---------------------------------------------------------------------*/
;*    %$flac-decoder-new ...                                           */
;*---------------------------------------------------------------------*/
(define (%$flac-decoder-new)
   ($flac-decoder-new))

;*---------------------------------------------------------------------*/
;*    flac-decoder-init ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-init o::flac-decoder)
   (with-access::flac-decoder o ($builtin md5check)
      ($flac-decoder-set-md5-checking
	 $builtin (if md5check $flac-true $flac-false))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-close ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-close o::flac-decoder)
   (with-access::flac-decoder o ($builtin)
      ($flac-decoder-delete $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    flac-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-error proc msg obj)
   (raise (instantiate::&flac-error
	     (proc proc)
	     (msg msg)
	     (obj obj)))
   0)

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-read o::flac-decoder size::long)
   (with-access::flac-decoder o (%port %flacbuffer)
      ;; Don't use any regular input functions because they take as
      ;; input ::bstring argument while flacbuffer is a ::string
      ($rgc-blit-string! %port %flacbuffer 0 size)))

;* {*---------------------------------------------------------------------*} */
;* {*    flac-handle-reset! ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-handle-reset! o::flac-handle)                         */
;*    (with-access::flac-handle o ($builtin)                           */
;*       ($flac-close $builtin)                                        */
;*       ($flac-open-feed $builtin)))                                  */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-decode ...                                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-decode m::flac-handle inbuf insz outbuf outsz)        */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (multiple-value-bind (status size)                            */
;* 	 ($bgl-flac-decode $builtin inbuf insz outbuf outsz)           */
;* 	 (values (flac-decode-status->symbol status) size))))          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-get-format ...                                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-get-format m::flac-handle)                            */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (multiple-value-bind (rate channels encoding)                 */
;* 	 ($bgl-flac-get-format $builtin)                               */
;* 	 (values rate channels (flac-format->symbol encoding)))))      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-position ...                                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-position m::flac-handle buf::bstring)                 */
;*    (with-access::flac-handle m ($builtin)                           */
;*       ($bgl-flac-position $builtin)))                               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    Force the initialization                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (let ((err ($flac-init)))                                           */
;*    (unless (=fx err $flac-ok)                                       */
;*       (raise (instantiate::&flac-error                              */
;* 		(proc "flac")                                          */
;* 		(msg ($flac-plain-strerror err))                       */
;* 		(obj #f)))))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-info ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-info m::flac-handle)                                  */
;*    (with-access::flac-handle m ($builtin)                           */
;*       ($bgl-flac-info $builtin)))                                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-seek ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-seek m::flac-handle ms)                               */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((f ($flac-timeframe $builtin (/fl (fixnum->flonum ms) 1000.)))) */
;* 	 ($flac-seek-frame $builtin f $flac-seek-set))))               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-volume-get ...                                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-volume-get m::flac-handle)                            */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((vol ($bgl-flac-getvolume $builtin)))                   */
;* 	 (if (<fx vol 0)                                               */
;* 	     (raise (instantiate::&flac-error                          */
;* 		   (proc "flac")                                       */
;* 		   (msg ($flac-plain-strerror vol))                    */
;* 		   (obj m)))                                           */
;* 	     vol))))                                                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-volume-set! ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-volume-set! m::flac-handle vol)                       */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((err ($flac-volume $builtin (/ (fixnum->flonum vol) 100.)))) */
;* 	 (when (<fx err 0)                                             */
;* 	    (raise (instantiate::&flac-error                           */
;* 		      (proc "flac")                                    */
;* 		      (msg ($flac-plain-strerror err))                 */
;* 		      (obj m)))))))                                    */
;*                                                                     */
