;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/mixer.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 12 08:04:38 2005                          */
;*    Last change :  Fri Aug 12 10:36:57 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic mixer control API                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mixer

   (export (class mixer
	      (devices::pair-nil (default '())))

	   (generic mixer-close ::mixer)
	   (generic mixer-volume-get ::mixer ::bstring)
	   (generic mixer-volume-set! ::mixer ::bstring ::int ::int)))

;*---------------------------------------------------------------------*/
;*    Abstract implementation                                          */
;*---------------------------------------------------------------------*/
(define-generic (mixer-close m::mixer))
(define-generic (mixer-volume-get m::mixer d::bstring))
(define-generic (mixer-volume-set! m::mixer d::bstring l::int r::int))
