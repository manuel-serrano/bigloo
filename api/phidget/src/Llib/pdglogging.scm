;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdglogging.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 09:58:32 2010                          */
;*    Last change :  Wed Sep 22 08:46:08 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget logging                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_logging

   (include "pdg.sch")

   (import __phidget_types)

   (export (phidget-enable-logging ::symbol #!optional name)
	   (phidget-disable-logging)
	   (phidget-log ::symbol ::bstring . args)))

;*---------------------------------------------------------------------*/
;*    phidget-enable-logging ...                                       */
;*---------------------------------------------------------------------*/
(define (phidget-enable-logging level #!optional name)
   (let* ((l::$pdg-log-level (symbol->level "phidget-enable-logging" level))
	  (n::string (cond
			((string? name) name)
			((not name) $pdg-log-string-null)
			(else (raise (instantiate::&phidget-error
					(proc "phidget-enable-logging")
					(msg "Illegal file name")
					(obj name))))))
	  (status::int ($pdg-enable-logging l n)))
      (unless (=fx status 0)
	 (raise (instantiate::&phidget-log-error
		   (proc "phidget-enable-logging")
		   (msg "Cannot enable logging")
		   (obj status))))))

;*---------------------------------------------------------------------*/
;*    phidget-disable-logging ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-disable-logging)
   (let ((status::int ($pdg-disable-logging)))
      (unless (=fx status 0)
	 (raise (instantiate::&phidget-log-error
		   (proc "phidget-disable-logging")
		   (msg "Cannot disable logging")
		   (obj status))))))

;*---------------------------------------------------------------------*/
;*    phidget-log ...                                                  */
;*---------------------------------------------------------------------*/
(define (phidget-log level fmt . args)
   (let* ((l::$pdg-log-level (symbol->level "phidget-enable-logging" level))
	  (s::string (apply format fmt args))
	  (status::int ($pdg-log l s)))
      (unless (=fx status 0)
	 (raise (instantiate::&phidget-log-error
		   (proc "phidget-log")
		   (msg "Cannot log")
		   (obj s))))))

;*---------------------------------------------------------------------*/
;*    symbol->level ...                                                */
;*---------------------------------------------------------------------*/
(define (symbol->level::$pdg-log-level id::bstring sym::symbol)
   (case sym
      ((critical) $pdg-log-critical)
      ((error) $pdg-log-error)
      ((warning) $pdg-log-warning)
      ((debug) $pdg-log-debug)
      ((info) $pdg-log-info)
      ((verbose) $pdg-log-verbose)
      (else (raise (instantiate::&phidget-error
		      (proc id)
		      (msg "Illegal log level")
		      (obj sym))))))

      
