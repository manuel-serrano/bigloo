;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Patient/value.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 14:43:22 1999                          */
;*    Last change :  Thu Aug  8 17:27:11 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Managment of the patient values.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module patient_value
   (import patient_invoke
	   engine_param)
   (export (patient-value::pair ::bstring)))

;*---------------------------------------------------------------------*/
;*    patient-value ...                                                */
;*    -------------------------------------------------------------    */
;*    Returns a shape of a value. For this, we request the patient     */
;*    to print out the value and to return this with the value type.   */
;*---------------------------------------------------------------------*/
(define (patient-value addr::bstring)
   (patient-call "bdb_output_value"
		 addr
		 (if *write-circle?*
		     "1"
		     "0")))
