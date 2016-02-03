;*=====================================================================*/
;*    .../project/bigloo/api/pulseaudio/src/Llib/pulseaudio.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 17:42:08 2011                          */
;*    Last change :  Wed Jan 27 16:40:47 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Direct use of PULSEAUDIO types and functions                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (extern
      (include "pulse/simple.h")
      (include "pulse/version.h")
      (include "pulse/sample.h")
      (include "pulse/error.h")

      ;; misc
      (type $int* void* "int *")
      (macro $pa-get-headers-version::string () "pa_get_headers_version")
      (macro $pa_strerror::string (::int) "pa_strerror")
      (macro &int::$int* (::int) "&")
      (macro $string-null::string "0L")

      ;; simple interface
      (type $pa-simple void* "pa_simple *")
      (macro $pa-simple-nil::$pa-simple "0L")
      (infix macro $pa-simple-nil?::bool ($pa-simple) " == 0L")
      (macro $pa-simple-free::void ($pa-simple) "pa_simple_free")
      ($bgl-pa-simple-new::$pa-simple (::string ::string ::string
					 ::long ::long ::long)
	 "bgl_pa_simple_new")
      (macro $pa-simple-write::int (::$pa-simple ::string ::long ::$int*)
	     "pa_simple_write")
      (macro $pa-simple-drain::int (::$pa-simple ::$int*)
	     "pa_simple_drain")
      (macro $pa-simple-flush::int (::$pa-simple ::$int*)
	     "pa_simple_flush")
      (macro $pa-simple-get-latency::double (::$pa-simple ::$int*)
	     "pa_simple_get_latency")

      ;; sample formats
      (macro $PA_SAMPLE_U8::long "PA_SAMPLE_U8")
      (macro $PA_SAMPLE_ALAW::long "PA_SAMPLE_ALAW")
      (macro $PA_SAMPLE_ULAW::long "PA_SAMPLE_ULAW")
      (macro $PA_SAMPLE_S16LE::long "PA_SAMPLE_S16LE")
      (macro $PA_SAMPLE_S16BE::long "PA_SAMPLE_S16BE")
      (macro $PA_SAMPLE_S24LE::long "PA_SAMPLE_S24LE")
      (macro $PA_SAMPLE_S24BE::long "PA_SAMPLE_S24BE")
      (macro $PA_SAMPLE_S32LE::long "PA_SAMPLE_S32LE")
      (macro $PA_SAMPLE_S32BE::long "PA_SAMPLE_S32BE")
      (macro $PA_SAMPLE_S24_32LE::long "PA_SAMPLE_S24_32LE")
      (macro $PA_SAMPLE_S24_32BE::long "PA_SAMPLE_S24_32BE")
      (macro $PA_SAMPLE_FLOAT32LE::long "PA_SAMPLE_FLOAT32LE")
      (macro $PA_SAMPLE_FLOAT32BE::long "PA_SAMPLE_FLOAT32BE")
      (macro $PA_SAMPLE_INVALID::long "PA_SAMPLE_INVALID")))
