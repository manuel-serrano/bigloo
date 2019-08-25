;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/port.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 20 16:53:27 1995                          */
;*    Last change :  Sun Aug 25 09:19:02 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.10.1 Ports (page 29, r4)                                       */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/io.texi@                                  */
;*       @node Input And Output@                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_ports_6_10_1
   
   (import  __error
	    __bexit
	    __r4_input_6_10_2
	    __object
	    __thread
	    __param
	    __gunzip
	    __url
	    __http
	    __ftp)
   
   (use     __type
	    __bigloo
	    __tvector
	    __socket
	    __os
	    __binary
	    __base64
	    __bignum
	    __rgc
	    __bit
	    
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_characters_6_6
	    __r4_control_features_6_9
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    
	    __evenv)
   
   (extern  (macro c-input-port?::bool  (::obj)
		   "INPUT_PORTP")
	    (macro c-input-string-port?::bool  (::obj)
		   "INPUT_STRING_PORTP")
	    (macro c-input-procedure-port?::bool (::obj)
		   "INPUT_PROCEDURE_PORTP")
	    (macro c-input-gzip-port?::bool (::obj)
		   "INPUT_GZIP_PORTP")
	    (macro c-output-port?::bool (::obj)
		   "OUTPUT_PORTP")
	    (macro c-output-string-port?::bool (::obj)
		   "BGL_OUTPUT_STRING_PORTP")
	    (macro c-output-procedure-port?::bool (::obj)
		   "BGL_OUTPUT_PROCEDURE_PORTP")
	    
	    (macro c-current-output-port::output-port (::dynamic-env)
		   "BGL_ENV_CURRENT_OUTPUT_PORT")
	    (macro c-current-error-port::output-port (::dynamic-env)
		   "BGL_ENV_CURRENT_ERROR_PORT")
	    (macro c-current-input-port::input-port (::dynamic-env)
		   "BGL_ENV_CURRENT_INPUT_PORT")
	    
	    (macro c-current-output-port-set!::void (::dynamic-env ::output-port)
		   "BGL_ENV_CURRENT_OUTPUT_PORT_SET")
	    (macro c-current-error-port-set!::void (::dynamic-env ::output-port)
		   "BGL_ENV_CURRENT_ERROR_PORT_SET")
	    (macro c-current-input-port-set!::void (::dynamic-env ::input-port)
		   "BGL_ENV_CURRENT_INPUT_PORT_SET")

	    (macro c-input-gzip-port-input-port::input-port (::input-port)
		   "BGL_INPUT_GZIP_PORT_INPUT_PORT")
	    
	    ($open-input-file::obj (::bstring ::bstring) "bgl_open_input_file")
	    ($open-input-pipe::obj (::bstring ::bstring) "bgl_open_input_pipe")
	    ($open-input-resource::obj (::bstring ::bstring) "bgl_open_input_resource")
 	    ($open-input-c-string::obj (::string) "bgl_open_input_c_string")
	    ($reopen-input-c-string::obj (::input-port ::string) "bgl_reopen_input_c_string")
	    ($open-input-substring::input-port (::bstring ::long ::long) "bgl_open_input_substring")
	    ($open-input-substring!::input-port (::bstring ::long ::long) "bgl_open_input_substring_bang")
	    ($open-input-procedure::input-port (::procedure ::bstring) "bgl_open_input_procedure")
	    ($input-port-timeout-set!::bool (::input-port ::long) "bgl_input_port_timeout_set")
	    ($output-port-timeout-set!::bool (::output-port ::long) "bgl_output_port_timeout_set")
	    ($open-output-file::obj (::bstring ::bstring) "bgl_open_output_file")
	    ($append-output-file::obj (::bstring ::bstring) "bgl_append_output_file")
	    ($open-output-string::output-port (::bstring) "bgl_open_output_string")
	    ($open-output-procedure::obj (::procedure ::procedure ::procedure ::bstring) "bgl_open_output_procedure")
	    ($close-input-port::obj (::obj) "bgl_close_input_port")
	    ($input-port-reopen!::obj (::input-port) "bgl_input_port_reopen")
	    ($input-port-clone!::input-port (::input-port ::input-port) "bgl_input_port_clone")
	    ($set-input-port-position!::void (::input-port ::long) "bgl_input_port_seek")
	    (macro c-input-port-position::long (::input-port)
		   "INPUT_PORT_FILEPOS")
	    (macro $input-port-fill-barrier::long (::input-port)
		   "INPUT_PORT_FILLBARRIER")
	    (macro $input-port-fill-barrier-set!::void (::input-port ::long)
		   "INPUT_PORT_FILLBARRIER_SET")
	    (macro $input-port-length::elong (::input-port)
		   "BGL_INPUT_PORT_LENGTH")
	    (macro $input-port-length-set!::void (::input-port ::elong)
		   "BGL_INPUT_PORT_LENGTH_SET")
	    (macro c-input-port-last-token-position::elong (::input-port)
		   "INPUT_PORT_TOKENPOS")
	    (macro c-input-port-bufsiz::long (::input-port)
		   "BGL_INPUT_PORT_BUFSIZ")
	    (macro c-closed-input-port?::bool (::obj)
		   "INPUT_PORT_CLOSEP")
	    (macro $closed-output-port?::bool (::obj)
		   "OUTPUT_PORT_CLOSEP")

	    (macro c-output-port-position::long (::output-port)
		   "BGL_OUTPUT_PORT_FILEPOS")
	    (c-set-output-port-position!::obj (::output-port ::long) "bgl_output_port_seek")
	    
	    ($close-output-port::obj (::output-port) "bgl_close_output_port")
	    (c-get-output-string::bstring (::output-port)"get_output_string")
	    (c-default-io-bufsiz::long "default_io_bufsiz")
	    (c-reset-eof::bool (::obj) "reset_eof")
	    (macro c-flush-output-port::obj (::output-port)
		   "bgl_flush_output_port")
	    (macro $reset-output-string-port::obj (::output-port)
		   "bgl_reset_output_string_port")
	    (macro $reset-output-port-error::obj (::output-port)
		   "bgl_reset_output_port_error")
	    
	    (c-fexists?::bool (::string) "fexists")
	    (macro c-delete-file::bool (::string) "unlink")
	    (macro c-delete-directory::bool  (::string) "rmdir")
	    (macro c-rename-file::int (::string ::string) "rename")
	    (macro $truncate-file::int (::string ::long) "truncate")
	    (macro $ftruncate::int (::output-port ::long) "bgl_output_port_truncate")
	    (macro c-mkdir::bool (::string ::long) "!BGL_MKDIR")
	    
	    (macro $port-isatty?::bool (::output-port) "bgl_port_isatty")
 	    (macro $output-port-name::bstring (::output-port) "OUTPUT_PORT_NAME")
 	    (macro $output-port-name-set!::void (::output-port ::bstring) "OUTPUT_PORT_NAME_SET")
 	    (macro $input-port-name::bstring (::input-port) "INPUT_PORT_NAME")
 	    (macro $input-port-name-set!::void (::input-port ::bstring) "INPUT_PORT_NAME_SET")

	    (macro c-output-port-chook::obj (::output-port)
		   "PORT_CHOOK")
	    (macro c-output-port-chook-set!::void (::output-port ::procedure)
		   "PORT_CHOOK_SET")
	    (macro $output-port-fhook::obj (::output-port)
		   "BGL_OUTPUT_PORT_FHOOK")
	    (macro $output-port-fhook-set!::void (::output-port ::obj)
		   "BGL_OUTPUT_PORT_FHOOK_SET")
	    (macro $output-port-flushbuf::obj (::output-port)
		   "BGL_OUTPUT_PORT_FLUSHBUF")
	    (macro $output-port-flushbuf-set!::void (::output-port ::obj)
		   "BGL_OUTPUT_PORT_FLUSHBUF_SET")
	    
	    (macro $input-port-chook::obj (::input-port)
		   "PORT_CHOOK")
	    (macro $input-port-chook-set!::void (::input-port ::procedure)
		   "PORT_CHOOK_SET")

	    (macro $input-port-useek::obj (::input-port)
		   "BGL_INPUT_PORT_USEEK")
	    (macro $input-port-useek-set!::void (::input-port ::procedure)
		   "BGL_INPUT_PORT_USEEK_SET")

	    (macro $input-port-buffer::bstring (::input-port)
		   "BGL_INPUT_PORT_BUFFER")
	    (macro $input-port-buffer-set!::void (::input-port ::bstring)
		   "bgl_input_port_buffer_set")
	    
	    (macro $output-port-buffer::bstring (::output-port)
		   "BGL_OUTPUT_PORT_BUFFER")
	    (macro $output-port-buffer-set!::void (::output-port ::bstring)
		   "bgl_output_port_buffer_set")
	    
	    ($directory?::bool (::string) "bgl_directoryp")
	    ($directory->list::obj (::string) "bgl_directory_to_list")
	    ($directory->path-list::obj (::string ::int ::char)
					"bgl_directory_to_path_list")
	    ($modification-time::elong (::string) "bgl_last_modification_time")
	    ($change-time::elong (::string) "bgl_last_change_time")
	    ($access-time::elong (::string) "bgl_last_access_time")
	    ($utime::int (::string ::elong ::elong) "bgl_utime")
	    
	    ($file-size::elong (::string) "bgl_file_size")
	    ($file-uid::long (::string) "bgl_file_uid")
	    ($file-gid::long (::string) "bgl_file_gid")
	    ($file-mode::long (::string) "bgl_file_mode")
	    ($file-type::symbol (::string) "bgl_file_type")
	    ($symlink::int (::string ::string) "bgl_symlink")
	    ($select::pair-nil (::long ::pair-nil ::pair-nil ::pair-nil) "bgl_select")
	    ($open-pipes::obj (::obj) "bgl_open_pipes")
	    ($lockf::bool (::output-port ::int ::long) "bgl_lockf")
	    (macro $F_LOCK::int "F_LOCK")
	    (macro $F_TLOCK::int "F_TLOCK")
	    (macro $F_ULOCK::int "F_ULOCK")
	    (macro $F_TEST::int "F_TEST"))

   (java    (class foreign
	       (method static c-input-port?::bool  (::obj)
		  "INPUT_PORTP")
	       (method static c-input-string-port?::bool  (::obj)
		  "INPUT_STRING_PORTP")
	       (method static c-input-procedure-port?::bool (::obj)
		  "INPUT_PROCEDURE_PORTP")
	       (method static c-input-gzip-port?::bool (::obj)
		  "INPUT_GZIP_PORTP")
	       (method static c-output-port?::bool (::obj)
		  "OUTPUT_PORTP")
	       (method static c-output-string-port?::bool (::obj)
		  "OUTPUT_STRING_PORTP")
	       (method static c-output-procedure-port?::bool (::obj)
		  "OUTPUT_PROCEDURE_PORTP")
	       
	       (field static c-default-io-bufsiz::int
		  "default_io_bufsiz")
	       
	       (method static c-current-output-port::output-port (::dynamic-env)
		  "getCurrentOutputPort")
	       (method static c-current-error-port::output-port (::dynamic-env)
		  "getCurrentErrorPort")
	       (method static c-current-input-port::input-port (::dynamic-env)
		  "getCurrentInputPort")
	       (method static c-current-output-port-set!::void (::dynamic-env ::output-port)
		  "setCurrentOutputPort")
	       (method static c-current-error-port-set!::void (::dynamic-env ::output-port)
		  "setCurrentErrorPort")
	       (method static c-current-input-port-set!::void (::dynamic-env ::input-port)
		  "setCurrentInputPort")
	       
	       (method static $open-input-file::obj (::bstring ::bstring)
		  "bgl_open_input_file")
	       (method static $open-input-pipe::obj (::bstring ::bstring)
		  "bgl_open_input_pipe")
	       (method static $open-input-resource::obj (::bstring ::bstring)
		  "bgl_open_input_resource")
	       (method static $open-input-c-string::obj (::string)
		  "bgl_open_input_c_string")
	       (method static $reopen-input-c-string::obj (::input-port ::string)
		  "bgl_reopen_input_c_string")
	       (method static $open-input-substring::input-port (::bstring ::int ::int)
		  "bgl_open_input_substring")
	       (method static $open-input-substring!::input-port (::bstring ::int ::int)
		  "bgl_open_input_substring_bang")
	       (method static $open-input-procedure::obj (::procedure ::bstring)
		  "bgl_open_input_procedure")
	       (method static $input-port-timeout-set!::bool (::input-port ::long)
		  "bgl_input_port_timeout_set")
	       (method static $output-port-timeout-set!::bool (::output-port ::long)
		  "bgl_output_port_timeout_set")
	       (method static $open-output-file::obj (::bstring ::bstring)
		  "bgl_open_output_file")
	       (method static $append-output-file::obj (::bstring ::bstring)
		  "bgl_append_output_file")
	       (method static $open-output-string::output-port (::bstring)
		  "bgl_open_output_string")
	       (method static $open-output-procedure::obj (::procedure ::procedure ::procedure ::bstring)
		  "bgl_open_output_procedure")
	       (method static $close-input-port::obj (::input-port)
		  "bgl_close_input_port")
	       (method static $input-port-reopen!::obj (::input-port)
		  "bgl_input_port_reopen")
	       (method static $input-port-clone!::input-port (::input-port ::input-port)
		  "bgl_input_port_clone")
	       (method static $set-input-port-position!::void (::input-port ::long)
		  "bgl_input_port_seek")
	       (method static c-set-output-port-position!::obj (::output-port ::long)
		  "bgl_output_port_seek")
	       (method static c-input-port-bufsiz::int (::input-port)
		  "bgl_input_port_bufsiz")
	       
	       (method static c-closed-input-port?::bool (::input-port)
		  "CLOSED_RGC_BUFFER")
	       (method static $closed-output-port?::bool (::output-port)
		  "CLOSED_OUTPUT_PORT")
	       (method static $close-output-port::obj (::output-port)
		  "bgl_close_output_port")
	       (method static c-get-output-string::bstring (::output-port)
		  "get_output_string")
	       (method static c-reset-eof::bool (::obj)
		  "reset_eof")
	       (method static c-flush-output-port::obj (::output-port)
		  "FLUSH_OUTPUT_PORT")
	       (method static $reset-output-string-port::obj (::output-port)
		  "bgl_reset_output_string_port")
	       (method static $reset-output-port-error::obj (::output-port)
		  "bgl_reset_output_port_error")
	       
	       (method static c-fexists?::bool (::string)
		  "fexists")
	       (method static c-delete-file::bool (::string)
		  "unlink")
	       (method static c-delete-directory::bool  (::string)
		  "rmdir")
	       (method static c-rename-file::int (::string ::string)
		  "rename")
	       (method static $truncate-file::int (::string ::long)
		  "truncate")
	       (method static $ftruncate::bool (::output-port ::long)
		  "bgl_output_port_truncate")
	       (method static c-mkdir::bool (::string ::int)
		  "mkdir")
	       
	       (method static c-output-port-position::long (::output-port)
		  "OUTPUT_PORT_FILEPOS")
	       (method static c-input-port-position::long (::input-port)
		  "INPUT_PORT_FILEPOS")
	       
	       (method static $input-port-fill-barrier::long (::input-port)
		  "INPUT_PORT_FILLBARRIER")
	       (method static $input-port-fill-barrier-set!::void (::input-port ::long)
		  "INPUT_PORT_FILLBARRIER_SET")
	       
	       (method static $input-port-length::elong (::input-port)
		  "BGL_INPUT_PORT_LENGTH")
	       (method static $input-port-length-set!::void (::input-port ::elong)
		  "BGL_INPUT_PORT_LENGTH_SET")
	       
	       (method static c-input-port-last-token-position::long (::input-port)
		  "INPUT_PORT_TOKENPOS")
	       (method static $input-port-name-set!::void (::input-port ::bstring)
		  "INPUT_PORT_NAME")
	       (method static $input-port-name::bstring (::input-port)
		  "INPUT_PORT_NAME")
	       (method static $output-port-name::bstring (::output-port)
		  "OUTPUT_PORT_NAME")
	       (method static $output-port-name-set!::void (::output-port ::bstring)
		  "OUTPUT_PORT_NAME_SET")
	       (method static c-output-port-chook::obj (::output-port)
		  "OUTPUT_PORT_CHOOK")
	       (method static c-output-port-chook-set!::void (::output-port ::procedure)
		  "OUTPUT_PORT_CHOOK_SET")
	       (method static $output-port-fhook::obj (::output-port)
		  "OUTPUT_PORT_FHOOK")
	       (method static $output-port-fhook-set!::void (::output-port ::obj)
		  "OUTPUT_PORT_FHOOK_SET")
	       (method static $output-port-flushbuf::obj (::output-port)
		  "OUTPUT_PORT_FLUSHBUF")
	       (method static $output-port-flushbuf-set!::void (::output-port ::obj)
		  "OUTPUT_PORT_FLUSHBUF_SET")
	       (method static $input-port-chook::obj (::input-port)
		  "INPUT_PORT_CHOOK")
	       (method static $input-port-chook-set!::void (::input-port ::procedure)
		  "INPUT_PORT_CHOOK_SET")
	       (method static $input-port-useek::obj (::input-port)
		  "INPUT_PORT_USEEK")
	       (method static $input-port-useek-set!::void (::input-port ::procedure)
		  "INPUT_PORT_USEEK_SET")
	       
	       (method static $input-port-buffer::bstring (::input-port)
		  "BGL_INPUT_PORT_BUFFER")
	       (method static $input-port-buffer-set!::void (::input-port ::bstring)
		  "bgl_input_port_buffer_set")
	       (method static $output-port-buffer::bstring (::output-port)
		  "BGL_OUTPUT_PORT_BUFFER")
	       (method static $output-port-buffer-set!::void (::output-port ::bstring)
		  "bgl_output_port_buffer_set")
	       
	       (method static c-input-gzip-port-input-port::input-port (::input-port)
		  "BGL_INPUT_GZIP_PORT_INPUT_PORT")
	       
	       (method static $directory?::bool (::string)
		  "bgl_directoryp")
	       (method static $directory->list::obj (::string)
		  "bgl_directory_to_list")
	       (method static $modification-time::elong (::string)
		  "bgl_last_modification_time")
	       (method static $change-time::elong (::string)
		  "bgl_last_change_time")
	       (method static $access-time::elong (::string)
		  "bgl_last_access_time")
	       (method static $utime::int (::string ::elong ::elong)
		  "bgl_utime")
	       (method static $file-size::elong (::string)
		  "bgl_file_size")
	       
	       (method static $file-uid::int (::string)
		  "bgl_file_uid")
	       (method static $file-gid::int (::string)
		  "bgl_file_gid")
	       (method static $file-mode::int (::string)
		  "bgl_file_mode")
	       (method static $file-type::symbol (::string)
		  "bgl_file_type")
	       (method static $symlink::int (::string ::string)
		  "bgl_symlink")
	       (method static $select::pair-nil (::long ::pair-nil ::pair-nil ::pair-nil)
		  "bgl_select")
	       (method static $open-pipes::obj (::obj)
		  "bgl_open_pipes")))
	    
   (export  (call-with-input-file ::bstring ::procedure)
	    (call-with-input-string ::bstring ::procedure)
	    (call-with-output-file ::bstring ::procedure)
	    (call-with-append-file ::bstring ::procedure)
	    (call-with-output-string::bstring ::procedure)
	    
	    (inline input-port? ::obj)
	    (inline input-string-port? ::obj)
	    (inline input-procedure-port? ::obj)
	    (inline input-gzip-port? ::obj)
	    (inline output-port? ::obj)
	    (inline port?::bool ::obj)
	    (inline output-string-port? ::obj)
	    (inline output-procedure-port? ::obj)
	    
	    (inline current-input-port::input-port) 
	    (inline current-error-port::output-port)
	    (inline current-output-port::output-port)
	    
	    (with-input-from-file ::bstring ::procedure)
	    (with-input-from-string ::bstring ::procedure)
	    (with-input-from-port ::input-port ::procedure)
	    (with-input-from-procedure ::procedure ::procedure)
	    (with-output-to-file ::bstring ::procedure)
	    (with-append-to-file ::bstring ::procedure)
	    (with-output-to-string::bstring ::procedure)
	    (with-error-to-string::bstring ::procedure)
	    (with-output-to-port ::output-port ::procedure)
	    (with-output-to-procedure ::procedure ::procedure)
	    
	    (with-error-to-file ::bstring ::procedure)
	    (with-error-to-port ::output-port ::procedure)
	    (with-error-to-procedure ::procedure ::procedure)
	    
	    (open-input-file ::bstring #!optional (bufinfo #t) (timeout 5000000))
	    (open-input-string::input-port string::bstring
	       #!optional (start 0) (end (string-length string)))
	    (open-input-string!::input-port string::bstring
	       #!optional (start 0) (end (string-length string)))
	    (open-input-procedure ::procedure #!optional (bufinfo #t))
	    (open-input-gzip-port ::input-port  #!optional (bufinfo #t))
	    
	    (inline input-port-timeout-set! ::input-port ::long)
	    (inline open-input-c-string ::string)
	    (inline reopen-input-c-string ::input-port ::string)
	    (open-output-file ::bstring #!optional (bufinfo #t))
	    (append-output-file ::bstring #!optional (bufinfo #t))
	    (open-output-string::output-port #!optional (bufinfo #t))
	    (open-output-procedure ::procedure #!optional (flush::procedure (lambda () #f)) (bufinfo #t) (close::procedure (lambda () #f)))
	    (inline output-port-timeout-set! ::output-port ::long)
	    (inline closed-input-port?::bool ::input-port)
	    (inline close-input-port ::input-port)
	    (inline get-output-string::bstring ::output-port)
	    (inline close-output-port ::output-port)
	    (inline flush-output-port ::output-port)
	    (inline reset-output-port ::output-port)
	    (inline reset-eof::bool ::input-port)
	    (inline output-port-name::bstring ::output-port)
	    (inline output-port-name-set! ::output-port ::bstring)
	    (inline output-port-position::long ::output-port)
	    (inline output-port-isatty?::bool ::output-port)
	    (inline set-output-port-position! ::output-port ::long)
	    (set-input-port-position! ::input-port ::long)
	    (inline input-port-position::long ::input-port)
	    (inline input-port-fill-barrier ::input-port)
	    (inline input-port-fill-barrier-set! ::input-port ::long)
	    (inline input-port-last-token-position::long ::input-port)
	    (inline input-port-reopen! ::input-port)
	    (inline input-port-clone! ::input-port ::input-port)
	    (inline input-port-name::bstring ::input-port)
	    (inline input-port-name-set! ::input-port ::bstring)
	    (inline input-port-length::elong ::input-port)
	    (inline output-port-close-hook::obj ::output-port)
	    (inline closed-output-port?::bool ::output-port)
	    (output-port-close-hook-set! ::output-port ::procedure)
	    (inline output-port-flush-hook::obj ::output-port)
	    (output-port-flush-hook-set! ::output-port ::obj)
	    (inline output-port-flush-buffer::obj ::output-port)
	    (inline output-port-flush-buffer-set! ::output-port ::obj)
	    (inline input-port-close-hook::obj ::input-port)
	    (input-port-close-hook-set! ::input-port ::procedure)
	    (inline input-port-seek::procedure ::input-port)
	    (input-port-seek-set! ::input-port ::procedure)

	    (inline input-port-buffer::bstring ::input-port)
	    (inline input-port-buffer-set! ::input-port ::bstring)
	    (inline output-port-buffer::bstring ::output-port)
	    (inline output-port-buffer-set! ::output-port ::bstring)
	    
	    (inline file-exists?::bool ::string)
	    (file-gzip? name)
	    (inline delete-file ::string)
	    (inline make-directory::bool ::string)
	    (make-directories::bool ::bstring)
	    (inline delete-directory ::string)
	    (inline rename-file::bool ::string ::string)
	    (inline truncate-file::bool ::string ::long)
	    (inline output-port-truncate::bool ::output-port ::long)
	    (copy-file ::string ::string)
	    (inline directory?::bool ::string)
	    (inline directory->list ::string)
	    (directory->path-list ::bstring)
	    (inline file-modification-time::elong ::string)
	    (inline file-change-time::elong ::string)
	    (inline file-access-time::elong ::string)
	    (inline file-times-set!::int ::string ::elong ::elong)
	    (inline file-size::elong ::string)
	    (inline file-uid::int ::string)
	    (inline file-gid::int ::string)
	    (inline file-mode::int ::string)
	    (inline file-type::symbol ::string)
            (inline make-symlink ::bstring ::bstring)
            (inline select::pair-nil #!key (timeout 0) (read '()) (write '()) (except '()))
	    (inline open-pipes #!optional name)
	    (input-port-protocol prototcol)
	    (input-port-protocol-set! protocol open)

	    (get-port-buffer::bstring ::obj ::obj ::int)
	    (lockf::bool ::output-port ::symbol #!optional (len 0)))
   
   (pragma  (c-input-port? (predicate-of input-port) nesting)
	    (c-output-port? (predicate-of output-port) nesting)
	    (c-output-string-port? nesting)
	    (c-input-string-port? nesting)
	    (file-exists? side-effect-free nesting)))

;*---------------------------------------------------------------------*/
;*    call-with-input-file ...                                         */
;*---------------------------------------------------------------------*/
(define (call-with-input-file string proc)
   (let ((port (open-input-file string)))
      (if (input-port? port)
	  (unwind-protect
	     (proc port)
	     (close-input-port port))
	  (error/errno $errno-io-port-error
	     "call-with-input-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    call-with-input-string ...                                       */
;*---------------------------------------------------------------------*/
(define (call-with-input-string string proc)
   (let ((port (open-input-string string)))
      ;; No need to force closing it, will be garbage collected anyhow.
      (let ((res (proc port)))
	 (close-input-port port)
	 res)))

;*---------------------------------------------------------------------*/
;*    call-with-output-file ...                                        */
;*---------------------------------------------------------------------*/
(define (call-with-output-file string proc) 
   (let ((port (open-output-file string)))
      (if (output-port? port)
	  (unwind-protect
	     (proc port)
	     (close-output-port port))
	  (error/errno $errno-io-port-error
	     "call-with-output-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    call-with-append-file ...                                        */
;*---------------------------------------------------------------------*/
(define (call-with-append-file string proc) 
   (let ((port (append-output-file string)))
      (if (output-port? port)
	  (unwind-protect
	     (proc port)
	     (close-output-port port))
	  (error/errno $errno-io-port-error
	     "call-with-append-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    call-with-output-string ...                                      */
;*---------------------------------------------------------------------*/
(define (call-with-output-string proc) 
   (let ((port (open-output-string)))
      (proc port)
      (close-output-port port)))

;*---------------------------------------------------------------------*/
;*    input-port? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (input-port? obj)
   (c-input-port? obj))

;*---------------------------------------------------------------------*/
;*    input-string-port? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (input-string-port? obj)
   (c-input-string-port? obj))

;*---------------------------------------------------------------------*/
;*    input-procedure-port? ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (input-procedure-port? obj)
   (c-input-procedure-port? obj))

;*---------------------------------------------------------------------*/
;*    input-gzip-port? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (input-gzip-port? obj)
   (c-input-gzip-port? obj))

;*---------------------------------------------------------------------*/
;*    output-port? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (output-port? obj)
   (c-output-port? obj))

;*---------------------------------------------------------------------*/
;*    output-string-port? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (output-string-port? obj)
   (c-output-string-port? obj))

;*---------------------------------------------------------------------*/
;*    output-procedure-port? ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (output-procedure-port? obj)
   (c-output-procedure-port? obj))

;*---------------------------------------------------------------------*/
;*    current-input-port ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (current-input-port)
   (c-current-input-port ($current-dynamic-env)))

;*---------------------------------------------------------------------*/
;*    current-output-port ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (current-output-port)
   (c-current-output-port ($current-dynamic-env)))

;*---------------------------------------------------------------------*/
;*    current-error-port ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (current-error-port)
   (c-current-error-port ($current-dynamic-env)))

;*---------------------------------------------------------------------*/
;*    @deffn input-port-reopen!@ ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (input-port-reopen! port::input-port)
   (unless ($input-port-reopen! port)
      (error/errno
	 $errno-io-port-error "input-port-reopen!" "Cannot reopen port" port)))

;*---------------------------------------------------------------------*/
;*    @deffn input-port-clone!! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (input-port-clone! dst::input-port src::input-port)
   ($input-port-clone! dst src))

;*---------------------------------------------------------------------*/
;*    @deffn with-input-from-file@ ...                                 */
;*---------------------------------------------------------------------*/
(define (with-input-from-file string thunk)
   (let ((port (open-input-file string)))
      (if (input-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-input-port (c-current-input-port denv)))
	     (unwind-protect
		(begin
		   (c-current-input-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-input-port-set! denv old-input-port)
		   (close-input-port port))))
	  (error/errno $errno-io-port-error
	     "with-input-from-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    @deffn with-input-from-string@ ...                               */
;*---------------------------------------------------------------------*/
(define (with-input-from-string string thunk)
   (let* ((port (open-input-string string))
	  (denv ($current-dynamic-env))
	  (old-input-port (c-current-input-port denv)))
      (unwind-protect
	 (begin
	    (c-current-input-port-set! denv port)
	    (thunk))
	 (begin
	    (c-current-input-port-set! denv old-input-port)
	    (close-input-port port)))))

;*---------------------------------------------------------------------*/
;*    with-input-from-port ...                                         */
;*---------------------------------------------------------------------*/
(define (with-input-from-port port thunk)
   (let* ((denv ($current-dynamic-env))
	  (old-input-port (c-current-input-port denv)))
      (unwind-protect
	 (begin
	    (c-current-input-port-set! denv port)
	    (thunk))
	 (c-current-input-port-set! denv old-input-port))))

;*---------------------------------------------------------------------*/
;*    @deffn with-input-from-procedure@ ...                            */
;*---------------------------------------------------------------------*/
(define (with-input-from-procedure proc thunk)
   (let ((port (open-input-procedure proc)))
      (if (input-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-input-port (c-current-input-port denv)))
	     (unwind-protect
		(begin
		   (c-current-input-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-input-port-set! denv old-input-port)
		   (close-input-port port))))
	  (error "with-input-from-procedure" "can't open procedure" proc))))

;*---------------------------------------------------------------------*/
;*    with-output-to-file ...                                          */
;*---------------------------------------------------------------------*/
(define (with-output-to-file string thunk)
   (let ((port (open-output-file string)))
      (if (output-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-output-port (c-current-output-port denv)))
	     (unwind-protect
		(begin
		   (c-current-output-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-output-port-set! denv old-output-port)
		   (close-output-port port))))
	  (error/errno $errno-io-port-error
	     "with-output-to-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    with-append-to-file ...                                          */
;*---------------------------------------------------------------------*/
(define (with-append-to-file string thunk)
   (let ((port (append-output-file string)))
      (if (output-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-output-port (c-current-output-port denv)))
	     (unwind-protect
		(begin
		   (c-current-output-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-output-port-set! denv old-output-port)
		   (close-output-port port))))
	  (error/errno $errno-io-port-error
	     "with-output-to-file" "can't open file" string))))

;*---------------------------------------------------------------------*/
;*    @deffn with-output-to-port@ ...                                  */
;*---------------------------------------------------------------------*/
(define (with-output-to-port port thunk)
   (let* ((denv ($current-dynamic-env))
	  (old-output-port (c-current-output-port denv)))
      (unwind-protect
	 (begin
	    (c-current-output-port-set! denv port)
	    (thunk))
	 (c-current-output-port-set! denv old-output-port))))

;*---------------------------------------------------------------------*/
;*    @deffn with-output-to-string@ ...                                */
;*---------------------------------------------------------------------*/
(define (with-output-to-string thunk)
   (let* ((port (open-output-string))
	  (denv ($current-dynamic-env))
	  (old-output-port (c-current-output-port denv))
	  (res #unspecified))
      (unwind-protect
	 (begin
	    (c-current-output-port-set! denv port)
	    (thunk))
	 (begin
	    (c-current-output-port-set! denv old-output-port)
	    (set! res (close-output-port port))))
      res))

;*---------------------------------------------------------------------*/
;*    with-output-to-procedure ...                                     */
;*---------------------------------------------------------------------*/
(define (with-output-to-procedure proc thunk)
   (let* ((port (open-output-procedure proc))
	  (denv ($current-dynamic-env))
	  (old-output-port (c-current-output-port denv))
	  (res #unspecified))
      (unwind-protect
	 (begin
	    (c-current-output-port-set! denv port)
	    (thunk))
	 (begin
	    (c-current-output-port-set! denv old-output-port)
	    (set! res (close-output-port port))))
      res))

;*---------------------------------------------------------------------*/
;*    @deffn with-error-to-string@ ...                                 */
;*---------------------------------------------------------------------*/
(define (with-error-to-string thunk)
   (let ((port (open-output-string)))
      (if (output-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-error-port (c-current-error-port denv))
		 (res #unspecified))
	     (unwind-protect
		(begin
		   (c-current-error-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-error-port-set! denv old-error-port)
		   (set! res (close-output-port port))))
	     res)
	  (error/errno $errno-io-port-error
		       'with-error-to-string
		       "can't open string"
		       #unspecified))))

;*---------------------------------------------------------------------*/
;*    @deffn with-error-to-file@ ...                                   */
;*---------------------------------------------------------------------*/
(define (with-error-to-file string thunk)
   (let ((port (open-output-file string)))
      (if (output-port? port)
	  (let* ((denv ($current-dynamic-env))
		 (old-output-port (c-current-error-port denv)))
	     (unwind-protect
		(begin
		   (c-current-error-port-set! denv port)
		   (thunk))
		(begin
		   (c-current-error-port-set! denv old-output-port)
		   (close-output-port port))))
	  (error/errno $errno-io-port-error
		       'with-error-to-file
		       "can't open file"
		       string))))

;*---------------------------------------------------------------------*/
;*    with-error-to-port ...                                           */
;*---------------------------------------------------------------------*/
(define (with-error-to-port port thunk)
   (let* ((denv ($current-dynamic-env))
	  (old-output-port (c-current-error-port denv)))
      (unwind-protect
	 (begin
	    (c-current-error-port-set! denv port)
	    (thunk))
	 (c-current-error-port-set! denv old-output-port))))

;*---------------------------------------------------------------------*/
;*    with-error-to-procedure ...                                      */
;*---------------------------------------------------------------------*/
(define (with-error-to-procedure proc thunk)
   (let* ((port (open-output-procedure proc))
	  (denv ($current-dynamic-env))
	  (old-error-port (c-current-error-port denv))
	  (res #unspecified))
      (unwind-protect
	 (begin
	    (c-current-error-port-set! denv port)
	    (thunk))
	 (begin
	    (c-current-error-port-set! denv old-error-port)
	    (set! res (close-output-port port))))
      res))

;*---------------------------------------------------------------------*/
;*    *input-port-protocols-mutex* ...                                 */
;*---------------------------------------------------------------------*/
(define *input-port-protocols-mutex* (make-mutex "input-port-protocols"))

;*---------------------------------------------------------------------*/
;*    *input-port-protocols* ...                                       */
;*---------------------------------------------------------------------*/
(define *input-port-protocols*
   `(("file:" . ,%open-input-file)
     ("string:" . ,(lambda (s p tmt) (open-input-string s)))
     ("| " . ,%open-input-pipe)
     ("pipe:" . ,%open-input-pipe)
     ("http://" . ,%open-input-http-socket)
     ("gzip:" . ,open-input-gzip-file)
     ("zlib:" . ,open-input-zlib-file)
     ("inflate:" . ,open-input-inflate-file)
     ("/resource/" . ,%open-input-resource)
     ("ftp://" . ,open-input-ftp-file)))

;*---------------------------------------------------------------------*/
;*    input-port-protocols ...                                         */
;*---------------------------------------------------------------------*/
(define (input-port-protocols)
   (synchronize *input-port-protocols-mutex*
      (reverse! (reverse *input-port-protocols*))))

;*---------------------------------------------------------------------*/
;*    input-port-protocol ...                                          */
;*---------------------------------------------------------------------*/
(define (input-port-protocol prototcol)
   (let ((cell (synchronize *input-port-protocols-mutex*
		  (assoc prototcol *input-port-protocols*))))
      (if (pair? cell)
	  (cdr cell)
	  #f)))

;*---------------------------------------------------------------------*/
;*    input-port-protocol-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (input-port-protocol-set! protocol open)
   (synchronize *input-port-protocols-mutex*
      (unless (and (procedure? open) (correct-arity? open 3))
	 (error "input-port-protocol-set!"
	    "Illegal open procedure for protocol"
	    protocol))
      (let ((c (assoc protocol *input-port-protocols*)))
	 (if (pair? c)
	     (set-cdr! c open)
	     (set! *input-port-protocols*
		(cons (cons protocol open) *input-port-protocols*)))))
   open)

;*---------------------------------------------------------------------*/
;*    get-port-buffer ...                                              */
;*---------------------------------------------------------------------*/
(define (get-port-buffer who bufinfo defsiz)
   (cond
      ((eq? bufinfo #t)
       ($make-string/wo-fill defsiz))
      ((eq? bufinfo #f)
       ($make-string/wo-fill 2))
      ((string? bufinfo)
       bufinfo)
      ((fixnum? bufinfo)
       (if (>=fx bufinfo 2)
	   ($make-string/wo-fill bufinfo)
	   ($make-string/wo-fill 2)))
      (else
       (error who "Illegal buffer" bufinfo))))

;*---------------------------------------------------------------------*/
;*    %open-input-file ...                                             */
;*---------------------------------------------------------------------*/
(define (%open-input-file string buf tmt)
   ($open-input-file string buf))

;*---------------------------------------------------------------------*/
;*    %open-input-pipe ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (%open-input-pipe string bufinfo tmt)
   (let ((buf (get-port-buffer "open-input-pipe" bufinfo 1024)))
      ($open-input-pipe string buf)))

;*---------------------------------------------------------------------*/
;*    %open-input-resource ...                                         */
;*---------------------------------------------------------------------*/
(define (%open-input-resource file bufinfo tmt)
   (let ((buf (get-port-buffer "open-input-file" bufinfo c-default-io-bufsiz)))
      ($open-input-resource file buf)))

;*---------------------------------------------------------------------*/
;*    %open-input-http-socket ...                                      */
;*---------------------------------------------------------------------*/
(define (%open-input-http-socket string bufinfo timeout)
   
   (define (parser ip status-code header clen tenc)
      (when (and (>=fx status-code 200) (<=fx status-code 299))
	 (cond
	    ((not (input-port? ip))
	     (open-input-string ""))
	    ((and (elong? clen) (>elong clen 0))
	     (input-port-fill-barrier-set! ip (elong->fixnum clen))
	     ($input-port-length-set! ip clen)
	     ip)
	    (else
	     ip))))
   
   (multiple-value-bind (protocol login host port abspath)
      (url-sans-protocol-parse string "http")
      (let loop ((ip #f)
		 (header '((user-agent: "Mozilla/5.0") (Connection: close))))
	 (let* ((sock (http :host host
			 :port port
			 :login login
			 :path abspath
			 :timeout timeout
			 :header header))
		(op (socket-output sock)))
	    (if (input-port? ip)
		;; the socket has been re-opened for seek, reuse
		;; the user input-port
		(input-port-clone! ip (socket-input sock))
		(set! ip (socket-input sock)))
	    (input-port-close-hook-set! ip
	       (lambda (ip)
		  (close-output-port op)
		  (socket-close sock)))
	    (input-port-seek-set! ip
	       (lambda (ip offset)
		  (socket-close sock)
		  (let ((r (string-append "bytes=" (integer->string offset) "-")))
		     (loop ip `((range: ,r) (user-agent: "Mozilla/5.0"))))))
	    (with-handler
	       (lambda (e)
		  (socket-close sock)
		  (when (isa? e &http-redirection)
		     (with-access::&http-redirection e (url)
			(open-input-file url bufinfo))))
	       (http-parse-response ip op parser))))))

;*---------------------------------------------------------------------*/
;*    open-input-file ...                                              */
;*    -------------------------------------------------------------    */
;*    This new version of open-input-file accept extended syntax.      */
;*    It may open plain file, strings, pipes, http/ftp files. The      */
;*    syntax is inspired by Web browsers, e.g.:                        */
;*        "/etc/passwd"                                                */
;*        "file:/etc/passwd"                                           */
;*        "string:bozo"                                                */
;*        "pipe:ls -lR /"                                              */
;*        "http://kaolin.unice.fr/Bigloo"                              */
;*        "http://localhost:10000/"                                    */
;*                                                                     */
;*    it also accepts the former form for pipes:                       */
;*        "| ls -lR /"                                                 */
;*    -------------------------------------------------------------    */
;*    Implementation note: remember that STRING-LENGTH is a O-macro    */
;*    thus, (STRING-LENGTH <a-constant-string>) is replaced with the   */
;*    actual length of the constant.                                   */
;*---------------------------------------------------------------------*/
(define (open-input-file string #!optional (bufinfo #t) (timeout 5000000))
   (let ((buffer (get-port-buffer "open-input-file" bufinfo c-default-io-bufsiz)))
      (let loop ((protos *input-port-protocols*))
	 (if (null? protos)
	     ;; a plain file
	     (%open-input-file string buffer timeout)
	     (let* ((cell (car protos))
		    (ident (car cell))
		    (l (string-length ident))
		    (open (cdr cell)))
		(if (substring=? string ident l)
		    ;; we have found the open function
		    (let ((name (substring string l (string-length string))))
		       (open name buffer timeout))
		    (loop (cdr protos))))))))

;*---------------------------------------------------------------------*/
;*    open-input-string ...                                            */
;*---------------------------------------------------------------------*/
(define (open-input-string string
	   #!optional (start 0) (end (string-length string)))
   (cond
      ((<fx start 0)
       (error "open-input-string" "Illegal start offset" start))
      ((>fx start (string-length string))
       (error "open-input-string" "Start offset out of bounds" start))
      ((>fx start end)
       (error "open-input-string" "Start offset greater than end" start))
      ((>fx end (string-length string))
       (error "open-input-string" "End offset out of bounds" end))
      (else
       ($open-input-substring string start end))))

;*---------------------------------------------------------------------*/
;*    open-input-string! ...                                           */
;*---------------------------------------------------------------------*/
(define (open-input-string! string
	   #!optional (start 0) (end (string-length string)))
   (cond
      ((<fx start 0)
       (error "open-input-string!" "Illegal start offset" start))
      ((>fx start (string-length string))
       (error "open-input-string!" "Start offset out of bounds" start))
      ((>fx start end)
       (error "open-input-string!" "Start offset greater than end" start))
      ((>fx end (string-length string))
       (error "open-input-string!" "End offset out of bounds" end))
      (else
       ($open-input-substring! string start end))))

;*---------------------------------------------------------------------*/
;*    open-input-procedure ...                                         */
;*---------------------------------------------------------------------*/
(define (open-input-procedure proc #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-procedure" bufinfo 1024)))
      ($open-input-procedure proc buf)))

;*---------------------------------------------------------------------*/
;*    open-input-gzip-port ...                                         */
;*---------------------------------------------------------------------*/
(define (open-input-gzip-port in::input-port #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-input-gzip-port" bufinfo c-default-io-bufsiz)))
      (port->gzip-port in buf)))

;*---------------------------------------------------------------------*/
;*    open-input-c-string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (open-input-c-string string)
   ($open-input-c-string string))

;*---------------------------------------------------------------------*/
;*    reopen-input-c-string ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (reopen-input-c-string port::input-port string)
   ($reopen-input-c-string port string))

;*---------------------------------------------------------------------*/
;*    input-port-timeout-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (input-port-timeout-set! port::input-port timeout::long)
   ($input-port-timeout-set! port timeout))

;*---------------------------------------------------------------------*/
;*    open-output-file ...                                             */
;*---------------------------------------------------------------------*/
(define (open-output-file string #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-file" bufinfo c-default-io-bufsiz)))
      ($open-output-file string buf)))

;*---------------------------------------------------------------------*/
;*    append-output-file ...                                           */
;*---------------------------------------------------------------------*/
(define (append-output-file string #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "append-output-file" bufinfo c-default-io-bufsiz)))
      ($append-output-file string buf)))

;*---------------------------------------------------------------------*/
;*    open-output-string ...                                           */
;*---------------------------------------------------------------------*/
(define (open-output-string #!optional (bufinfo #t))
   (let ((buf (get-port-buffer "open-output-file" bufinfo 128)))
      ($open-output-string buf)))

;*---------------------------------------------------------------------*/
;*    open-output-procedure ...                                        */
;*---------------------------------------------------------------------*/
(define (open-output-procedure proc
	   #!optional
	   (flush::procedure (lambda () #f))
	   (bufinfo #t)
	   (close::procedure (lambda () #f)))
   (cond
      ((not (correct-arity? proc 1))
       (error/errno $errno-io-port-error
	  "open-output-procedure"
	  "Illegal write procedure"
	  proc))
      ((or (not (procedure? flush))
	   (not (correct-arity? flush 0)))
       (error/errno $errno-io-port-error
	  "open-output-procedure"
	  "Illegal flush procedure"
	  flush))
      ((or (not (procedure? close))
	   (not (correct-arity? close 0)))
       (error/errno $errno-io-port-error
	  "open-output-procedure"
	  "Illegal close procedure"
	  flush))
      (else
       (let ((buf (get-port-buffer "open-output-procedure" bufinfo 128)))
	  ($open-output-procedure proc flush close buf)))))

;*---------------------------------------------------------------------*/
;*    output-port-timeout-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (output-port-timeout-set! port::output-port timeout::long)
   ($output-port-timeout-set! port timeout))

;*---------------------------------------------------------------------*/
;*    closed-input-port? ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (closed-input-port? port)
   (c-closed-input-port? port))

;*---------------------------------------------------------------------*/
;*    close-input-port ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (close-input-port port)
   ($close-input-port port))

;*---------------------------------------------------------------------*/
;*    get-output-string ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (get-output-string port)
   (c-get-output-string port))

;*---------------------------------------------------------------------*/
;*    close-output-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (close-output-port port)
   ($close-output-port port))

;*---------------------------------------------------------------------*/
;*    flush-output-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (flush-output-port port)
   (c-flush-output-port port))

;*---------------------------------------------------------------------*/
;*    reset-string-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (reset-output-port port)
   ($reset-output-port-error port)
   (if (c-output-string-port? port)
       ($reset-output-string-port port)
       (flush-output-port port)))

;*---------------------------------------------------------------------*/
;*    reset-eof ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (reset-eof port)
   (c-reset-eof port))

;*---------------------------------------------------------------------*/
;*    set-input-port-position! ...                                     */
;*---------------------------------------------------------------------*/
(define (set-input-port-position! port::input-port pos::long)
   (let ((useek ($input-port-useek port)))
      (if (procedure? useek)
	  (useek port pos)
	  ($set-input-port-position! port pos)))
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    input-port-position ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (input-port-position port)
   (c-input-port-position port))

;*---------------------------------------------------------------------*/
;*    input-port-fill-barrier ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (input-port-fill-barrier port::input-port)
   ($input-port-fill-barrier port))
   
;*---------------------------------------------------------------------*/
;*    input-port-fill-barrier-set! ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (input-port-fill-barrier-set! port::input-port pos::long)
   ($input-port-fill-barrier-set! port pos)
   pos)
   
;*---------------------------------------------------------------------*/
;*    input-port-last-token-position ...                               */
;*---------------------------------------------------------------------*/
(define-inline (input-port-last-token-position port)
   (c-input-port-last-token-position port))

;*---------------------------------------------------------------------*/
;*    output-port-name ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (output-port-name port)
   ($output-port-name port))

;*---------------------------------------------------------------------*/
;*    output-port-name-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (output-port-name-set! port name)
   ($output-port-name-set! port name))

;*---------------------------------------------------------------------*/
;*    set-output-port-position! ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (set-output-port-position! port::output-port pos::long)
   (unless (c-set-output-port-position! port pos)
      (error/errno $errno-io-port-error
	 "set-output-port-position!" "Cannot seek port" port)))
   
;*---------------------------------------------------------------------*/
;*    output-port-position ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (output-port-position port)
   (c-output-port-position port))

;*---------------------------------------------------------------------*/
;*    output-port-isatty? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (output-port-isatty? port)
   (cond-expand
      (bigloo-c ($port-isatty? port))
      (else #t)))
   
;*---------------------------------------------------------------------*/
;*    input-port-name ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (input-port-name port)
   ($input-port-name port))

;*---------------------------------------------------------------------*/
;*    input-port-name-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (input-port-name-set! port name)
   ($input-port-name-set! port name))

;*---------------------------------------------------------------------*/
;*    input-port-length ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (input-port-length port)
   ($input-port-length port))

;*---------------------------------------------------------------------*/
;*    output-port-close-hook ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (output-port-close-hook port)
   (c-output-port-chook port))

;*---------------------------------------------------------------------*/
;*    closed-output-port? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (closed-output-port? port)
   ($closed-output-port? port))

;*---------------------------------------------------------------------*/
;*    output-port-close-hook-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (output-port-close-hook-set! port proc)
   (if (not (and (procedure? proc) (correct-arity? proc 1)))
       (error/errno $errno-io-port-error
	  "output-port-close-hook-set!" "Illegal hook" proc)
       (begin
	  (c-output-port-chook-set! port proc)
	  proc)))

;*---------------------------------------------------------------------*/
;*    output-port-flush-hook ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (output-port-flush-hook port)
   ($output-port-fhook port))

;*---------------------------------------------------------------------*/
;*    output-port-flush-hook-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (output-port-flush-hook-set! port proc)
   (if (and (procedure? proc) (not (correct-arity? proc 2)))
       (error/errno $errno-io-port-error
	  "output-port-flush-hook-set!" "Illegal hook" proc)
       (begin
	  ($output-port-fhook-set! port proc)
	  proc)))

;*---------------------------------------------------------------------*/
;*    output-port-flush-buffer ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (output-port-flush-buffer port)
   ($output-port-flushbuf port))

;*---------------------------------------------------------------------*/
;*    output-port-flush-buffer-set! ...                                */
;*---------------------------------------------------------------------*/
(define-inline (output-port-flush-buffer-set! port buf)
   ($output-port-flushbuf-set! port buf)
   buf)

;*---------------------------------------------------------------------*/
;*    input-port-close-hook ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (input-port-close-hook port)
   ($input-port-chook port))

;*---------------------------------------------------------------------*/
;*    input-port-close-hook-set! ...                                   */
;*---------------------------------------------------------------------*/
(define (input-port-close-hook-set! port proc)
   (if (not (and (procedure? proc) (correct-arity? proc 1)))
       (error/errno $errno-io-port-error
	  "input-port-close-hook-set!" "Illegal hook" proc)
       (begin
	  ($input-port-chook-set! port proc)
	  proc)))

;*---------------------------------------------------------------------*/
;*    input-port-seek ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (input-port-seek port)
   ($input-port-useek port))

;*---------------------------------------------------------------------*/
;*    input-port-seek-set! ...                                         */
;*---------------------------------------------------------------------*/
(define (input-port-seek-set! port proc)
   (if (not (and (procedure? proc) (correct-arity? proc 2)))
       (error/errno $errno-io-port-error
	  "input-port-seek-set!" "Illegal seek procedure" proc)
       (begin
	  ($input-port-useek-set! port proc)
	  proc)))

;*---------------------------------------------------------------------*/
;*    input-port-buffer ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (input-port-buffer port)
   ($input-port-buffer port))

;*---------------------------------------------------------------------*/
;*    input-port-buffer-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (input-port-buffer-set! port buffer)
   (begin ($input-port-buffer-set! port buffer) port))

;*---------------------------------------------------------------------*/
;*    output-port-buffer ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (output-port-buffer port)
   ($output-port-buffer port))

;*---------------------------------------------------------------------*/
;*    output-port-buffer-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (output-port-buffer-set! port buffer)
   (begin ($output-port-buffer-set! port buffer) port))

;*---------------------------------------------------------------------*/
;*    file-exists? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (file-exists? name)
   (c-fexists? name))
   
;*---------------------------------------------------------------------*/
;*    file-gzip? ...                                                   */
;*---------------------------------------------------------------------*/
(define (file-gzip? name)
   (and (file-exists? name)
	(with-input-from-file name
	   (lambda ()
	      (with-handler
		 (lambda (e)
		    #f)
		 (gunzip-parse-header (current-input-port)))))))
   
;*---------------------------------------------------------------------*/
;*    delete-file ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (delete-file string)
   (not (c-delete-file string)))

;*---------------------------------------------------------------------*/
;*    make-directory ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (make-directory string)
   (c-mkdir string #o777))

;*---------------------------------------------------------------------*/
;*    make-directories ...                                             */
;*---------------------------------------------------------------------*/
(define (make-directories string)
   (or (directory? string)
       (make-directory string)
       (let ((dname (dirname string)))
	  (if (or (=fx (string-length dname) 0) (file-exists? dname))
	      #f
	      (let ((aux (make-directories dname)))
		 (if (char=? (string-ref string (-fx (string-length string) 1))
			     (file-separator))
		     aux
		     (make-directory string)))))))
      
;*---------------------------------------------------------------------*/
;*    delete-directory ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (delete-directory string)
   (not (c-delete-directory string)))

;*---------------------------------------------------------------------*/
;*    rename-file ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (rename-file string1 string2)
   (if (eq? (c-rename-file string1 string2) 0) #t #f))

;*---------------------------------------------------------------------*/
;*    truncate-file ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (truncate-file path size)
   ($truncate-file path size))

;*---------------------------------------------------------------------*/
;*    output-port-truncate ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (output-port-truncate oport size)
   ($ftruncate oport size))

;*---------------------------------------------------------------------*/
;*    copy-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (copy-file string1 string2)
   (let ((pi (open-input-binary-file string1))
	 (po (open-output-binary-file string2)))
      (cond
	 ((not (binary-port? pi))
	  (if (binary-port? po) (close-binary-port po))
	  #f)
	 ((not (binary-port? po))
	  (close-binary-port pi)
	  #f)
	 (else
	  (let ((s (make-string 1024)))
	     (let loop ((l (input-fill-string! pi s)))
		(if (=fx l 1024)
		    (begin
		       (output-string po s)
		       (loop (input-fill-string! pi s)))
		    (begin
		       (output-string po (string-shrink! s l))
		       (close-binary-port pi)
		       (close-binary-port po)
		       #t))))))))

;*---------------------------------------------------------------------*/
;*    port? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (port? obj)
   (or (output-port? obj) (input-port? obj)))

;*---------------------------------------------------------------------*/
;*    directory? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (directory? string)
   ($directory? string))

;*---------------------------------------------------------------------*/
;*    directory->list ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (directory->list string)
   ($directory->list string))

;*---------------------------------------------------------------------*/
;*    directory->path-list ...                                         */
;*---------------------------------------------------------------------*/
(define (directory->path-list dir)
   (let ((l (string-length dir)))
      (cond
	 ((=fx l 0)
	  '())
	 ((char=? (string-ref dir (-fx l 1)) (file-separator))
	  (cond-expand
	     (bigloo-c
	      ($directory->path-list dir (-fx l 1) (file-separator)))
	     (else
	      (map! (lambda (f) (string-append dir f))
		    (directory->list dir)))))
	 (else
	  (cond-expand
	     (bigloo-c
	      ($directory->path-list dir l (file-separator)))
	     (else
	      (map! (lambda (f) (make-file-name dir f))
		    (directory->list dir))))))))
	   
;*---------------------------------------------------------------------*/
;*    @deffn file-modification-time@ ...                               */
;*---------------------------------------------------------------------*/
(define-inline (file-modification-time file)
   ($modification-time file))

;*---------------------------------------------------------------------*/
;*    @deffn file-change-time@ ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (file-change-time file)
   ($change-time file))

;*---------------------------------------------------------------------*/
;*    @deffn file-access-time@ ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (file-access-time file)
   ($access-time file))

;*---------------------------------------------------------------------*/
;*    @deffn file-times-set!@ ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (file-times-set! file atime mtime)
   ($utime file atime mtime))

;*---------------------------------------------------------------------*/
;*    @deffn file-size@ ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (file-size file)
   ($file-size file))

;*---------------------------------------------------------------------*/
;*    @deffn file-uid@ ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (file-uid file)
   ($file-uid file))

;*---------------------------------------------------------------------*/
;*    @deffn file-gid@ ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (file-gid file)
   ($file-gid file))

;*---------------------------------------------------------------------*/
;*    @deffn file-mode@ ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (file-mode file)
   ($file-mode file))

;*---------------------------------------------------------------------*/
;*    @deffn file-type@ ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (file-type file)
   ($file-type file))

;*---------------------------------------------------------------------*/
;*    make-symlink ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (make-symlink path1 path2)
   ($symlink path1 path2))
       
;*---------------------------------------------------------------------*/
;*    select ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (select #!key (timeout 0) (read '()) (write '()) (except '()))
   ($select timeout read write except))

;*---------------------------------------------------------------------*/
;*    open-pipes ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (open-pipes #!optional name)
   ($open-pipes name))

;*---------------------------------------------------------------------*/
;*    lockf ...                                                        */
;*---------------------------------------------------------------------*/
(define (lockf port cmd #!optional (len 0))
   (cond-expand
      (bigloo-c
       (case cmd
	  ((lock) ($lockf port $F_LOCK len))
	  ((tlock) ($lockf port $F_TLOCK len))
	  ((ulock) ($lockf port $F_ULOCK len))
	  ((test) ($lockf port $F_TEST len))
	  (else (error "lockf" "Bad command" cmd))))
      (else #f)))
	   
