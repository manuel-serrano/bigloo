;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/bconfigure.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 29 09:31:00 2000                          */
;*    Last change :  Tue Sep  8 20:46:29 2015 (serrano)                */
;*    Copyright   :  2000-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The machine dependent configuration.                             */
;*    -------------------------------------------------------------    */
;*    In order to avoid daunting bootstrap problem, I have decided not */
;*    to produce this file automatically. It is written and maintained */
;*    by (my) hand.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __configure
   
   (import  __error)
   
   (use     __type
	    __tvector
	    __bit
	    __bigloo
	    __param
	    __bexit
	    __object
	    __thread
	    __bignum
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_vectors_6_8
	    __r5_control_features_6_4
	    __r4_output_6_10_3
	    __r4_ports_6_10_1)

   (extern (macro $configure-release-number::string "BGL_RELEASE_NUMBER")
	   (macro $configure-specific-version::string "BGL_SPECIFIC_VERSION")
	   (macro $configure-homeurl::string "BGL_HOMEURL")
	   (macro $configure-shell::string "SHELL")
	   (macro $configure-shell-mv::string "BGL_SHELL_MV")
	   (macro $configure-shell-rm::string "BGL_SHELL_RM")
           (macro $configure-c-compiler-style::string "C_COMPILER_STYLE")
	   (macro $configure-c-compiler::string "C_COMPILER")
	   (macro $configure-c-compiler-o-option::string "C_COMPILER_O_OPTION")
           (macro $configure-c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	   (macro $configure-c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	   (macro $configure-c-compiler-rpath::string "C_COMPILER_RPATH")
	   (macro $configure-c-flag::string "C_FLAGS")
	   (macro $configure-c-strip-flag::string "C_STRIP_FLAGS")
	   (macro $configure-c-prof-flag::string "C_PROFILE_FLAGS")
           (macro $configure-c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	   (macro $configure-c-string-split::bool "C_STRING_SPLIT")
           (macro $configure-c-linker-style::string "C_LINKER_STYLE")
	   (macro $configure-c-linker-o-option::string "C_LINKER_O_OPTION")
           (macro $configure-c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
	   (macro $configure-c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	   (macro $configure-ld-library-dir::string "BGL_LD_LIBRARY_DIR")
	   (macro $configure-library-directory::string "LIBRARY_DIRECTORY")
	   (macro $configure-non-custom-gc-directory::string "BGL_NON_CUSTOM_GC_DIR")
 	   (macro $configure-zip-directory::string "ZIP_DIRECTORY")
 	   (macro $configure-dll-directory::string "DLL_DIRECTORY")
 	   (macro $configure-user-libraries::string "USER_LIBRARIES")
	   (macro $configure-c-beautifier::string "C_BEAUTIFIER")
	   (macro $configure-dirname-cmd::string "DIRNAME_CMD")
	   (macro $configure-library-base-name::string "LIBRARY_BASE_NAME")
	   (macro $configure-heap-debug-copt::string "BGL_HEAP_DEBUG_COPT")
 	   (macro $configure-have-shared-library::bool "HAVE_SHARED_LIBRARY")
	   (macro $configure-shared-link-option::string "ADDITIONAL_SHARED_LINK_OPTION")
	   (macro $configure-static-link-option::string "ADDITIONAL_STATIC_LINK_OPTION")
	   (macro $configure-auto-finalizer::bool "BGL_AUTO_FINALIZER")
	   (macro $configure-have-dlopen::bool "HAVE_DLOPEN")
	   (macro $configure-dlopen-lib::string "DLOPEN_LD_OPT")
	   (macro $configure-have-bigloo-abort::bool "BGL_HAVE_BIGLOO_ABORT")
	   (macro $configure-java::string "BGL_JAVA")
	   (macro $configure-jar::string "BGL_JAR")
	   (macro $configure-java-shell::string "BGL_JAVA_SHELL")
	   (macro $configure-jflags::string "BGL_JAVA_OPT")
	   (macro $configure-jvflags::string "BGL_JAVA_VOPT")
	   (macro $configure-default-back-end::string "BGL_DEFAULT_BACK_END")
	   (macro $configure-gc-lib::string "BGL_GC_LIBRARY")
	   (macro $configure-gc-custom::bool "BGL_GC_CUSTOM")
	   (macro $configure-have-bdb::bool "BGL_HAVE_BDB")
	   (macro $configure-dns-cache-enabled::bool "BGL_DNS_CACHE")
	   (macro $configure-big-endian::bool "BGL_BIG_ENDIAN")
	   (macro $configure-regexp-family::string "BGL_REGEXP_FAMILY")
	   (macro $configure-int-size::int "BGL_INT_BIT_SIZE")
	   (macro $configure-elong-size::int "BGL_ELONG_BIT_SIZE")
	   (macro $configure-have-unistring::bool "BGL_HAVE_UNISTRING"))
   
   (java   (class $configure
	      (field static release-number::string "BGL_RELEASE_NUMBER")
	      (field static specific-version::string "BGL_SPECIFIC_VERSION")
	      (field static homeurl::string "BGL_HOMEURL")
	      (field static shell::string "SHELL")
	      (field static shell-mv::string "BGL_SHELL_MV")
	      (field static shell-rm::string "BGL_SHELL_RM")
	      (field static c-compiler-style::string "C_COMPILER_STYLE")
              (field static c-compiler::string "C_COMPILER")
	      (field static c-compiler-o-option::string "C_COMPILER_O_OPTION")
              (field static c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	      (field static c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	      (field static c-compiler-rpath::string "C_COMPILER_RPATH")
	      (field static c-flag::string "C_FLAGS")
	      (field static c-strip-flag::string "C_STRIP_FLAGS")
	      (field static c-prof-flag::string "C_PROFILE_FLAGS")
              (field static c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	      (field static c-string-split::bool "C_STRING_SPLIT")
              (field static c-linker-style::string "C_LINKER_STYLE")
	      (field static c-linker-o-option::string "C_LINKER_O_OPTION")
              (field static c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
              (field static c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	      (field static ld-library-dir::string "LD_LIBRARY_DIR")
	      (field static library-directory::string "LIBRARY_DIRECTORY")
	      (field static non-custom-gc-directory::string "BGL_NON_CUSTOM_GC_DIR")
	      (field static zip-directory::string "ZIP_DIRECTORY")
	      (field static dll-directory::string "DLL_DIRECTORY")
	      (field static user-libraries::string "USER_LIBRARIES")
	      (field static c-beautifier::string "C_BEAUTIFIER")
	      (field static dirname-cmd::string "DIRNAME_CMD")
	      (field static library-base-name::string "LIBRARY_BASE_NAME")
	      (field static heap-debug-copt::string "BGL_HEAP_DEBUG_COPT")
	      (field static have-shared-library::bool "HAVE_SHARED_LIBRARY")
	      (field static shared-link-option::string "ADDITIONAL_SHARED_LINK_OPTION")
	      (field static static-link-option::string "ADDITIONAL_STATIC_LINK_OPTION")
	      (field static auto-finalizer::bool "BGL_AUTO_FINALIZER")
	      (field static have-dlopen::bool "HAVE_DLOPEN")
	      (field static dlopen-lib::string "DLOPEN_LD_OPT")
	      (field static have-bigloo-abort::bool "BGL_HAVE_BIGLOO_ABORT")
	      (field static java::string "BGL_JAVA")
	      (field static jar::string "BGL_JAR")
	      (field static java-shell::string "BGL_JAVA_SHELL")
	      (field static jflags::string "BGL_JAVA_OPT")
	      (field static jvflags::string "BGL_JAVA_VOPT")
	      (field static default-back-end::string "BGL_DEFAULT_BACK_END")
	      (field static gc-lib::string "BGL_GC_LIBRARY")
	      (field static gc-custom::bool "BGL_GC_CUSTOM")
	      (field static have-bdb::bool "BGL_HAVE_BDB")
	      (field static dns-cache-enabled::bool "BGL_DNS_CACHE")
	      (field static big-endian::bool "BGL_BIG_ENDIAN")
	      (field static regexp-family::string "BGL_REGEXP_FAMILY")
	      (field static int-size::int "BGL_INT_BIT_SIZE")
	      (field static elong-size::int "BGL_ELONG_BIT_SIZE")
	      (field static have-unistring::bool "BGL_HAVE_UNISTRING")
	      "bigloo.configure"))

   (export    (bigloo-config #!optional config)
	      (bigloo-configuration::pair)
	      (bigloo-configuration-add-entry! ::symbol ::obj)))

;*---------------------------------------------------------------------*/
;*    library-safety ...                                               */
;*---------------------------------------------------------------------*/
(define (library-safety)
   (cond-expand
      (bigloo-unsafe 'unsafe)
      (bigloo-safe 'safe)
      (bigloo-profile 'profile)
      (else (error "library-safety" "no safety specified" #unspecified))))

;*---------------------------------------------------------------------*/
;*    *bigloo-configuration* ...                                       */
;*---------------------------------------------------------------------*/
(define *bigloo-configuration*
   `((release-number . ,$configure-release-number)
     (specific-version . ,$configure-specific-version)
     (library-safety . ,(library-safety))
     (homeurl . ,$configure-homeurl)
     (shell . ,$configure-shell)
     (c-compiler-style . ,$configure-c-compiler-style)
     (c-compiler . ,$configure-c-compiler)
     (c-compiler-o-option . ,$configure-c-compiler-o-option)
     (c-compiler-debug-option . ,$configure-c-compiler-debug-option)
     (c-compiler-optim-flag . ,$configure-c-compiler-optim-flag)
     (c-compiler-rpath . ,$configure-c-compiler-rpath)
     (c-flag . ,$configure-c-flag)
     (c-strip-flag . ,$configure-c-strip-flag)
     (c-prof-flag . ,$configure-c-prof-flag)
     (c-object-file-extension . ,$configure-c-object-file-extension)
     (c-string-split . ,$configure-c-string-split)
     (c-linker-style . ,$configure-c-linker-style)
     (c-linker-o-option . ,$configure-c-linker-o-option)
     (c-linker-debug-option . ,$configure-c-linker-debug-option)
     (c-linker-optim-flags . ,$configure-c-linker-optim-flags)
     (ld-library-dir . ,$configure-ld-library-dir)
     (library-directory . ,$configure-library-directory)
     (non-custom-gc-directory . ,$configure-non-custom-gc-directory)
     (zip-directory . ,$configure-zip-directory)
     (dll-directory . ,$configure-dll-directory)
     (user-libraries . ,$configure-user-libraries)
     (c-beautifier . ,$configure-c-beautifier)
     (dirname-cmd . ,$configure-dirname-cmd)
     (library-base-name . ,$configure-library-base-name)
     (heap-debug-copt . ,$configure-heap-debug-copt)
     (have-shared-library . ,$configure-have-shared-library)
     (shared-link-option . ,$configure-shared-link-option)
     (static-link-option . ,$configure-static-link-option)
     (auto-finalizer . ,$configure-auto-finalizer)
     (have-dlopen . ,$configure-have-dlopen)
     (dlopen-lib . ,$configure-dlopen-lib)
     (have-bigloo-abort . ,$configure-have-bigloo-abort)
     (java . ,$configure-java)
     (jar . ,$configure-jar)
     (java-shell . ,$configure-java-shell)
     (jflags . ,$configure-jflags)
     (jvflags . ,$configure-jvflags)
     (default-back-end . ,$configure-default-back-end)
     (gc-lib . ,$configure-gc-lib)
     (gc-custom . ,$configure-gc-custom)
     (have-bdb . ,$configure-have-bdb)
     (dns-cache-enabled . ,$configure-dns-cache-enabled)
     (shell-mv . ,$configure-shell-mv)
     (shell-rm . ,$configure-shell-rm)
     (endianess . ,(if $configure-big-endian 'big-endian 'little-endian))
     (regexp . ,$configure-regexp-family)
     (int-size . ,$configure-int-size)
     (elong-size . ,$configure-elong-size)
     (have-unistring . ,$configure-have-unistring)))

;*---------------------------------------------------------------------*/
;*    bigloo-config ...                                                */
;*---------------------------------------------------------------------*/
(define (bigloo-config #!optional config)
   (if (not config)
       (bigloo-configuration)
       (let ((c (assq config (bigloo-configuration))))
	  (if (pair? c)
	      (cdr c)
	      #unspecified))))

;*---------------------------------------------------------------------*/
;*    bigloo-configuration ...                                         */
;*---------------------------------------------------------------------*/
(define (bigloo-configuration)
   (list-copy *bigloo-configuration*))

;*---------------------------------------------------------------------*/
;*    bigloo-configuration-add-entry! ...                              */
;*---------------------------------------------------------------------*/
(define (bigloo-configuration-add-entry! key val)
   (set! *bigloo-configuration* (cons (cons key val) *bigloo-configuration*)))
