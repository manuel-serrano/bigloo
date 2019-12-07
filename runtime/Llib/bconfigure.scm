;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Llib/bconfigure.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 29 09:31:00 2000                          */
;*    Last change :  Sat Dec  7 18:48:05 2019 (serrano)                */
;*    Copyright   :  2000-19 Manuel Serrano                            */
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
	   (macro $configure-c-ld::string "C_LD")
	   (macro $configure-c-compiler-o-option::string "C_COMPILER_O_OPTION")
           (macro $configure-c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	   (macro $configure-c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	   (macro $configure-c-compiler-rpath::string "C_COMPILER_RPATH")
	   (macro $configure-c-flags::string "C_FLAGS")
	   (macro $configure-c-pic-flag::string "C_PICFLAGS")
	   (macro $configure-c-nan-flag::string "C_NANFLAGS")
	   (macro $configure-c-strip-flag::string "C_STRIP_FLAGS")
	   (macro $configure-c-prof-flag::string "C_PROFILE_FLAGS")
           (macro $configure-c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	   (macro $configure-c-string-split::bool "C_STRING_SPLIT")
           (macro $configure-c-linker-style::string "C_LINKER_STYLE")
	   (macro $configure-c-linker-flags::string "C_LINKER_FLAGS")
	   (macro $configure-c-linker-o-option::string "C_LINKER_O_OPTION")
           (macro $configure-c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
	   (macro $configure-c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	   (macro $configure-c-linker-soname-option::string "C_LINKER_SONAME_OPTION")
	   (macro $configure-c-linker-shared-option::string "C_LINKER_SHARED_OPTION")
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
	   (macro $configure-shared-lib-suffix::string "SHARED_LIB_SUFFIX")
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
	   (macro $configure-gc::int "BGL_GC")
	   (macro $configure-have-bdb::bool "BGL_HAVE_BDB")
	   (macro $configure-dns-cache-enabled::bool "BGL_DNS_CACHE")
	   (macro $configure-big-endian::bool "BGL_BIG_ENDIAN")
	   (macro $configure-regexp-family::string "BGL_REGEXP_FAMILY")
	   (macro $configure-nan-tagging::bool "BGL_NAN_TAGGING")
	   (macro $configure-int-size::int "BGL_INT_BIT_SIZE")
	   (macro $configure-elong-size::int "BGL_ELONG_BIT_SIZE")
	   (macro $configure-have-unistring::bool "BGL_HAVE_UNISTRING")
	   (macro $configure-have-syslog::bool "BGL_HAVE_SYSLOG")
	   (macro $configure-have-overflow::bool "BGL_HAVE_OVERFLOW")
	   (macro $configure-class-display-min-size::long "BGL_CLASS_DISPLAY_MIN_SIZE")
	   (macro $configure-os-class::string "OS_CLASS")
	   (macro $configure-os-name::string "OS_NAME")
	   (macro $configure-os-arch::string "OS_ARCH")
	   (macro $configure-os-version::string "OS_VERSION")
	   (macro $configure-thread-local-storage::bool "BGL_HAS_THREAD_LOCALSTORAGE")
	   (macro $configure-have-spinlock::bool "BGL_HAVE_SPINLOCK")
	   (macro $cfg-no-gc::long "BGL_NO_GC")
	   (macro $cfg-boehm-gc::long "BGL_BOEHM_GC")
	   (macro $cfg-saw-gc::long "BGL_SAW_GC")
	   (macro $configure-have-alloca::bool "BGL_HAVE_ALLOCA")
	   (macro $configure-have-c99stackalloc::bool "BGL_HAVE_C99STACKALLOC")
	   )
   
   (java   (class $configure
	      (field static release-number::string "BGL_RELEASE_NUMBER")
	      (field static specific-version::string "BGL_SPECIFIC_VERSION")
	      (field static homeurl::string "BGL_HOMEURL")
	      (field static shell::string "SHELL")
	      (field static shell-mv::string "BGL_SHELL_MV")
	      (field static shell-rm::string "BGL_SHELL_RM")
	      (field static c-compiler-style::string "C_COMPILER_STYLE")
              (field static c-compiler::string "C_COMPILER")
              (field static c-ld::string "C_LD")
	      (field static c-compiler-o-option::string "C_COMPILER_O_OPTION")
              (field static c-compiler-debug-option::string "C_COMPILER_DEBUG_OPTION")
	      (field static c-compiler-optim-flag::string "C_COMPILER_OPTIM_FLAGS")
	      (field static c-compiler-rpath::string "C_COMPILER_RPATH")
	      (field static c-flags::string "C_FLAGS")
	      (field static c-pic-flag::string "C_PICFLAGS")
	      (field static c-nan-flag::string "C_NANFLAGS")
	      (field static c-strip-flag::string "C_STRIP_FLAGS")
	      (field static c-prof-flag::string "C_PROFILE_FLAGS")
              (field static c-object-file-extension::string "C_OBJECT_FILE_EXTENSION")
	      (field static c-string-split::bool "C_STRING_SPLIT")
              (field static c-linker-style::string "C_LINKER_STYLE")
	      (field static c-linker-flags::string "C_LINKER_FLAGS")
	      (field static c-linker-o-option::string "C_LINKER_O_OPTION")
              (field static c-linker-debug-option::string "C_LINKER_DEBUG_OPTION")
              (field static c-linker-optim-flags::string "C_LINKER_OPTIM_FLAGS")
	      (field static c-linker-soname-option::string "C_LINKER_SONAME_OPTION")
	      (field static c-linker-shared-option::string "C_LINKER_SHARED_OPTION")
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
	      (field static shared-lib-suffix::string "SHARED_LIB_SUFFIX")
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
	      (field static nan-tagging::bool "BGL_NAN_TAGGING")
	      (field static have-unistring::bool "BGL_HAVE_UNISTRING")
	      (field static have-syslog::bool "BGL_HAVE_SYSLOG")
	      (field static have-overflow::bool "BGL_HAVE_OVERFLOW")
	      (field static class-display-min-size::int "BGL_CLASS_DISPLAY_MIN_SIZE")
	      (field static os-class::string "BGL_OS_CLASS")
	      (field static os-name::string "BGL_OS_NAME")
	      (field static os-arch::string "BGL_OS_ARCH")
	      (field static os-version::string "BGL_OS_VERSION")
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
     (c-ld . ,$configure-c-ld)
     (c-compiler-o-option . ,$configure-c-compiler-o-option)
     (c-compiler-debug-option . ,$configure-c-compiler-debug-option)
     (c-compiler-optim-flag . ,$configure-c-compiler-optim-flag)
     (c-compiler-rpath . ,$configure-c-compiler-rpath)
     (c-flags . ,$configure-c-flags)
     (c-pic-flag . ,$configure-c-pic-flag)
     (c-nan-flag . ,$configure-c-nan-flag)
     (c-strip-flag . ,$configure-c-strip-flag)
     (c-prof-flag . ,$configure-c-prof-flag)
     (c-object-file-extension . ,$configure-c-object-file-extension)
     (c-string-split . ,$configure-c-string-split)
     (c-linker-style . ,$configure-c-linker-style)
     (c-linker-flags . ,$configure-c-linker-flags)
     (c-linker-o-option . ,$configure-c-linker-o-option)
     (c-linker-debug-option . ,$configure-c-linker-debug-option)
     (c-linker-optim-flags . ,$configure-c-linker-optim-flags)
     (c-linker-soname-option . ,$configure-c-linker-soname-option)
     (c-linker-shared-option . ,$configure-c-linker-shared-option)
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
     (shared-lib-suffix . ,$configure-shared-lib-suffix)
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
     (gc . ,(cond-expand (bigloo-c (gc-name $configure-gc)) (else "java")))
     (have-bdb . ,$configure-have-bdb)
     (dns-cache-enabled . ,$configure-dns-cache-enabled)
     (shell-mv . ,$configure-shell-mv)
     (shell-rm . ,$configure-shell-rm)
     (endianess . ,(if $configure-big-endian 'big-endian 'little-endian))
     (regexp . ,$configure-regexp-family)
     (int-size . ,$configure-int-size)
     (elong-size . ,$configure-elong-size)
     (nan-tagging . ,$configure-nan-tagging)
     (have-unistring . ,$configure-have-unistring)
     (have-syslog . ,$configure-have-syslog)
     (have-overflow . ,$configure-have-overflow)
     (class-display-min-size . ,$configure-class-display-min-size)
     (os-class . ,$configure-os-class)
     (os-name . ,$configure-os-name)
     (os-arch . ,$configure-os-arch)
     (os-version . ,$configure-os-version)
     (thread-local-storage . ,(cond-expand (bigloo-c $configure-thread-local-storage) (else #f)))
     (have-spinlock . ,(cond-expand (bigloo-c $configure-have-spinlock) (else #f)))
     (have-alloca . ,(cond-expand (bigloo-c $configure-have-alloca) (else #f)))
     (have-c99-stack-alloc . ,(cond-expand (bigloo-c $configure-have-c99stackalloc) (else #f)))))

;*---------------------------------------------------------------------*/
;*    bigloo-config ...                                                */
;*---------------------------------------------------------------------*/
(define (bigloo-config #!optional config)
   (if (not config)
       (bigloo-configuration)
       (let ((c (assq config *bigloo-configuration*)))
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
   (let ((old (assq key *bigloo-configuration*)))
      (if (pair? old)
	  (set-cdr! old val)
	  (set! *bigloo-configuration*
	     (cons (cons key val) *bigloo-configuration*)))))

;*---------------------------------------------------------------------*/
;*    gc-name ...                                                      */
;*---------------------------------------------------------------------*/
(define (gc-name::bstring num)
   (cond-expand
      (bigloo-c
       (cond
	  ((=fx num $cfg-no-gc) "no")
	  ((=fx num $cfg-boehm-gc) "boehm")
	  ((=fx num $cfg-saw-gc) "saw")
	  (else "unknown")))
      (else
       "unknown")))
   
