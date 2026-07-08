;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/api/bpmlib/src/Llib/verify.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul  7 08:11:45 2026                          */
;*    Last change :  Wed Jul  8 14:08:29 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    BPM verifiers.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    __bpmlib_verify ...                                              */
;*---------------------------------------------------------------------*/
(module __bpmlib_verify
   (export verify-package
      verify-package-info))

;*---------------------------------------------------------------------*/
;*    verify-package ...                                               */
;*---------------------------------------------------------------------*/
(define (verify-package pkg dir)
   (if (klist? pkg)
       (begin
          (for-each (lambda (k)
                       (let ((v (getk (car k) pkg)))
                          (cond
                             ((symbol? v)
                              (error/loc "package"
                                 "package missing property" (car k)
                                 pkg))
                             ((not ((cadr k) v))
                              (error/loc "package"
                                 (format "wrong package property \"~s\"" (car k))
                                 v pkg)))))
             ;; required properties
             `((:name ,name?)
               (:description ,description?)
               (:homepage ,url?)))
          (let ((deps (getk dependencies: pkg))
                (devdeps (getk devDependencies: pkg)))
             (unless (memq deps '(not-found multiple))
                (if (slist? deps)
                    (for-each verify-dependency deps)
                    (error "package" "Illegal dependency" deps)))
             (unless (memq devdeps '(not-found multiple))
                (if (slist? devdeps)
                    (for-each verify-dependency devdeps)
                    (error "package" "Illegal dependency" deps))))
          ;; check package and directory names
          (unless (equal? dir (getk name: pkg))
             (error "package" "package name and directory name mismatch" dir)))
       (error/loc "package" "Illegal package" pkg pkg)))

;*---------------------------------------------------------------------*/
;*    verify-dependency ...                                            */
;*---------------------------------------------------------------------*/
(define (verify-dependency dep)
   (unless (name? (car dep))
      (error/loc "package" "Illegal dependency" dep dep))
   (unless (or (url? (cadr dep)) (range? (cadr dep)))
      (error/loc "package" "Illegal dependency version" dep dep)))
      
;*---------------------------------------------------------------------*/
;*    verify-package-info ...                                          */
;*---------------------------------------------------------------------*/
(define (verify-package-info info)
   (if (klist? info)
       (for-each (lambda (k)
                    (let ((v (getk (car k) info)))
                       (cond
                          ((symbol? v)
                           (error/loc "package-info"
                              "package missing property" (car k)
                              info))
                          ((not ((cadr k) v))
                           (error/loc "package-info"
                              (format "wrong package property \"~s\"" (car k))
                              v info)))))
          `((:checksum ,checksum?)
            (:description ,description?)
            (:url ,package-url?)
            (:homepage ,url?)))
       (error/loc "package-info" "Illegal package-info" info info)))

;*---------------------------------------------------------------------*/
;*    name? ...                                                        */
;*---------------------------------------------------------------------*/
(define (name? name)
   (when (string? name)
      (pregexp-match "[[:alpha:]][[:alpha:][:digit:]_$-]+" name)))

;*---------------------------------------------------------------------*/
;*    version? ...                                                     */
;*---------------------------------------------------------------------*/
(define (version? version)
   (when (string? version)
      (pregexp-match "[[:digit:]]+.[[:digit:]]+.[[:digit:]]+" version)))

;*---------------------------------------------------------------------*/
;*    range? ...                                                       */
;*---------------------------------------------------------------------*/
(define (range? version)
   (version? version))

;*---------------------------------------------------------------------*/
;*    package-url? ...                                                 */
;*---------------------------------------------------------------------*/
(define (package-url? url)
   (when (string? url)
      (pregexp-match "([[:alpha:]]+)://[^/]+/.+/([^/]+[.]tgz)" url)))
      
;*---------------------------------------------------------------------*/
;*    url? ...                                                         */
;*---------------------------------------------------------------------*/
(define (url? url)
   (when (string? url)
      (pregexp-match "([[:alpha:]]+)://[^/]+/.+" url)))

;*---------------------------------------------------------------------*/
;*    checksum? ...                                                    */
;*---------------------------------------------------------------------*/
(define (checksum? checksum)
   (when (string? checksum)
      (let ((l (string-length checksum)))
         (and (or (=fx l 64) (=fx l 128))
              (pregexp-match "[[:xdigit:]]+" checksum)))))

;*---------------------------------------------------------------------*/
;*    description? ...                                                 */
;*---------------------------------------------------------------------*/
(define (description? descr)
   (string? descr))

;*---------------------------------------------------------------------*/
;*    klist? ...                                                       */
;*---------------------------------------------------------------------*/
(define (klist? l)
   (let loop ((l l))
      (cond
         ((null? l) #t)
         ((not (pair? l)) #f)
         ((not (pair? (car l))) #f)
         ((not (keyword? (caar l))) #f)
         ((not (pair? (cdar l))) #f)
         ((not (null? (cddr l))) #f)
         (else (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    slist? ...                                                       */
;*---------------------------------------------------------------------*/
(define (slist? l)
   (let loop ((l l))
      (cond
         ((null? l) #t)
         ((not (pair? l)) #f)
         ((not (pair? (car l))) #f)
         ((not (keyword? (caar l))) #f)
         ((not (pair? (cdar l))) #f)
         ((not (null? (cddr l))) #f)
         (else (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    getk ...                                                         */
;*---------------------------------------------------------------------*/
(define (getk key l)
   (let loop ((l l))
      (cond
         ((null? l)
          'not-found)
         ((eq? (caar l) key)
          (if (pair? (assq key (cdr l)))
              'multiple
              (cadr l)))
         (else
          (loop (cdr l))))))
      
;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc proc msg obj container)
   (match-case (cond
                ((epair? obj) (cer obj))
                ((epair? container) (cer container))
                (else #f))
      ((at ?fname ?loc) (error/location proc msg obj fname loc))
      (else (error proc msg obj))))

