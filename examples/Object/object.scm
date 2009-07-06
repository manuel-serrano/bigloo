;<font size="-3"><pre>
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Object/object.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Mar  6 10:46:53 1997                          */
;*    Last change :  Thu Mar  6 11:00:33 1997 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An example of Bigloo objects                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module wedding
   (static (final-class person 
	      name::string
	      fname::string
	      (sex::symbol read-only))
           (wide-class married-man::person
	      mate::person)
           (wide-class married-woman::person
	      maiden-name::string
	      mate::person)))

(define-generic (person-print p::person . op)
   (error "person-print" "No method defined for this object" p))

(define-method (person-print p::person . op)
   (with-access::person p (name fname sex)
      (print "firstname : " fname)
      (print "name      : " name)
      (print "sex       : " sex)
      p))

(define-method (person-print p::married-woman . op)
   (with-access::married-woman p (name fname sex mate)
      (call-next-method)
      (print "married to: " (person-fname mate) 
                            " " 
                            (person-name mate))
      p))

(define (birth name::string fname::string sex)
   [assert (sex) (memq sex '(male female))]
   (instantiate::person 
      (name name)
      (fname fname)
      (sex sex)))

(define (get-married! woman::person man::person)
   (if (not (and (eq? (person-sex woman) 'female)
                 (eq? (person-sex man) 'male)))
       (error "get-married" 
              "Illegal wedding" 
              (cons woman man))
       (let* ((mname (person-name woman))
              (wife  (widen!::married-woman woman
                      (maiden-name mname)
                      (mate man))))
          (person-name-set! wife (person-name man))
          (widen!::married-man man
             (mate woman)))))

(define (couple? woman::person man::person)
   (and (married-woman? woman)
        (married-man? man)
        (eq? (married-woman-mate woman) man)
        (eq? (married-man-mate man) woman)))

(define *junior* (birth "Jones" "Junior" 'male))
(define *pamela* (birth "Smith" "Pamela" 'female))

(get-married! *pamela* *junior*)

(define *old-boy-junior* *junior*)
(define *old-girl-pamela* *pamela*)
(couple? *pamela* *junior*)

(person-print *pamela*)

(print (eq? *old-boy-junior* *junior*)) 
(print (eq? *old-girl-pamela* *pamela*))

(define (divorce! woman::person man::person)
   (if (not (couple? woman man))
       (error "divorce!"
              "Illegal divorce"
              (cons woman man))
       (let ((mname (married-woman-maiden-name 
                      woman)))
          (begin
             (shrink! woman)
             (person-name-set! woman mname))
          (shrink! man))))

(divorce! *pamela* *junior*)

(person-print *pamela*)

(print (eq? *old-boy-junior* *junior*))
(print (eq? *old-girl-pamela* *pamela*))

;</pre></font>
