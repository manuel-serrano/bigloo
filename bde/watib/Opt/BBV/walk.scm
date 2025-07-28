(module opt_bbv
   (from (cfg_node "Opt/CFG/node.scm"))

   (import (cfg_order "Opt/CFG/order.scm"))

   (static (class context
      map::obj))

   (static (class types
      isnull::bool
      notnull::bool))

   (static (class specialization
      origin::cfg-node
      id::bint
      version
      context::context))

   (static (class bbv-state
      (id-counter::long (default 0))
      all-specializations::obj
      all-specializations-by-id::obj
      merge-history::obj
      queue::obj
      reachability::obj))

   (export (bbv::cfg g::cfg version-limit::bint)))

;; QUEUE
(define (make-queue) (cons '() '()))
(define (queue-empty? queue) (null? (car queue)))
(define (queue-get! queue)
  (let ((x (caar queue)))
    (set-car! queue (cdar queue))
    (if (queue-empty? queue) (set-cdr! queue '()))
    x))
(define (queue-put! queue x)
  (let ((entry (cons x '())))
    (if (queue-empty? queue)
      (set-car! queue entry)
      (set-cdr! (cdr queue) entry))
    (set-cdr! queue entry)
    x))
(define (queue-peek queue) (if (queue-empty? queue) #f (caar queue)))

;; CONTEXT
(define (make-context locals::pair-nil)
   (let ((table (make-hashtable)))
      (for-each
         (lambda (id) (hashtable-put! table id (instantiate::types (isnull #t) (notnull #t))))
         locals)
      (instantiate::context (map table))))

(define (context-equal? ctx1 ctx) #t)

;; HELPERS
(define (***NotImplemented*** name::symbol) (error "NotImplemented" name '?))

(define (make-init-state::bbv-state cfg::cfg)
   (let ((init (instantiate::bbv-state
                  (all-specializations (make-hashtable))
                  (all-specializations-by-id (make-hashtable))
                  (merge-history (make-hashtable))
                  (queue (make-queue))
                  (reachability (ssr-make-graph)))))
      (with-access::bbv-state init (all-specializations)
         (for-each
            (lambda (bb)
               (with-access::cfg-node bb (idx) (hashtable-put! all-specializations idx '())))
            (-> cfg rpostorder)))
      init))

(define (new-id!::bint state::bbv-state)
   (set! (-> state id-counter) (+ 1 (-> state id-counter)))
   (-> state id-counter))

(define (add-specialization! state::bbv-state specialization::specialization)
   (with-access::bbv-state state (all-specializations all-specializations-by-id)
   (with-access::specialization specialization (origin)
      (hashtable-put! all-specializations-by-id (-> specialization id) specialization)
      (hashtable-put!
         all-specializations
         (-> origin idx)
         (cons
            specialization
            (hashtable-get
               all-specializations
               (-> origin idx)))))))

(define (get-specialization-by-id::specialization state::bbv-state id::bint)
   (with-access::bbv-state state (all-specializations-by-id)
      (hashtable-get all-specializations-by-id id)))

(define (get-most-recent-merge::specialization state::bbv-state specialization::specialization)
   (with-access::bbv-state state (merge-history)
   (with-access::specialization specialization (id)
      (let loop ((id id))
         (let ((merged-to (hashtable-get merge-history id)))
            (if (or (not merged-to) (= merged-to id))
               (get-specialization-by-id state id)
               (loop merged-to)))))))

(define (get-specialization::specialization state::bbv-state origin::cfg-node target-context::context)
   (with-access::bbv-state state (all-specializations)
      (let loop ((versions (hashtable-get all-specializations (-> origin idx))))
         (if (null? versions)
            #f
            (with-access::specialization (car versions) (context)
               (if (context-equal? context target-context)
                  (get-most-recent-merge state specialization)
                  (loop (cdr versions))))))))

(define (get-specializations::pair-nil state::bbv-state specialization::specialization)
   (with-access::bbv-state state (all-specializations)
   (with-access::specialization specialization (origin)
      (map cdr (hashtable-get all-specializations (-> origin idx))))))

(define (get-active-specializations::pair-nil state::bbv-state specialization::specialization)
   (map (lambda (spec) (reachable? state spec)) (get-specializations state specialization)))

(define (get-active-specializations-count::bint state::bbv-state specialization::specialization)
   (length (get-active-specializations state specialization)))

;; BBV ALGORITHM STEPS
(define (reachable? state::bbv-state specialization::specialization)
   (with-access::bbv-state state (reachability)
      (with-access::specialization specialization (id)
         (ssr-connected? reachability id))))

(define (walk state::bbv-state specialization::specialization) (***NotImplemented*** 'walk))

(define (reach::specialization state::bbv-state origin::cfg-node context::context from::specialization)
   (with-access::bbv-state state (reachability)
      (let ((target
               (or
                  (get-specialization state origin context)
                  (let ((new-specialization
                        (instantiate::specialization
                           (origin origin)
                           (id (new-id! state))
                           (version #f)
                           (context context))))
                     (add-specialization! state specialization)
                     new-specialization))))
         (with-access::specialization target (id)
            (if from
               (ssr-add-edge! reachability (-> from id) id
                  :onconnect (lambda (reachable)
                                 (queue-put!
                                    (-> state queue)
                                    (get-specialization-by-id bbv-state reachable)))))
            target))))

(define (merge? state::bbv-state specialization::specialization version-limit::bint)
   (when (> (get-active-specializations-count state specialization) version-limit)
      (***NotImplemented*** 'merge?)))

;; BBV CORE ALGO
(define (bbv::cfg g::cfg version-limit::bint)
   (let ((state (make-init-state cfg)))
      (with-access::bbv-state state (queue)
      (with-access::cfg g (entry func)
         (let ((new-entry (reach state entry (make-context (iota (length (-> func locals)))) #f)))
            (let loop ()
               (when (not (queue-empty? queue))
                  (let ((specialization (queue-get! queue)))
                     (merge? state specialization version-limit)
                     (with-access::specialization specialization (version)
                        (if (and (reachable? state specialization) (not version))
                            (walk state specialization)))
                     (loop))))
            (multiple-value-bind (-size rpostorder) (reverse-postorder! entry)
               (let ((new-cfg (instantiate::cfg
                                 (entry (get-most-recent-merge state new-entry))
                                 (size (-fx 0 -size))
                                 (rpostorder rpostorder)
                                 (func func))))
            
                  new-cfg)))))))