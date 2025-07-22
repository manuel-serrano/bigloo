;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Compute a reverse postorder for a CFG.
;;
;; We have a simple DFS which annotates the node of the CFG with indices, and
;; fills predecessor information.

(module cfg_order
   (from (cfg_node "Opt/CFG/node.scm"))

   (export (reverse-postorder! entry::cfg-node)))

(define (dfs! n::cfg-node i::long l::pair-nil)
   (define (loop! succs i l)
      (if (null? succs)
          (begin
             (set! (-> n idx) i)
             (values (-fx i 1) (cons n l)))
          (multiple-value-bind (i l) (dfs! (car succs) i l)
             (with-access::cfg-node (car succs) (preds)
                (set! preds (cons n preds)))
             (loop! (cdr succs) i l))))

   (if (=fx (-> n idx) 1) ; is the node unvisited
       (begin
          (set! (-> n idx) 2) ;; avoid loops
          (loop! (get-succs (-> n end)) i l))
       (values i l)))

;; returns the number of nodes and a list of them in reverse postorder
(define (reverse-postorder! entry::cfg-node)
   (dfs! entry 0 '()))
