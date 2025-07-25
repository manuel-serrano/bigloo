(module opt_bbv
   (from (cfg_node "Opt/CFG/node.scm"))

   (export (bbv::cfg g::cfg)))

(define (bbv::cfg g::cfg)
   g)
