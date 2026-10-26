(module bps
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-bps)) )

;;
(define (test-bps)
   (test-module "bps" "bps.scm"))
