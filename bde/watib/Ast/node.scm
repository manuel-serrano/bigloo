;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Definitions of the data structures representing wasm code.

(module ast_node
   (export (class modulefield::object)

           ;; remove information using shrinking/widening ?
           (class func::modulefield
              body
              formals::pair-nil
              type
              locals::pair-nil
              pos::pair)

           (class export::modulefield
              name::bstring
              idx::idxp)

           (class data::modulefield
              data::bstring)

           (class global::modulefield
              type::pair
              body::pair-nil
              pos::pair)

           (class memory::modulefield
              at::symbol
              lim::pair)

           (class import::modulefield
              mod::bstring
              name::bstring)

           (class import-func::import
              deftype::pair)

           (class import-mem::import
              memtype::memory)

           (class import-global::import
              globaltype::pair)

           (class import-tag::import
              tagtype::pair)

           (class prog::object
              env::env
              funcs::vector
              data::vector
              globals::vector
              exports::pair-nil
              funcrefs::pair-nil
              imports::pair-nil)

           (abstract-class source
              position::bint)

           (class from-stack::source)

           (class from-instr::source
              i::instruction)

           (class instruction::object
              intype::pair-nil
              outtype::pair-nil
              (__actouttype::pair-nil (default '()))
              (actouttype (get (lambda (p::instruction)
                                  (if (null? (-> p __actouttype))
                                      (-> p outtype)
                                      (-> p __actouttype))))
                          read-only)
              parent::modulefield
              ; default will have to be removed
              (sources::pair-nil (default '()))
              opcode::symbol)

           (abstract-class parameter)

           (class idxp::parameter
              idx::bint)

           (class labelidxp::idxp
              type::pair-nil)

           (class funcidxp::idxp
              type::pair)

           (class localidxp::idxp
              init?::bool
              type)

           (class typeidxp::idxp
              type)

           (class tagidxp::idxp
              type::pair)

           (class globalidxp::idxp
              mut?::bool
              type)

           (class fieldidxp::idxp
              mut?::bool
              type)

           (class memidxp::idxp)
           (class dataidxp::idxp)

           (class i32p::parameter
              num)
           (class i64p::parameter
              num)
           (class f32p::parameter
              num)
           (class f64p::parameter
              num)

           (class typep::parameter
              type)

           (class one-arg::instruction
              x::parameter)

           (class two-args::instruction
              x::parameter
              y::parameter)

           (class three-args::instruction
              x::parameter
              y::parameter
              z::parameter)

           (class br_table::instruction
              ;; non-empty list of labelidxp
              labels::pair)

           (class sequence::instruction
              body::pair-nil)

           (class if-then::instruction
              then::sequence)

           (class if-else::if-then
              else::sequence)

           (class block::sequence)

           (class loop::sequence)

           (class catch-branch::object
              label::labelidxp)

           (class catch::catch-branch
              tag::tagidxp)

           (class catch_ref::catch-branch
              tag::tagidxp)

           (class catch_all::catch-branch)

           (class catch_all_ref::catch-branch)

           (class try_table::sequence
              catches::pair-nil)

           ;; section 3.1.6
           (class env::object
              (ntype::bint (default 0))
              (type-table (default (create-hashtable eqtest: eq?)))
              (types::vector (default (make-vector 100000)))
              (type-names::vector (default (make-vector 100000)))

              (field-names::vector (default (make-vector 100000)))

              (nlocal::bint (default 0))
              (local-names::pair-nil (default '()))
              (local-types::vector (default (make-vector 0)))

              (nlabel::bint (default 0))
              (label-names::pair-nil (default '()))
              (label-types::vector (default (make-vector 10000)))

              (refs (default (create-hashtable eqtest: eq?)))

              (ndata::bint (default 0))
              (data-table (default (create-hashtable eqtest: eq?)))

              (nglobal::bint (default 0))
              (global-table (default (create-hashtable eqtest: eq?)))
              (global-types::vector (default (make-vector 1000000)))
              (global-names::vector (default (make-vector 1000000)))

              (nfunc::bint (default 0))
              (func-table (default (create-hashtable eqtest: eq?)))
              (func-types::vector (default (make-vector 1000000)))
              (func-names::vector (default (make-vector 1000000)))

              (ntag::bint (default 0))
              (tag-table (default (create-hashtable eqtest: eq?)))
              (tag-types::vector (default (make-vector 10000)))
              (tag-names::vector (default (make-vector 10000)))

              (nmem::bint (default 0))
              (mem-table (default (create-hashtable eqtest: eq?)))
              (mem-types::vector (default (make-vector 10000)))
              (mem-names::vector (default (make-vector 10000)))

              (return::pair-nil (default '()))
              (last-type (default #f))
              (error-list::pair-nil (default '()))
              (parent::modulefield (default (instantiate::modulefield))))))
