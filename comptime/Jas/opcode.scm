(module jas_opcode
   (import jas_lib jas_classfile)
   (export resolve-opcodes) )

;;
;; Resolve cops
;;

;; Direct association between mnemonics and opcode
(define *cop-list* '(
 nop             aconst_null  iconst_m1       iconst_0      iconst_1     ;   0
 iconst_2        iconst_3     iconst_4        iconst_5      lconst_0     ;   5
 lconst_1        fconst_0     fconst_1        fconst_2      dconst_0     ;  10
 dconst_1        bipush       sipush          ldc           ldc_w        ;  15
 ldc2_w          iload        lload           fload         dload        ;  20
 aload           iload_0      iload_1         iload_2       iload_3      ;  25
 lload_0         lload_1      lload_2         lload_3       fload_0      ;  30
 fload_1         fload_2      fload_3         dload_0       dload_1      ;  35
 dload_2         dload_3      aload_0         aload_1       aload_2      ;  40
 aload_3         iaload       laload          faload        daload       ;  45
 aaload          baload       caload          saload        istore       ;  50
 lstore          fstore       dstore          astore        istore_0     ;  55
 istore_1        istore_2     istore_3        lstore_0      lstore_1     ;  60
 lstore_2        lstore_3     fstore_0        fstore_1      fstore_2     ;  65
 fstore_3        dstore_0     dstore_1        dstore_2      dstore_3     ;  70
 astore_0        astore_1     astore_2        astore_3      iastore      ;  75
 lastore         fastore      dastore         aastore       bastore      ;  80
 castore         sastore      pop             pop2          dup          ;  85
 dup_x1          dup_x2       dup2            dup2_x1       dup2_x2      ;  90
 swap            iadd         ladd            fadd          dadd         ;  95
 isub            lsub         fsub            dsub          imul         ; 100
 lmul            fmul         dmul            idiv          ldiv         ; 105
 fdiv            ddiv         irem            lrem          frem         ; 110
 drem            ineg         lneg            fneg          dneg         ; 115
 ishl            lshl         ishr            lshr          iushr        ; 120
 lushr           iand         land            ior           lor          ; 125
 ixor            lxor         iinc            i2l           i2f          ; 130
 i2d             l2i          l2f             l2d           f2i          ; 135
 f2l             f2d          d2i             d2l           d2f          ; 140
 i2b             i2c          i2s             lcmp          fcmpl        ; 145
 fcmpg           dcmpl        dcmpg           ifeq          ifne         ; 150
 iflt            ifge         ifgt            ifle          if_icmpeq    ; 155
 if_icmpne       if_icmplt    if_icmpge       if_icmpgt     if_icmple    ; 160
 if_acmpeq       if_acmpne    goto            jsr           ret          ; 165
 tableswitch     lookupswitch ireturn         lreturn       freturn      ; 170
 dreturn         areturn      return          getstatic     putstatic    ; 175
 getfield        putfield     invokevirtual   invokespecial invokestatic ; 180
 invokeinterface xxxunusedxxx new             newarray      anewarray    ; 185
 arraylength     athrow       checkcast       instanceof    monitorenter ; 190
 monitorexit     wide         multianewarray  ifnull        ifnonnull    ; 195
 goto_w          jsr_w        handler         line          comment      ; 200
 localvar                                                                ; 205
))

(define (init-cop-value)
   (define (walk l n)
      (cond ((null? l) l)
	    (else (putprop! (car l) 'as-cop-value n)
		  (walk (cdr l) (+fx n 1)) )))
   (walk *cop-list* 0) )

(init-cop-value)

(define (get-cop-name cop)
   (list-ref *cop-list* cop) )

(define (get-cop-value classfile cop)
   (if (symbol? cop)
       (getprop cop 'as-cop-value)
       (jas-error classfile "Bad mnemonic" cop) ))

(define (clean-cop-value)
   (for-each (lambda (cop) (remprop! cop 'as-cop-value))
	     *cop-list* ))

(define (resolve-opcodes classfile param locals code)
   (let ( (vars (append param locals)) )
      (define (resolve-opcode ins)
	 (if (not (pair? ins))
	     ins
	     (let ( (cop (get-cop-value classfile (car ins))) )
		(if cop
		    (resolve-args classfile vars cop (cdr ins))
		    (jas-error classfile "Bad mnemonic" (car ins)) ))))
      (map resolve-opcode code) ))

(define (resolve-args classfile locals cop args)
   (case cop
      ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15  ; NOP tCONST_n
	  46 47 48 49 50 51 52 53 79 80 81 82 83 84 85 86 ; tALOAD tASTORE
	  87 88 89 90 91 92 93 94 95 ; POPx DUPx SWAP
	  96 97 98 99 100 101 102 103 104 105 106 107 ;tADD tSUB tMUL
	  108 109 110 111 112 113 114 115 116 117 118 119 ; tDIV tREM tNEG
	  120 121 122 123 124 125 126 127 128 129 130 131 ; bit
	  133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 ; conv
	  148 149 150 151 152 172 173 174 175 176 177 ; cmp tRETURN
	  190 191 194 195 ; ARRAYLENGTH ATHROW MONITORE[NTER/XIT]
	  )
       (match-case args
	  (() (cons cop '()))
	  (else (syntax-error classfile cop args)) ))
      ((16 17) ; tIPUSH
       (match-case args
	  (((? fixnum?)) (cons cop args))
	  (((? elong?)) (list cop (elong->fixnum (car args))))
	  (((? uint32?)) (list cop (uint32->fixnum (car args))))
	  (((? int32?)) (list cop (int32->fixnum (car args))))
	  (((? int16?)) (list cop (int16->fixnum (car args))))
	  (((? uint16?)) (list cop (uint16->fixnum (car args))))
	  (((? int8?)) (list cop (int8->fixnum (car args))))
	  (((? uint8?)) (list cop (uint8->fixnum (car args))))
	  (else (syntax-error classfile cop args)) ))
      ((18 19) ; LDC
       (match-case args
	  (((or (? fixnum?) (? flonum?) (? string?))) (cons 18 args))
	  (((or (? int32?) (? uint32?))) (cons 18 args))
	  (else (syntax-error classfile cop args)) ))
      ((20) ; LDC2
       (match-case args
	  (((or (? fixnum?) (? flonum?) (? elong?) (? llong?)))
	   (cons 20 args))
	  (((or (? uint64?) (? int64?)))
	   (cons 20 args))
	  (else (syntax-error classfile cop args)) ))
      ((21 22 23 24 25 54 55 56 57 58 169) ; XLOAD XSTORE RET
       (match-case args
	  (((and ?var (? symbol?))) (list cop (v-index classfile var locals)))
	  (else (syntax-error classfile cop args)) ))
      ((26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45) ; LOAD_n
       (match-case args
	  (() (let ( (d (-fx cop 26)) )
		 (list (+fx 21 (/fx d 4)) (remainderfx d 4)) ))
	  (else (syntax-error classfile cop args)) ))
      ((59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78) ; STORE_n
       (match-case args
	  (() (let ( (d (-fx cop 59)) )
		 (list (+fx 54 (/fx d 4)) (remainderfx d 4)) ))
	  (else (syntax-error classfile cop args)) ))
      ((132) ; IINC
       (match-case args
	  (((and ?var (? symbol?)) (and ?n (? fixnum?)))
	   (list cop (v-index classfile var locals) n) )
	  (else (syntax-error classfile cop args)) ))
      ((153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
	    198 199 200 201 ) ; all IFs GOTO JSR
       (match-case args
	  (((and ?lab (? symbol?))) (list cop lab))
	  (else (syntax-error classfile cop args)) ))
      ((170) ; TABLESWITCH
       (match-case args
	  (((and ?def (? symbol?)) (and ?beg (? fixnum?)) .
				   (and ?labs ((? symbol?) ...)) )
	   (cons cop args) )
	  (else (syntax-error classfile cop args)) ))
      ((171) ; LOOKUPSWITCH
       (match-case args
	  (((and ?def (? symbol?)) . (and ?table ((? slot?) ...)))
	   (cons cop args) )
	  (else (syntax-error classfile cop args)) ))
      ((178 179 180 181) ; [GET|PUT][FIELD|STATIC]
       (match-case args
	  (((and ?var (? symbol?))) (list cop (declared-field classfile var)))
	  (else (syntax-error classfile cop args)) ))
      ((182 183 184 185) ; INVOKE**
       (match-case args
	  (((and ?var (? symbol?))) (list cop (declared-method classfile var)))
	  (else (syntax-error classfile cop args)) ))
      ((187) ; NEW
       (match-case args
	  (((and ?var (? symbol?))) (list cop (declared-class classfile var)))
	  (else (syntax-error classfile cop args)) ))
      ((188) ; NEWARRAY
       (match-case args
	  ((?type) (list cop (get-type-num classfile type)))
	  (else (syntax-error classfile cop args)) ))
      ((189 192 193) ; ANEWARRAY CHECKCAST INSTANCEOF
       (match-case args
	  ((?type) (list cop (as-type classfile type)))
	  (else (syntax-error classfile cop args)) ))
      ((196) ; WIDE
       (match-case args
	  (((?wcop . ?wargs))
	   (let ( (ncop (get-cop-value classfile wcop)) )
	      (if ncop
		  (resolve-args classfile locals ncop wargs)
		  (jas-error classfile "Bad wide mnemonic" wcop) )))
	  (else (syntax-error classfile cop args)) ))
      ((197) ; MULTINEWARRAY
       (match-case args
	  ((?type (and ?n (? fixnum?))) (list cop (as-type classfile type) n))
	  (else (syntax-error classfile cop args)) ))
      ((202) ; HANDLER
       (match-case args
	  ((?beg ?end ?lab ?type)
	   (list cop beg end lab
		 (if (eq? type 0) 0 (pool-class-by-name classfile type)) ))))
      ((203) ; LINE
       (match-case args
	  (((? fixnum?) (? fixnum?)) (cons cop args))
	  (else (syntax-error classfile cop args)) ))
      ((204) ; COMMENT
       (cons cop args) )
      ((205) ; LOCALVAR
       (match-case args
	  (((and ?beg (? symbol?))
	    (and ?end (? symbol?))
	    (and ?name (? string?))
	    ?type
	    (and ?var (? symbol?)) )
	   (list cop beg end name
		 (as-type classfile type)
		 (v-index classfile var locals)) )
	  (else (syntax-error classfile cop args)) ))
      (else (jas-error classfile "unknown code operation" cop)) ))

;; Error
(define (syntax-error classfile cop args)
   (jas-error classfile "Syntax error" (cons (get-cop-name cop) args)) )

;; test a slot of a LOOKUPSWITCH
(define (slot? slot)
   (and (pair? slot) (fixnum? (car slot)) (symbol? (cdr slot))) )

;; give the encoded type for NEWARRAY
(define (get-type-num classfile type)
   (let ( (slot (assq type '((boolean . 4) (char . 5) (float . 6)
					   (double . 7) (byte . 8) (short . 9)
					   (int . 10) (long . 11) ))) )
      (if slot
	  (cdr slot)
	  (syntax-error classfile 188 (list type)) )))

;; Find a local variable index
(define (v-index classfile var locals)
   (define (walk l n)
      (cond
	 ((null? l)         (jas-error classfile "unknown variable" var))
	 ((eq? (car l) var) n)
	 (else              (walk (cdr l) (+fx n 1))) ))
   (walk locals 0) )

