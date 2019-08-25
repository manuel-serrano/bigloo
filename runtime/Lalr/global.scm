;; ---------------------------------------------------------------------- ;;
;; FICHIER               : global.scm                                     ;;
;; DATE DE CREATION      : Mon Jul  3 14:09:45 1995                       ;;
;; DERNIERE MODIFICATION : Tue Jul  4 09:45:43 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Les variables globales ...                                             ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_global
   
   (include "Lalr/lalr.sch")
   
   (import __error
	   __param)
   
   (use    __type
	   __bigloo
	   __tvector
	   __structure
	   __tvector
	   __bexit
	   __bignum
	   __object
	   __thread
	   __rgc
	   __bit
	   
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_strings_6_7
	   __r4_pairs_and_lists_6_3
	   __r4_input_6_10_2
	   __r4_control_features_6_9
	   __r4_vectors_6_8
	   __r4_ports_6_10_1
	   __r4_output_6_10_3
	   __evenv)
   
   (export  grammar
	    rrhs         
	    rlhs         
	    ritem        
	    rprec
	    nullable     
	    derives      
	    fderives     
	    firsts       
	    kernel-base  
	    kernel-end   
	    shift-symbol 
	    shift-set    
	    red-set      
	    state-table  
	    acces-symbol 
	    reduction-table 
	    shift-table  
	    consistent   
	    lookaheads   
	    LA           
	    LAruleno     
	    lookback     
	    goto-map     
	    from-state   
	    to-state     
	    includes     
	    F            
	    action-table 
	    nitems          
	    nrules          
	    nvars           
	    nterms          
	    nsyms           
	    nstates         
	    first-state     
	    last-state      
	    final-state     
	    first-shift     
	    last-shift      
	    first-reduction 
	    last-reduction  
	    nshifts         
	    maxrhs          
	    ngotos          
	    token-set-size
	    terminals
	    nonterminals
	    actions
	    STATE-TABLE-SIZE
	    (initialize-all)))

;; - Tableaux 
(define rrhs            #f)
(define rlhs            #f)
(define ritem           #f)
(define rprec		#f)
(define nullable        #f)
(define derives         #f)
(define fderives        #f)
(define firsts          #f)
(define kernel-base     #f)
(define kernel-end      #f)
(define shift-symbol    #f)
(define shift-set       #f)
(define red-set         #f)
(define state-table     #f)
(define acces-symbol    #f)
(define reduction-table #f)
(define shift-table     #f)
(define consistent      #f)
(define lookaheads      #f)
(define LA              #f)
(define LAruleno        #f)
(define lookback        #f)
(define goto-map        #f)
(define from-state      #f)
(define to-state        #f)
(define includes        #f)
(define F               #f)
(define action-table #f)

;; - Variables
(define nitems          #f)
(define nrules          #f)
(define nvars           #f)
(define nterms          #f)
(define nsyms           #f)
(define nstates         #f)
(define first-state     #f)
(define last-state      #f)
(define final-state     #f)
(define first-shift     #f)
(define last-shift      #f)
(define first-reduction #f)
(define last-reduction  #f)
(define nshifts         #f)
(define maxrhs          #f)
(define ngotos          #f)
(define token-set-size  #f)
(define grammar         #f)
(define terminals       #f)
(define nonterminals    #f)
(define actions         #f)

(define (initialize-all)
  (set! rrhs            #f)
  (set! rlhs            #f)
  (set! ritem           #f)
  (set! rprec		#f)
  (set! nullable        #f)
  (set! derives         #f)
  (set! fderives        #f)
  (set! firsts          #f)
  (set! kernel-base     #f)
  (set! kernel-end      #f)
  (set! shift-symbol    #f)
  (set! shift-set       #f)
  (set! red-set         #f)
  (set! state-table     (make-vector STATE-TABLE-SIZE '()))
  (set! acces-symbol    #f)
  (set! reduction-table #f)
  (set! shift-table     #f)
  (set! consistent      #f)
  (set! lookaheads      #f)
  (set! LA              #f)
  (set! LAruleno        #f)
  (set! lookback        #f)
  (set! goto-map        #f)
  (set! from-state      #f)
  (set! to-state        #f)
  (set! includes        #f)
  (set! F               #f)
  (set! action-table    #f)
  (set! nstates         #f)
  (set! first-state     #f)
  (set! last-state      #f)
  (set! final-state     #f)
  (set! first-shift     #f)
  (set! last-shift      #f)
  (set! first-reduction #f)
  (set! last-reduction  #f)
  (set! nshifts         #f)
  (set! maxrhs          #f)
  (set! ngotos          #f)
  (set! token-set-size  #f)
  (set! grammar         #f)
  (set! terminals       #f)
  (set! nonterminals    #f)
  (set! actions         #f))


;; - Constantes
(define STATE-TABLE-SIZE 1009)
