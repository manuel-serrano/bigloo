;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/dtoa.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Fri Feb 18 14:43:08 2011                          */
;*    Last change :  Sun Sep 23 17:34:34 2018 (serrano)                */
;*    Copyright   :  2011-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Correct and fast double-to-string conversion.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_numbers_6_5_flonum_dtoa

   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_booleans_6_1
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_characters_6_6
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r5_control_features_6_4

	    __bit
	    __evenv
	    __intext)

   (export (real->string::bstring d::double))
   
   (extern (export real->string "bgl_real_to_string"))

   (option (set! *init-mode* 'intern)))

;*---------------------------------------------------------------------*/
;*    static                                                           */
;*    -------------------------------------------------------------    */
;*    This macro is used to improve the C compilation of the           */
;*    large vector constants used in this module.                      */
;*---------------------------------------------------------------------*/
(define-macro (static v)
   
   (define (int-vector v)
      (let* ((vname (gensym 'v))
	     (tname (gensym 'bgl_vector))
	     (tdecl (format "struct ~a { __CNST_ALIGN\n#if( !defined( TAG_VECTOR ) )\n header_t header;\n#endif\n unsigned long len; ~(; ); }"
		      tname
		      (map (lambda (i) (format "obj_t obj~a" i))
			 (iota (vector-length v)))))
	     (vdecl (format "static struct ~a ~a = { __CNST_FILLER \n#if( !defined( TAG_VECTOR ) ) \nMAKE_HEADER( VECTOR_TYPE, 0 ),\n#endif\n ~a, ~(, ) }"
		       tname vname
		       (vector-length v)
		       (map (lambda (i) (format "BINT( ~a )" i))
			  (vector->list v)))))
	 `(begin
	     (pragma ,(format "~a; ~a" tdecl vdecl))
	     (pragma::vector ,(format "\n#if( !defined( TAG_VECTOR ) )\nBVECTOR( &(~a.header) )\n#else\nBVECTOR( &(~a.len) )\n#endif\n" vname vname)))))

   (define (llong-vector v)
      (let* ((l (vector->list v))
	     (vname (gensym 'v))
	     (aname (gensym 'a))
	     (lname (gensym 'bgl_llong))
	     (tname (gensym 'bgl_vector))
	     (tdecl (format "struct ~a { __CNST_ALIGN \n#if( !defined( TAG_VECTOR ) )\nheader_t header;\n#endif\n unsigned long len; ~(; ); }"
		       tname
		       (map (lambda (i) (format "obj_t obj~a" i))
			  (iota (vector-length v)))))
	     (vdecl (format "static struct ~a ~a = { __CNST_FILLER \n#if( !defined( TAG_VECTOR ) ) \nMAKE_HEADER( VECTOR_TYPE, 0 ),\n#endif\n ~a, ~(, ) }"
		       tname vname
		       (vector-length v)
		       (map (lambda (i) (format "BREF( &(~a[ ~a ].header) )" aname i))
			  (iota (vector-length v))))))
	 `(begin
	     (pragma ,(format "struct ~a { __CNST_ALIGN header_t header; BGL_LONGLONG_T llong; };" lname))
	     (pragma ,(format "static struct ~a ~a[] = { ~(, ) };"
			 lname aname
			 (map (lambda (i)
				 (format "{ __CNST_FILLER MAKE_HEADER( LLONG_TYPE, 0 ), ~a }" i))
			    l)))
	     (pragma ,(format "~a; ~a" tdecl vdecl))
	     (pragma::vector ,(format "\n#if( !defined( TAG_VECTOR ) )\nBVECTOR( &(~a.header) )\n#else\nBVECTOR( &(~a.len) )\n#endif\n" vname vname)))))
   
   (cond-expand
      (bigloo-c
       (if (llong? (vector-ref (cadr v) 0))
	   (llong-vector (cadr v))
	   (int-vector (cadr v))))
      (else v)))
  
;*---------------------------------------------------------------------*/
;*    constants                                                        */
;*---------------------------------------------------------------------*/
(define (dividers-offset) 330)
(define (first-exact) 330)
(define (last-exact) 357)

;; biased-e (the one in the double) contains the exponent-bias and is such that
;;  1.f*2^e (with f mantissa-bits)
;; here we use the mantissa rather as 1f (that is without the '.')
;; and we thus have to add the length of 'f' to the bias.
(define *exponent-bias* (+fx #x3ff 52)) ;; == 1023 + 52bits of mantissa.
(define *min-exponent* (negfx *exponent-bias*))
(define *mantissa-size* 53)             ;; 53 if we count hidden bit.
(define *first-normal* 2.2250738585072e-308) ;;everything smaller is a denormal

;; only true, when on 64bit machine:
(define *exponent-mask* #lx7FF0000000000000)
(define *mantissa-mask* #lx000FFFFFFFFFFFFF)
(define *hidden-bit*    #lx0010000000000000)

;*---------------------------------------------------------------------*/
;*    dividers-v ...                                                   */
;*---------------------------------------------------------------------*/
(define (dividers-v)
   (static '#(#l7830057919084424892 #l4893786199427765557 #l6117232749284706947 #l7646540936605883684 #l4779088085378677302 #l5973860106723346628 #l7467325133404183285 #l4667078208377614553 #l5833847760472018191 #l7292309700590022739 #l9115387125737528424 #l5697116953585955265 #l7121396191982444081 #l8901745239978055101 #l5563590774986284438 #l6954488468732855548 #l8693110585916069435 #l5433194116197543397 #l6791492645246929246 #l8489365806558661557 #l5305853629099163473 #l6632317036373954342 #l8290396295467442927 #l5181497684667151829 #l6476872105833939787 #l8096090132292424734 #l5060056332682765458 #l6325070415853456823 #l7906338019816821029 #l4941461262385513143 #l6176826577981891429 #l7721033222477364286 #l4825645764048352679 #l6032057205060440848 #l7540071506325551061 #l4712544691453469413 #l5890680864316836766 #l7363351080396045958 #l9204188850495057447 #l5752618031559410904 #l7190772539449263630 #l8988465674311579538 #l5617791046444737211 #l7022238808055921514 #l8777798510069901893 #l5486124068793688683 #l6857655085992110854 #l8572068857490138567 #l5357543035931336604 #l6696928794914170755 #l8371160993642713444 #l5231975621026695903 #l6539969526283369878 #l8174961907854212348 #l5109351192408882717 #l6386688990511103397 #l7983361238138879246 #l4989600773836799529 #l6237000967295999411 #l7796251209119999264 #l4872657005699999540 #l6090821257124999425 #l7613526571406249281 #l4758454107128905800 #l5948067633911132251 #l7435084542388915313 #l4646927838993072071 #l5808659798741340089 #l7260824748426675111 #l9076030935533343889 #l5672519334708339930 #l7090649168385424913 #l8863311460481781141 #l5539569662801113213 #l6924462078501391516 #l8655577598126739396 #l5409735998829212122 #l6762169998536515153 #l8452712498170643941 #l5282945311356652463 #l6603681639195815579 #l8254602048994769474 #l5159126280621730921 #l6448907850777163651 #l8061134813471454564 #l5038209258419659102 #l6297761573024573878 #l7872201966280717348 #l4920126228925448342 #l6150157786156810428 #l7687697232696013035 #l4804810770435008147 #l6006013463043760183 #l7507516828804700229 #l4692198018002937643 #l5865247522503672054 #l7331559403129590068 #l9164449253911987585 #l5727780783694992240 #l7159725979618740301 #l8949657474523425376 #l5593535921577140860 #l6991919901971426075 #l8739899877464282594 #l5462437423415176621 #l6828046779268970776 #l8535058474086213470 #l5334411546303883419 #l6668014432879854274 #l8335018041099817842 #l5209386275687386151 #l6511732844609232689 #l8139666055761540861 #l5087291284850963038 #l6359114106063703798 #l7948892632579629747 #l4968057895362268592 #l6210072369202835740 #l7762590461503544675 #l4851619038439715422 #l6064523798049644277 #l7580654747562055347 #l4737909217226284592 #l5922386521532855740 #l7402983151916069675 #l4626864469947543547 #l5783580587434429433 #l7229475734293036792 #l9036844667866295990 #l5648027917416434993 #l7060034896770543742 #l8825043620963179677 #l5515652263101987298 #l6894565328877484123 #l8618206661096855154 #l5386379163185534471 #l6732973953981918089 #l8416217442477397611 #l5260135901548373507 #l6575169876935466884 #l8218962346169333605 #l5136851466355833503 #l6421064332944791878 #l8026330416180989848 #l5016456510113118655 #l6270570637641398319 #l7838213297051747899 #l4898883310657342436 #l6123604138321678046 #l7654505172902097557 #l4784065733063810973 #l5980082166329763716 #l7475102707912204646 #l4671939192445127903 #l5839923990556409879 #l7299904988195512349 #l9124881235244390437 #l5703050772027744023 #l7128813465034680029 #l8911016831293350036 #l5569385519558343772 #l6961731899447929715 #l8702164874309912144 #l5438853046443695090 #l6798566308054618863 #l8498207885068273579 #l5311379928167670986 #l6639224910209588733 #l8299031137761985917 #l5186894461101241198 #l6483618076376551497 #l8104522595470689372 #l5065326622169180857 #l6331658277711476071 #l7914572847139345089 #l4946608029462090681 #l6183260036827613351 #l7729075046034516689 #l4830671903771572930 #l6038339879714466163 #l7547924849643082704 #l4717453031026926690 #l5896816288783658362 #l7371020360979572953 #l9213775451224466191 #l5758609657015291369 #l7198262071269114212 #l8997827589086392765 #l5623642243178995478 #l7029552803973744348 #l8786941004967180435 #l5491838128104487771 #l6864797660130609714 #l8580997075163262143 #l5363123171977038839 #l6703903964971298549 #l8379879956214123187 #l5237424972633826992 #l6546781215792283740 #l8183476519740354675 #l5114672824837721671 #l6393341031047152089 #l7991676288808940112 #l4994797680505587570 #l6243497100631984462 #l7804371375789980578 #l4877732109868737861 #l6097165137335922326 #l7621456421669902908 #l4763410263543689317 #l5954262829429611647 #l7442828536787014559 #l4651767835491884099 #l5814709794364855124 #l7268387242956068905 #l9085484053695086131 #l5678427533559428832 #l7098034416949286040 #l8872543021186607550 #l5545339388241629719 #l6931674235302037148 #l8664592794127546436 #l5415370496329716522 #l6769213120412145653 #l8461516400515182066 #l5288447750321988791 #l6610559687902485989 #l8263199609878107486 #l5164499756173817179 #l6455624695217271474 #l8069530869021589342 #l5043456793138493339 #l6304320991423116673 #l7880401239278895842 #l4925250774549309901 #l6156563468186637376 #l7695704335233296721 #l4809815209520810450 #l6012269011901013063 #l7515336264876266329 #l4697085165547666455 #l5871356456934583069 #l7339195571168228837 #l9173994463960286046 #l5733746539975178779 #l7167183174968973473 #l8958978968711216842 #l5599361855444510526 #l6999202319305638157 #l8749002899132047697 #l5468126811957529810 #l6835158514946912263 #l8543948143683640329 #l5339967589802275205 #l6674959487252844007 #l8343699359066055009 #l5214812099416284380 #l6518515124270355476 #l8148143905337944345 #l5092589940836215215 #l6365737426045269019 #l7957171782556586274 #l4973232364097866421 #l6216540455122333026 #l7770675568902916283 #l4856672230564322677 #l6070840288205403346 #l7588550360256754183 #l4742843975160471364 #l5928554968950589205 #l7410693711188236507 #l4631683569492647816 #l5789604461865809771 #l7237005577332262213 #l9046256971665327767 #l5653910607290829854 #l7067388259113537318 #l8834235323891921647 #l5521397077432451029 #l6901746346790563787 #l8627182933488204734 #l5391989333430127958 #l6739986666787659948 #l8424983333484574935 #l5265614583427859334 #l6582018229284824168 #l8227522786606030210 #l5142201741628768881 #l6427752177035961102 #l8034690221294951377 #l5021681388309344611 #l6277101735386680763 #l7846377169233350954 #l4903985730770844346 #l6129982163463555433 #l7662477704329444291 #l4789048565205902682 #l5986310706507378352 #l7482888383134222941 #l4676805239458889338 #l5846006549323611672 #l7307508186654514591 #l9134385233318143238 #l5708990770823839524 #l7136238463529799405 #l8920298079412249256 #l5575186299632655785 #l6968982874540819731 #l8711228593176024664 #l5444517870735015415 #l6805647338418769269 #l8507059173023461586 #l5316911983139663491 #l6646139978924579364 #l8307674973655724205 #l5192296858534827628 #l6490371073168534535 #l8112963841460668169 #l5070602400912917605 #l6338253001141147007 #l7922816251426433759 #l4951760157141521099 #l6189700196426901374 #l7737125245533626718 #l4835703278458516698 #l6044629098073145873 #l7555786372591432341 #l4722366482869645213 #l5902958103587056517 #l7378697629483820646 #l4611686018427387904 #l5764607523034234880 #l7205759403792793600 #l9007199254740992000 #l5629499534213120000 #l7036874417766400000 #l8796093022208000000 #l5497558138880000000 #l6871947673600000000 #l8589934592000000000 #l5368709120000000000 #l6710886400000000000 #l8388608000000000000 #l5242880000000000000 #l6553600000000000000 #l8192000000000000000 #l5120000000000000000 #l6400000000000000000 #l8000000000000000000 #l5000000000000000000 #l6250000000000000000 #l7812500000000000000 #l4882812500000000000 #l6103515625000000000 #l7629394531250000000 #l4768371582031250000 #l5960464477539062500 #l7450580596923828125 #l4656612873077392578 #l5820766091346740722 #l7275957614183425903 #l9094947017729282379 #l5684341886080801486 #l7105427357601001858 #l8881784197001252323 #l5551115123125782702 #l6938893903907228377 #l8673617379884035472 #l5421010862427522170 #l6776263578034402712 #l8470329472543003390 #l5293955920339377119 #l6617444900424221398 #l8271806125530276748 #l5169878828456422967 #l6462348535570528709 #l8077935669463160887 #l5048709793414475554 #l6310887241768094443 #l7888609052210118054 #l4930380657631323783 #l6162975822039154729 #l7703719777548943412 #l4814824860968089632 #l6018531076210112040 #l7523163845262640050 #l4701977403289150031 #l5877471754111437539 #l7346839692639296924 #l9183549615799121156 #l5739718509874450722 #l7174648137343063403 #l8968310171678829253 #l5605193857299268283 #l7006492321624085354 #l8758115402030106693 #l5473822126268816683 #l6842277657836020854 #l8552847072295026067 #l5345529420184391292 #l6681911775230489115 #l8352389719038111394 #l5220243574398819621 #l6525304467998524526 #l8156630584998155658 #l5097894115623847286 #l6372367644529809108 #l7965459555662261385 #l4978412222288913365 #l6223015277861141707 #l7778769097326427133 #l4861730685829016958 #l6077163357286271198 #l7596454196607838997 #l4747783872879899373 #l5934729841099874217 #l7418412301374842771 #l4636507688359276732 #l5795634610449095915 #l7244543263061369894 #l9055679078826712367 #l5659799424266695229 #l7074749280333369037 #l8843436600416711296 #l5527147875260444560 #l6908934844075555700 #l8636168555094444625 #l5397605346934027890 #l6747006683667534863 #l8433758354584418579 #l5271098971615261612 #l6588873714519077015 #l8236092143148846269 #l5147557589468028918 #l6434446986835036147 #l8043058733543795184 #l5026911708464871990 #l6283639635581089987 #l7854549544476362484 #l4909093465297726553 #l6136366831622158191 #l7670458539527697739 #l4794036587204811087 #l5992545734006013858 #l7490682167507517323 #l4681676354692198327 #l5852095443365247908 #l7315119304206559886 #l9143899130258199857 #l5714936956411374911 #l7143671195514218638 #l8929588994392773298 #l5580993121495483311 #l6976241401869354139 #l8720301752336692674 #l5450188595210432921 #l6812735744013041151 #l8515919680016301439 #l5322449800010188399 #l6653062250012735499 #l8316327812515919374 #l5197704882822449609 #l6497131103528062011 #l8121413879410077514 #l5075883674631298446 #l6344854593289123058 #l7931068241611403822 #l4956917651007127389 #l6196147063758909236 #l7745183829698636545 #l4840739893561647841 #l6050924866952059801 #l7563656083690074751 #l4727285052306296719 #l5909106315382870899 #l7386382894228588624 #l4616489308892867890 #l5770611636116084862 #l7213264545145106078 #l9016580681431382598 #l5635362925894614123 #l7044203657368267654 #l8805254571710334568 #l5503284107318959105 #l6879105134148698881 #l8598881417685873602 #l5374300886053671001 #l6717876107567088751 #l8397345134458860939 #l5248340709036788087 #l6560425886295985109 #l8200532357869981386 #l5125332723668738366 #l6406665904585922958 #l8008332380732403697 #l5005207737957752311 #l6256509672447190388 #l7820637090558987986 #l4887898181599367491 #l6109872726999209364 #l7637340908749011705 #l4773338067968132315 #l5966672584960165394 #l7458340731200206743 #l4661462957000129214 #l5826828696250161518 #l7283535870312701897 #l9104419837890877372 #l5690262398681798357 #l7112827998352247947 #l8891034997940309933 #l5556896873712693708 #l6946121092140867135 #l8682651365176083919 #l5426657103235052449 #l6783321379043815562 #l8479151723804769452 #l5299469827377980908 #l6624337284222476135 #l8280421605278095168 #l5175263503298809480 #l6469079379123511850 #l8086349223904389813 #l5053968264940243633 #l6317460331175304541 #l7896825413969130677 #l4935515883730706673 #l6169394854663383341 #l7711743568329229176 #l4819839730205768235 #l6024799662757210294 #l7530999578446512867 #l4706874736529070542 #l5883593420661338178 #l7354491775826672722 #l9193114719783340903 #l5745696699864588064 #l7182120874830735080 #l8977651093538418850 #l5611031933461511781 #l7013789916826889727 #l8767237396033612159 #l5479523372521007599 #l6849404215651259499 #l8561755269564074374 #l5351097043477546483 #l6688871304346933104 #l8361089130433666380 #l5225680706521041488 #l6532100883151301860 #l8165126103939127325 #l5103203814961954578 #l6379004768702443222 #l7973755960878054028 #l4983597475548783767 #l6229496844435979709 #l7786871055544974637 #l4866794409715609148 #l6083493012144511435 #l7604366265180639294 #l4752728915737899558 #l5940911144672374448 #l7426138930840468060 #l4641336831775292537 #l5801671039719115672 #l7252088799648894590 #l9065110999561118238 #l5665694374725698898 #l7082117968407123623 #l8852647460508904529 #l5532904662818065330 #l6916130828522581663 #l8645163535653227079 #l5403227209783266924 #l6754034012229083655 #l8442542515286354569 #l5276589072053971606 #l6595736340067464507 #l8244670425084330634 #l5152919015677706646 #l6441148769597133308 #l8051435961996416635 #l5032147476247760397 #l6290184345309700496 #l7862730431637125620 #l4914206519773203512 #l6142758149716504390 #l7678447687145630488 #l4799029804466019055 #l5998787255582523819 #l7498484069478154774 #l4686552543423846733 #l5858190679279808417 #l7322738349099760521 #l9153422936374700651 #l5720889335234187907 #l7151111669042734884 #l8938889586303418605 #l5586805991439636628 #l6983507489299545785 #l8729384361624432231 #l5455865226015270144 #l6819831532519087681 #l8524789415648859601 #l5327993384780537250 #l6659991730975671563 #l8324989663719589454 #l5203118539824743409 #l6503898174780929261 #l8129872718476161576 #l5081170449047600985 #l6351463061309501231 #l7939328826636876539 #l4962080516648047837 #l6202600645810059796 #l7753250807262574745 #l4845781754539109216 #l6057227193173886520 #l7571533991467358150 #l4732208744667098843 #l5915260930833873554 #l7394076163542341943 #l4621297602213963714 #l5776622002767454643 #l7220777503459318304 #l9025971879324147880 #l5641232424577592425 #l7051540530721990531 #l8814425663402488164 #l5509016039626555102 #l6886270049533193878 #l8607837561916492348 #l5379898476197807717 #l6724873095247259646 #l8406091369059074558 #l5253807105661921599 #l6567258882077401998 #l8209073602596752498 #l5130671001622970311 #l6413338752028712889 #l8016673440035891111 #l5010420900022431944 #l6263026125028039931 #l7828782656285049914 #l4892989160178156196 #l6116236450222695245 #l7645295562778369056 #l4778309726736480660 #l5972887158420600825 #l7466108948025751031 #l4666318092516094394 #l5832897615645117993 #l7291122019556397492 #l9113902524445496865 #l5696189077778435540 #l7120236347223044425 #l8900295434028805532 #l5562684646268003457 #l6953355807835004322 #l8691694759793755402 #l5432309224871097126 )))

;*---------------------------------------------------------------------*/
;*    dividers-e ...                                                   */
;*---------------------------------------------------------------------*/
(define (dividers-e)
   (static '#(-1149 -1145 -1142 -1139 -1135 -1132 -1129 -1125 -1122 -1119 -1116 -1112 -1109 -1106 -1102 -1099 -1096 -1092 -1089 -1086 -1082 -1079 -1076 -1072 -1069 -1066 -1062 -1059 -1056 -1052 -1049 -1046 -1042 -1039 -1036 -1032 -1029 -1026 -1023 -1019 -1016 -1013 -1009 -1006 -1003 -999 -996 -993 -989 -986 -983 -979 -976 -973 -969 -966 -963 -959 -956 -953 -949 -946 -943 -939 -936 -933 -929 -926 -923 -920 -916 -913 -910 -906 -903 -900 -896 -893 -890 -886 -883 -880 -876 -873 -870 -866 -863 -860 -856 -853 -850 -846 -843 -840 -836 -833 -830 -827 -823 -820 -817 -813 -810 -807 -803 -800 -797 -793 -790 -787 -783 -780 -777 -773 -770 -767 -763 -760 -757 -753 -750 -747 -743 -740 -737 -733 -730 -727 -724 -720 -717 -714 -710 -707 -704 -700 -697 -694 -690 -687 -684 -680 -677 -674 -670 -667 -664 -660 -657 -654 -650 -647 -644 -640 -637 -634 -631 -627 -624 -621 -617 -614 -611 -607 -604 -601 -597 -594 -591 -587 -584 -581 -577 -574 -571 -567 -564 -561 -557 -554 -551 -547 -544 -541 -538 -534 -531 -528 -524 -521 -518 -514 -511 -508 -504 -501 -498 -494 -491 -488 -484 -481 -478 -474 -471 -468 -464 -461 -458 -454 -451 -448 -444 -441 -438 -435 -431 -428 -425 -421 -418 -415 -411 -408 -405 -401 -398 -395 -391 -388 -385 -381 -378 -375 -371 -368 -365 -361 -358 -355 -351 -348 -345 -342 -338 -335 -332 -328 -325 -322 -318 -315 -312 -308 -305 -302 -298 -295 -292 -288 -285 -282 -278 -275 -272 -268 -265 -262 -258 -255 -252 -248 -245 -242 -239 -235 -232 -229 -225 -222 -219 -215 -212 -209 -205 -202 -199 -195 -192 -189 -185 -182 -179 -175 -172 -169 -165 -162 -159 -155 -152 -149 -146 -142 -139 -136 -132 -129 -126 -122 -119 -116 -112 -109 -106 -102 -99 -96 -92 -89 -86 -82 -79 -76 -72 -69 -66 -62 -59 -56 -52 -49 -46 -43 -39 -36 -33 -29 -26 -23 -19 -16 -13 -9 -6 -3 1 4 7 11 14 17 21 24 27 31 34 37 41 44 47 50 54 57 60 64 67 70 74 77 80 84 87 90 94 97 100 104 107 110 114 117 120 124 127 130 134 137 140 143 147 150 153 157 160 163 167 170 173 177 180 183 187 190 193 197 200 203 207 210 213 217 220 223 227 230 233 237 240 243 246 250 253 256 260 263 266 270 273 276 280 283 286 290 293 296 300 303 306 310 313 316 320 323 326 330 333 336 339 343 346 349 353 356 359 363 366 369 373 376 379 383 386 389 393 396 399 403 406 409 413 416 419 423 426 429 433 436 439 442 446 449 452 456 459 462 466 469 472 476 479 482 486 489 492 496 499 502 506 509 512 516 519 522 526 529 532 535 539 542 545 549 552 555 559 562 565 569 572 575 579 582 585 589 592 595 599 602 605 609 612 615 619 622 625 628 632 635 638 642 645 648 652 655 658 662 665 668 672 675 678 682 685 688 692 695 698 702 705 708 712 715 718 722 725 728 731 735 738 741 745 748 751 755 758 761 765 768 771 775 778 781 785 788 791 795 798 801 805 808 811 815 818 821 824 828 831 834 838 841 844 848 851 854 858 861 864 868 871 874 878 881 884 888 891 894 898 901 904 908 911 914 918 921 924 927 931 934 937 941 944 947 951 954 957 961 964 967 971 974 977 981 984 987 991 994 997 1001 1004 1007 1011 1014 1017 1020 1024 1027 1030 1034 1037 1040 1044 )))

;*---------------------------------------------------------------------*/
;*    cached-divider ...                                               */
;*---------------------------------------------------------------------*/
(define (cached-divider k)
   (let* ((index (+fx (dividers-offset) k))
	  (v (vector-ref (dividers-v) index))
	  (e (vector-ref (dividers-e) index))
	  (exact? (and (>=fx index (first-exact)) (<=fx index (last-exact)))))
      (values v e exact?)))

;*---------------------------------------------------------------------*/
;*    compute-denormal ...                                             */
;*---------------------------------------------------------------------*/
(define (compute-denormal mantissa)
   ;; denormals do not have the implicit bit anymore.
   ;; in order to have a continuity the exponent is not used the same way
   ;; anymore, but has to be decremented by 1.
   ;; basically: when we had before (2^p + f)*2^e
   ;;            we now have f*2^(e+1)
   ;; in other words. it's as if we stayed in the previous exponent but
   ;; without the implicit bit.
   ;; We are going to shift the denormalized numbers (see the
   ;; mantissa-conversions)
   ;; eg 8.0000.0000.0000 is going to be shifted so it "resembles" a normal 
   ;;   10.0000.0000.0000.
   (let loop ((e (+fx *min-exponent* 1))
	      (m mantissa))
      (if (not (zerollong? (bit-andllong m *hidden-bit*)))
	  (values m e)
	  (loop (-fx e 1) (bit-lshllong m 1)))))

;*---------------------------------------------------------------------*/
;*    decompose-double ...                                             */
;*---------------------------------------------------------------------*/
(define (decompose-double d::double)
   ;; returns an 64bit llong 'm' + exponent 'e'
   ;; such that m*2^e = d
   ;; furthermore m will be normalized (even when d is a denormal)
   ;;    (obvious exception of course 0.0)
   (let* ((el (double->llong-bits d))
	  (biased-e (llong->fixnum (bit-rshllong
				      (bit-andllong el *exponent-mask*)
				      52))) ;; 52 bits mantissa
	  (mantissa (bit-andllong el *mantissa-mask*)))
      (cond
	 ((not (zerofx? biased-e))
	  (values (+llong *hidden-bit* mantissa)
		  (-fx biased-e *exponent-bias*)))
	 ((zerollong? mantissa) ;; -0.0 or 0.0
	  (values #l0 51)) ;; 51 is the most convenient in later computations
	 (else
	  (compute-denormal mantissa)))))

;*---------------------------------------------------------------------*/
;*    *maxvalfx/mantissa* ...                                          */
;*---------------------------------------------------------------------*/
(define *maxvalfx/mantissa*
   ;; the following value ist the biggest value where we simply can convert an
   ;; integer floating point to fixnum and then print the integer.
   ;; we have to pay attention, as fixnums might be bigger than the 52 bits of
   ;; the double's mantissa. Even if the fixnum yielded a mathematically
   ;; correct string-representation of the double it is not guaranteed to be
   ;; the smallest one. By limiting the max-val to 52 bits we are however
   ;; certain to get the smallest string.
   (let ((maxvalfx_fl (fixnum->flonum (maxvalfx)))
	 ;; that's the biggest integer floating point
	 ;; number.
	 ;; In hex-form: 433fffffffffffff
	 (maxmantissa 9007199254740991.0))
      (if (<fl maxvalfx_fl maxmantissa)
	  maxvalfx_fl
	  maxmantissa)))

;*---------------------------------------------------------------------*/
;*    fill-fixnum! ...                                                 */
;*---------------------------------------------------------------------*/
(define (fill-fixnum! buffer::bstring pos::bint n::bint)
   (cond
      ((zerofx? n)
       (string-set! buffer pos #\0)
       (+fx pos 1))
      ((<fx n 0)
       (string-set! buffer pos #\-)
       ;; ok the following is only true here!
       ;; indeed: in general negative numbers have a greater range than
       ;; positive numbers!
       (fill-fixnum! buffer (+fx pos 1) (negfx n)))
      (else
       (let ((nb-digits (let loop ((digits 0)
				   (n n))
			   (if (zerofx? n)
			       digits
			       (loop (+fx digits 1)
				     (/fx n 10))))))
	  (let loop ((n n)
		     (i (+fx pos (-fx nb-digits 1))))
	     (if (zerofx? n)
		 (+fx pos nb-digits)
		 (begin
		    (string-set! buffer i
				 (integer->char (+fx (modulofx n 10)
						     (char->integer #\0))))
		    (loop (/fx n 10)
			  (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    fixnum->double-string ...                                        */
;*---------------------------------------------------------------------*/
(define (fixnum->double-string buffer::bstring pos::bint n::bint sign?::bool)
   (when sign?
      (string-set! buffer pos #\-))
   (let* ((new-pos (if sign? (+fx pos 1) pos))
	  (after-n-pos (fill-fixnum! buffer new-pos n)))
      (string-set! buffer after-n-pos #\.)
      (string-set! buffer (+fx after-n-pos 1) #\0)
      (+fx after-n-pos 2)))

;*---------------------------------------------------------------------*/
;*    *1/log2_10* ...                                                  */
;*---------------------------------------------------------------------*/
(define *1/log2_10* (/fl (log 2) (log 10)))

;*---------------------------------------------------------------------*/
;*    k-estimation ...                                                 */
;*---------------------------------------------------------------------*/
(define (k-estimation e)
   ;; returns an estimation of k such that v < 10^k with v=f*2^e
   ;; could undershoot by 1.
   ;; examples:
   ;;  (k-estimation 0)   => 16
   ;;  (k-estimation -52) => 0
   ;;
   ;; this estimates log10 of v where v = f*2^e
   ;; given that log10(v) == log2(v)/log2(10) and
   ;; e+(len(f)-1) is quite close to log2(v) this is simplified to
   ;; (e+len(f)-1)/log2(10). According to the paper the result can't undershoot
   ;; by more than 0.631 and to ensure that imprecisions of floating point
   ;; operations don't make us overshoot we simply substract 1e10 (thus
   ;; undershooting by at most 0.631+1e10)
   ;; Using doubles len_f = 52
   ;; The result will undershoot by at most 1.

   ;; Note: If the number was a denormal the result could overshoot.
   ;; In this case the result will be something like 0.000..e-..
   ;; Obviously the leading 0s are not needed.
   ;; TODO: special case for denormals.
   
   (let* ((e+len_f-1 (fixnum->flonum (+fx e 51))))
      (flonum->fixnum (ceilingfl (-fl (*fl e+len_f-1 *1/log2_10*) 1e-10)))))

;*---------------------------------------------------------------------*/
;*    string-shift! ...                                                */
;*---------------------------------------------------------------------*/
(define (string-shift! str from to len)
   (if (<fx from to)
       (let loop ((i (+fx from (-fx len 1)))
		  (j (+fx to (-fx len 1))))
	  (when (>=fx i from)
	     (string-set! str j (string-ref str i))
	     (loop (-fx i 1) (-fx j 1))))
       (let loop ((i from)
		  (j to))
	  (when (<fx i (+fx from len))
	     (string-set! str j (string-ref str i))
	     (loop (+fx i 1) (+fx j 1))))))

;*---------------------------------------------------------------------*/
;*    prettify-string! ...                                             */
;*---------------------------------------------------------------------*/
(define (prettify-string! buffer::bstring pos::bint end-pos::bint k::bint)
   ;; create a nice string out of the generated digits.
   (let ((nb-digits (-fx end-pos pos)))
      (cond
	 ((and (<=fx nb-digits k)
	       (<=fx k 19))
	  ;; we have the first nb-digits already in. Add some 0s
	  ;; and finish with a .0
	  (let loop ((i nb-digits))
	     (cond
		((>= i k)
		 (string-set! buffer (+fx pos i) #\.)
		 (string-set! buffer (+fx pos (+fx i 1)) #\0)
		 (+fx pos (+fx i 2)))
		(else
		 (string-set! buffer (+fx pos i) #\0)
		 (loop (+fx i 1))))))
	 ((and (<fx 0 k)
	       (<=fx k 19))
	  ;; comma number. Just insert a '.' at the correct
	  ;; location.
	  (string-shift! buffer (+fx pos k) (+fx pos (+fx k 1))
			 (-fx nb-digits k))
	  (string-set! buffer (+fx pos k) #\.)
	  (+fx pos (+fx nb-digits 1)))
	 ((and (<fx -6 k)
	       (<=fx k 0))
	  ;; something like 0.000abcde
	  ;; add '0.' and some '0's
	  (let ((offset (-fx 2 k)))
	     (string-shift! buffer pos (+fx pos offset) nb-digits)
	     (string-set! buffer pos #\0)
	     (string-set! buffer (+fx pos 1) #\.)
	     (let loop ((i (+fx pos 2)))
		(cond
		   ((>=fx i (+fx pos offset))
		    (+fx pos (+fx nb-digits offset)))
		   (else
		    (string-set! buffer i #\0)
		    (loop (+fx i 1)))))))
	 ((=fx nb-digits 1)
	  ;; just add e...
	  (string-set! buffer (+fx pos 1) #\e)
	  (if (>fx k 0)
	      (fill-fixnum! buffer (+fx pos 2) (absfx (-fx k 1)))
	      (begin
		 (string-set! buffer (+fx pos 2) #\-)
		 (fill-fixnum! buffer (+fx pos 3)
			       (absfx (-fx k 1))))))
	 (else
	  ;; leave the first digit. then add a '.' and at the end
	  ;; followed by 'e'...
	  (string-shift! buffer (+fx pos 1) (+fx pos 2)
			 (-fx nb-digits 1))
	  (string-set! buffer (+fx pos 1) #\.)
	  (string-set! buffer (+fx pos (+fx nb-digits 1)) #\e)
	  (if (>fx k 0)
	      (fill-fixnum! buffer (+fx pos (+fx nb-digits 2))
			    (absfx (-fx k 1)))
	      (begin
		 (string-set! buffer (+fx pos (+fx nb-digits 2)) #\-)
		 (fill-fixnum! buffer (+fx pos (+fx nb-digits 3))
			       (absfx (-fx k 1)))))))))

;*---------------------------------------------------------------------*/
;*    *precision-shift* ...                                            */
;*---------------------------------------------------------------------*/
;; flonum is 64 bits. 53bits mantissa (when counting the hidden bit).
;; as we don't want to fight with negative numbers that leaves us 10bits we can
;; use as "precision"-bits.
(define *precision-shift* 10)

;*---------------------------------------------------------------------*/
;*    fast-divider ...                                                 */
;*---------------------------------------------------------------------*/
(define (fast-divider k-est n e_n m+ even?)
   ;; we want the "last" k such that 10^(k-1)=v*2^e_n smaller than
   ;; the unshifted n*2^e_n
   ;; 'k-est' might be one off. (i.e. too small)
   ;; -> if 10^k-est < v*2^e_n than k-est was off, and we are done.
   ;;
   ;; is v*2^e-v comp-< n*2^e-n
   ;; if not exact? then v*2^e-v + 1 comp-< n*2^e-n
   (define (less? v e_v exact?)
      (let ((comp-< (if even? <=llong <llong))
	    (comp-v (if exact? v (+llong v 1))))
	 (cond
	    ((>fx e_v e_n) #f)
	    ((<fx e_v e_n) #t)
	    ((comp-< comp-v (+llong n m+))
	     #t)
	    (else #f))))
   
   (let ((comp (if even? >=bx >bx)))
      (receive (v e_v exact?)
	 (cached-divider k-est)
	 (if (less? v e_v exact?)
	     ;; e-v might be too small now.
	     ;; shift 'v' so that the 'e's are equal.
	     (let loop ((v v)
			(e_v e_v)
			(exact? exact?))
		(cond
		   ((=fx e_v e_n)
		    (values (+fx k-est 1) v exact?))
		   ((=llong (remainderllong v #l2) #l0)
		    ;; the last digit is a 0
		    ;; if we shift and we were exact, we are still exact.
		    (loop (bit-rshllong v 1) (+fx e_v 1) exact?))
		   (else
		    (loop (bit-rshllong v 1) (+fx e_v 1) #f))))
	     ;; try the next one below
	     (fast-divider (-fx k-est 1) n e_n m+ even?)))))

;*---------------------------------------------------------------------*/
;*    generate-digits ...                                              */
;*---------------------------------------------------------------------*/
(define (generate-digits buffer pos
			 n
			 div
			 error-multiplier
			 error
			 m- m+ even?)
   ;; div must be >= the real divider.
   (define (set-digit! n)
      (string-set! buffer pos
	 (integer->char (+fx (char->integer #\0) (llong->fixnum n))))
      (+fx pos 1))

   ;; multiply  r by 10 and generate next digit.
   ;; difficulty: we are actually not sure, that we have the "space" to
   ;; multiply n by 10.
   (define (next-iteration r div error-multiplier error m- m+)
      (cond
	 ((<llong r (/llong (maxvalllong) #l10))
	  ;; enough space
	  (generate-digits buffer (+fx pos 1)
			   (*llong r #l10)
			   div
			   error-multiplier
			   (*llong error #l10)
			   (*llong m- #l10) (*llong m+ #l10)
			   even?))
	 ((<llong r (/llong (maxvalllong) #l5))
	  ;; we can not multiply by 10 (otherwise the previous
	  ;; case would have been caught), but we can multiply
	  ;; by 5.
	  ;; divide the divider by 2 and multiply n by 5.
	  (generate-digits buffer (+fx pos 1)
			   (*llong r #l5)
			   (if (evenllong? div)
			       (bit-rshllong div 1)
			       (+llong (bit-rshllong div 1) #l1))
			   (if (or (not (zerollong? error-multiplier))
				   (evenllong? div))
			       error-multiplier
			       #l1)
			   (*llong error #l5)
			   (*llong m- #l5)
			   (*llong m+ #l5)
			   even?))
	 (else ;; shift, so we get more space.
	  (next-iteration (bit-rshllong r 1)
			  (if (evenllong? div)
			      (bit-rshllong div 1)
			      (+llong (bit-rshllong div 1) #l1))
			  (if (or (not (zerollong? error-multiplier))
				  (evenllong? div))
			      error-multiplier
			      #l1)
			  (if (evenllong? error)
			      (bit-rshllong error 1)
			      (+llong (bit-rshllong error 1) #l1))
			  (bit-rshllong m- 1)
			  (bit-rshllong m+ 1)))))

   (let* ((q (/llong n div))
 	  (r (remainderllong n div))
	  (comp-< (lambda (a b) (if (or even?
					(not (zerollong? error-multiplier)))
				    (<=llong a b)
				    (<llong a b))))
	  (comp-> (lambda (a b) (comp-< b a)))
	  (new-error (+llong error (*llong error-multiplier q)))
	  (tc1 (comp-< (+llong r new-error) m-))
	  (tc2 (comp-> (+llong r m+) div)))
      (cond
	 ((and (not tc1) (not tc2))
	  (set-digit! q)
	  (next-iteration r div
			  error-multiplier new-error
			  m- m+))
	 ((and tc1 (not tc2))
	  (set-digit! q))
	 ((and (not tc1) tc2)
	  (set-digit! (+llong q #l1)))
	 (else ;; (and tc1 tc2)
	  (cond
	     ((<=llong (*llong (+llong r new-error) #l2) div)
	      (set-digit! q))
	     ((=llong q #l9)
	      (set-digit! q))
	     (else
	      (set-digit! (+llong q #l1))))))))

;*---------------------------------------------------------------------*/
;*    difficult-fill-double! ...                                       */
;*---------------------------------------------------------------------*/
(define (difficult-fill-double! buffer pos d)
   ;; mantissa 'f' is 53 bits long (includes the hidden bit)
   (receive (f e)
      (decompose-double d)
      (let* (;; k-est is such that 10^k-est < d <= 10^k-est+1
	     (k-est (k-estimation e))
	     (even? (zerollong? (bit-andllong f #l1)))
	     ;; shift the mantissa to the left as far as possible.
	     ;; as bigloo is signed avoid the sign-bit, though.
	     (shifted (bit-lshllong f *precision-shift*))
	     ;; ^p-1? should be read as: "is the mantissa of form
	     ;; 2^(p-1) where p == mantissa-size". In other words: with the
	     ;; exception of the hidden bit the whole mantissa is equal to
	     ;; zero.
	     (^p-1? (zerollong? (bit-andllong f *mantissa-mask*)))
	     ;; 'low' is the limit between v- (the next lower double) and d
	     ;; 'high' the limit between d and v+ (the next higher double)
	     (m- (bit-lshllong #l1 (if ^p-1? 8 9)))
	     (m+ (bit-lshllong #l1 9)))
	 (receive (k divider exact?)
	    (fast-divider k-est shifted e m+ even?)
	    (let ((end-pos (generate-digits
			    buffer pos
			    shifted
			    (if exact? divider (+llong divider #l1))
			    (if exact? #l0 #l1)
			    #l0
			    m- m+ even?)))
	       (prettify-string! buffer pos end-pos k))))))

;*---------------------------------------------------------------------*/
;*    copy-string-into! ...                                            */
;*---------------------------------------------------------------------*/
(define (copy-string-into! str buffer pos)
   (let ((len (string-length str)))
      (blit-string! str 0 buffer pos len)
      (+fx pos len)))

;*---------------------------------------------------------------------*/
;*    fill-double! ...                                                 */
;*---------------------------------------------------------------------*/
(define (fill-double! buffer::bstring pos::bint d::double)
   (let* ((sign-b? (not (zerofx? (signbitfl d))))
	  (abs-d (if sign-b? (negfl d) d)))
      ;; easy cases first
      (cond
	 ((=fl 0.0 d)
	  (if sign-b?
	      (copy-string-into! "-0.0" buffer pos)
	      (copy-string-into! "0.0"  buffer pos)))
	 ((infinitefl? d)
	  (if sign-b?
	      (copy-string-into! "-inf.0" buffer pos)
	      (copy-string-into! "+inf.0" buffer pos)))
	 ((nanfl? d)
	  (copy-string-into! "+nan.0" buffer pos))
	 ;; the following takes care of all smaller integers.
	 ;; a small test, that can yield huge speed improvements.
	 ((and (<fl abs-d *maxvalfx/mantissa*)
	       (=fl abs-d (fixnum->flonum (flonum->fixnum abs-d))))
	  (fixnum->double-string buffer pos (flonum->fixnum abs-d) sign-b?))
	 (else
	  (if sign-b?
	      (begin
		 (string-set! buffer pos #\-)
		 (difficult-fill-double! buffer (+fx pos 1) (negfl d)))
	      (difficult-fill-double! buffer pos d))))))

;*---------------------------------------------------------------------*/
;*    real->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (real->string d)
  (let* ((str (make-string 50))
	 (len (fill-double! str 0 d)))
     (string-shrink! str len)))
