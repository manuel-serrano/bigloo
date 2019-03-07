;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/multimedia/src/Llib/midi.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar  7 17:33:01 2019                          */
;*    Last change :  Thu Mar  7 19:03:03 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Midi support (sequencer and tools).                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-midi

    (export (class miditrack
	      (%tempo::long (default 500000))
	      (num::long read-only)
	      (ip::obj read-only (default #f))
	      (start::long read-only (default 0))
	      (datastart::long read-only (default 0))
	      (len::long read-only (default 0))
	      (wtick::uint32 (default #u32:0))
	      (status::long (default -1))
	      (oldstatus::long (default 0))
	      (eot::bool (default #f)))

	   (class midiplayer
	      (midiplayer-init)
	      (topen::procedure (default miditrack-open))
	      (mthd::procedure (default values))
	      (tempo::procedure (default (lambda (mt::miditrack t) t)))
	      (noteon::procedure read-only (default midiplayer-noteon))
	      (noteoff::procedure read-only (default midiplayer-noteoff))
	      (ctrlchange::procedure read-only (default midiplayer-prgmchange))
	      (prgmchange::procedure read-only (default midiplayer-prgmchange))
	      (meta-text::procedure read-only (default (lambda (mt s) s)))
	      (meta-track::procedure read-only (default (lambda (mt s) s)))
	      (meta-copyright::procedure read-only (default (lambda (mt s) s)))
	      (eot::procedure read-only (default (lambda (mt::miditrack) mt))))

	   (generic midiplayer-init ::midiplayer)

	   (generic midi-write-byte ::obj ::long)
	   (generic midi-write-bytes ::obj ::u8vector)
	   (generic midi-write-string ::obj ::bstring)

	   (miditrack-open::miditrack ::long ::obj ::long)
	   
	   (midiplayer-noteon ::miditrack ::long ::long ::long ::obj)
	   (midiplayer-noteoff ::miditrack ::long ::long ::long ::obj)
	   (midiplayer-ctrlchange ::miditrack ::long ::long ::long ::obj)
	   (midiplayer-prgmchange ::miditrack ::long ::long ::obj)

	   (midiplayer-play ::midiplayer ::obj ::obj)
	   
	   (midi-note-name::bstring ::long)
	   (midi-program-name::bstring ::long)
	   (midi-controller-name::bstring ::long)
	   (midi-program-index ::bstring)))

;*---------------------------------------------------------------------*/
;*    midi-text-names ...                                              */
;*---------------------------------------------------------------------*/
(define midi-text-names
   '#("" "text" "copyright" "track" "instrument" "lyric" "marker" "cue"))

;*---------------------------------------------------------------------*/
;*    midi-controller-names ...                                        */
;*---------------------------------------------------------------------*/
(define midi-controller-names
   '#("Bank Select"
      "Modulation Wheel or Lever"
      "Breath Controller"
      "Undefined"
      "Foot Controller"
      "Portamento Time"
      "Data Entry MSB"
      "Channel Volume"
      "Balance"
      "Undefined"
      "Pan"
      "Expression Controller"
      "Effect Control 1"
      "Effect Control 2"
      "Undefined"
      "Undefined"
      "General Purpose Controller 1"
      "General Purpose Controller 2"
      "General Purpose Controller 3"
      "General Purpose Controller 4"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "LSB for Control 0"
      "LSB for Control 1"
      "LSB for Control 2"
      "LSB for Control 3"
      "LSB for Control 4"
      "LSB for Control 5"
      "LSB for Control 6"
      "LSB for Control 7"
      "LSB for Control 8"
      "LSB for Control 9"
      "LSB for Control 10"
      "LSB for Control 11"
      "LSB for Control 12"
      "LSB for Control 13"
      "LSB for Control 14"
      "LSB for Control 15"
      "LSB for Control 16"
      "LSB for Control 17"
      "LSB for Control 18"
      "LSB for Control 19"
      "LSB for Control 20"
      "LSB for Control 21"
      "LSB for Control 22"
      "LSB for Control 23"
      "LSB for Control 24"
      "LSB for Control 25"
      "LSB for Control 26"
      "LSB for Control 27"
      "LSB for Control 28"
      "LSB for Control 29"
      "LSB for Control 30"
      "LSB for Control 31"
      "Damper Pedal on/off (Sustain)"
      "Portamento On/Off"
      "Sostenuto On/Off"
      "Soft Pedal On/Off"
      "Legato Footswitch"
      "Hold 2"
      "Sound Controller 1 (default: Sound Variation)"
      "Sound Controller 2 (default: Timbre/Harmonic Intens.)"
      "Sound Controller 3 (default: Release Time)"
      "Sound Controller 4 (default: Attack Time)"
      "Sound Controller 5 (default: Brightness)"
      "Sound Controller 6 (default: Decay Time - see MMA RP-021)"
      "Sound Controller 7 (default: Vibrato Rate - see MMA RP-021)"
      "Sound Controller 8 (default: Vibrato Depth - see MMA RP-021)"
      "Sound Controller 9 (default: Vibrato Delay - see MMA RP-021)"
      "Sound Controller 10 (default undefined - see MMA RP-021)"
      "General Purpose Controller 5"
      "General Purpose Controller 6"
      "General Purpose Controller 7"
      "General Purpose Controller 8"
      "Portamento Control"
      "Undefined"
      "Undefined"
      "Undefined"
      "High Resolution Velocity Prefix"
      "Undefined"
      "Undefined"
      "Effects 1 Depth"
      "Effects 2 Depth"
      "Effects 3 Depth"
      "Effects 4 Depth"
      "Effects 5 Depth"
      "Data Increment"
      "Data Decrement"
      "Non-Registered Parameter Number (NRPN)"
      "Non-Registered Parameter Number (NRPN)"
      "Registered Parameter Number (RPN)"
      "Registered Parameter Number (RPN)"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "Undefined"
      "[Channel Mode Message] All Sound Off"
      "[Channel Mode Message] Reset All Controllers"
      "[Channel Mode Message] Local Control On/Off"
      "[Channel Mode Message] All Notes Off"
      "[Channel Mode Message] Omni Mode Off (+ all notes off)"
      "[Channel Mode Message] Omni Mode On (+ all notes off)"
      "[Channel Mode Message] Mono Mode On (+ poly off, + all notes off)"
      "[Channel Mode Message] Poly Mode On (+ mono off, +all notes off)"))
      
;*---------------------------------------------------------------------*/
;*    midi-program-names ...                                           */
;*---------------------------------------------------------------------*/
(define midi-program-names
   '#(""
      ;; piano
      "Acoustic Grand Piano"
      "Bright Acoustic Piano"
      "Electric Grand Piano"
      "Honky-tonk Piano"
      "Electric Piano 1"
      "Electric Piano 2"
      "Harpsichord"
      "Clavinet"
      ;; Chromatic Percussion:
      "Celesta"
      "Glockenspiel"
      "Music Box"
      "Vibraphone"
      "Marimba"
      "Xylophone"
      "Tubular Bells"
      "Dulcimer"
      ;; organ
      "Drawbar Organ"
      "Percussive Organ"
      "Rock Organ"
      "Church Organ"
      "Reed Organ"
      "Accordion"
      "Harmonica"
      "Tango Accordion"
      ;; Guitar
      "Acoustic Guitar (nylon)"
      "Acoustic Guitar (steel)"
      "Electric Guitar (jazz)"
      "Electric Guitar (clean)"
      "Electric Guitar (muted)"
      "Overdriven Guitar"
      "Distortion Guitar"
      "Guitar harmonics"
      ;; Bass
      "Acoustic Bass"
      "Electric Bass (finger)"
      "Electric Bass (pick)"
      "Fretless Bass"
      "Slap Bass 1"
      "Slap Bass 2"
      "Synth Bass 1"
      "Synth Bass 2"
      ;; Strings
      "Violin"
      "Viola"
      "Cello"
      "Contrabass"
      "Tremolo Strings"
      "Pizzicato Strings"
      "Orchestral Harp"
      "Timpani"
      "String Ensemble 1"
      "String Ensemble 2"
      "Synth Strings 1"
      "Synth Strings 2"
      "Choir Aahs"
      "Voice Oohs"
      "Synth Voice"
      "Orchestra Hit"
      ;; Brass
      "Trumpet"
      "Trombone"
      "Tuba"
      "Muted Trumpet"
      "French Horn"
      "Brass Section"
      "Synth Brass 1"
      "Synth Brass 2"
      ;; Reed
      "Soprano Sax"
      "Alto Sax"
      "Tenor Sax"
      "Baritone Sax"
      "Oboe"
      "English Horn"
      "Bassoon"
      "Clarinet"
      ;; Pipe
      "Piccolo"
      "Flute"
      "Recorder"
      "Pan Flute"
      "Blown Bottle"
      "Shakuhachi"
      "Whistle"
      "Ocarina"
      ;; Synth Lead
      "Lead 1 (square)"
      "Lead 2 (sawtooth)"
      "Lead 3 (calliope)"
      "Lead 4 (chiff)"
      "Lead 5 (charang)"
      "Lead 6 (voice)"
      "Lead 7 (fifths)"
      "Lead 8 (bass + lead)"
      ;; Synth Pad
      "Pad 1 (new age)"
      "Pad 2 (warm)"
      "Pad 3 (polysynth)"
      "Pad 4 (choir)"
      "Pad 5 (bowed)"
      "Pad 6 (metallic)"
      "Pad 7 (halo)"
      "Pad 8 (sweep)"
      ;; Synth Effects
      "FX 1 (rain)"
      "FX 2 (soundtrack)"
      "FX 3 (crystal)"
      "FX 4 (atmosphere)"
      "FX 5 (brightness)"
      "FX 6 (goblins)"
      "FX 7 (echoes)"
      "FX 8 (sci-fi)"
      ;; Ethnic
      "Sitar"
      "Banjo"
      "Shamisen"
      "Koto"
      "Kalimba"
      "Bag pipe"
      "Fiddle"
      "Shanai"
      ;; Percussive
      "Tinkle Bell"
      "Agogo"
      "Steel Drums"
      "Woodblock"
      "Taiko Drum"
      "Melodic Tom"
      "Synth Drum"
      ;; Sound effects
      "Reverse Cymbal"
      "Guitar Fret Noise"
      "Breath Noise"
      "Seashore"
      "Bird Tweet"
      "Telephone Ring"
      "Helicopter"
      "Applause"
      "Gunshot"))

;*---------------------------------------------------------------------*/
;*    midi-note-names ...                                              */
;*---------------------------------------------------------------------*/
(define midi-note-names
   '#("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

;*---------------------------------------------------------------------*/
;*    midiplayer-init ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (midiplayer-init mdp::midiplayer)
   (with-access::midiplayer mdp (topen
				   tempo mthd 
				   noteon noteoff ctrlchange prgmchange
				   meta-text meta-copyright meta-track
				   eot)
      (unless (correct-arity? topen 3)
	 (error "midiplayer-init" "wrong topen" topen))
      (unless (correct-arity? mthd 5)
	 (error "midiplayer-init" "wrong mthd" mthd))
      (unless (correct-arity? eot 1)
	 (error "midiplayer-init" "wrong eot" eot))
      (unless (correct-arity? tempo 2)
	 (error "midiplayer-init" "wrong tempo" tempo))
      (unless (correct-arity? noteon 5)
	 (error "midiplayer-init" "wrong noteon" noteon))
      (unless (correct-arity? noteoff 5)
	 (error "midiplayer-init" "wrong noteoff" noteoff))
      (unless (correct-arity? ctrlchange 5)
	 (error "midiplayer-init" "wrong ctrlchange" ctrlchange))
      (unless (correct-arity? prgmchange 4)
	 (error "midiplayer-init" "wrong prgmchange" prgmchange))
      (unless (correct-arity? meta-text 2)
	 (error "midiplayer-init" "wrong meta-text" meta-text))
      (unless (correct-arity? meta-track 2)
	 (error "midiplayer-init" "wrong meta-track" meta-track))
      (unless (correct-arity? meta-copyright 2)
	 (error "midiplayer-init" "wrong meta-copyright" meta-copyright))
      mdp))

;*---------------------------------------------------------------------*/
;*    midi-write-byte ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (midi-write-byte op long)
   (when (output-port? op)
      (write-byte long op)))

;*---------------------------------------------------------------------*/
;*    midi-write-byte ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (midi-write-bytes op u8vec)
   (when (output-port? op)
      (let ((len (u8vector-length u8vec)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (write-byte (uint8->fixnum (u8vector-ref u8vec i)) op)
	       (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    midi-write-string ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (midi-write-string op str)
   (when (output-port? op)
      (display-string str op)))

;*---------------------------------------------------------------------*/
;*    midiplayer-noteon ...                                            */
;*---------------------------------------------------------------------*/
(define (midiplayer-noteon mt::miditrack nn::long kk::long vv::long op)
   (midi-write-byte op (bit-or #x90 nn))
   (midi-write-byte op kk)
   (midi-write-byte op vv))
   
;*---------------------------------------------------------------------*/
;*    midiplayer-noteoff ...                                           */
;*---------------------------------------------------------------------*/
(define (midiplayer-noteoff mt::miditrack nn::long kk::long vv::long op)
   (midi-write-byte op (bit-or #x80 nn))
   (midi-write-byte op kk)
   (midi-write-byte op vv))

;*---------------------------------------------------------------------*/
;*    midiplayer-ctrlchange ...                                        */
;*---------------------------------------------------------------------*/
(define (midiplayer-ctrlchange mt::miditrack nn::long cc::long nn::long op)
   (midi-write-byte op (bit-or #xb nn))
   (midi-write-byte op cc)
   (midi-write-byte op nn))

;*---------------------------------------------------------------------*/
;*    midiplayer-prgmchange ...                                        */
;*---------------------------------------------------------------------*/
(define (midiplayer-prgmchange mt::miditrack nn::long pp::long op)
   (midi-write-byte op (bit-or #xc nn))
   (midi-write-byte op pp))

;*---------------------------------------------------------------------*/
;*    midi-note-name ...                                               */
;*---------------------------------------------------------------------*/
(define (midi-note-name kk::long)
   (let ((note (remainderfx kk (vector-length midi-note-names)))
	 (octave (/fx kk (vector-length midi-note-names))))
      (format "~a[~a]" (vector-ref midi-note-names note) octave)))

;*---------------------------------------------------------------------*/
;*    midi-controller-name ...                                         */
;*---------------------------------------------------------------------*/
(define (midi-controller-name cc::long)
   (if (<fx cc (vector-length midi-controller-names))
       (vector-ref midi-controller-names cc)
       "unknown controller"))

;*---------------------------------------------------------------------*/
;*    midi-program-name ...                                            */
;*---------------------------------------------------------------------*/
(define (midi-program-name pp::long)
   (if (<fx pp (vector-length midi-program-names))
       (vector-ref midi-program-names pp)
       "unknown program"))

;*---------------------------------------------------------------------*/
;*    midi-progran-index ...                                           */
;*---------------------------------------------------------------------*/
(define (midi-program-index prgm)
   (let loop ((i (-fx (vector-length midi-program-names) 1)))
      (when (>=fx i 0)
	 (if (string-ci=? (vector-ref midi-program-names i) prgm)
	     i
	     (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    char->int32 ...                                                  */
;*---------------------------------------------------------------------*/
(define (char->int32 c)
   (fixnum->int32 (char->integer c)))

;*---------------------------------------------------------------------*/
;*    read-int16 ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-int16 ip)
   (let* ((b1 (read-char ip))
	  (b0 (read-char ip)))
      (+s32 (bit-lshs32 (char->int32 b1) 8)
	 (char->int32 b0))))

;*---------------------------------------------------------------------*/
;*    read-int24 ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-int24 ip)
   (let* ((b2 (read-char ip))
	  (b1 (read-char ip))
	  (b0 (read-char ip)))
      (+s32 (bit-lshs32 (char->int32 b2) 16)
	 (+s32 (bit-lshs32 (char->int32 b1) 8)
	    (char->int32 b0)))))

;*---------------------------------------------------------------------*/
;*    read-int32 ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-int32 ip)
   (let* ((b3 (read-char ip))
	  (b2 (read-char ip))
	  (b1 (read-char ip))
	  (b0 (read-char ip)))
      (+s32 (bit-lshs32 (char->int32 b3) 24)
	 (+s32 (bit-lshs32 (char->int32 b2) 16)
	    (+s32 (bit-lshs32 (char->int32 b1) 8)
	       (char->int32 b0))))))

;*---------------------------------------------------------------------*/
;*    read-int ...                                                     */
;*---------------------------------------------------------------------*/
(define (read-int len::long ip::input-port)
   (let loop ((val (char->integer (read-char ip)))
	      (len len))
      (if (=fx len 1)
	  val
	  (loop (+fx (bit-lsh val 8) (char->integer (read-char ip)))
	     (-fx len 1)))))

;*---------------------------------------------------------------------*/
;*    read-vlq ...                                                     */
;*---------------------------------------------------------------------*/
(define (read-vlq ip)
   (let ((c (char->integer (read-char ip))))
      (if (<=fx c #x7f)
	  c
	  (let loop ((val (bit-lsh (-fx c 128) 7)))
	     (let ((c (char->integer (read-char ip))))
		(if (<= c 127)
		    (+fx val c)
		    (loop (bit-lsh (+fx val c) 7))))))))

;*---------------------------------------------------------------------*/
;*    check-byte ...                                                   */
;*---------------------------------------------------------------------*/
(define (check-byte ip byte)
   (let ((c (read-byte ip)))
      (unless (=fx c byte)
	 (error "check-byte" (format "byte ~a expected" byte) c))))

;*---------------------------------------------------------------------*/
;*    tempo->usec ...                                                  */
;*---------------------------------------------------------------------*/
(define (tempo->usec tick tempo tpb)
   (/fx (*fx tick tempo) tpb))

;*---------------------------------------------------------------------*/
;*    midi-read-mthd ...                                               */
;*---------------------------------------------------------------------*/
(define (midi-read-mthd ip)
   (let ((ty (read-type ip)))
      (if (not (string=? ty "MThd"))
	  (error "read-midi-mthd" "Bad header" ty)
	  (let* ((sz (read-int32 ip))
		 (fmt (read-int16 ip))
		 (tck (int32->fixnum (read-int16 ip)))
		 (div (int32->fixnum (read-int16 ip))))
	     (if (=fx (bit-rsh div 15) 0)
		 (values sz fmt tck 500000 (bit-and div #b111111111111111))
		 (case (-fx #x80 (bit-and (bit-rsh div 8) #x7f))
		    ((24)
		     (values sz fmt tck 500000 (*fx 12 (bit-and div #xff))))
		    ((25)
		     (values sz fmt tck 400000 (*fx 10 (bit-and div #xff))))
		    ((29)
		     (values sz fmt tck 100000000 (*fx 2997 (bit-and div #xff))))
		    ((30)
		     (values sz fmt tck 500000 (*fx 15 (bit-and div #xff))))
		    (else
		     (error "read-midi-mthd"
			"invalid number of SMPTE frames per second"
			(-fx #x80 (bit-and (bit-rsh div 8) #x7f))))))))))

;*---------------------------------------------------------------------*/
;*    midi-read-mtrk ...                                               */
;*---------------------------------------------------------------------*/
(define (midi-read-mtrk ip)
   (let ((ty (read-type ip)))
      (if (not (string=? ty "MTrk"))
	  (error "read-midi-mtrk" "Bad header" ty)
	  (read-int32 ip))))
   
;*---------------------------------------------------------------------*/
;*    read-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-type ip)
   (read-chars 4 ip))

;*---------------------------------------------------------------------*/
;*    midiplayer-play ...                                              */
;*---------------------------------------------------------------------*/
(define (midiplayer-play player::midiplayer ip op)
   (multiple-value-bind (sz fmt tck tempo ppq)
      (midi-read-mthd ip)
      (with-access::midiplayer player (mthd)
	 (multiple-value-bind (sz fmt tck tempo ppq)
	    (mthd sz fmt tck tempo ppq)
	    (case (int32->fixnum fmt)
	       ((0) (read-track player ip op tempo ppq))
	       ((1) (read-multi-track player ip op tempo ppq tck))
	       ((2) (error "midiplayer-play" "format not supported" fmt))
	       (else (error "midiplayer-play" "bad midi format" fmt)))))))

;*---------------------------------------------------------------------*/
;*    miditrack-read-event ...                                         */
;*---------------------------------------------------------------------*/
(define (miditrack-read-event t::miditrack player::midiplayer op)
   (with-access::midiplayer player ((seteot eot)
				    (settempo tempo)
				    noteon noteoff ctrlchange prgmchange
				    meta-text meta-copyright meta-track)
      (with-access::miditrack t (num ip wtick status oldstatus eot %tempo)
	 (cond
	    ((=fx status #xff)
	     (let ((c (read-byte ip))
		   (l (read-vlq ip)))
		(case c
		   ((0)
		    (read-byte ip)
		    (tprint "!!!!!!!!!! ss=" (read-int16 ip))
		    #unspecified)
		   ((#x01)
		    (meta-text t (read-chars l ip)))
		   ((#x02)
		    (meta-copyright t (read-chars l ip)))
		   ((#x03)
		    (meta-track t (read-chars l ip)))
		   ((#x04 #x05 #x06 #x07)
		    (let ((text (read-chars l ip)))
		       #unspecified))
		   ((#x20)
		    ;; channel prefix
		    (let ((cc (read-byte ip)))
		       #unspecified))
		   ((#x21)
		    ;; midi port
		    (let ((port (read-int l ip)))
		       #unspecified))
		   ((#x2f)
		    (set! eot #t)
		    (seteot t)
		    %tempo)
		   ((#x51)
		    ;; duration in usec of a quarter note (une noire)
		    (set! %tempo (settempo t (read-int l ip)))
		    #unspecified)
		   ((#x54)
		    (let* ((hr (read-byte ip))
			   (mm (read-byte ip))
			   (se (read-byte ip))
			   (fr (read-byte ip))
			   (ff (read-byte ip)))
		       #unspecified))
		   ((#x59)
		    (let ((sig (read-int l ip)))
		       #unspecified))
		   ((#x58)
		    (let* ((nn (char->integer (read-char ip)))
			   (dd (char->integer (read-char ip)))
			   (cc (char->integer (read-char ip)))
			   (bb (char->integer (read-char ip))))
		       #unspecified))
		   (else
		    (tprint "  !!!!!!!!!!!!!! else=" c " " (integer->string c 16))))))
	    ((or (=fx status #xf0) (=fx status #xf7))
	     (midi-write-string op (read-chars (read-vlq ip) ip))
	     #unspecified)
	    ((=fx (bit-rsh status 4) #xb)
	     ;; controller change
	     (let* ((cc (char->integer (read-char ip)))
		    (nn (char->integer (read-char ip))))
		(ctrlchange t (bit-and status #b1111) cc nn op)))
	    ((=fx (bit-rsh status 4) #xc)
	     ;; program change
	     (let ((pp (char->integer (read-char ip))))
		(prgmchange t (bit-and status #b1111) pp op)))
	    ((=fx (bit-rsh status 4) #xd)
	     ;; channel key press
	     (let ((pp (char->integer (read-char ip))))
		(midi-write-byte op status)
		(midi-write-byte op pp)
		#unspecified))
	    ((or (=fx (bit-rsh status 4) #xa)
		 (=fx (bit-rsh status 4) #xe))
	     (let ((cc (char->integer (read-char ip)))
		   (vv (char->integer (read-char ip))))
		(midi-write-byte op status)
		(midi-write-byte op cc)
		(midi-write-byte op vv)
		#unspecified))
	    ((or (=fx (bit-rsh status 4) #x9))
	     (let* ((kk (char->integer (read-char ip)))
		    (vv (char->integer (read-char ip))))
		(noteon t (bit-and status #b1111) kk vv op)
		#unspecified))
	    ((or (=fx (bit-rsh status 4) #x8))
	     (let* ((kk (char->integer (read-char ip)))
		    (vv (char->integer (read-char ip))))
		(noteoff t (bit-and status #b1111) kk vv op)
		#unspecified))
	    ((=fx status -1)
	     #unspecified)
	    (else
	     (tprint "status else=" status " "
		(integer->string status 16)))))))
   
;*---------------------------------------------------------------------*/
;*    read-track ...                                                   */
;*    -------------------------------------------------------------    */
;*    ip: the input port                                               */
;*    op: the output port (possibly a MIDI port)                       */
;*    tpb: tick per beat (aka ppqn)                                    */
;*    -------------------------------------------------------------    */
;*    Read a complete track.                                           */
;*---------------------------------------------------------------------*/
(define (read-track player::midiplayer ip op tempo ppq)
   (let ((offs (input-port-position ip)))
      (with-access::midiplayer player (noteon noteoff ctrlchange prgmchange
				       meta-text meta-copyright meta-track)
	 (let ((tklen (midi-read-mtrk ip)))
	    (let ((track (instantiate::miditrack
			    (num 0)
			    (%tempo tempo)
			    (start offs)
			    (datastart (input-port-position ip))
			    (len (int32->fixnum tklen))
			    (ip ip))))
	       (with-access::miditrack track (status oldstatus eot %tempo)
		  (let loop ()
		     (let ((dt (read-vlq ip)))
			(set! status (read-byte ip))
			(when (<fx status 128)
			   (begin
			      (unread-char! (integer->char status) ip)
			      (set! status oldstatus))
			   (set! oldstatus status))
			(when (and (>fx dt 0) (not (=fx status #xff)))
			   (sleep (tempo->usec dt %tempo ppq)))
			(miditrack-read-event track player op)
			(unless eot
			   (loop)))))
	       track)))))

;*---------------------------------------------------------------------*/
;*    miditrack-step ...                                               */
;*    -------------------------------------------------------------    */
;*    step inside a midi track of a multi-track midi file.             */
;*---------------------------------------------------------------------*/
(define-generic (miditrack-step t::miditrack player::midiplayer op tick::uint32 ppq)
   (with-access::midiplayer player (noteon noteoff ctrlchange prgmchange
				      meta-text meta-copyright meta-track)
      (with-access::miditrack t (num ip wtick status oldstatus eot)
	 (when (<=u32 wtick tick)
	    (let loop ()
	       (miditrack-read-event t player op)
	       (if eot
		   (set! wtick (fixnum->uint32 -1))
		   (let ((dt (read-vlq ip)))
		      (set! status (read-byte ip))
		      (if (<fx status 128)
			  (begin
			     (unread-char! (integer->char status) ip)
			     (set! status oldstatus))
			  (set! oldstatus status))
		      (if (>fx dt 0)
			  (set! wtick (+u32 tick (fixnum->int32 dt)))
			  (loop)))))))))

;*---------------------------------------------------------------------*/
;*    miditrack-open ...                                               */
;*---------------------------------------------------------------------*/
(define (miditrack-open::miditrack num file offs)
   
   (define (open-input-port/position file offs)
      (let ((ip (open-input-file file)))
	 (set-input-port-position! ip offs)
	 ip))

   (let* ((ip (open-input-port/position file offs))
	  (tklen (int32->fixnum (midi-read-mtrk ip))))
      (instantiate::miditrack
	 (num num)
	 (start offs)
	 (datastart (input-port-position ip))
	 (len tklen)
	 (ip ip))))

;*---------------------------------------------------------------------*/
;*    read-multi-track ...                                             */
;*---------------------------------------------------------------------*/
(define (read-multi-track player::midiplayer ip op tempo ppq ntk::long)
   (tprint "READ-MULTI ..." ip)
   (let ((ti (read-track player ip op tempo ppq)))
      (with-access::midiplayer player (topen)
	 (let* ((file (input-port-name ip))
		(offs (input-port-position ip))
		(tracks (let loop ((i 1)
				   (offs (input-port-position ip)))
			   (if (=fx i ntk)
			       '()
			       (let ((t (topen i file offs)))
				  (with-access::miditrack t (datastart len)
				     (cons t
					(loop (+fx i 1) (+fx datastart len))))))))
		(res (with-access::miditrack ti (%tempo) (/fx %tempo ppq))))
	    (close-input-port ip)
	    (let loop ((tick #u32:0))
	       (let ((t0 (current-microseconds)))
		  (for-each (lambda (t::miditrack)
			       (miditrack-step t player op tick ppq))
		     tracks)
		  (let ((dt (llong->fixnum (-llong (current-microseconds) t0))))
		     (sleep (-fx res dt))
		     (loop (+u32 tick #u32:1)))))
	    (for-each (lambda (t::miditrack)
			 (with-access::miditrack t (ip)
			    (close-input-port ip)))
	       tracks)))))
						   

  
   
 
