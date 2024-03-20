;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESCRIPTION
;;; CLUSTER ENGINE LIBRARY FOR OPENMUSIC
;;; PWGL VERSION BY ORJAN SANDRED (2010)
;;; PLAIN LISP VERSION BY ORJAN SANDRED, TORSTEN ANDERS AND JULIEN VINCENOT (2020)
;;; OM VERSION BY PAULO HENRIQUE RAPOSO (2021)
;;; UPDATE may 03/2022
;;; - ADDED THE FUNCTIONS CLUSTER-ENGINE2 (TUTORIAL UNDER CONSTRUCTION) AND CLUSTER-ENGINE3 (SUPPORTS AN UNLIMITED NUMBER OF VOICES)

(in-package :om)

(mapc 'compile&load (list
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "iter" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "from-studio-flat" :type "lisp")
 ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "pw-common-language" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "01-domain" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "02-engine" :type "lisp")						 
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "03-Fwd-rules" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "04-Backtrack-rules" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05-rules-interface":type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05a-rules-interface-1engine" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05b-rules-interface-2engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05c-rules-interface-2engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05d-rules-interface-2engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05d-2-rules-interface-2engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05e-rules-interface-2engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05f-rules-interface-3engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05g-rules-interface-any-n-engines" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05h-rules-higher-level" :type "lisp")		
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05i-rules-stop-search" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05n-rules-interface-nn-engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "05o-new-canon-rule" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06-heuristic-rules-interface" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06a-heuristic-rules-interface-1engine" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06b-heuristic-rules-interface-2engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06c-heuristic-rules-interface-2engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06d-heuristic-rules-interface-2engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06e-heuristic-rules-interface-2engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06f-heuristic-rules-interface-3engines" :type "lisp")						 						 						 						 						 
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06g-heuristic-rules-interface-any-n-engines" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "06h-heuristic-rules-interface-added2020" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "07-backjumping" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "08-decode" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "09-utilities" :type "lisp")
 ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "09b-markov-tools" :type "lisp")	
 ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "09c-cluster-energy-profile" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "_000-main-interface" :type "lisp") 
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "_001-gen_domains" :type "lisp")							 
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "10-OM-interface" :type "lisp")
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "12-debug-tools" :type "lisp")	
 (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "13-convert-pmc-rules" :type "lisp")		
 ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "pw_profile_functions" :type "lisp")
                    )
)

(fill-library '( ("ENGINE" Nil Nil (ce::Cluster-Engine 
									ce::Cluster-Engine2
	                                ce::Cluster-Engine3
									ce::rules-to-cluster) Nil)

                ("DOMAIN, DEBUG AND MORE" Nil Nil (ce::metricdomain 
												   ce::Stoprule-time 
												   ce::Stoprule-index 
												   ce::Rpredefine-meter 
												   ce::prefs) Nil)
				
				("RULES ONE ENGINE" Nil Nil (ce::Rrhythms-one-voice
											 ce::Rindex-rhythms-one-voice
											 ce::Rpitches-one-voice 
											 ce::Rindex-pitches-one-voice 
											 ce::Rtime-signatures 
											 ce::Rindex-time-signatures 
											 ce::Ronly-m-motifs 
											 ce::Rrhythms-one-voice-at-timepoints 
											 ce::HRrhythms-one-voice 
											 ce::HRindex-rhythms-one-voice 
											 ce::HRpitches-one-voice 
											 ce::HRindex-pitches-one-voice 
											 ce::HRtime-signatures 
											 ce::HRindex-time-signatures 
											 ce::Rpmc-one-voice 
											 ce::Rjbs-one-voice 
			                                 ce::setend 
									          ) Nil)
				 
  				("RULES ONE VOICE" Nil Nil (ce::Rrhythm-pitch-one-voice 
											ce::Rindex-rhythm-pitch-one-voice 
											ce::Rmetric-hierarchy
											ce::Rnote-meter 
											ce::Rmeter-note  
											ce::Rmel-interval-one-voice 
											ce::HRrhythm-pitch-one-voice 
											ce::HRindex-rhythm-pitch-one-voice 
											ce::HRduration-meter 
											ce::HRmeter-duration
  											 ) Nil)
	 			 
 				("RULES TWO VOICES" Nil Nil (ce::Rrhythm-rhythm 
											 ce::Rrhythm-hierarchy 
											 ce::Rcanon 
											 ce::HRrhythm-rhythm
 											 ) Nil)
										 
               ("RULES TWO OR MORE VOICES" Nil Nil (ce::Rpitch-pitch 
												    ce::Rchords
													ce::Rchords-bass-positions 
												    ce::Rlist-all-events 
												    ce::HRpitch-pitch 
												    ce::HRlist-all-events 
				   								 	) Nil)
													
               ("UTILITIES" Nil Nil (ce::apply_and 
								     ce::apply_minus
									 ce::midics-to-pcs
								     ;ce::test-seq-follows-markov_chain? 
								     ;ce::test-seq-follows-energy_profile?
							          ) Nil)	
									  
		       ("JBS-CONSTRAINTS-DEMO" Nil Nil (ce::allowed-intervals-rule) Nil)								 
																					 
))

(print 
"
CLUSTER ENGINE LIBRARY FOR OPENMUSIC 
 - PWGL VERSION BY ORJAN SANDRED (2010)
 - PLAIN LISP VERSION - ORJAN SANDRED, TORSTEN ANDERS AND JULIEN VINCENOT (2020)
 - OM VERSION - PAULO HENRIQUE RAPOSO (2021)
")



