(in-package cluster-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; OM-VERSION -PHRAPOSO - 2021 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(defun make-voices (rhythm pitch timsig tempo)
 (om::make-instance 'om::voice
                    :tree (om::mktree rhythm timsig)
                    :chords (remove nil pitch)
                    :tempo tempo))

(defun poly-engine->score (linear-solution)
(let ((tempo 90)
       (rhythm-pitch (loop for n from 0 to (- (length linear-solution) 2) by 2
                           collect (list (nth n linear-solution) (nth (1+ n) linear-solution)))))
 (mapcar #'(lambda (input1)
  (make-voices (first input1) (second input1) (om::last-elem linear-solution) tempo)) rhythm-pitch))) 
 
(defun poly-engine->score-with-tempo (linear-solution tempo)
(let ((rhythm-pitch (loop for n from 0 to (- (length linear-solution) 2) by 2
                          collect (list (nth n linear-solution) (nth (1+ n) linear-solution)))))						   
 (mapcar #'(lambda (input1)
  (make-voices (first input1) (second input1) (om::last-elem linear-solution) tempo)) rhythm-pitch))) 
	  
(defun fix-ompatch-rule (lambda-patch)
 (let ((exp (om::function-lambda-expression lambda-patch)))
  (eval `(function ,exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLUSTER-ENGINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ClusterEngine is a siplified version of ClusterEngine2
(om::defmethod! Cluster-Engine ((no-of-variables integer)
                                (rnd? t)
                                (debug? t)
                                (rules t )
                                 
                                (tempo number)
                                 
                                (output t)
                                (metric-domain t)
                                (rhythmdomain0 list)
                                (pitchdomain0 list)
                                &optional (rhythmdomain1 nil) ( pitchdomain1 nil) (rhythmdomain2 nil) (pitchdomain2 nil)
                                (rhythmdomain3 nil) (pitchdomain3 nil) (rhythmdomain4 nil) (pitchdomain4 nil)
                                (rhythmdomain5 nil) (pitchdomain5 nil) (rhythmdomain6 nil) (pitchdomain6 nil)
                                (rhythmdomain7 nil) (pitchdomain7 nil) (rhythmdomain8 nil) (pitchdomain8 nil)
                                (rhythmdomain9 nil) (pitchdomain9 nil))
								
:initvals '( 10 t nil nil 90 :voices ((4 4)) ((1/4)) nil nil nil nil nil nil nil nil nil nil)								 
:indoc '("integer" "t/nil" "t/nil" "t/nil" "number" "voices-or-list" "list-of-timsig" "ratios" "midics" "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics" ) 
:icon 01
:menuins '((5 (("voices" :voices) ("list" :list)) ) )
:doc "The Cluster Engine - the main function.
Pitch domains cannot exist without at least one duration in the rhythm domain.
Domains with only one value will not use up any time in the search process.
"
(let* ((all-domains (list rhythmdomain0 pitchdomain0 rhythmdomain1 pitchdomain1 rhythmdomain2 pitchdomain2 rhythmdomain3 pitchdomain3
                                    rhythmdomain4 pitchdomain4 rhythmdomain5 pitchdomain5 rhythmdomain6 pitchdomain6 rhythmdomain7 pitchdomain7
                                    rhythmdomain8 pitchdomain8 rhythmdomain9 pitchdomain9))
         (default-pitch (loop for n from 0 to (1- (length all-domains)) by 2
                                  collect (if (and (not (equal nil (nth n all-domains))) 
                                                          (equal nil (nth (1+ n) all-domains)))
                                                  (om::x-append (list (nth n all-domains)) (list '((6000))))
                                                  (om::x-append (list (nth n all-domains)) (list (nth (1+ n) all-domains))))))
         (list-of-domains (remove nil (om::flat-once default-pitch))))

(if (equal output :list)
    (mapcar #'(lambda (n) (remove nil n)) (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains))
    (poly-engine->score-with-tempo (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains) tempo))))

(om::defmethod! Cluster-Engine2 ((no-of-variables integer)
                                 (rnd? t)
                                 (debug? t)
                                 (rules t)
                                 (bktr-rule t)
                                 (tempo number)
                                 (fwd-rule t)
                                (output t)
                                (metric-domain t)
                                (rhythmdomain0 list)
                                (pitchdomain0 list)
                                &optional (rhythmdomain1 nil) ( pitchdomain1 nil) (rhythmdomain2 nil) (pitchdomain2 nil)
                                (rhythmdomain3 nil) (pitchdomain3 nil) (rhythmdomain4 nil) (pitchdomain4 nil)
                                (rhythmdomain5 nil) (pitchdomain5 nil) (rhythmdomain6 nil) (pitchdomain6 nil)
                                (rhythmdomain7 nil) (pitchdomain7 nil) (rhythmdomain8 nil) (pitchdomain8 nil)
                                (rhythmdomain9 nil) (pitchdomain9 nil))

 :initvals '( 10 t nil nil :bktr-rule1 90 :fwd-indep :voices ((4 4)) ((1/4)) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)								 
 :indoc '("integer" "t/nil" "t/nil"  "t/nil" "select" "number" "select" "voices-or-list" "list-of-timsig" 
	      "ratios" "midics" "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics" 
		  "ratios" "midics" "ratios" "midics"  "ratios" "midics"  "ratios" "midics"  "ratios" "midics" ) 
  :icon 01
  :menuins '( (4 (("bktr-rule1" :bktr-rule1) ("bktr-rule2" :bktr-rule2) ("bktr-rule3" :bktr-rule3)) ) 
 	         (6 (("fwd-indep" :fwd-indep) ("fwd-rule2" :fwd-rule2) ("fwd-rule3" :fwd-rule3) 
				 ("fwd-rule4" :fwd-rule4) ("fwd-rule5" :fwd-rule5) ("fwd-rule6" :fwd-rule6) ("fwd-rule6B" :fwd-rule6B)) ) 
 	         (7 (("voices" :voices) ("list" :list)) ) )
  :doc "Forward rules:
-fwd-indep: Forward is independant from backtracking.
This rule does not condsider how the engines backtracked when stepping 
forward in the search. 

-fwd-rule2: If the system backtracked earlier, the system will step forward 
these engines (in reverse order) before picking any other engine.
Backjumping is not taken into consideration when stepping forward (if several 
steps where backjumped, only one forward step will be forced).

-fwd-rule3: Index plays a role in forward stepping. If an engine 
backtracked, it will be forced to step forward by this rule (like 
fwd-rule2), however if the index for this engine when forwarding it is 
lower than when it was backtracked, it will not be forced to step 
forward until the system reaches the correct index. Instead the system 
will step forward according to the general priorities (below).

-fwd-rule4: Index plays a role in forward stepping. As fwd-rule3 but instead 
of leting the system use general rules if index is lower than at the moment 
of backtracking, the engine that needs to catch up will be forced to catch up.

-fwd-rule5: Count value plays a role in forward stepping. If an engine was 
backtracked, it will be forced to step forward by this rule (like fwd-rule2). 
However if the count value for this engine when forwarding it is lower than 
when it was backtracked, the general rules (see below) will determine how to 
step forward until the system reaches the correct count value for this engine.

-fwd-rule6: Rhythm and metric engines are stepped forward acording to
time points, pitch engines are stepped forward accoriding to ount value: If an 
engine was backtracked, it will be forced to step forward by this rule (like 
fwd-rule2). However if the time point/count value for this engine when forwarding 
it is lower than when it was backtracked, it will be stepped forward without 
deducting it from the backtrak route. If the point/vaule is higher than when
it was backtracked, the rule will skip and go to next step in the backtrack
history.

-fwd-rule6B: As fwd-rule6, but if the time point/count value for this engine 
when forwarding it is lower than when it was backtracked, it will be stepped
forward accoring to general rules. 

If backtracking does not play a role, the system will decide what engine to 
forward according to these three priorities: 
1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start 
with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are 
equal, the default search order determines which voice to search next.
"
 ;(:groupings '(3 2 2 1 1 2)  :extension-pattern '(2) :x-proportions '((0.2 0.1 0.1)(0.1 0.3)(0.1 0.3)(0.4)(0.4)(0.2 0.2)) :w 0.5)

                 (when (not metric-domain) (setf metric-domain (create-metric-domain-vector '((4 4)) '((3 4)) '(nil))))
                 (when (typep metric-domain 'list) (setf metric-domain (create-metric-domain-vector metric-domain 
                                                                                                    (make-list (length metric-domain) :initial-element '(3 4))
                                                                                                    (make-list (length metric-domain) :initial-element nil))))
               
                 
                 (let* (;(no-of-engines (- (length (ccl::pwgl-subviews ccl::%box%)) 8)) ;8 since there are 8 inputs before the domain
                           (no-of-engines (length (remove nil (list metric-domain rhythmdomain0 pitchdomain0 rhythmdomain1 pitchdomain1 rhythmdomain2 pitchdomain2 rhythmdomain3 pitchdomain3 
                                                                                                                  rhythmdomain4 pitchdomain4 rhythmdomain5 pitchdomain5 rhythmdomain6 pitchdomain6 rhythmdomain7 pitchdomain7 
                                                                                                                  rhythmdomain8 pitchdomain8 rhythmdomain9 pitchdomain9)))) ;; FOR OM -PHRAPOSO (2021)
                        (domains (loop for n from 1 to (1- no-of-engines) 
                                       for sub-domain in (list rhythmdomain0 pitchdomain0 rhythmdomain1 pitchdomain1 rhythmdomain2 pitchdomain2 rhythmdomain3 pitchdomain3
                                                               rhythmdomain4 pitchdomain4 rhythmdomain5 pitchdomain5 rhythmdomain6 pitchdomain6 rhythmdomain7 pitchdomain7
                                                               rhythmdomain8 pitchdomain8 rhythmdomain9 pitchdomain9)
                                       collect sub-domain))
                        (no-of-voices (/ (1- no-of-engines) 2))
                        (locked-engines (analyze-domain-for-locked-engines domains metric-domain))
                        (vrules (create-rule-vector rules no-of-engines locked-engines))
                        (vheuristic-rules (create-heuristic-rule-vector rules no-of-engines locked-engines))
                        (backtrack-rule (cond ((equal bktr-rule :bktr-rule1)
                                               'backtrack-rule1)
                                              ((equal bktr-rule :bktr-rule2)
                                               'backtrack-rule2)
                                              ((equal bktr-rule :bktr-rule3)
                                               'backtrack-rule3)
                                              (t nil)))
                        (forward-rule (cond ((equal fwd-rule :fwd-indep)
                                             'fwd-rule-indep)
                                            ((equal fwd-rule :fwd-rule2)
                                             'fwd-rule2)
                                            ((equal fwd-rule :fwd-rule3)
                                             'fwd-rule3)
                                            ((equal fwd-rule :fwd-rule4)
                                             'fwd-rule4)
                                            ((equal fwd-rule :fwd-rule5)
                                             'fwd-rule5)
                                            ((equal fwd-rule :fwd-rule6)
                                             'fwd-rule6)
                                            ((equal fwd-rule :fwd-rule6B)
                                             'fwd-rule6B)
                                            (t nil))))

                   (if (equal output :list) 
                       (remove nil (time (poly-engine no-of-variables domains metric-domain no-of-voices locked-engines forward-rule backtrack-rule rnd? vrules vheuristic-rules debug?)))                 
                       (poly-engine->score-with-tempo
                      (time (poly-engine no-of-variables domains metric-domain no-of-voices locked-engines forward-rule backtrack-rule rnd? vrules vheuristic-rules debug?))
                      tempo))
                   ))
				   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RULES -> CLUSTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   
(om::defmethod! rules-to-cluster (&rest rules?)
	:initvals '(nil)
    :indoc '("rules") 
	:icon 01
	:doc "Use this function to collect all rules before inputting them to the Cluster engine.
It is possible to input the output of this function to a second Rules-to-Cluster function (to help
organizing your rules in groups)." 
 (rules->cluster rules?))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DOMAIN, DEBUG AND MORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (om::defmethod! metricdomain ((timesign1 list) (tuplets1 list) (alt-beatlength1 t)
 	                                &optional (timesign2 '()) (tuplets2 '()) (alt-beatlength2 '())
 	                                (timesign3 '()) (tuplets3 '()) (alt-beatlength3 '())
 	                                (timesign4 '()) (tuplets4 '()) (alt-beatlength4'())
 	                                (timesign5 '()) (tuplets5 '()) (alt-beatlength5'())
 	                                (timesign6 '()) (tuplets6 '()) (alt-beatlength6'())
 	                                (timesign7 '()) (tuplets7 '()) (alt-beatlength7'())
 	                                (timesign8 '()) (tuplets8 '()) (alt-beatlength8'())) 
 	:initvals '((4 4) (1 2 3 4) () nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
  	:indoc '("time-signature1" "tuplets1" "alt-beatlength1" 
 		     "time-signature2" "tuplets2" "alt-beatlength2" 
 		     "time-signature3" "tuplets3" "alt-beatlength3" 
 		     "time-signature4" "tuplets4" "alt-beatlength4" 
 		     "time-signature5" "tuplets5" "alt-beatlength5"
 		     "time-signature6" "tuplets6" "alt-beatlength6"
 		     "time-signature7" "tuplets7" "alt-beatlength7" 
 		     "time-signature8" "tuplets8" "alt-beatlength8") 
  	:icon 01
  	:doc "This function sets the metric domain in more detail than just a list of possible time signatures.

 	<tuplets> is a list of allowed subdivisions of the beat. This setting only has an effect if 
 	using the rule r-metric-hierarchy.

 	<alt-beatlength> allows the user to define another beat length than the time 
 	signature indicates. This will affect rules that consrain events located on
 	beats:
 	    - nil:  If this input is nil, the default beat length for a time signature 
 	            will be used (i.e. the time signature '(4 4) will have the beat length
 	            1/4, the time signature '(6 8) will have the beat length 1/8, etc.
 	    - [a fraction]: If this input is a fraction, it will be used as the beat
 	            length. For example, if the time signature is '(6 8), the fraction
 	            3/8 will give replace the default beat length 1/8. The subdivision of 
 	            beats (the tuplets input) will relate to the alternative beat length.
 	    - [a list]:  If this input is a list, it will define an arbitrary beat
 	            division of the measure. For example, if the time signature is
 	           '(9 8), the list '(3/8 2/8 2/8 2/8) will distribute the beats
 	           accordingly. NOTE: the sum of the beats have to add up to the length 
 	           of a measure. The subdivision of beats (the tuplets input) will
 	           relate to the default beat length.
 	    "	
   (metric-domain timesign1 tuplets1 alt-beatlength1
  	                   timesign2 tuplets2 alt-beatlength2
  	                   timesign3 tuplets3 alt-beatlength3
 	                   timesign4 tuplets4 alt-beatlength4
 	                   timesign5 tuplets5 alt-beatlength5
 	                   timesign6 tuplets6 alt-beatlength6
 	                   timesign7 tuplets7 alt-beatlength7
 	                   timesign8 tuplets8 alt-beatlength8))

(om::defmethod! Stoprule-time ((voices list) (stoptime integer) (input-mode t))
 :initvals '((0) 4 :or)
 :indoc '("voices" "stoptime" "input-mode") 
 :icon 01
 :menuins '( (2 (("OR" :or) ("AND" :and) ("meter" :meter))) ) 
 :doc "This rule will not affect the choice of musical parameters (it will always be true), but it will stop the search when a time point has been reached. The solution found when the time point is reached will be returned as a valid solution.

The rule compares the START TIME of motifs to the stoptime (i.e. the end time or the time point for durations inside motifs of a motif is not checked). The stop will happen when a duration reaches the stop time.

<voices> is one or a list of voices that will be checked.

<stoptime> is a timepoint in the score, counting from 0, where the search will stop (ex. the time point 5/2 is 2 whole notes + 1 half note into the score). The stoptime has to be reached in all given voices. Note that the rule ignors the pitch information.

<input-mode>

 - OR: The search will stop when one of the voices in the given list reached the stop time. The metric structure will be ignored.
 - AND: The search will stop when all the voices in the given list reached the stop time. The metric structure will be ignored.
 - meter: The start time for the measures will determine the stop.


Note that if the system has not assigned a meter for durations, they will not be displayed in the score.

"
(Stop-rule-time voices stoptime input-mode))
 

(om::defmethod! Stoprule-index ((voices list) (stopindex integer) (input-mode t) (parameters t))
 :initvals '((0) 4 :or :pitch)
 :indoc '("voices" "stopindex" "input-mode" "parameters") 
 :icon 01
 :menuins '( (2 (("OR" :or)))
             (3 (("pitch" :pitch) ("rhythm" :rhythm) ("pitch/rhythm" :pitch/rhythm))) ) 
 :doc "This rule will not affect the choice of musical parameters (it will always be true), but it will stop the search when an index has been reached. The solution found when the index is reached will be returned as a valid solution.

<voices> is one or a list of voices that will be checked.

<stopindex> is an index in the score, counting from 0, where the search will stop.

<input-mode>

 - OR: The search will stop when one of the voices in the given list reached the stop time. The metric structure will be ignored.
"
 (Stop-rule-index voices stopindex input-mode parameters)) 

(om::defmethod! Rpredefine-meter ((timesig-list list)) ; CORRECTED 16/09/2021-phgraposo
 :initvals '( ((4 4) (3 4)) )
 :indoc '("time-signatures") 
 :icon 01
 :doc "This rule predefines the time signature to follow a given sequence.
WARNING: This rule will preset the sequence of time signature and 
will not allow the system to backtrack them.

It the given list of time signatures is shorted than what is needed 
in the solution, the remaining time signatures will be picked
randomly.
"
(R-predefine-meter timesig-list))

(om::defmethod! prefs ((backjump? t)
                              (max-nr-of-loops integer)
                              (bktr-rp1v t)  
                              (bktr-rr2v t) 
                              (bktr-rh2v t) 
                              (bktr-rmh2v t)
                              (bktr-dm1v t) 
                              (bktr-nm1v t) 
                              (bktr-md1v t) 
                              (bktr-mn1v t) 
                              (bktr-ppnv t) 
                              (bktr-leNv t))
 	:initvals '(t 10000000 :self :other :other :rhythm :rhythm :self :rhythm :self :next-pitch :next)
 	:indoc '("backjump?" "max-nr-of-loops" "bktr-rp1v" "bktr-rr2v" "bktr-rh2v" "bktr-rmh2v" "bktr-dm1v" "bktr-nm1v" "bktr-md1v" "bktr-mn1v" "bktr-ppnv" "bktr-leNv") 
 	:icon 01
        :menuins '((0 (("t" t) ("nil" nil)) )
                         (2 (("self" :self) ("other" :other)) )
                         (3 (("other" :other) ("self" :self)) )
                         (4 (("other" :other) ("self" :self)) )
                         (5 (("rhythm" :rhythm) ("meter" :meter)) )
                         (6 (("rhythm" :rhythm) ("meter" :meter)) )
                         (7 (("self" :self) ("rhythm/pitch" :rhythm/pitch) ("pitch/rhythm" :pitch/rhythm)) )
                         (8 (("rhythm" :rhythm) ("meter" :meter)) )
                         (9 (("self" :self) ("rhythm/pitch" :rhythm/pitch) ("pitch/rhythm" :pitch/rhythm)) )
                         (10 (("next-pitch" :next-pitch) ("next-rhythm" :next-rhythm) ("current-pitch" :current-pitch) ("current-rhythm" :current-rhythm)) )
                         (11 (("next" :next) ("self" :self)) ) )
 	:doc "By evaluating this function you may change some default settings of the 
system. The function should not be connected to other functions. Note that you 
need to evaluate this function every time you restart OM or after you 
change a setting to change the preferences.

<backjump?>     Backjumping speeds up backtracking by jumping 
directly to the variable that caused a failed ruletest instead of 
step-by-step backtracking. The speed difference vary from no 
difference to a huge difference. The way backjumping is used in this 
system, it should not cause the system to miss possible solutions. It 
is strongly recommended to keep backjumping on.

<max-nr-of-loops> is the maximum search loops the engine will do before stopping. 
---

The following variables sets what engine a failed rule prefers to
backtrack. If the prefered engine cannot be backtracked, the system
will make another choice based on lower priorities. Note that if
more than one rule fails, the choice will be based on the most
frequently proposed engine to backtrack. 

The default settings can be found by opening a new preference function.

<bktr-rp1v>  r-rhyth-pitch-one-vocie
 - self (default): backtrack the engine (rhythm or pitch) where the failed variable was found.
 - other: backtrack the engine (rhythm or pitch) that is associated with the enginewhere the failed variable was found.

<bktr-rr2v> r-rhythm-rhythm
 - self: backtrack the engine (voice 1 or 2) where the failed varialbe was found.
 - other (default): backtrack the engine (voice 1 or 2) that is associated with the engine where the failed variable was found.

<bktr-rh2v> r-rhythm-hierarchy
 - self: backtrack the engine (voice 1 or 2) where the failed variable was found.
 - other (default): backtrack the engine (voice 1 or 2) that is associated with the engine where the failed variable was found.

<bktr-rmh2v> r-metric-hierarchy
 - rhythm (default): backtrack the rhythm engine
 - meter: backtrack the metric engine.

<bktr-dm1v> r-note-meter if pitch information is NOT asked for
 - rhythm (default): backtrack the rhythm engine
 - meter: backtrack the metric engine.

<bktr-nm1v> r-note-meter if pitch information is asked for
 - self (default): backtrack the engine (rhythm, pitch or meter) where the failed variable was found.
 - rhythm/pitch: backtrack the engine (rhythm or pitch) that is associated with the engine where the failed variable was found. The metric engine always prefer to backtrack the rhythm engine.
 - pitch/rhythm: backtrack the engine (rhythm or pitch) that is associated with the engine where the failed variable was found. The metric engine always prefer to backtrack the pitch engine.

<bktr-md1v> r-meter-note if pitch information is NOT asked for
 - rhythm (default): backtrack the rhythm engine
 - meter: backtrack the metric engine.

<bktr-mn1v> r-meter-note if pitch information is asked for
 - self (default): backtrack the engine (rhythm, pitch or meter) where the failed variable was found.
 - rhythm/pitch: backtrack the engine (rhythm or pitch) that is associated with the engine where the failed variable was found. The metric engine always prefer to backtrack the rhythm engine.
 - pitch/rhythm: backtrack the engine (rhythm or pitch) that is associated with the engine where the failed variable was found. The metric engine always prefer to backtrack the pitch engine.

<bktr-ppnv> r-pitch-pitch
 - next-pitch (default): backtrack the pitch engine after the engine where the failed variable was found (the order is defined by the voice input).
 - next-rhythm: backtrack the pitch engine after the engine where the failed variable was found (the order is defined by the voice input).
 - current-pitch: if the failed variable is in a pitch engine, backtrack the same engine. If it is in a rhythm engine, backtrack the associated pitch engine.
 - current-rhythm: if the failed variable is in a rhythm engine, backtrack the same engine. If it is in a pitch engine, backtrack the associated rhythm engine.

<bktr-leNv> r-list-all-events
 - next:  backtrack the engine after the engine where the failed variable was found (the order is defined by the voice input).
 - self:  backtrack the engine where the failed variable was found.
"
 (preferences backjump? max-nr-of-loops bktr-rp1v bktr-rr2v bktr-rh2v bktr-rmh2v bktr-dm1v bktr-nm1v bktr-md1v bktr-mn1v bktr-ppnv bktr-leNv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RULES ONE ENGINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! Rrhythms-one-voice ((rule t) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 :durations :true/false 1)
:indoc '( "rule" "voice-number" "input-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (2 (("durations" :durations) ("dur/time" :dur/time) ("motifs" :motifs) ("motif/time" :motif/time) ("all-durations" :all-durations)) )
                  (3 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "<rule> is a logic statement in the form of a function. The output of the 
function has to be either true or false. If there are more than one 
input to the function, they will receive consecutive durations (or 
consecutive motifs depending on the input-mode). 

<voice> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - durations: The rule will receive individual durations, one for each input.
 - dur/time: As above, but the start-time of the duration will be indicated.
             Format: '(duration start-time), Ex. '(1/4 9/4)
 - motifs: The rule will receive motifs, one (consecutive) motif for each 
           input. A motif is a collection of durations that are grouped in 
           a list. Motifs are designed in the domain and cannot be 
           redesignedby the engine. Note that a motif may be a single 
           duration (a list with one duration-ratio) if it is defined as 
           such in the domain.
 - motif/time: As the previous selection, but with the start-time of the 
               first event in the motif added. Format: '(motif start-time)
               Ex. '((1/4 -1/8) 9/8)
 - all-durations: All durations in the voice that are assigned at the time 
                the rule is checked are given as a list of duration ratios.  
                The list will thus become longer and longer during the 
                search. The rule can only have ONE input in this mode.
[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
(R-rhythms-one-voice (fix-ompatch-rule rule) voices input-mode rule-type weight))

(om::defmethod! Rindex-rhythms-one-voice ((rule t) (positions list) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1))
 :initvals '(nil (0) 0 :index-for-cell :true/false 1)
 :indoc '( "rule" "list-of-positions" "voice-number" "input-mode" "rule-type" "weight-number") 
 :icon 01
 :menuins '( (3 (("index-for-cell" :index-for-cell) ("position-for-duration" :position-for-duration))) 
                   (4 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
 :doc "<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. The function should have as many 
inputs as there are positions given in the <positions> list.

<positions> is a list of positions where the logic statement is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See also input-mode below.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - index-for-cell: The rule will receive motifs as lists of duration 
                   ratios. The positions given above refer to the index
                   number for the motifs in the solution. Each input 
                   will receive one corresponding motif.
 - position-for-duration: The rule will receive individual
                          durations, one for each given position.
                          The positions refer to the position
                          of the individual duraions in the solution.
                          Rests are included.

[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-index-rhythms-one-voice (fix-ompatch-rule rule) positions voices input-mode rule-type weight))

(om::defmethod! Rpitches-one-voice ((rule t) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 :pitches :true/false 1)
:indoc '( "rule" "voice-number" "input-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (2 (("pitches" :pitches) ("pitch/nth" :pitch/nth) ("motifs" :motifs) ("motif/nth" :motif/nth) ("motif/index" :motif/index) ("all-pitches" :all-pitches)) )
                  (3 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. If there are more than one 
input to the function, they will receive consecutive pitches (or 
consecutive motifs depending on the input-mode). 

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - pitches: The rule will receive individual  pitches, one for each input.
 - pitch/nth: As above, but the position of the pitch is also indicated.
              Format: '(pitch nth) Ex. '(60 3).
 - motifs: The rule will receive (consecutive) motifs, one motif for each 
           input. A motif is a collection of pitches that are grouped in 
           a list. Motifs are designed in the domain and cannot be redesigned
           by the engine. Note that a motif may be a single pitch (a list 
           with one MIDI note number) if it is defined as such in the domain.
 - motif/nth: As above, but the position of the pitches is also indicated.
              Format: '((pitch-motif) (nth-list)) Ex. '((60 64) (3 4)).
 - motif/index: As motifs, but the index of the variable is also indicated.
                Format: '((pitch-motif) index) Ex. '((60 64) 2).
 - all-pitches: All pitches in the voice that are assigned at the time the  
                rule is checked are given as a list of MIDI note numbers.  
                The list will thus become longer and longer during the 
                search. The rule can only have ONE input in this mode.

[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-pitches-one-voice (fix-ompatch-rule rule) voices input-mode rule-type weight))

(om::defmethod! Rindex-pitches-one-voice ((rule t) (positions list) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1))
 :initvals '(nil (0) 0 :index-for-cell :true/false 1)
 :indoc '( "rule" "list-of-positions" "voice-number" "input-mode" "rule-type" "weight-number") 
 :icon 01
 :menuins '( (3 (("index-for-cell" :index-for-cell) ("position-for-pitches" :position-for-pitches))) 
                   (4 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
 :doc "<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. The function should have as many 
inputs as there are positions given in the <positions> list.

<positions> is a list of positions where the logic statement is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See also input-mode below.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - index-for-cell: The rule will receive motifs as lists of MIDI note 
                   numbers The positions given above refer to the index
                   number for the motifs in the solution. Each input 
                   will receive one corresponding motif.
 - position-for-pitches: The rule will receive individual
                          pitches, one for each given position.
                          The positions refer to the position
                          of the individual duraions in the solution.

[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-index-pitches-one-voice (fix-ompatch-rule rule) positions voices input-mode rule-type weight))
 
(om::defmethod! Rtime-signatures ((rule t) (input-mode t) &optional (rule-type :true/false) (weigth 1))
:initvals '(nil :timesigns :true/false 1)
:indoc '( "rule" "input-mode" "rule-type" "weight-number" ) 
:icon 01
:menuins '( (1 (("timesigns" :timesigns) ("all-timesigns" :all-timesigns) ) )
                  (3 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. If there are more than one 
input to the function, they will receive consecutive time signatures.

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - timesigns: The rule will receive individual time signatures, one for 
              each input. Ex. '(4 4)
 - all-timesigns: All time signatures in the score that are assigned at the
                time the rule is checked are given as a list of time   
                signatures. The list will thus become longer and longer 
                during the search. The rule can only have ONE input in 
                this mode. Ex. '((4 4) (6 8))
[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-time-signatures (fix-ompatch-rule rule) input-mode rule-type weight))

(om::defmethod! Rindex-time-signatures ((rule t) (indexes list) &optional (rule-type :true/false) (weight 1) )
:initvals '(nil (0) :true/false 1)
:indoc '( "rule" "list-of-positions" "rule-type" "weight-number") 
:icon 01
:menuins '( (2 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) )
:doc "<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. The function should have as many 
inputs as there are positions given in the <positions> list.

<indexes> is a list of positions where the logic statement is applied.
Indexes are counted from 0. Every position in this list corresponds to
an input in the rule. 

[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-index-time-signatures (fix-ompatch-rule rule) indexes rule-type weight))

(om::defmethod! Ronly-m-motifs ((voices t) &optional (rule-type :true/false) (weight 1))
:initvals '(0 :true/false 1)
:indoc '( "voice-number" "rule-type" "weight-number") 
:icon 01
:menuins  '( (1 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) )
:doc "This rule will force the system to only pick transposable pitch
motifs (motifs defined as melodic intervals and flagged with the 
letter m). The first variable in a voice is an exception: this 
variable always has to be a pitch defined as a MIDI note value.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

[Backtracking: This rule will trigger backtracking in its own engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-only-m-motifs voices rule-type weight))


(om::defmethod! Rrhythms-one-voice-at-timepoints  ((rule t) (voices t) (timepoints list) (input-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 (0) :motifs-start :true/false 1)
:indoc '( "rule" "voice-number" "list-of-timepoints" "input-mode") 
:icon 01
:menuins '( (3 (("motifs-start" :motifs-start) ("motifs-end" :motifs-end) ("dur-start" :dur-start)))
                  (4 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) )  
:doc "Rule for rhythms that exist at timepoints in one voice.

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. .If there are more than one 
input to the function, they will receive the information for consecutive
timepoints.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<timepoints> is a list of timepoints (starting from 0) counted from the 
beginning of the score where the rule will be checked. For example
the timepoint 15/4 will be 15 quarter notes from the beginning of the 
score. 

<input-mode>              
- motifs-start: The start time for motifs will be compared to the given 
                timepoint. An input receives the as 
                '(offset-to-timepoint (motif)). Offset is the duration 
                between the startingpoint of the motif and given timepoint. 
                Ex. (-1/4 (1/8 -1/8 1/16 1/16))
- motifs-end:   The end time for motifs will be compared to the given 
                timepoint. An input receives the as 
                '(offset-to-timepoint (motif)). Offset is the duration 
                between the endingpoint of the motif and given timepoint. 
                Ex. (1/8 (1/8 -1/8 1/16 1/16))
- dur-start:    The start time for durations and rests will be compared to 
                the given timepoint. An input receives the as 
                '(offset-to-timepoint duration). Offset is the duration 
                between the startingpoint of the event and given timepoint. 
                Ex. (-1/16 1/4)

This rule always prefer to backtrack the rhythm engine that it belongs to."
 (R-rhythms-one-voice-at-timepoints (fix-ompatch-rule rule) voices timepoints input-mode rule-type weight))

(om::defmethod! HRrhythms-one-voice ((rule t) (voices t) (rule-type t))
:initvals '(nil 0 :durations)
:indoc '( "rule" "voice-number" "rule-type") 
:icon 01
:menuins '( (2 (("durations" :durations) ("dur/time" :dur/time) ("motifs" :motifs) ("motif/time" :motif/time) ("all-durations" :all-durations) ))) 
:doc "Heuristic rule for durations in one voice.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. If there are 
more than one input to the function, it will receive consecutive 
pitches or consecutive motifs depending on the input-mode (see below). 

<voices> is the number for the voice (starting at 0) that the heuristic 
rule affects. It is possible to give a list of several voice numbers: 
The rule will then be applied to every voice in the list (independant 
of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - durations: The rule will receive individual  pitches, one for each input.
 - dur/time: As above, but the start time of the duration will be indicated.
             Format: '(duration start-time), Ex. '(1/4 9/4)
 - motifs: The rule will receive (consecutive) motifs, one motif for each 
           input. A motif is a collection of durations that are grouped in 
           a list. Motifs are designed in the domain and cannot be 
           redesignedby the engine. Note that a motif may be a single 
           duration (a list with one duration ratio) if it is defined as 
           such in the domain.
 - motif/time: As the previous selection, but with the start time of the 
               first event in the motif added. Format: '(motif start-time)
               Ex. '((1/4 -1/8) 9/8)
 - all-durations: All durations in the voice that are assigned at the time 
                the rule is checked are given as a list of duration ratios.  
                The list will thus become longer and longer during the 
                search. The rule can only have ONE input in this mode.
"
 (HR-rhythms-one-voice (fix-ompatch-rule rule) voices rule-type))

(om::defmethod! HRindex-rhythms-one-voice ((rule t) (positions list) (voices t) (rule-type t))
:initvals '(nil (0) 0 :index-for-cell)
:indoc '( "rule" "list-of-positions" "voice-number" "rule-type") 
:icon 01
:menuins '( (3 (("index-for-cell" :index-for-cell) ("position-for-duration" :position-for-duration))) ) 
:doc  "Heuristic index rule for durations in one voice.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. The function 
should have as many inputs as there are positions given in the 
<positions> list. 

<positions> is a list of positions where the heuristic rule is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See also input-mode below.

<voices> is the number for the voice (starting at 0) that the heuristic 
rule affects. It is possible to give a list of several voice numbers: 
The rule will then be applied to every voice in the list (independant 
of each other).


<input-mode> determines what type of variables the heuristic rule will
receive in its inputs:
 - index-for-cell: The rule will receive motifs as lists of duration 
                   ratios. The positions given above refer to the index
                   number for the motifs in the solution. Each input 
                   will receive one corresponding motif.
 - position-for-duration: The rule will receive individual
                          durations, one for each given position.
                          The positions refer to the position
                          of the individual duraions in the solution.
                          Rests are included.
"
 (HR-index-rhythms-one-voice (fix-ompatch-rule rule) positions voices rule-type))

(om::defmethod! HRpitches-one-voice ((rule t) (voices t) (input-mode t))
:initvals '(nil 0 :pitches)
:indoc '( "rule" "voice-number" "input-mode" ) 
:icon 01
:menuins '( (2 (("pitches" :pitches) ("pitch/nth" :pitch/nth) ("motifs" :motifs) ("motif/nth" :motif/nth) ("motif/index" :motif/index) ("all-pitches" :all-pitches)) ) )
:doc  "Heuristic rule for pitches in one voice.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. If there are more 
than one input to the function, it will receive consecutive pitches 
or consecutive motifs depending on the input-mode (see below). 

<voices> is the number for the voice (starting at 0) that the heuristic 
rule affects. It is possible to give a list of several voice numbers: 
The rule will then be applied to every voice in the list (independant 
of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - pitches The rule will receive individual  pitches, one for each input.
 - pitch/nth: As above, but the position of the pitch is also indicated.
              Format: '(pitch nth) Ex. '(60 3).
 - motifs: The rule will receive (consecutive) motifs, one motif for each 
           input. A motif is a collection of pitches that are grouped in 
           a list. Motifs are designed in the domain and cannot be redesigned
           by the engine. Note that a motif may be a single pitch (a list 
           with one MIDI note number) if it is defined as such in the domain.
 - motif/nth: As above, but the position of the pitches is also indicated.
              Format: '((pitch-motif) (nth-list)) Ex. '((60 64) (3 4)).
 - motif/index: As motifs, but the index of the variable is also indicated.
                Format: '((pitch-motif) index) Ex. '((60 64) 2).
 - all-pitches: All pitches in the voice that are assigned at the time the  
                rule is checked are given as a list of MIDI note numbers.  
                The list will thus become longer and longer during the 
                search. The rule can only have ONE input in this mode.
"
 (HR-pitches-one-voice (fix-ompatch-rule rule) voices input-mode))

(om::defmethod! HRindex-pitches-one-voice ((rule t) (positions list) (voices t) (input-mode t))
:initvals '(nil (0) 0 :index-for-cell)
:indoc '( "rule" "list-of-positions" "voice-number" "input-mode") 
:icon 01
:menuins '( (3 (("index-for-cell" :index-for-cell) ("position-for-pitches" :position-for-pitches)) ) ) 
:doc  "Heuristic index rule for pitches in one voice.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. The function 
should have as many inputs as there are positions given in the 
<positions> list. 

<positions> is a list of positions where the heuristic rule is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See also input-mode below.

<voices> is the number for the voice (starting at 0) that the heuristic 
rule affects. It is possible to give a list of several voice numbers: 
The rule will then be applied to every voice in the list (independant 
of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - index-for-cell: The rule will receive motifs as lists of MIDI note 
                   numbers The positions given above refer to the index
                   number for the motifs in the solution. Each input 
                   will receive one corresponding motif.
 - position-for-pitches: The rule will receive individual
                          pitches, one for each given position.
                          The positions refer to the position
                          of the individual duraions in the solution.
"
 (HR-index-pitches-one-voice (fix-ompatch-rule rule) positions voices input-mode))


(om::defmethod! HRtime-signatures ((rule t) (input-mode t))
:initvals '(nil :timesigns)
:indoc '( "rule" "input-mode") 
:icon 01
:menuins '( (1 (("timesigns" :timesigns) ("all-timesigns" :all-timesigns)) ) )
:doc   "Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. If there are 
more than one input to the function, they will receive consecutive 
time signatures.

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - timesigns: The rule will receive individual time signatures, one for 
              each input. Ex. '(4 4)
 - all-timesigns: All time signatures in the score that are assigned at the
                time the rule is checked are given as a list of time   
                signatures. The list will thus become longer and longer 
                during the search. The rule can only have ONE input in 
                this mode. Ex. '((4 4) (6 8))

"
 (HR-time-signatures (fix-ompatch-rule rule) input-mode))


(om::defmethod! HRindex-time-signatures ((rule t) (indexes list))
:initvals '(nil (0))
:indoc '( "rule" "list-of-positions") 
:icon 01
:doc  "Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. The function 
should have as many inputs as there are positions given in the 
<indexes> list.

<indexes> is a list of positions where the logic statement is applied.
Indexes are counted from 0. Every position in this list corresponds to
an input in the rule. See also input-mode below.

"
 (HR-index-time-signatures (fix-ompatch-rule rule) indexes))

(om::defmethod! Rpmc-one-voice ((pmcrules0 t) (ruletype0 t) (voice0 integer) 
                                       &optional (pmcrules1 nil) (ruletype1 nil) (voice1 0)
                                                       (pmcrules2 nil) (ruletype2 nil) (voice2 0)
                                                       (pmcrules3 nil) (ruletype3 nil) (voice3 0)
                                                       (pmcrules4 nil) (ruletype4 nil) (voice4 0)
                                                       (pmcrules5 nil) (ruletype5 nil) (voice5 0)
                                                       (pmcrules6 nil) (ruletype6 nil) (voice6 0)
                                                       (pmcrules7 nil) (ruletype7 nil) (voice7 0)
                                                       (pmcrules8 nil) (ruletype8 nil) (voice8 0)
                                                       (pmcrules9 nil) (ruletype9 nil) (voice9 0))
:initvals '(nil :pitches 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0)
:indoc '( "pmc-rules" "rule-type" "voice" "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice" 
             "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice"  "pmc-rules" "rule-type" "voice" ) 
:icon 01
:menuins '( (1 (("pitches" :pitches) ("durations" :durations)) )
                  (4 (("pitches" :pitches) ("durations" :durations)) )
                  (7 (("pitches" :pitches) ("durations" :durations)) )
                  (10 (("pitches" :pitches) ("durations" :durations)) )
                  (13 (("pitches" :pitches) ("durations" :durations)) )
                  (16 (("pitches" :pitches) ("durations" :durations)) )
                  (19 (("pitches" :pitches) ("durations" :durations)) )
                  (22 (("pitches" :pitches) ("durations" :durations)) )
                  (25 (("pitches" :pitches) ("durations" :durations)) )
                  (28 (("pitches" :pitches) ("durations" :durations)) ) ) 
:doc "This function makes it possible to use PMC formated rules with the Poly-engine. The function expects a list of rules (for
example from a lisp lambda function - see tutorial 010d). The following PMC related variables and functions are supported:
L, RL, LEN, (cur-index)

Both wildcard rules and index rules are possible to use. Wildcard rules are expected to use variables 
in order."
 (R-pmc-one-voice pmcrules0 ruletype0 voice0 pmcrules1 ruletype1 voice1 pmcrules2 ruletype2 voice2 pmcrules3 ruletype3 voice3 pmcrules4 ruletype4 voice4 pmcrules5 ruletype5 voice5 pmcrules6 ruletype6 voice6 pmcrules7 ruletype7 voice7 pmcrules8 ruletype8 voice8 pmcrules9 ruletype9 voice9)) 

(om::defmethod! Rjbs-one-voice ((jbsrule0 t) (ruletype0 t) (voice0 integer) 
                                       &optional (jbsrule1 nil) (ruletype1 nil) (voice1 0)
                                                       (jbsrule2 nil) (ruletype2 nil) (voice2 0)
                                                       (jbsrule3 nil) (ruletype3 nil) (voice3 0)
                                                       (jbsrule4 nil) (ruletype4 nil) (voice4 0)
                                                       (jbsrule5 nil) (ruletype5 nil) (voice5 0)
                                                       (jbsrule6 nil) (ruletype6 nil) (voice6 0)
                                                       (jbsrule7 nil) (ruletype7 nil) (voice7 0)
                                                       (jbsrule8 nil) (ruletype8 nil) (voice8 0)
                                                       (jbsrule9 nil) (ruletype9 nil) (voice9 0))
:initvals '(nil :pitches 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0 nil nil 0)
:indoc '( "jbs-rule" "rule-type" "voice" "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice" 
             "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice"  "jbs-rule" "rule-type" "voice" ) 
:icon 01
:menuins '( (1 (("pitches" :pitches) ("durations" :durations)) )
                  (4 (("pitches" :pitches) ("durations" :durations)) )
                  (7 (("pitches" :pitches) ("durations" :durations)) )
                  (10 (("pitches" :pitches) ("durations" :durations)) )
                  (13 (("pitches" :pitches) ("durations" :durations)) )
                  (16 (("pitches" :pitches) ("durations" :durations)) )
                  (19 (("pitches" :pitches) ("durations" :durations)) )
                  (22 (("pitches" :pitches) ("durations" :durations)) )
                  (25 (("pitches" :pitches) ("durations" :durations)) )
                  (28 (("pitches" :pitches) ("durations" :durations)) ) ) 
:doc "This function makes it possible to use rules from the JBS-constraint library. 
The function can handle both true/false rules and heuristic rules.

Rules that use the (cur-slen) function are not supported (it is possible 
to use these rules by letting them pass the setend function).

The voice input can be a list with all voices that the rule should affect.

Score-PMC-rules are NOT supported. 
"
 (R-jbs-one-voice jbsrule0 ruletype0 voice0 jbsrule1 ruletype1 voice1 jbsrule2 ruletype2 voice2 jbsrule3 ruletype3 voice3 jbsrule4 ruletype4 voice4 jbsrule5 ruletype5 voice5 jbsrule6 ruletype6 voice6 jbsrule7 ruletype7 voice7 jbsrule8 ruletype8 voice8 jbsrule9 ruletype9 voice9))

(om::defmethod! setend ((rule t) (end-point integer))
:initvals '(nil 12 )
:indoc '( "rule" "end-point-number") 
:icon 01
:doc  "This function will set the endpoint for a jbs or a pmc rule. The rule should
pass through this function before going into the r-jbs-one-voice function (or the
r-pmc-one-voice function). The end point is the position (i.e. not the index) for 
the last value in a voice where the rule is checked. It also replaces 
the '(cur-slen) expression by the end point.

Note that the first value has the position 1 (this is compatible with
how index numbers are counted in the PMC engine).

"
 (set-end rule end-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RULES ONE VOICE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! Rrhythm-pitch-one-voice ((rule t) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1)) 
 :initvals '(nil 0 :rhythm/pitch :true/false 1) 
 :indoc '("rule" "voice-number" "input-mode" "rule-type" "weight-number") 
 :icon 01
 :menuins '( (2 (("rhythm/pitch" :rhythm/pitch ) ("include-rests" :include-rests ) ("rhythm/pitch-segment" :rhythm/pitch-segment) 
                         ("rhythm/time/pitch" :rhythm/time/pitch) ("rhythm/pitch-list-ALL" :rhythm/pitch-list-ALL ) ))
                    (3 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
 :doc "Rule for rhythm-pitch pairs in one voice. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with a duration-pitch pair, one list for each input (for example '(1/4 60)). 
Rests will have the pitch nil indicated (for example '(-1/8 nil)).If there 
is more than one input to the function, they will receive consecutive 
rhythm-pitch pairs.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - rhythm/pitch: The rule will receive rhythm-pitch pairs excluding (i.e. 
                 skipping) rests.
 - include-rests: The rule will receive rhythm-pitch pairs including rests.
 - rhythm/pitch-segment: For rules with one input, this setting is  
                  identical to the rhythm/pitch setting. For rules with
                  more than one input, the rule will receive consecutive
                  pitch/rhythm pairs between rests. The rule does not
                  check rhythm/pitch pairs that are divided by a rest. 
                  This setting is good for rules that are only valid 
                  within a phrase.
 - rhythm/time/pitch: The rule will receive rhythm-pitch pairs excluding (i.e. 
                 skipping) rests including the inormation about the
                 absolute onset time:
                 Format '(duration timepoint pitch)
 - rhythm/pitch-list-ALL: The rule receives a list with all duration-pitch
                  pair up to the point where the rule is checked. Rests are 
                  included in the list. The rule should only have one input.
				  
*** NOT SUPPORTED **********************************
<gracenotes?> gives the option to leave out grace notes when checking the 
rule:
 - normal: Grace notes are included and checked by the rule. Note: if 
           grace notes are not used in the domain, this setting will create 
           faster rules.
 - exclude-gracenotes: Grace notes are removed and not seen by the rule.
***************************************************************************

[Backtracking: By default this rule will trigger backtracking in its own 
               engine. If this is not possible, it will trigger backtracking 
               in the other engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-rhythm-pitch-one-voice (fix-ompatch-rule rule) voices input-mode :normal rule-type weight))

(om::defmethod! Rindex-rhythm-pitch-one-voice ((rule t) (positions list) (voices t) (input-mode t) &optional (rule-type :true/false) (weight 1)) 
 :initvals '(nil (0) 0 :nth-note :true/false 1)
 :indoc '("rule" "list-of-positions" "voice-number" "input-mode" "rule-type" "weight-number") 
 :icon 01
 :menuins '( (3 (("nth-note" :nth-note) ("nth-duration-incl-rests" :nth-duration-include-rests) ) ) 
                   (4 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
 :doc  "Index rule for rhythm-pitch pairs in one voice. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with a duration-pitch pair, one list for each input (for example '(1/4 60)). 
Rests will have the pitch nil indicated (for example '(-1/8 nil)). The 
function should have as many inputs as there are positions given in the 
<positions> list. 

<positions> is a list of positions where the heuristic rule is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See more under input-mode below.

<voices> is the number for the voice (starting at 0) that the rule affects. 
It is possible to give a list of several voice numbers: The rule will then 
be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - nth-note: Positions refer to the notes in the voice, excluding (i.e. not
             counting) rests. Notes are counted from 0.
 - nth-duration-incl-rests: Positions refer to the durations in the voice,
                            including rests. Durations are counted from 0.

[Backtracking: By default this rule will trigger backtracking in its own 
               engine. If this is not possible, it will trigger backtracking 
               in the other engine.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-index-rhythm-pitch-one-voice (fix-ompatch-rule rule) positions voices input-mode rule-type weight))

(om::defmethod! Rmetric-hierarchy ((voices t) (rule-mode t) &optional (rule-type :true/false) (weight 1)) 
 :initvals '( 0 :durations :true/false 1)
 :indoc '("voice-number" "rule-mode" "rule-type" "weight-number") 
 :icon 01
 :menuins '( (1 (("durations" :durations) ("include-rests" :include-rests) ) ) 
                   (2 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
 :doc "Rule for metric hierarchy: the onsets of events will bve forced to line

<voices> is the number for the voice (starting at 0) that the rule 
affects. A list of numbers indicate that the rule is valid for every
voice in the list.

<rule-mode> 
 - durations: The rule only affects durations (including grace notes). 
              Endpoints of durations are not affected.
 - include-rests: The rule affects all events (including rests). Also
              endpoints of events will be affected.

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-metric-hierarchy voices rule-mode rule-type weight))

(om::defmethod! Rnote-meter ((rule t) (voices t) (format t) (metric-structure t) (rest-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 :offs :beats :incl-rests :true/false 1)
:indoc '( "rule" "voice-numbers" "format" "metric-structure" "rest-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (2 (("offs" :offs) ("d_offs" :d_offs) ("d_offs_m" :d_offs_m) ("d_offs_m_n" :d_offs_m_n)) ) 
                  (3 (("beats" :beats) ("1st-beat" :1st-beat)) ) 
                  (4 (("incl-rests" :incl-rests) ("durations" :durations)) )
                  (5 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "Rule for the metric position of notes and rests. The rule checks all 
events in one voice.

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive the 
information on regarding an events offset to its beat or the first beat of 
its measure. If the rule has more than one input it will receive information 
for consecutive events. The exact information and format of an input depends 
on settings (see below).

<voices> is the number for the voice (starting at 0) that the rule 
affects. A list of numbers indicate that the rule is valid for every
voice in the list.

<format> sets how the information for an input is formated. 
 - offs:       An input will receive the offset to the following beat 
               (i.e. the duration until the following beat). Offset 0 
               indicates that the event is synchronized with the beat.
               If the duration of an event is not necessary to know, this
               setting should be chosen.
 - d_offs:     An input will receive a list with the duration of the 
               event and the offset to the following beat (i.e. the duration 
               until the following beat). Offset 0 indicates that the event
               is synchronized with the beat. Example: '(1/4 -1/8)
 - d_offs_m:   An input will receive a list with the duration of the 
               event, the offset to the following beat (i.e. the duration 
               until the following beat) and the time signature. 
               The time signature is given for the measure where the events
               onset exist (it may be sustained into another measure). 
               ONLY ISE THIS SETTING IF YOU NEED TO KNOW THE TIME SIGNATURE.
               Example: '(1/4 -1/8 (3 4))
 - d_offs_m_n: An input will receive a list with the duration of the 
               event, the offset to the following beat (i.e. the duration 
               until the following beat), the time signature and the pitch. 
               The time signature is given for the measure where the events
               onset exist (it may be sustained into another measure). 
               ONLY ISE THIS SETTING IF YOU NEED TO KNOW THE PITCH.
               Example: '(1/4 -1/8 (3 4) 60)

<metric-structure> 
 - beats: The offsets will relate to the following beat.
 - 1st-beat:  The offset will relate to the following 1st beat in the 
              next measure.

<rest-mode>
 - incl.rests: The rule will be checked for durations and rests (rests are
              indicated as negative durations). If rests are not included
              in the domain, this setting should be chosen.
 - durations: The rule will not be checked for rests (if the rule has
              more than one input, rests will be skipped).

*** NOT SUPPORTED **********************************
<gracenotes?> 
 - normal:    The rule will include grace notes as separate events. If
              grace notes are not included in the domain, this setting 
              should be chosen.
 - exclude-gracenotes: The rule will not be checkes for grace notes (if 
              the rule has more than one input, grace notes will be 
              skipped).
*******************************************************************************
Efficiency: The most efficient setting is <offs>, <incl.rests>, since
it the rule can assume that an offset always is a new onset and it can 
check the rule before the next event is assigned. Least efficient is the
<d_offs_m> setting, since the rule cannot be checked until the time 
signature is known for a metric point. 

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-note-meter (fix-ompatch-rule rule) voices format metric-structure rest-mode :normal rule-type weight))

(om::defmethod! Rmeter-note ((rule t) (voices t) (metric-structure t) (input-mode1 t) (input-mode2 t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 :beats :offset :norm :true/false 1)
:indoc '( "rule" "voice-numbers" "metric-structure" "input-mode1" "input-mode2" "rule-type" "weight-number") 
:icon 01
:menuins '((2 (("beats" :beats) ("1st-beat" :1st-beat)) ) 
                 (3 (("offset" :offset) ("offset_dur" :offset_dur) ("offset_dur_meter" :offset_dur_meter) ("offset_dur_pitch_meter" :offset_dur_pitch_meter) 
                      ("offset_motif" :offset_motif ) ("offset_motif_meter" :offset_motif_meter) ) ) 
                  (4 (("norm" :norm) ("list" :list)) )
                  (5 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "Rule for notes at metric timepoints. The rule will check
all metric timepoints (see the setting for the metric structure). 
Grace notes are ignored by the rule.

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive the 
information on regarding the offset to the onset for the note that 
coinside with a metric time point (i.e. every beat or only the first
beat of every measure). If the rule has more than one input it will 
receive information for consecutive metric timepoints. The exact 
information and format of an input depends on settings (see below).

<voices> is the number for the voice (starting at 0) that the rule 
affects. A list of numbers indicate that the rule is valid for every
voice in the list.

<input-mode1> sets how the information for an input is formated. 
 - offset:     An input will receive the offset to the onset for the 
               event that coinside with the metric timepoint. Offset 0 
               indicates that the event is synchronized with the beat.
               If the duration of a note is not necessary to know, this
               setting should be chosen. Example: '(-1/8)
 - offset_dur: An input will receive a list with the duration of the 
               event and its offset to the beat that is checked. 
               Offset 0 indicates that the event is synchronized with 
               the beat. If the pitch of a note is not necessary to 
               know, this setting should be chosen. Example: '(-1/8 1/4)
 - offset_dur_pitch: An input will receive a list with the duration and
               pitch of the note and its offset to the beat that is 
               checked. Offset 0 indicates that the note is synchronized 
               with the beat. Example: '(-1/8 1/4 60)
 - offset_dur_pitch_meter: As the previous option, but also accesses the
               time signature. Example: '(-1/8 1/4 60 (3 4))
 - offset_motif: An input will receive the offset to the onset for the 
               rhytm motif that coinside with the metric timepoint. Offset 
               0 indicates that the motif is synchronized with the beat.
               The motif will also be accessed. If the meter is not
               necessary to know, use this mode (and noot the following).
               Example: '(-1/8 (1/8 1/16 1/16))
 - offset_motif_meter: As the previous input mode, but also the time 
               signature will be accessed.
               Example: '(-1/8 (1/8 1/16 1/16) (3 4))

<input-mode2>
 - norm: This setting is the normal behaviour of the function. An input receives
              the information for one time point. More than one input  
              will receive information for consecutive timepoints.
 - list: The rule must have exactly one input. The input will receive a 
              list of all time points that are known when the rule is
              checked. 

<metric-structure> 
 - beats: The rule will be applied at every beat.
 - 1st-beat:  The rule will be applied at the first beat of every measure.

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-meter-note (fix-ompatch-rule rule) voices metric-structure input-mode1 input-mode2 rule-type weight))

(om::defmethod! Rmel-interval-one-voice ((voices t) (segments? t) (match-dur t) (durations t) (match-pitch t) (intervals t) &optional (rule-type :true/false) (weight 1))
:initvals '( 0 :normal := 1/4 := 500 :true/false 1)
:indoc '( "voice-numbers" "segments?" "match-dur" "durations" "match-pitch" "intervals" "rule-type" "weight") 
:icon 01
:menuins '((1 (("normal" :normal) ("break-at-rests" :break-at-rests)) ) 
                 (2 (("=" :=) ("/=" :/=) ("member" :member) ("longer-than" :longer-than) ("shorter-than" :shorter-than )) ) 
                  (4 (("=" :=) ("/=" :/=) ("member" :member) ("smaller-than" :smaller-than) ("larger-than" :larger-than )) ) 
                  (6 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "Restricts what melodic intervals are allowed from an event with a specific duration.
If the duration the melodic interval start at is
   =:            identical to a value
   member:       member of a list of values
   longer-than:  longer than a value
   shorter-than: shorter than a value

then the melodic interval has to be
=:            equal to an interval
member:       member of a list of intervals
smaller-than: smaller than an interval
larger-than:  larger than an interval

[The prefered back route will be the same as for R-rhythm-pitch-one-voice.]
"
 (R-mel-interval-one-voice voices :normal segments? match-dur durations match-pitch intervals rule-type weight))

(om::defmethod! HRrhythm-pitch-one-voice ((rule t) (voices t) (input-mode t)) 
 :initvals '(nil 0 :rhythm/pitch) 
 :indoc '("rule" "voice-number" "input-mode") 
 :icon 01
 :menuins '( (2 (("rhythm/pitch" :rhythm/pitch ) ("include-rests" :include-rests ) ("rhythm/pitch-segment" :rhythm/pitch-segment) 
                         ("rhythm/time/pitch" :rhythm/time/pitch) ("rhythm/pitch-list-ALL" :rhythm/pitch-list-ALL ) )) ) 
 :doc "Heuristic rule for rhythm-pitch pairs in one voice. 

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. Each input will 
receive a list with a duration-pitch pair, one list for each input (for 
example '(1/4 60)). Rests will have the pitch nil indicated (for example 
'(-1/8 nil)).If there is more than one input to the function, they will 
receive consecutive rhythm-pitch pairs.

<voices> is the number for the voice (starting at 0) that the heuristic rule 
affects. It is possible to give a list of several voice numbers: The rule 
will then be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - rhythm/pitch: The heuristic rule will receive rhythm-pitch pairs  
                 excluding (i.e. skipping) rests.
 - include-rests: The heuristic rule will receive rhythm-pitch pairs 
                  including rests.
 - rhythm/pitch-segment: For heuristic rules with one input, this setting  
                          is identical to the rhythm/pitch setting. For 
                          heuristic rules with more than one input, the rule  
                          will receive consecutive pitch/rhythm pairs between 
                          rests. The heuristic rule does not check 
                          rhythm/pitch pairs that are divided by a rest. This
                          setting is good for heuristic rules that are only 
                          valid within a phrase.
 - rhythm/time/pitch: The heuristic  rule will receive rhythm-pitch pairs  
                      excluding (i.e.skipping) rests including the inormation 
                      about the absolute onset time:
                      Format '(duration timepoint pitch)
 - rhythm/pitch-list-ALL: The rule receives a list with all duration-pitch
                  pair up to the point where the rule is checked. Rests are 
                  included in the list. The rule should only have one input.

*******NOT SUPPORTED ***********************************************************
<gracenotes?> gives the option to leave out grace notes when checking the 
heuristic rule:
 - normal: Grace notes are included and checked by the heuristic rule. Note: 
           if grace notes are not used in the domain, this setting will  
           create faster heuristic rules.
 - exclude-gracenotes: Grace notes are removed and not seen by the heuristic
                       rule.
*****************************************************************************************
"
 (HR-rhythm-pitch-one-voice (fix-ompatch-rule rule) voices input-mode :normal))

(om::defmethod! HRindex-rhythm-pitch-one-voice ((rule t) (positions list) (voices t) (input-mode t)) 
 :initvals '(nil (0) 0 :nth-note) 
 :indoc '("rule" "list-of-positions" "voice-number" "input-mode") 
 :icon 01
 :menuins '( (2 (("nth-note" :nth-note) ("nth-duration-incl-rests" :nth-duration-incl-rests ) )) ) 
 :doc "Heuristic index rule for rhythm-pitch pairs in one voice. 

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight. Each input will 
receive a list with a duration-pitch pair, one list for each input 
(for example '(1/4 60)). Rests will have the pitch nil indicated 
(for example '(-1/8 nil)). The function should have as many inputs 
as there are positions given in the <positions> list. 

<positions> is a list of positions where the heuristic rule is applied.
Positions are counted from 0. Every position in this list corresponds to
an input in the rule. See more under input-mode below.

<voices> is the number for the voice (starting at 0) that the heuristic rule 
affects. It is possible to give a list of several voice numbers: The rule 
will then be applied to every voice in the list (independant of each other).

<input-mode> determines what type of variables the rule will receive in 
its inputs:
 - rhythm/pitch: The rule will receive rhythm-pitch pairs excluding (i.e. 
                 skipping) rests.
 - include-rests: The rule will receive rhythm-pitch pairs including rests.
"
 (HR-index-rhythm-pitch-one-voice (fix-ompatch-rule rule) positions voices input-mode))

(om::defmethod! HRduration-meter ((rule t) (voices t) (format t) (metric-structure t) (rest-mode t))
:initvals '(nil 0 :dur_offset :beats :incl-rests)
:indoc '( "rule" "voice-number" "format" "metric-structure" "rest-mode") 
:icon 01
:menuins '( (2 (("offs" :offs) ("d_offs" :d_offs) ("d_offs_m" :d_offs_m) ("d_offs_m_n" :d_offs_m_n)) ) 
                  (3 (("beats" :beats) ("1st-beat" :1st-beat)) ) 
                  (4 (("incl-rests" :incl-rests) ("durations" :durations)) ) ) 
:doc  "Heuristic rule for the metric position of durations and rests. The rule 
checks all events in one voice.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight.  Each input will  
receive the information on regarding an events offset to its beat or the 
first beat of its measure. If the rule has more than one input it will  
receive information for consecutive events. The exact information and 
format of an input depends on settings (see below).

<voices> is the number for the voice (starting at 0) that the rule 
affects. A list of numbers indicate that the rule is valid for every
voice in the list.

<format> sets how the information for an input is formated. 
 - dur_offset: An input will receive a list with the duration of the 
               event and the offset to the following beat (i.e. the duration 
               until the following beat). Offset 0 indicates that the event
               is synchronized with the beat. Example: '(1/4 -1/8)
 - offset:     An input will receive the offset to the following beat 
               (i.e. the duration until the following beat). Offset 0 
               indicates that the event is synchronized with the beat.
               If the duration of an event is not necessary to know, this
               setting should be chosen.

<metric-structure> 
 - beats: The offsets will relate to the following beat.
 - 1st-beat:  The offset will relate to the following 1st beat in the 
              next measure.

<rest-mode>
 - incl.rests: The rule will be checked for durations and rests (rests are
              indicated as negative durations). If rests are not included
              in the domain, this setting should be chosen.
 - durations: The rule will not be checked for rests (if the rule has
              more than one input, rests will be skipped).

*******NOT SUPPORTED ***********************************************************
<gracenotes?>
 - normal:    The rule will include grace notes as separate events. If
              grace notes are not included in the domain, this setting 
              should be chosen.
 - exclude-gracenotes: The rule will not be checkes for grace notes (if 
              the rule has more than one input, grace notes will be 
              skipped).
 ******************************************************************************
"
 (HR-duration-meter (fix-ompatch-rule rule) voices metric-structure rest-mode :normal))

(om::defmethod! HRmeter-duration ((rule t) (voices t) (format t) (metric-structure t))
:initvals '(nil 0 :offset :beats)
:indoc '( "rule" "voice-number" "format" "metric-structure") 
:icon 01
:menuins '( (2 (("offset" :offset) ("offset_dur" :offset_dur) ("list-all-offsets" :list-all-offsets) ("list-all-offs_dur" :list-all-offs_dur)) ) 
                  (3 (("beats" :beats) ("1st-beat" :1st-beat)) ) ) 
:doc "Heuristic rule for durations and rests at metric timepoints. The heuristic 
rule will affect all metric timepoints (see the setting for the metric 
structure). Grace notes are ignored by the rule.

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

<rule> is a function that outputs a numerical weight.  Each input will  
receive the information on regarding the offset to the onset for the 
event that coinside with a metric time point (i.e. every beat or only 
the first beat of every measure). If the rule has more than one input 
it will receive information for consecutive metric timepoints The exact 
information and format of an input depends on settings (see below).

<voices> is the number for the voice (starting at 0) that the rule 
affects. A list of numbers indicate that the rule is valid for every
voice in the list.

<format> sets how the information for an input is formated. 
 - offset:     An input will receive the offset to the onset for the 
               event that coinside with the metric timepoint. Offset 0 
               indicates that the event is synchronized with the beat.
               If the duration of an event is not necessary to know, this
               setting should be chosen.
 - offset_dur: An input will receive a list with the duration of the 
               event and its offset to the beat that is checked beat. 
               Offset 0 indicates that the event is synchronized with 
               the beat. Example: '(-1/8 1/4)
 - list-all-offsets: The rule should only have one input. It will receive
               a list of all offsets (as described above) upto the
               point where the rule is checked.
 - list-all-offs_dur: The rule should only have one input. It will receive
               a list of all offset-duration pairs (as described above)
               upto the point where trhe rule is checked.

<metric-structure> 
 - beats: The rule will be applied at every beat.
 - 1st-beat:  The rule will be applied at the first beat of every measure.
"
 (HR-meter-duration (fix-ompatch-rule rule) voices format metric-structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RULES TWO VOICES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;RHYTHM RHYTHM

(om::defmethod! Rrhythm-rhythm ((rule t) (voice1 integer) (voice2 integer) (input-mode1 t) (input-mode2 t) (input-filter t) &optional (rule-type :true/false) (weight 1))
:initvals '(nil 0 1 :dl_offs :norm :at-durations-vl :true/false 1)
:indoc '( "rule" "voice1" "voice2" "input-mode1" "input-mode2" "input-filter" "rule-type" "weight-number") 
:icon 01
:menuins '( (3 (("d1_offs" :d1_offs) ("d1_offs_d2" :d1_offs_d2)) ) 
                  (4 (("norm" :norm) ("list" :list)) )
                  (5 (("at-durations-v1" :at-durations-v1) ("at-events-v1" :at-events-v1) ("break-at-rest-v1" :break-at-rest-v1) ("break-at-rest-v1-v2" :break-at-rest-v1-v2) ) )
                  (6 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "Rule for the relation between durations in two voices. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with information regarding a timepoint in the score. If the rule has
more than one input it will receive information for consecutive timepoints. 
If the input-mode2 is set to list, the rule can only have one input.
The exact information and format of the list depends on the selected input 
mode (see below).

<voice1> and <voice2> are the numbers for the voices (starting at 0) that 
the rule affects. 

<input-mode1> determines what format for the variables:
 - d1_offset: The rule will receive a list with a duration in voice 1 and 
              the offset to the event in voice 2 that exist at the onset 
              for the duration in voice 1. Rests and grace notes in voice 1 
              are ignored. Grace notes are ignored in voice 2, but rests are
              included. Format: '(duration offset), example: '(1/2 -1/8).
 - d1_offset_d2: Identical to d1_offset but also returns the duration (or 
              rest) in voice 2. Format: '(duration1 offset duration2), 
              example: '(1/2 -1/8 1/4).

<input-mode2>
 - norm: This setting is the normal behaviour of the function. An input receives
              the information for one time point. More than one input it 
              will receive information for consecutive timepoints.
 - list: The rule must have exactly one input. The input will receive a 
              list of all time points that are known when the rule is
              checked. If any of the break-at-rest settings are used, the
              rule will check each segment at a time.

<input-filter> determines what information the rule will receive:
 - at-durations-v1: The rule will receive informaton for all durations  
              (grace notes and rests excluded) in voice 1.
 - at-events-v1: The rule will receive informaton for all events  
              (grace notes excluded) in voice 1. Rests are included.
 - break-at-rest-v1: This will only differ from the above setting for
              rules with more than one input. The rule will not not
              check durations that are separated by a rest in voice 1.
              If the list setting is chosen above, the list will be
              segmented at rests in voice 1.
 - break-at-rest-v1-v2: The rule will not check points where voice 1 or
              voice 2 (or both) have a rest. If the rule has more than 
              one input, it will not check timepoints that are separated 
              by a rest in voice 1 or timepoints that have rests in voice 2.
              If the list setting is chosen above, the list will be
              segmented at rests in voice 1 or 2.


The latter 2 settings are useful for rules that are only valid within 
a phrase.


[Backtracking: By default this rule will trigger backtracking in the other
               engine than the engine that failed. If this is not possible, 
               it will trigger backtracking in its own engine.]

A word on efficiency:
The most efficient input mode is the d1_offset (if the input-filter is set
to at-durations-v1): the system is able to check this type of rule earlier than rules 
with other input modes. This is due to that it can consider the last endpoint
as a new onset, without knowing the duration for this new event.

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate)."
 (R-rhythm-rhythm (fix-ompatch-rule rule) voice1 voice2 input-mode1 input-mode2 input-filter rule-type weight))

(om::defmethod! HRrhythm-rhythm ((rule t) (voice1 integer) (voice2 integer) (input-mode1 t) (input-mode2 t) (input-filter t))
:initvals '(nil 0 1 :dl_offs :norm :at-durations-vl)
:indoc '( "rule" "voice1" "voice2" "input-mode1" "input-mode2" "input-filter") 
:icon 01
:menuins '( (3 (("d1_offs" :d1_offs) ("d1_offs_d2" :d1_offs_d2)) ) 
                  (4 (("norm" :norm) ("list" :list)) )
                  (5 (("at-durations-v1" :at-durations-v1) ("at-events-v1" :at-events-v1) ("break-at-rest-v1" :break-at-rest-v1) ("break-at-rest-v1-v2" :break-at-rest-v1-v2) ) ) ) 
:doc "Heuristic rule for the relation between durations in two voices. 

Heuristic rules sort the candidates locally according to weights. 
The true/false rules will test candidates that receive high weights by
the heuristic rules before other candidates (and in this way give them
higher priority). A heuristic rule can never fail a candidate, nor 
can it trigger backtracking of an engine. 

NOTE that this heuristic rule will have most effect on durations: offsets
will be very little affected (if any at all).

<rule> is a function that outputs a numerical weight. Each input will 
receive a list with information regarding a timepoint in the score. If the 
rule has more than one input it will receive information for consecutive 
timepoints. If the input-mode2 is set to list, the rule can only have one 
input. The exact information and format of the list depends on the 
selected input mode (see below).

<voice1> and <voice2> are the numbers for the voices (starting at 0) that 
the rule affects. 

<input-mode1> determines what format for the variables:
 - d1_offset: The rule will receive a list with a duration in voice 1 and 
              the offset to the event in voice 2 that exist at the onset 
              for the duration in voice 1. Rests and grace notes in voice 1 
              are ignored. Grace notes are ignored in voice 2, but rests are
              included. Format: '(duration offset), example: '(1/2 -1/8).
 - d1_offset_d2: Identical to d1_offset but also returns the duration (or 
              rest) in voice 2. Format: '(duration1 offset duration2), 
              example: '(1/2 -1/8 1/4).

<input-mode2>
 - norm: This setting is the normal behaviour of the function. An input receives
              the information for one time point. More than one input it 
              will receive information for consecutive timepoints.
 - list: The rule must have exactly one input. The input will receive a 
              list of all time points that are known when the rule is
              checked. If any of the break-at-rest settings are used, the
              rule will check each segment at a time.

<input-filter> determines what information the rule will receive:
 - at-durations-v1: The rule will receive informaton for all durations  
              (grace notes and rests excluded) in voice 1.
 - break-at-rest-v1: This will only differ from the above setting for
              rules with more than one input. The rule will not not
              check durations that are separated by a rest in voice 1.
              If the list setting is chosen above, the list will be
              segmented at rests in voice 1.
 - break-at-rest-v1-v2: The rule will not check points where voice 1 or
              voice 2 (or both) have a rest. If the rule has more than 
              one input, it will not check timepoints that are separated 
              by a rest in voice 1 or timepoints that have rests in voice 2.
              If the list setting is chosen above, the list will be
              segmented at rests in voice 1 or 2.


The latter 2 settings are useful for rules that are only valid within 
a phrase.


[Backtracking: By default this rule will trigger backtracking in the other
               engine than the engine that failed. If this is not possible, 
               it will trigger backtracking in its own engine.]

A word on efficiency:
The most efficient input mode is the d1_offset (if the input-filter is set
to at-durations-v1): the system is able to check this type of rule earlier than 
rules with other input modes. This is due to that it can consider the last endpoint
as a new onset, without knowing the duration for this new event. This setting
will have more of an impact than other settings. Other input modes will only have
an effect for durations (not offsets).
"
 (HR-rhythm-rhythm (fix-ompatch-rule rule) voice1 voice2 input-mode1 inp0ut-mode2 input-filter))

(om::defmethod! Rcanon ((voices list ) (parameter t) (offset number) (interval integer))
:initvals '((0 1) :rhythm 1/2 700)
:indoc '( "list-of-voice-numbers" "parameter" "duration" "pitch-interval" ) 
:icon 01
:menuins '( (1 (("rhythm" :rhythm) ("rhythm&pitch" :rhythm&pitch) ("pitch" :pitch)) ) ) 
:doc  "Rule for canon between two voices.

<voices> is a list with the number of the two voice (starting at 0) for 
the canon. It is possible to give a list with sublists: The rule will then 
be applied to every voice-pair in the list (independant of each other).

<parameter> 
 - rhythm:       The canon is only for rhythm.
 - rhythm&pitch: The canon is both for rhythm and pitch
 - pitch:        The canon is only for rhythm.

<offset>   This is the duration before the 2nd voice starts (note value).
<interval> This is the pitch interval between the fux and the comes.

Note that the use of grace notes immediately preceeding rests (duration 0 before negative duration) or rhythm cells ending with a grace note is not very efficient in this rule (any result will however be correct).

[Backtracking: This voice backtracks as the r-list-all-events rule].
"
 (R-canon voices parameter offset interval))

(om::defmethod! Rrhythm-hierarchy ((voices list) (rule-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '( (01) :dur->dur :true/false 1)
:indoc '("voices" "rule-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (1 (("dur->dur" :dur->dur) ("include-rests" :include-rests) ("cells->durations" :cells->durations) )) 
                  (2 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "Rule for a hierarchic relation between onsets in two or more voices. 

<voices> is a list of voice numbers (counted from 0) where the order of 
the voices determines the hierarchical relationship (the first voice 
being the most fundamental). It is also possible to define several 
(independant) relationship by using sublists for each hierarchic 
structure.

<rule-mode>
 - dur->dur: The rule only affects durations. Rests will be ignored 
             in the higher voice in the hierarchy, and not be considered 
             valid onset points in the lower voice.
 - include-rests: The rule will also include the onset points for rests 
             in both voices.
 - cells->durations: As dur->dur, but the onsets in higher voice in the 
             hierarchy will be taken from the onsets for a rhythmic 
             motif (as it is defined in the domain).

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-rhythm-hierarchy voices rule-mode rule-type weight)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RULES TWO VOICES OR MORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! Rpitch-pitch ((rule t) (list-all-voices list) (timepoints list) (input-mode t) (format t) &optional (rule-type :true/false) (weight 1))
:initvals '( nil (0 1) (0) :all :pitch :true/false 1)
:indoc '("rule" "voices-list" "timepoints-list" "input-mode" "format" "rule-type" "weight-number") 
:icon 01
:menuins '( (3 (("all" :all) ("beat" :beat) ("1st-beat" :1st-beat) ("1st-voice" :1st-voice) ("at-timepoints" :at-timepoints) )) 
                  (4 (("pitch" :pitch) ("p_d_offs" :p_d_offs) ("p_d_offs+timepoint" :p_d_offs+timepoint))) 
                  (5 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "Rule that accesses simultaneous pitches in 2 or more voices. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with simultaneous pitches (optional with duration and offsets). If the 
rule has more than one input it will receive information for consecutive 
events (the positions of the events are determined by the input-mode 
setting).

<list-of-voices> is a list with the numbers of the voices (starting at 0) that 
the rule accesses. Any number of voices can be accessed by the rule. It is also 
possible to give a list of lists of voices: all sublists will generate a 
separate rule. All voices must have a pitch and rhythm domain. For efficiency 
reasons it is advised to rather split the rule into several simpler rules 
(that only access a few number of voices) if possible. This will make it 
possible to find failed candidates sooner, and to be more precise about what 
caused backtracking.

<timepoints>  This input is only considered if the input-mode is 
              at-timepoints. This is a list of absolute timepoints 
              (starting at 0) where the rue will be checked. 
              Ex. (0 5/4 9/4)

<input-mode> sets at what time points the rule will be checked. 
 - all:        The rule will be checked at every harmonic slice.
               Every time any voice has a new note event, the rule
               will be checked.
- beat:        The rule will be checked at metric beats.
- 1st-beat:    The rule will be checked at the first [down] beat
               in every measure.
- 1st-voice:   The rule will be checked at the onset of every new note 
               in the first voice in the list-of-voices.
- at-timepoints:  The rule will be checked at the timepoints in the 
               timepoints input.

*** NOT SUPPORTED ********************************************************
<gracenotes?>  
- exclude-gracenotes:    Pitches that relate to grace notes will be ignored.
- normal:  Grace notes are also checked by the rule:
               they are related to the regular notes in the other
               voices.
***********************************************************************************
               
<format> sets the format an input on the abstraction will receive. Note
that rests will be indicated as nil (without the specification of teh exact 
duration of the rest).
- pitch:       Each input receives a list with simultaneous pitches.
               Each pitch belongs to the corresponding voice indicated 
               in the <list-voices> input. Ex. '(67 60)
- p_d_offs:    Each input receives a list of lists. Each sublist represent 
               a corresponding voice indicated in the <list-voices> input.
               A sublist contains the information about an events pitch, 
               duration and offset between the events onset and the time
               point the rule was checked (this is determined by the
               <input-mode> setting). Offsets are either 0 or a negative 
               distance. Ex. '((67 1/4 0) (60 1/4 -1/8)).
- p_d_offs+timepoint: Each input receives a list of lists. Each sublist  
               represent a corresponding voice indicated in the <list-voices> 
               input. In addition, the time point for when the rule is 
               checked is added as a final value (note that this value 
               is not in a sublist). A sublist contains the information 
               about an events pitch, duration and offset between the 
               events onset and the time point the rule was checked (this 
               is determined by the <input-mode> setting). Offsets are 
               either 0 or a negative distance. 
               Ex. '((67 1/4 0) (60 1/4 -1/8) 5/4).

list-all-voices can include any number of voices. All voices must have a pitch 
and rhythm domain.

DO NOT use this rule to restrict rests in relation to other rests: use instead
the r-rhythm-rhythm.

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-pitch-pitch (fix-ompatch-rule rule) list-all-voices timepoints input-mode :exclude-gracenotes format rule-type weight))

(om::defmethod! Rchords ((list-voices list) (model list) (timepoints list) (input-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '( (0 1) ((400 700) (300 700)) (0) :all :true/false 1)
:indoc '("list-voices" "model-list" "timepoints-list" "input-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (3 (("all" :all) ("beat" :beat) ("1st-beat" :1st-beat) ("1st-voice" :1st-voice) ("at-timepoints" :at-timepoints) ))  
                  (4 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "This rule restrics possible chord formations between voices.

<list-of-voices> is a list with the numbers of the voices (starting at 0) that 
the rule accesses. Any number of voices can be accessed by the rule.
If more tan 2 voices are accessed, the rule will split up into several
rules to more efficient find a solution.

<model> is a chord or a list of chords. A chord is expressed as intervals from a 
bass note. The rule will only allow the chords (and all their inversions) from this 
list. Chords in the solution do not need to be complete (i.e. some notes may be missing).
Pitches may exist in any octave.

<timepoints>  This input is only considered if the input-mode is 
              at-timepoints. This is a list of absolute timepoints 
              (starting at 0) where the rue will be checked. 
              Ex. (0 5/4 9/4)

<input-mode> sets at what time points the rule will be checked. 
 - all:        The rule will be checked at every harmonic slice.
               Every time any voice has a new note event, the rule
               will be checked.
- beat:        The rule will be checked at metric beats.
- 1st-beat:    The rule will be checked at the first [down] beat
               in every measure.
- 1st-voice:   The rule will be checked at the onset of every new note 
               in the first voice in the list-of-voices.
- at-timepoints: The rule will be checked at the timepoints in the 
               timepoints input.

[Backtracking behaves the same as fro the R-pitch-pitch rule.]
"
 (R-chords list-voices model timepoints input-mode :exclude-gracenotes :true/false 1))

(om::defmethod! Rchords-bass-positions ((list-voices list) (model list) (bass-positions list) (timepoints list) (input-mode t) &optional (rule-type :true/false) (weight 1))
:initvals '( (0 1) ((400 700) (300 700)) (0 2) (0) :all :true/false 1)
:indoc '("list-voices" "model-list" "bass-positions-numbers" "timepoints-list" "input-mode" "rule-type" "weight-number") 
:icon 01
:menuins '( (4 (("all" :all) ("beat" :beat) ("1st-beat" :1st-beat) ("1st-voice" :1st-voice) ("at-timepoints" :at-timepoints) ))  
                  (5 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc  "This rule restrics possible chord formations between voices.

<list-of-voices> is a list with the numbers of the voices (starting at 0) that 
the rule accesses. Any number of voices can be accessed by the rule.
If more tan 2 voices are accessed, the rule will split up into several
rules to more efficient find a solution.

<model> is a chord or a list of chords. A chord is expressed as intervals from a 
bass note. The rule will only allow the chords (and all their inversions) from this 
list. Chords in the solution do not need to be complete (i.e. some notes may be missing).
Pitches may exist in any octave.

<bass-positions> is a list of allowed bass notes. The numbers corresponds to the same order of the intervals on the model chord. 
Example: model: (400 700) = major chord.
                bass-positions (0 2) = fundamental and fifth on the bass. 

<timepoints>  This input is only considered if the input-mode is 
              at-timepoints. This is a list of absolute timepoints 
              (starting at 0) where the rue will be checked. 
              Ex. (0 5/4 9/4)

<input-mode> sets at what time points the rule will be checked. 
 - all:        The rule will be checked at every harmonic slice.
               Every time any voice has a new note event, the rule
               will be checked.
- beat:        The rule will be checked at metric beats.
- 1st-beat:    The rule will be checked at the first [down] beat
               in every measure.
- 1st-voice:   The rule will be checked at the onset of every new note 
               in the first voice in the list-of-voices.
- at-timepoints: The rule will be checked at the timepoints in the 
               timepoints input.

[Backtracking behaves the same as fro the R-pitch-pitch rule.]
"
 (R-chords-bass-positions list-voices model bass-positions timepoints input-mode :exclude-gracenotes :true/false 1))

(om::defmethod! Rlist-all-events ((rule t) (voices list) (parameter t) &optional (rule-type :true/false) (weight 1))
:initvals '( nil (0 1) :pitches :true/false 1)
:indoc '("rule" "voices-list" "parameter" "rule-type" "weight-number") 
:icon 01
:menuins '( (2 (("pitches" :pitches) ("durations" :durations) ) )  
                  (3 (("true/false" :true/false) ("heur-switch" :heur-switch)) ) ) 
:doc "Rule that accesses lists with ALL pitches or durations in any number of voices. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with all events in the corresponding voice. The logic statement must have 
the same number of inputs as the number of voices it accesses.

<voices> is a list with the numbers for the voices (starting at 0) that 
the rule affects. Any number of voices can be accessed by the rule, 
however for efficiency reasons it is advised to rather split the rule
into several simpler rules (that only access a few number of voices) if possible.
If voices are given as a list of lists, each sublist will be considered
an individual rule.

<parameter> determines if the rule will access pitches or durations:

[Backtracking: By default this rule will trigger backtracking in the 
               engine following the one that failed. If this is not 
               possible, it will trigger backtracking in its own engine.
               The order of the engines is the same as in the <voices>
               input.]

Optional inputs:
By expanding the function it is possible to use the rule as a heuristic switch 
rule. A heuristic switch rule is still using a logic statement (that 
outputs true or false), but the effect of the rule is different: If the rule 
is true, the weight (given in the <weight> input) is passed to the engine. 
If it is false, a weight of 0 will be passed. A candidate that receive a 
high weight will have a higher priority for being picked when the true/false 
rules are checked. A heuristic rule can never fail a candidate, nor can it 
trigger backtracking of the engine. Heuristic rules only sort the 
candidates locally before the strict rules are applied. Depending on the 
context, heuristic rules might have more or less of an effect. 

Heuristic switch rules differs slightly form regular heuristic rules (the 
latter don't output true or false, but a weight that might vary depending
on the candidate).
"
 (R-list-all-events (fix-ompatch-rule rule) voices parameter rule-type weight))

(om::defmethod! HRlist-all-events ((rule t) (voices list) (parameter t))
:initvals '( nil (0 1) :pitches)
:indoc '("rule" "voices-list" "parameter") 
:icon 01
:menuins '( (2 (("pitches" :pitches) ("durations" :durations) ) )  ) 
:doc "Heuristic rule that accesses lists with ALL pitches or durations in any number of voices. 

<rule> is a logic statement in he form of a function. The output of the 
function has to be either true or false. Each input will receive a list 
with all events in the corresponding voice. The logic statement must have 
the same number of inputs as the number of voices it accesses.

<voices> is a list with the numbers for the voices (starting at 0) that 
the rule affects. Any number of voices can be accessed by the rule, 
however for efficiency reasons it is advised to rather split the rule
into several simpler rules (that only access a few number of voices) if possible.
If voices are given as a list of lists, each sublist will be considered
an individual heuristic rule.

<parameter> determines if the rule will access pitches or durations:
"
 (HR-list-all-events (fix-ompatch-rule rule) voices parameter))

(om::defmethod! HRpitch-pitch ((rule t) (list-voices list) (timepoints list) (input-mode t))
:initvals '( nil (0 1) (0) :all)
:indoc '("rule" "voices-list" "timepoints-list" "input-mode") 
:icon 01
:menuins '( (3 (("all" :all) ("beat" :beat) ("1st-beat" :1st-beat) ("1st-voice" :1st-voice) ("at-timepoints" :at-timepoints) ))  ) 
:doc     "Heuristic rule that accesses simultaneous pitches in 2 or more voices. 
<rule> is a function that outputs a numerical weight.  Each input will 
receive a list with simultaneous pitches. If the rule has more than one 
input it will receive information for consecutive events (the positions 
of the events are determined by the input-mode setting).

<list-of-voices> is a list with the numbers of the voices (starting at 0) that 
the rule accesses. Any number of voices can be accessed by the rule.
All voices must have a pitch and rhythm domain. For efficiency reasons it 
is advised to rather split the rule into several simpler rules (that only 
access a few number of voices) if possible. This will make it possible to 
find failed candidates sooner, and to be more precise about what caused 
backtracking.

<timepoints>  This input is only considered if the input-mode is 
              at-timepoints. This is a list of absolute timepoints 
              (starting at 0) where the rue will be checked. 
              Ex. (0 5/4 9/4)

<input-mode> sets at what time points the rule will be checked. 
- all:        The rule will be checked at every harmonic slice.
               Every time any voice has a new note event, the rule
               will be checked.
- beat:        The rule will be checked at metric beats.
- 1st-beat:    The rule will be checked at the first [down] beat
               in every measure.
- 1st-voice:   The rule will be checked at the onset of every new note 
               in the first voice in the list-of-voices.
- at-timepoints: The rule will be checked at the timepoints in the 
               timepoints input.

*** NOT SUPPORTED **********************************************************
<gracenotes?>  
- exclude-gracenotes: Pitches that relate to grace notes will be ignored.
- normal: Grace notes are also checked by the rule:
               they are related to the regular notes in the other
               voices.
*************************************************************************************            
list-of-voices can include any number of voices. All voices must have a pitch 
and rhythm domain.
"
 (HR-pitch-pitch (fix-ompatch-rule rule) list-voices timepoints input-mode :exclude-gracenotes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! midics-to-pcs ((n number))
:initvals '( nil )
:indoc '("number, list or trees.") 
:icon 01
:doc  "Midics to Pitch-Classes"
(mod (/ n 100) 12))

(om::defmethod! midics-to-pcs ((n list))
:initvals '( nil )
:indoc '("number, list or trees.") 
:icon 01
:doc  "Midics to Pitch-Classes"
(mapcar #'midics-to-pcs n))

(om::defmethod! apply_minus ((list list))
:initvals '( nil )
:indoc '("list") 
:icon 01
:doc  "Test that all values in a list are true."
 (apply-minus list))

(om::defmethod! apply_and ((list list))
:initvals '( nil )
:indoc '("list") 
:icon 01
:doc  "Test that all values in a list are true."
 (apply-and list))

(om::defmethod! test-seq-follows-markov_chain? ((markov-table-ref list) (+-% number))
:initvals '( nil nil )
:indoc '("list" "list") 
:icon 01
:doc  "This is a logic statement that checks if a given sequence can be considered a representation of a markov table.
The +-% is the maximum deviation (in percent) that the sequence can have from the markov table."
 (test-seq-follows-markov-chain? markov-table-ref +-%))

(om::defmethod! test-seq-follows-energy_profile? ((reference-sequence list) (deviation% number))
:initvals '( nil nil )
:indoc '("list" "list") 
:icon 01
:doc  "This is a logic statement that checks if a sequence follows the energy profile
of a given reference sequence.
<deviation%> is the maximum allowed deviation compared to max energy in profile.
"
 (test-seq-follows-energy-profile? reference-sequence deviation%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;; FROM JBS-CONSTRAINTS (PWGL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defmacro mode-switch (&body body)
  `(ecase mode?
     (:true/false (cons :true/false ,(second (first body))))
     (:heuristic (cons :heuristic ,(second (second body))))))

(defmacro absolute-switch (&body body)
  `(ecase absolute?
     (:absolute ,(second (first body)))
     (:up/down ,(second (second body)))))

(om::defmethod! allowed-intervals-rule ((intervals list) (absolute? t)  (mode? t) (weight number))
:initvals '( (-100 200 -300) :absolute :true/false 1)
:indoc '("intervals-list" "abs/up-down" "mode" "weight-number") 
:icon 02
:menuins '( (1 (("absolute" :absolute) ("up/down" :up/down) ))
                  (2 (("ture/false" :true/false) ("heuristic" :heuristic) ))) 
:doc "This rule allows only the intervals indicated in 'intervals'.
If the menu 'absolute?' is 'absolute,
that means that intervals are intented in absolute mode. If this menu is 'up/down', that means that
the intervals are divided into ascending and descending.
ATTENTION : in the mode true/false, the rule is perfectly applied.
In the mode heuristic, the rule is applied as much as possible"
  (let ((ris nil))
    (push intervals ris)
    (mode-switch
     (1
      (absolute-switch
	(1
	 `(* ?1
	     ?2
	     (ccl::?if
	      (member (abs (- ?2 ?1)) ',(om::om-abs (first (nreverse ris)))))))
	(2
	 `(* ?1
	     ?2
	     (ccl::?if
	      (member (- ?2 ?1) ',(first (nreverse ris))))))
	(otherwise (error "Got ~s, was expecting one of 1, 2." absolute?))))
     (2
      (absolute-switch
	(1
	 `(* ?1
	     ?2
	     (ccl::?if
	      (if (member (abs (- ?2 ?1)) ',(om::om-abs (first (nreverse ris))))
		  ,weight
		  0))))
	(2
	 `(* ?1
	     ?2
	     (ccl::?if
	      (if (member (- ?2 ?1) ',(first (nreverse ris))) ,weight 0))))
	(otherwise (error "Got ~s, was expecting one of 1, 2." absolute?))))
     (otherwise (error "Got ~s, was expecting one of 1, 2." mode?)))))
	 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLUSTER-ENGINE3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;UNLIMITED NUMBER OF RHYTHM AND PITCH DOMAINS

 (om::defmethod! Cluster-Engine3 ((no-of-variables integer)
                                 (rnd? t)
                                 (debug? t)
                                 (rules t )
                         
                                 (tempo number)
                         
                                 (output t)
                                 (metric-domain t)
                                 (rhythmdomain0 list)
                                 (pitchdomain0 list)
                                 &rest domains? )
						
 :initvals '( 10 t nil nil 90 :voices ((4 4)) ((1/4)) nil)								 
 :indoc '("integer" "t/nil" "t/nil" "t/nil" "number" "voices-or-list" "list-of-timsig" "ratios" "midics" "more-domains?") 
 :icon 01
 :menuins '((5 (("voices" :voices) ("list" :list)) ) )
 :doc "The Cluster Engine - the main function.
 Pitch domains cannot exist without at least one duration in the rhythm domain.
 Domains with only one value will not use up any time in the search process.
 "
 (let* ((all-domains (om::x-append (list rhythmdomain0 pitchdomain0) domains?))
          (default-pitch (loop for n from 0 to (1- (length all-domains)) by 2
                                   collect (if (and (not (equal nil (nth n all-domains))) 
                                                           (equal nil (nth (1+ n) all-domains)))
                                                   (om::x-append (list (nth n all-domains)) (list '((6000))))
                                                   (om::x-append (list (nth n all-domains)) (list (nth (1+ n) all-domains))))))
          (list-of-domains (remove nil (om::flat-once default-pitch))))

 (if (equal output :list)
     (mapcar #'(lambda (n) (remove nil n)) (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains))
     (poly-engine->score-with-tempo (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains) tempo))))
