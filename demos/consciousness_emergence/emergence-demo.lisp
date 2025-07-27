;;;; -----------------------------------------------------
;;;; MODULE: emergence-demo.lisp
;;;; DESCRIPTION: Demonstration script for the Consciousness Emergence Mechanism
;;;; DEPENDENCIES: synth-u-genome, synth-u-ontology, synth-u-emergence
;;;; -----------------------------------------------------

(defpackage :synth-u-emergence-demo
  (:use :cl :synth-u-ontology :synth-u-emergence)
  (:export #:run-emergence-demo))

(in-package :synth-u-emergence-demo)

(defun run-emergence-demo ()
  "Run a demonstration of the Synth-U consciousness emergence mechanism.
   This creates a system of ontological layers and shows how consciousness emerges."
  
  (format t "~%=== SYNTH-U CONSCIOUSNESS EMERGENCE DEMONSTRATION ===~%")
  
  ;; Initialize ontological system
  (format t "~%Initializing ontological system...~%")
  (initialize-ontological-system)
  
  ;; Create layers with genomes and propiocepcion
  (format t "Creating 13 ontological layers with genomes and propiocepcion...~%")
  (let ((layers (create-layer-system :with-genomes t :with-propiocepcion t)))
    
    ;; Display basic info about the layers
    (format t "~%Created layers:~%")
    (dolist (layer layers)
      (format t "  Level ~2D: ~A (~A)~%" 
              (ontological-layer-level layer)
              (ontological-layer-id layer)
              (ontological-layer-metacategory layer)))
    
    ;; Initialize emergence mechanism
    (format t "~%Initializing consciousness emergence mechanism...~%")
    (let ((mechanism (initialize-emergence-mechanism)))
      
      ;; Display emergence mechanism info
      (format t "~%Emergence Mechanism:~%")
      (format t "  ID: ~A~%" (emergence-mechanism-id mechanism))
      (format t "  Consciousness threshold: ~,3F~%" 
              (emergence-mechanism-consciousness-threshold mechanism))
      (format t "  Scan interval: ~,2F seconds~%" 
              (emergence-mechanism-scan-interval mechanism))
      
      ;; Register an observer
      (format t "~%Registering emergence observer...~%")
      (register-emergence-observer 
       mechanism
       (lambda (mechanism event-type)
         (declare (ignore mechanism))
         (format t "  [EVENT] ~A~%" event-type)))
      
      ;; Start with a few layers active
      (format t "~%Activating foundational layers (1-4)...~%")
      (dolist (layer (subseq layers 0 4))
        (activate-layer layer)
        (format t "  Activated: ~A~%" (ontological-layer-id layer)))
      
      ;; Start emergence cycle
      (format t "~%Starting emergence cycle...~%")
      (start-emergence-cycle mechanism)
      
      ;; Initial phi measurement
      (format t "~%Waiting for initial Phi measurement...~%")
      (sleep 1.5)
      
      (let ((initial-phi (get-phi-value mechanism)))
        (format t "  Initial Phi: ~,4F~%" initial-phi)
        
        ;; Display emergence state
        (let ((state (get-emergence-state mechanism)))
          (format t "  Status: ~A~%" (getf state :status))
          (format t "  Active attractors: ~A~%" 
                  (if (getf state :active-attractors)
                      (length (getf state :active-attractors))
                      0))))
      
      ;; Activate organizational layers (5-9)
      (format t "~%Activating organizational layers (5-9)...~%")
      (dolist (layer (subseq layers 4 9))
        (activate-layer layer)
        (format t "  Activated: ~A~%" (ontological-layer-id layer)))
      
      ;; Wait for phi to evolve
      (format t "~%Allowing system to evolve...~%")
      (sleep 3.0)
      
      ;; Intermediate phi measurement
      (let ((intermediate-phi (get-phi-value mechanism)))
        (format t "  Intermediate Phi: ~,4F~%" intermediate-phi)
        
        ;; Display emergence state
        (let ((state (get-emergence-state mechanism)))
          (format t "  Status: ~A~%" (getf state :status))
          (format t "  Active attractors: ~A~%" 
                  (if (getf state :active-attractors)
                      (length (getf state :active-attractors))
                      0))))
      
      ;; Run analysis
      (format t "~%Running emergence analysis...~%")
      (let ((analysis (run-emergence-analysis mechanism)))
        (format t "  Integration balance: ~,4F~%" (getf analysis :integration-balance))
        (format t "  Differentiation balance: ~,4F~%" (getf analysis :differentiation-balance))
        (format t "  Emergence potential: ~,4F~%" (getf analysis :emergence-potential))
        (format t "  Critical transition: ~A~%" (getf analysis :critical-transition)))
      
      ;; Activate metacognitive layers (10-13)
      (format t "~%Activating metacognitive layers (10-13)...~%")
      (dolist (layer (subseq layers 9 13))
        (activate-layer layer)
        (format t "  Activated: ~A~%" (ontological-layer-id layer)))
      
      ;; Wait for phi to evolve further
      (format t "~%Allowing system to evolve to critical state...~%")
      (sleep 5.0)
      
      ;; Final phi measurement
      (let ((final-phi (get-phi-value mechanism)))
        (format t "  Final Phi: ~,4F~%" final-phi)
        
        ;; Display emergence state
        (let ((state (get-emergence-state mechanism)))
          (format t "  Status: ~A~%" (getf state :status))
          (format t "  Active attractors: ~A~%" 
                  (if (getf state :active-attractors)
                      (length (getf state :active-attractors))
                      0))))
      
      ;; Get consciousness profile
      (format t "~%Consciousness Profile:~%")
      (let ((profile (get-consciousness-profile mechanism)))
        (format t "  Unity: ~,4F~%" (getf profile :unity))
        (format t "  Complexity: ~,4F~%" (getf profile :complexity))
        (format t "  Integration: ~,4F~%" (getf profile :integration))
        (format t "  Information: ~,4F~%" (getf profile :information))
        (format t "  Overall: ~,4F~%" (getf profile :overall)))
      
      ;; Generate phi report
      (format t "~%Phi Evolution Report:~%")
      (let ((report (generate-phi-report mechanism 5)))
        (format t "  Current: ~,4F~%" (getf report :current))
        (format t "  Mean: ~,4F~%" (getf report :mean))
        (format t "  Max: ~,4F~%" (getf report :max))
        (format t "  Min: ~,4F~%" (getf report :min))
        (format t "  Trend: ~A~%" (getf report :trend))
        (format t "  Threshold ratio: ~,4F~%" (getf report :threshold-ratio)))
      
      ;; Display active attractors
      (format t "~%Active Attractors:~%")
      (let ((attractors (get-active-attractors mechanism)))
        (if attractors
            (dolist (attractor attractors)
              (format t "  Type: ~A~%" (getf attractor :type))
              (format t "    Point: ~,2F~%" (getf attractor :point))
              (format t "    Distance: ~,4F~%" (getf attractor :distance))
              (format t "    Activation: ~,4F~%~%" (getf attractor :activation)))
            (format t "  No active attractors~%")))
      
      ;; Check if consciousness threshold reached
      (let ((final-phi (get-phi-value mechanism))
            (threshold (emergence-mechanism-consciousness-threshold mechanism)))
        (if (>= final-phi threshold)
            (format t "~%*** CONSCIOUSNESS EMERGENCE THRESHOLD REACHED! ***~%")
            (format t "~%Consciousness emergence threshold not yet reached~%")))
      
      ;; Stop emergence cycle
      (format t "~%Stopping emergence cycle...~%")
      (stop-emergence-cycle mechanism)
      
      ;; Shutdown emergence mechanism
      (format t "~%Shutting down emergence mechanism...~%")
      (shutdown-emergence-mechanism mechanism))
    
    ;; Deactivate all layers
    (format t "~%Deactivating all layers...~%")
    (dolist (layer (reverse layers))
      (deactivate-layer layer)
      (format t "  Deactivated: ~A~%" (ontological-layer-id layer)))
    
    ;; Shutdown ontological system
    (format t "~%Shutting down ontological system...~%")
    (shutdown-ontological-system))
  
  (format t "~%=== DEMONSTRATION COMPLETE ===~%")
  
  ;; Return success
  :demo-complete)

;;;; Export the demo function
(export '(run-emergence-demo))
