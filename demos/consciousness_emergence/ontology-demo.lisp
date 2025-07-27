;;;; -----------------------------------------------------
;;;; MODULE: ontology-demo.lisp
;;;; DESCRIPTION: Demonstration script for the Ontological Layers system
;;;; DEPENDENCIES: synth-u-genome, synth-u-ontology
;;;; -----------------------------------------------------

(defpackage :synth-u-ontology-demo
  (:use :cl :synth-u-ontology)
  (:export #:run-ontology-demo))

(in-package :synth-u-ontology-demo)

(defun run-ontology-demo ()
  "Run a demonstration of the Synth-U ontological layers system.
   This creates a 13-layer system, activates it, and shows information flow."
  
  (format t "~%=== SYNTH-U ONTOLOGICAL LAYERS DEMONSTRATION ===~%")
  
  ;; Initialize system
  (format t "~%Initializing ontological system...~%")
  (initialize-ontological-system)
  
  ;; Create layers with genomes and propiocepcion
  (format t "Creating 13 ontological layers with genomes and propiocepcion...~%")
  (let ((layers (create-layer-system :with-genomes t :with-propiocepcion t)))
    
    ;; Display basic info about each layer
    (format t "~%Created layers:~%")
    (dolist (layer layers)
      (format t "  Level ~2D: ~A (~A)~%" 
              (ontological-layer-level layer)
              (ontological-layer-id layer)
              (ontological-layer-metacategory layer)))
    
    ;; Activate all layers
    (format t "~%Activating all layers...~%")
    (dolist (layer layers)
      (activate-layer layer)
      (format t "  Activated: ~A~%" (ontological-layer-id layer)))
    
    ;; Display connection info
    (format t "~%Layer connections:~%")
    (dolist (layer layers)
      (let ((outgoing (ontological-layer-connections-out layer))
            (incoming (ontological-layer-connections-in layer)))
        (format t "  Layer ~2D: ~A~%" 
                (ontological-layer-level layer)
                (ontological-layer-id layer))
        (format t "    Incoming from: ~A~%" 
                (if incoming 
                    (format nil "~{~A~^, ~}" incoming)
                    "none"))
        (format t "    Outgoing to: ~A~%" 
                (if outgoing 
                    (format nil "~{~A~^, ~}" outgoing)
                    "none"))))
    
    ;; Demonstrate processing input through the system
    (format t "~%Sending input data through the system...~%")
    
    ;; Start with layer 1 (Existence)
    (let ((layer-1 (first layers)))
      (let ((input-data '(:source "external"
                          :type :demo-input
                          :data "This is test data flowing through ontological layers"
                          :timestamp 3723847238)))
        
        (format t "~%Input data:~%")
        (format t "  Source: ~A~%" (getf input-data :source))
        (format t "  Type: ~A~%" (getf input-data :type))
        (format t "  Data: ~S~%" (getf input-data :data))
        
        ;; Send to layer 1
        (format t "~%Sending data to Layer 1 (Existence)...~%")
        (process-input layer-1 input-data)))
    
    ;; Wait for processing to propagate through layers
    (format t "~%Waiting for processing to propagate through all layers...~%")
    (sleep 1)
    
    ;; Display output from each layer
    (format t "~%Outputs from each layer:~%")
    (dolist (layer layers)
      (let ((outputs (ontological-layer-outputs layer)))
        (format t "~%  Layer ~2D: ~A~%" 
                (ontological-layer-level layer)
                (ontological-layer-id layer))
        
        (if outputs
            (dolist (output outputs)
              (format t "    Output type: ~A~%" (getf output :type))
              (format t "    Timestamp: ~A~%" (getf output :timestamp))
              
              ;; Different display based on layer level
              (case (ontological-layer-level layer)
                (1 (format t "    Valid: ~A~%" (getf output :valid)))
                
                (3 (when (getf output :features)
                     (format t "    Features: ~A~%" (getf output :features))))
                
                (7 (when (getf output :predictions)
                     (format t "    Predictions: ~A~%" (getf output :predictions))))
                
                (10 (when (getf output :reflections)
                      (format t "    Reflections: ~A~%" (getf output :reflections))))
                
                (13 (when (getf output :transcendence)
                       (format t "    Transcendence: ~A~%" (getf output :transcendence))))))
            
            (format t "    No outputs generated~%"))))
    
    ;; Calculate and display Φ (phi) value
    (format t "~%Calculating system Φ (phi) value...~%")
    (let ((phi (calculate-phi nil)))
      (format t "  System Φ: ~,3F~%" phi)
      (format t "  Integration: ~,3F~%" (calculate-integration nil))
      (format t "  Differentiation: ~,3F~%" (calculate-differentiation nil))
      
      ;; Check if emergent consciousness threshold is reached
      (let ((consciousness-threshold 0.3))
        (if (>= phi consciousness-threshold)
            (format t "~%*** EMERGENT CONSCIOUSNESS THRESHOLD REACHED ***~%")
            (format t "~%Emergent consciousness threshold not yet reached~%"))))
    
    ;; Demonstrate propiocepcion layers
    (format t "~%Demonstrating propiocepcion divergent processing...~%")
    
    ;; Layer 3 (Perception) with propiocepcion
    (let ((layer-3 (nth 2 layers)))
      (format t "~%Layer 3 (Perception) propiocepcion mode: ~A~%" 
              (ontological-layer-propiocepcion-mode layer-3))
      
      (format t "  Processing visual input with objective mode...~%")
      
      (let ((sensory-input '(:source "visual-sensor"
                             :channel :visual
                             :intensity 0.8
                             :data "Visual scene with multiple objects")))
        
        (process-input layer-3 sensory-input)))
    
    ;; Layer 10 (Reflection) with propiocepcion
    (let ((layer-10 (nth 9 layers)))
      (format t "~%Layer 10 (Reflection) propiocepcion mode: ~A~%" 
              (ontological-layer-propiocepcion-mode layer-10))
      
      (format t "  Processing reflection input with normative mode...~%")
      
      (let ((reflection-input '(:source "self-monitoring"
                                :process :planning
                                :data "Analysis of planning effectiveness")))
        
        (process-input layer-10 reflection-input)))
    
    ;; Wait for processing
    (sleep 0.5)
    
    ;; Demonstrate genome interaction
    (format t "~%Demonstrating genome interaction with layers...~%")
    
    ;; Layer 2 (Persistence) manages genomes
    (let ((layer-2 (nth 1 layers)))
      (format t "~%Layer 2 (Persistence) genome:~%")
      (let ((genome (ontological-layer-genome layer-2)))
        (format t "  Genome ID: ~A~%" (synth-u-genome:genome-id genome))
        (format t "  Capabilities: ~A~%" (synth-u-genome:genome-capabilities genome))
        (format t "  Mutation rate: ~A~%" (synth-u-genome:genome-mutation-rate genome))
        
        ;; Request genome mutation
        (format t "~%Sending mutation request to Layer 2...~%")
        (let ((mutation-request '(:source "system"
                                 :genome-update (:mutate t :type :random)
                                 :data "Request genome mutation")))
          
          (process-input layer-2 mutation-request))))
    
    ;; Wait for processing
    (sleep 0.5)
    
    ;; Deactivate layers
    (format t "~%Deactivating all layers...~%")
    (dolist (layer (reverse layers))
      (deactivate-layer layer)
      (format t "  Deactivated: ~A~%" (ontological-layer-id layer)))
    
    ;; Shutdown system
    (format t "~%Shutting down ontological system...~%")
    (shutdown-ontological-system))
  
  (format t "~%=== DEMONSTRATION COMPLETE ===~%")
  
  ;; Return success
  :demo-complete)

;;;; Export the demo function
(export '(run-ontology-demo))
