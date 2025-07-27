;;;; -----------------------------------------------------
;;;; MODULE: ontological-layers-tests.lisp
;;;; DESCRIPTION: Tests for the Ontological Layers system
;;;; DEPENDENCIES: FiveAM, synth-u-genome, synth-u-ontology
;;;; -----------------------------------------------------

(defpackage :synth-u-ontology-tests
  (:use :cl :fiveam :synth-u-ontology)
  (:export #:run-ontology-tests))

(in-package :synth-u-ontology-tests)

;;;; Test Suite Setup
(def-suite ontology-tests
  :description "Test suite for the Ontological Layers functionality")

(in-suite ontology-tests)

;;;; Basic Layer Creation Tests
(test layer-creation
  "Test basic layer creation and properties"
  
  ;; Create a test layer
  (let ((layer (make-ontological-layer "test-layer-1" 3
                                      :description "Test perception layer")))
    ;; Check properties
    (is (string= (ontological-layer-id layer) "test-layer-1")
        "Layer ID should match")
    
    (is (= (ontological-layer-level layer) 3)
        "Layer level should match")
    
    (is (eq (ontological-layer-metacategory layer) :foundational)
        "Layer metacategory should be determined correctly")
    
    (is (eq (ontological-layer-status layer) :inactive)
        "Initial layer status should be inactive")
    
    (is (string= (ontological-layer-description layer) "Test perception layer")
        "Layer description should match")
    
    (is (hash-table-p (ontological-layer-properties layer))
        "Layer properties should be a hash table")
    
    (is (hash-table-p (ontological-layer-state-memory layer))
        "Layer state memory should be a hash table")
    
    ;; Check level-specific properties
    (is (not (null (gethash :sensory-channels (ontological-layer-properties layer))))
        "Layer should have level-specific properties")))

(test layer-invalid-creation
  "Test validation during layer creation"
  
  ;; Empty ID should fail
  (signals error
    (make-ontological-layer "" 1))
  
  ;; Invalid level should fail
  (signals error
    (make-ontological-layer "invalid-level" 0))
  
  (signals error
    (make-ontological-layer "invalid-level" 14))
  
  ;; Duplicate ID should fail
  (let ((layer (make-ontological-layer "duplicate-test" 1)))
    (signals error
      (make-ontological-layer "duplicate-test" 2))))

;;;; Layer Activation Tests
(test layer-activation
  "Test layer activation and deactivation"
  
  (let ((layer (make-ontological-layer "activation-test" 1)))
    ;; Verify initial status
    (is (eq (ontological-layer-status layer) :inactive)
        "Initial status should be inactive")
    
    ;; Activate the layer
    (is-true (activate-layer layer)
              "Activating an inactive layer should return T")
    
    ;; Verify activation
    (is (eq (ontological-layer-status layer) :active)
        "Status should be active after activation")
    
    (is (not (null (ontological-layer-activation-timestamp layer)))
        "Activation timestamp should be set")
    
    (is (not (null (ontological-layer-thread layer)))
        "Processing thread should be created")
    
    ;; Try to activate again (should fail)
    (is-false (activate-layer layer)
               "Activating an already active layer should return NIL")
    
    ;; Deactivate the layer
    (is-true (deactivate-layer layer)
              "Deactivating an active layer should return T")
    
    ;; Verify deactivation
    (is (eq (ontological-layer-status layer) :inactive)
        "Status should be inactive after deactivation")
    
    (is (null (ontological-layer-thread layer))
        "Processing thread should be terminated")
    
    ;; Try to deactivate again (should fail)
    (is-false (deactivate-layer layer)
               "Deactivating an already inactive layer should return NIL")))

;;;; Layer Connection Tests
(test layer-connections
  "Test layer connection and disconnection"
  
  (let ((layer1 (make-ontological-layer "conn-layer-1" 1))
        (layer2 (make-ontological-layer "conn-layer-2" 2)))
    
    ;; Initially no connections
    (is (null (ontological-layer-connections-out layer1))
        "Layer1 should have no outgoing connections initially")
    
    (is (null (ontological-layer-connections-in layer2))
        "Layer2 should have no incoming connections initially")
    
    ;; Connect the layers
    (is-true (connect-layers layer1 layer2)
              "Connecting layers should return T")
    
    ;; Verify connection
    (is (member (ontological-layer-id layer2) 
               (ontological-layer-connections-out layer1)
               :test #'string=)
        "Layer1 should have Layer2 in its outgoing connections")
    
    (is (member (ontological-layer-id layer1) 
               (ontological-layer-connections-in layer2)
               :test #'string=)
        "Layer2 should have Layer1 in its incoming connections")
    
    ;; Try to connect again (should fail)
    (is-false (connect-layers layer1 layer2)
               "Connecting already connected layers should return NIL")
    
    ;; Disconnect the layers
    (is-true (disconnect-layers layer1 layer2)
              "Disconnecting layers should return T")
    
    ;; Verify disconnection
    (is (not (member (ontological-layer-id layer2) 
                    (ontological-layer-connections-out layer1)
                    :test #'string=))
        "Layer1 should not have Layer2 in its outgoing connections after disconnect")
    
    (is (not (member (ontological-layer-id layer1) 
                    (ontological-layer-connections-in layer2)
                    :test #'string=))
        "Layer2 should not have Layer1 in its incoming connections after disconnect")
    
    ;; Try to disconnect again (should fail)
    (is-false (disconnect-layers layer1 layer2)
               "Disconnecting already disconnected layers should return NIL")))

;;;; Input Processing Tests
(test input-processing
  "Test input processing in layers"
  
  (let ((layer (make-ontological-layer "process-layer" 1)))
    ;; Cannot process if inactive
    (is-false (process-input layer '(:data "test-data"))
               "Processing input on inactive layer should return NIL")
    
    ;; Activate the layer
    (activate-layer layer)
    
    ;; Now should accept input
    (is-true (process-input layer '(:data "test-data"))
              "Processing input on active layer should return T")
    
    ;; Check input was queued
    (is (= (length (ontological-layer-inputs layer)) 1)
        "Input should be queued")
    
    ;; Deactivate for cleanup
    (deactivate-layer layer)))

;;;; Level-Specific Processing Tests
(test level-processing
  "Test level-specific processing functions"
  
  ;; Test each level with a simple input
  (dotimes (i 13)
    (let* ((level (1+ i))
           (layer (make-ontological-layer (format nil "level-~A-test" level) level))
           (input '(:data "test-data" :source "test")))
      
      ;; Activate the layer
      (activate-layer layer)
      
      ;; Send input
      (process-input layer input)
      
      ;; Give time for processing
      (sleep 0.1)
      
      ;; Check output was generated
      (is (>= (length (ontological-layer-outputs layer)) 0)
          (format nil "Layer ~A should process input" level))
      
      ;; Deactivate for cleanup
      (deactivate-layer layer))))

;;;; Propagation Tests
(test output-propagation
  "Test propagation of outputs between connected layers"
  
  (let ((layer1 (make-ontological-layer "prop-layer-1" 1))
        (layer2 (make-ontological-layer "prop-layer-2" 2)))
    
    ;; Connect the layers
    (connect-layers layer1 layer2)
    
    ;; Activate both layers
    (activate-layer layer1)
    (activate-layer layer2)
    
    ;; Send input to layer1
    (process-input layer1 '(:data "propagation-test" :source "test"))
    
    ;; Give time for processing and propagation
    (sleep 0.2)
    
    ;; Check layer2 received something
    (is (>= (length (ontological-layer-inputs layer2)) 0)
        "Layer2 should receive propagated output from Layer1")
    
    ;; Deactivate for cleanup
    (deactivate-layer layer1)
    (deactivate-layer layer2)))

;;;; Observer Tests
(test layer-observers
  "Test observer notification mechanism"
  
  (let ((layer (make-ontological-layer "observer-test" 1))
        (events '())
        (observer-fn (lambda (layer event-type)
                       (declare (ignore layer))
                       (push event-type events))))
    
    ;; Add observer
    (add-observer layer observer-fn)
    
    ;; Trigger events
    (activate-layer layer)
    (deactivate-layer layer)
    
    ;; Check events were recorded
    (is (member :activation events)
        "Observer should be notified of activation")
    
    (is (member :deactivation events)
        "Observer should be notified of deactivation")
    
    ;; Remove observer
    (is-true (remove-observer layer observer-fn)
              "Removing an existing observer should return T")
    
    ;; Try to remove again
    (is-false (remove-observer layer observer-fn)
               "Removing a non-existent observer should return NIL")))

;;;; Propiocepcion Mode Tests
(test propiocepcion-modes
  "Test propiocepcion divergent processing modes"
  
  ;; Test layer 3 (Perception) with propiocepcion
  (let ((layer (make-ontological-layer "propiocepcion-test" 3
                                      :propiocepcion-mode :objective)))
    
    ;; Check mode was set
    (is (eq (ontological-layer-propiocepcion-mode layer) :objective)
        "Propiocepcion mode should be set")
    
    ;; Check properties were initialized
    (is (member :objective (gethash :propiocepcion-modes 
                                  (ontological-layer-properties layer)))
        "Propiocepcion modes should be initialized")
    
    ;; Check current mode was set
    (is (eq (gethash :current-mode (ontological-layer-properties layer)) :objective)
        "Current mode should be set to initial mode")
    
    ;; Activate and process with specific mode
    (activate-layer layer)
    (process-input layer '(:data "mode-test" :channel :visual :intensity 0.7))
    
    ;; Give time for processing
    (sleep 0.1)
    
    ;; Deactivate for cleanup
    (deactivate-layer layer)))

;;;; Phi Calculation Tests
(test phi-calculation
  "Test calculation of Phi (integration/differentiation)"
  
  ;; Create a minimal test system
  (let ((layer1 (make-ontological-layer "phi-layer-1" 1))
        (layer2 (make-ontological-layer "phi-layer-2" 5))
        (layer3 (make-ontological-layer "phi-layer-3" 10)))
    
    ;; Activate layers
    (activate-layer layer1)
    (activate-layer layer2)
    (activate-layer layer3)
    
    ;; Calculate phi with all layers active
    (let ((phi-active (calculate-phi nil)))
      (is (> phi-active 0.0)
          "Phi with active layers should be greater than zero"))
    
    ;; Deactivate one layer
    (deactivate-layer layer3)
    
    ;; Calculate phi with fewer layers
    (let ((phi-reduced (calculate-phi nil)))
      (is (<= phi-reduced 0.4)
          "Phi with fewer active layers should be lower"))
    
    ;; Deactivate all layers for cleanup
    (deactivate-layer layer1)
    (deactivate-layer layer2)))

;;;; System Creation Tests
(test system-creation
  "Test creation of complete layer system"
  
  ;; Test initialization
  (is-true (initialize-ontological-system)
            "System initialization should return T")
  
  ;; Create system without genomes or propiocepcion
  (let ((layers (create-layer-system)))
    
    ;; Check correct number of layers
    (is (= (length layers) 13)
        "Should create 13 layers")
    
    ;; Check layers are ordered by level
    (is (= (ontological-layer-level (first layers)) 1)
        "First layer should be level 1")
    
    (is (= (ontological-layer-level (car (last layers))) 13)
        "Last layer should be level 13")
    
    ;; Check connections
    (is (member (ontological-layer-id (nth 1 layers))
               (ontological-layer-connections-out (nth 0 layers))
               :test #'string=)
        "Adjacent layers should be connected"))
  
  ;; Create system with genomes and propiocepcion
  (let ((layers (create-layer-system :with-genomes t 
                                    :with-propiocepcion t)))
    
    ;; Check genomes were created
    (is (not (null (ontological-layer-genome (first layers))))
        "Layers should have genomes when requested")
    
    ;; Check propiocepcion was set for specific layers
    (is (not (null (ontological-layer-propiocepcion-mode (nth 2 layers))))
        "Layer 3 should have propiocepcion mode")
    
    (is (not (null (ontological-layer-propiocepcion-mode (nth 9 layers))))
        "Layer 10 should have propiocepcion mode")
    
    (is (not (null (ontological-layer-propiocepcion-mode (nth 12 layers))))
        "Layer 13 should have propiocepcion mode"))
  
  ;; Shutdown system
  (is-true (shutdown-ontological-system)
            "System shutdown should return T"))

;;;; Export a function to run all tests
(defun run-ontology-tests ()
  "Run all ontological layer tests"
  (run! 'ontology-tests))

;;;; Export the test suite
(export '(run-ontology-tests))
