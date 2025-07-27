;;;; -----------------------------------------------------
;;;; MODULE: emergence-mechanism-tests.lisp
;;;; DESCRIPTION: Tests for the Consciousness Emergence Mechanism
;;;; DEPENDENCIES: FiveAM, synth-u-genome, synth-u-ontology, synth-u-emergence
;;;; -----------------------------------------------------

(defpackage :synth-u-emergence-tests
  (:use :cl :fiveam :synth-u-ontology :synth-u-emergence)
  (:export #:run-emergence-tests))

(in-package :synth-u-emergence-tests)

;;;; Test Suite Setup
(def-suite emergence-tests
  :description "Test suite for the Consciousness Emergence Mechanism functionality")

(in-suite emergence-tests)

;;;; Basic Mechanism Creation Tests
(test mechanism-creation
  "Test basic emergence mechanism creation and properties"
  
  ;; Initialize ontological system (required for emergence)
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (synth-u-ontology:create-layer-system)
  
  ;; Create test emergence mechanism
  (let ((mechanism (make-emergence-mechanism
                     :consciousness-threshold 0.3
                     :scan-interval 0.5
                     :max-history-length 50)))
    
    ;; Check properties
    (is (string= (subseq (emergence-mechanism-id mechanism) 0 10) "emergence-")
        "Emergence mechanism ID should have correct prefix")
    
    (is (eq (emergence-mechanism-status mechanism) :inactive)
        "Initial status should be inactive")
    
    (is (= (emergence-mechanism-consciousness-threshold mechanism) 0.3)
        "Consciousness threshold should match")
    
    (is (= (emergence-mechanism-scan-interval mechanism) 0.5)
        "Scan interval should match")
    
    (is (= (emergence-mechanism-max-phi-history-length mechanism) 50)
        "Max phi history length should match")
    
    (is (= (emergence-mechanism-last-phi-value mechanism) 0.0)
        "Initial last phi value should be zero")
    
    (is (null (emergence-mechanism-phi-history mechanism))
        "Initial phi history should be empty")
    
    (is (null (emergence-mechanism-emergence-events mechanism))
        "Initial emergence events should be empty")
    
    (is (listp (emergence-mechanism-attractor-basins mechanism))
        "Attractor basins should be a list")
    
    (is (not (null (emergence-mechanism-attractor-basins mechanism)))
        "Attractor basins should be initialized with defaults")
    
    ;; Shutdown ontological system
    (synth-u-ontology:shutdown-ontological-system)))

(test mechanism-invalid-creation
  "Test validation during emergence mechanism creation"
  
  ;; Invalid consciousness threshold should fail
  (signals error
    (make-emergence-mechanism :consciousness-threshold -0.1))
  
  (signals error
    (make-emergence-mechanism :consciousness-threshold 1.5))
  
  ;; Invalid scan interval should fail
  (signals error
    (make-emergence-mechanism :scan-interval 0.0))
  
  (signals error
    (make-emergence-mechanism :scan-interval -1.0)))

(test mechanism-initialization
  "Test emergence mechanism initialization"
  
  ;; Initialize ontological system (required for emergence)
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (synth-u-ontology:create-layer-system)
  
  ;; Initialize emergence mechanism
  (let ((mechanism (initialize-emergence-mechanism)))
    
    ;; Check properties
    (is (not (null mechanism))
        "Emergence mechanism should be created")
    
    (is (eq (emergence-mechanism-status mechanism) :inactive)
        "Initial status should be inactive")
    
    (is (listp (emergence-mechanism-integration-matrix mechanism))
        "Integration matrix should be initialized")
    
    (is (listp (emergence-mechanism-differentiation-matrix mechanism))
        "Differentiation matrix should be initialized")
    
    ;; Shutdown ontological system
    (synth-u-ontology:shutdown-ontological-system)))

;;;; Cycle Control Tests
(test mechanism-cycle-control
  "Test emergence cycle control"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (let ((layers (synth-u-ontology:create-layer-system)))
    
    ;; Activate some layers
    (dolist (layer (subseq layers 0 5))
      (synth-u-ontology:activate-layer layer))
    
    ;; Initialize emergence mechanism
    (let ((mechanism (initialize-emergence-mechanism)))
      
      ;; Start emergence cycle
      (is-true (start-emergence-cycle mechanism)
                "Starting emergence cycle should return T")
      
      ;; Check status
      (is (eq (emergence-mechanism-status mechanism) :scanning)
          "Status should be scanning after starting cycle")
      
      ;; Check thread
      (is (not (null (emergence-mechanism-thread mechanism)))
          "Processing thread should be created")
      
      ;; Try to start again (should fail)
      (is-false (start-emergence-cycle mechanism)
                 "Starting an already started cycle should return NIL")
      
      ;; Give time for at least one scan
      (sleep 1.0)
      
      ;; Check phi history
      (is (not (null (emergence-mechanism-phi-history mechanism)))
          "Phi history should be updated after scanning")
      
      ;; Stop emergence cycle
      (is-true (stop-emergence-cycle mechanism)
                "Stopping emergence cycle should return T")
      
      ;; Check status
      (is (eq (emergence-mechanism-status mechanism) :inactive)
          "Status should be inactive after stopping cycle")
      
      ;; Check thread
      (is (null (emergence-mechanism-thread mechanism))
          "Processing thread should be terminated")
      
      ;; Try to stop again (should fail)
      (is-false (stop-emergence-cycle mechanism)
                 "Stopping an already stopped cycle should return NIL")
      
      ;; Shutdown emergence mechanism
      (is-true (shutdown-emergence-mechanism mechanism)
                "Shutting down emergence mechanism should return T")
      
      ;; Check status
      (is (eq (emergence-mechanism-status mechanism) :shutdown)
          "Status should be shutdown after shutting down")))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; Observer Tests
(test mechanism-observers
  "Test observer notification mechanism"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (synth-u-ontology:create-layer-system)
  
  ;; Initialize emergence mechanism
  (let ((mechanism (initialize-emergence-mechanism))
        (events '())
        (observer-fn (lambda (mechanism event-type)
                       (declare (ignore mechanism))
                       (push event-type events))))
    
    ;; Register observer
    (is-true (register-emergence-observer mechanism observer-fn)
              "Registering observer should return T")
    
    ;; Trigger events
    (start-emergence-cycle mechanism)
    (sleep 0.2)
    (stop-emergence-cycle mechanism)
    (shutdown-emergence-mechanism mechanism)
    
    ;; Check events were recorded
    (is (member :start-cycle events)
        "Observer should be notified of start-cycle")
    
    (is (member :stop-cycle events)
        "Observer should be notified of stop-cycle")
    
    (is (member :shutdown events)
        "Observer should be notified of shutdown")
    
    ;; Unregister observer
    (is-true (unregister-emergence-observer mechanism observer-fn)
              "Unregistering an existing observer should return T")
    
    ;; Try to unregister again
    (is-false (unregister-emergence-observer mechanism observer-fn)
               "Unregistering a non-existent observer should return NIL"))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; Parameter Setting Tests
(test mechanism-parameter-setting
  "Test setting emergence mechanism parameters"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (synth-u-ontology:create-layer-system)
  
  ;; Initialize emergence mechanism
  (let ((mechanism (initialize-emergence-mechanism)))
    
    ;; Set parameters
    (is-true (set-emergence-parameters mechanism
                                      :consciousness-threshold 0.4
                                      :scan-interval 0.7
                                      :max-history-length 75)
              "Setting parameters should return T")
    
    ;; Check parameters were updated
    (is (= (emergence-mechanism-consciousness-threshold mechanism) 0.4)
        "Consciousness threshold should be updated")
    
    (is (= (emergence-mechanism-scan-interval mechanism) 0.7)
        "Scan interval should be updated")
    
    (is (= (emergence-mechanism-max-phi-history-length mechanism) 75)
        "Max phi history length should be updated")
    
    ;; Test parameter validation
    (signals error
      (set-emergence-parameters mechanism :consciousness-threshold -0.1))
    
    (signals error
      (set-emergence-parameters mechanism :scan-interval 0.0))
    
    (signals error
      (set-emergence-parameters mechanism :max-history-length 0)))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; State Access Tests
(test mechanism-state-access
  "Test access to emergence mechanism state"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (let ((layers (synth-u-ontology:create-layer-system)))
    
    ;; Activate some layers
    (dolist (layer (subseq layers 0 5))
      (synth-u-ontology:activate-layer layer))
    
    ;; Initialize emergence mechanism
    (let ((mechanism (initialize-emergence-mechanism)))
      
      ;; Start emergence cycle
      (start-emergence-cycle mechanism)
      
      ;; Give time for at least one scan
      (sleep 0.5)
      
      ;; Get phi value
      (let ((phi-value (get-phi-value mechanism)))
        (is (numberp phi-value)
            "Phi value should be a number")
        
        (is (>= phi-value 0.0)
            "Phi value should be non-negative"))
      
      ;; Get emergence state
      (let ((state (get-emergence-state mechanism)))
        (is (listp state)
            "Emergence state should be a list")
        
        (is (member :status state)
            "Emergence state should include status")
        
        (is (member :phi state)
            "Emergence state should include phi")
        
        (is (member :threshold state)
            "Emergence state should include threshold")
        
        (is (member :last-scan state)
            "Emergence state should include last scan timestamp")
        
        (is (member :active-attractors state)
            "Emergence state should include active attractors")
        
        (is (member :event-count state)
            "Emergence state should include event count"))
      
      ;; Stop emergence cycle
      (stop-emergence-cycle mechanism)))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; Analysis Tests
(test mechanism-analysis
  "Test emergence analysis functions"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (let ((layers (synth-u-ontology:create-layer-system)))
    
    ;; Activate all layers
    (dolist (layer layers)
      (synth-u-ontology:activate-layer layer))
    
    ;; Initialize emergence mechanism
    (let ((mechanism (initialize-emergence-mechanism)))
      
      ;; Start emergence cycle
      (start-emergence-cycle mechanism)
      
      ;; Give time for several scans
      (sleep 1.5)
      
      ;; Run emergence analysis
      (let ((analysis (run-emergence-analysis mechanism)))
        (is (listp analysis)
            "Analysis result should be a list")
        
        (is (member :id analysis)
            "Analysis should include ID")
        
        (is (member :status analysis)
            "Analysis should include status")
        
        (is (member :current-phi analysis)
            "Analysis should include current phi")
        
        (is (member :consciousness-threshold analysis)
            "Analysis should include consciousness threshold")
        
        (is (member :phi-mean analysis)
            "Analysis should include phi mean")
        
        (is (member :active-attractors analysis)
            "Analysis should include active attractors")
        
        (is (member :emergence-potential analysis)
            "Analysis should include emergence potential")
        
        (is (member :critical-transition analysis)
            "Analysis should include critical transition flag"))
      
      ;; Get active attractors
      (let ((attractors (get-active-attractors mechanism)))
        (is (listp attractors)
            "Active attractors should be a list"))
      
      ;; Get consciousness profile
      (let ((profile (get-consciousness-profile mechanism)))
        (is (listp profile)
            "Consciousness profile should be a list")
        
        (is (member :unity profile)
            "Profile should include unity score")
        
        (is (member :complexity profile)
            "Profile should include complexity score")
        
        (is (member :integration profile)
            "Profile should include integration score")
        
        (is (member :information profile)
            "Profile should include information score")
        
        (is (member :overall profile)
            "Profile should include overall score"))
      
      ;; Generate phi report
      (let ((report (generate-phi-report mechanism 5)))
        (is (listp report)
            "Phi report should be a list")
        
        (is (member :current report)
            "Report should include current phi")
        
        (is (member :mean report)
            "Report should include mean phi")
        
        (is (member :max report)
            "Report should include max phi")
        
        (is (member :min report)
            "Report should include min phi")
        
        (is (member :trend report)
            "Report should include trend")
        
        (is (member :entries report)
            "Report should include entries"))
      
      ;; Stop emergence cycle
      (stop-emergence-cycle mechanism)))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; Integration/Differentiation Matrix Tests
(test integration-differentiation-matrices
  "Test integration and differentiation matrices"
  
  ;; Initialize ontological system
  (synth-u-ontology:initialize-ontological-system)
  
  ;; Create layers for testing
  (synth-u-ontology:create-layer-system)
  
  ;; Test matrix initialization
  (let ((integration-matrix (synth-u-emergence::initialize-integration-matrix))
        (differentiation-matrix (synth-u-emergence::initialize-differentiation-matrix)))
    
    ;; Check integration matrix
    (is (= (length integration-matrix) 13)
        "Integration matrix should have 13 rows")
    
    (is (= (length (first integration-matrix)) 13)
        "Integration matrix rows should have 13 columns")
    
    (is (every (lambda (row) 
                (every (lambda (val) 
                         (and (numberp val) 
                              (>= val 0.0) 
                              (<= val 1.0)))
                       row))
              integration-matrix)
        "Integration matrix values should be numbers between 0 and 1")
    
    ;; Check differentiation matrix
    (is (= (length differentiation-matrix) 13)
        "Differentiation matrix should have 13 rows")
    
    (is (= (length (first differentiation-matrix)) 13)
        "Differentiation matrix rows should have 13 columns")
    
    (is (every (lambda (row) 
                (every (lambda (val) 
                         (and (numberp val) 
                              (>= val 0.0) 
                              (<= val 1.0)))
                       row))
              differentiation-matrix)
        "Differentiation matrix values should be numbers between 0 and 1"))
  
  ;; Shutdown ontological system
  (synth-u-ontology:shutdown-ontological-system))

;;;; Export a function to run all tests
(defun run-emergence-tests ()
  "Run all emergence mechanism tests"
  (run! 'emergence-tests))

;;;; Export the test suite
(export '(run-emergence-tests))
