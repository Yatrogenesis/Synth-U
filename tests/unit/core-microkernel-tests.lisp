;;;; -----------------------------------------------------
;;;; MODULE: core-microkernel-tests.lisp
;;;; DESCRIPTION: Tests for the Core Microkernel
;;;; DEPENDENCIES: FiveAM, synth-u-core
;;;; -----------------------------------------------------

(defpackage :synth-u-core-tests
  (:use :cl :fiveam :synth-u-core)
  (:export #:run-core-tests))

(in-package :synth-u-core-tests)

;;;; Test Suite Setup
(def-suite core-microkernel-tests
  :description "Test suite for the Core Microkernel functionality")

(in-suite core-microkernel-tests)

;;;; Utility Functions for Testing
(defun cleanup-test-kernels ()
  "Remove all test kernels from the system"
  (synth-u-core:shutdown-microkernel-system)
  (synth-u-core:initialize-microkernel-system))

;;;; Basic Microkernel Creation Tests
(test microkernel-creation
  "Test basic microkernel creation and properties"
  (cleanup-test-kernels)
  
  ;; Create a test kernel
  (let ((kernel (make-microkernel "test-1" :vision-core)))
    ;; Check properties
    (is (string= (microkernel-id kernel) "test-1")
        "Kernel ID should match")
    
    (is (eq (microkernel-role kernel) :vision-core)
        "Kernel role should match")
    
    (is (eq (microkernel-status kernel) :inactive)
        "Initial status should be :inactive")
    
    ;; Check registration
    (is (not (null (find-kernel "test-1")))
        "Kernel should be registered and findable")))

(test microkernel-invalid-creation
  "Test validation during microkernel creation"
  (cleanup-test-kernels)
  
  ;; Empty ID should fail
  (signals error
    (make-microkernel "" :vision-core))
  
  ;; Create a kernel
  (make-microkernel "test-duplicate" :vision-core)
  
  ;; Duplicate ID should fail
  (signals error
    (make-microkernel "test-duplicate" :linguistic-init)))

;;;; Activation Tests
(test microkernel-activation
  "Test microkernel activation and deactivation"
  (cleanup-test-kernels)
  
  (let ((kernel (make-microkernel "test-activation" :vision-core)))
    ;; Initial state
    (is (eq (microkernel-status kernel) :inactive))
    (is (null (microkernel-thread kernel)))
    
    ;; Activate
    (is-true (activate-microkernel kernel))
    (is (eq (microkernel-status kernel) :active))
    (is (not (null (microkernel-thread kernel))))
    
    ;; Activate again should return NIL (already active)
    (is-false (activate-microkernel kernel))
    
    ;; Deactivate
    (is-true (deactivate-microkernel kernel))
    (is (eq (microkernel-status kernel) :inactive))
    (is (null (microkernel-thread kernel)))
    
    ;; Deactivate again should return NIL (already inactive)
    (is-false (deactivate-microkernel kernel))))

;;;; Role Reassignment Tests
(test microkernel-role-reassignment
  "Test reassigning roles to a microkernel"
  (cleanup-test-kernels)
  
  (let ((kernel (make-microkernel "test-reassign" :vision-core)))
    ;; Initial role
    (is (eq (microkernel-role kernel) :vision-core))
    
    ;; Reassign to new role
    (is-true (reassign-role kernel :linguistic-init))
    (is (eq (microkernel-role kernel) :linguistic-init))
    
    ;; Reassigning to same role should return NIL
    (is-false (reassign-role kernel :linguistic-init))
    
    ;; Reassign again
    (is-true (reassign-role kernel :orchestration))
    (is (eq (microkernel-role kernel) :orchestration))))

;;;; Message Processing Tests
(test microkernel-message-processing
  "Test sending and processing messages"
  (cleanup-test-kernels)
  
  (let ((kernel (make-microkernel "test-messages" :generic)))
    ;; Inactive kernel should not accept messages
    (is-false (process-input kernel '(:test "data")))
    
    ;; Activate and try again
    (activate-microkernel kernel)
    (is-true (process-input kernel '(:test "data")))
    
    ;; Check queue (internal state)
    (is (= 1 (length (microkernel-input-queue kernel))))
    
    ;; Process should consume from queue
    (process-kernel-cycle kernel)
    (is (= 0 (length (microkernel-input-queue kernel))))
    
    ;; Output should be generated
    (is (= 1 (length (microkernel-output-queue kernel))))
    
    ;; Deactivate when done
    (deactivate-microkernel kernel)))

;;;; Inter-kernel Message Tests
(test inter-kernel-messages
  "Test sending messages between kernels"
  (cleanup-test-kernels)
  
  (let ((kernel1 (make-microkernel "sender" :generic))
        (kernel2 (make-microkernel "receiver" :generic)))
    
    ;; Activate kernels
    (activate-microkernel kernel1)
    (activate-microkernel kernel2)
    
    ;; Send message
    (is-true (send-message-to-kernel "sender" "receiver" '(:hello "world")))
    
    ;; Check receiver queue
    (is (= 1 (length (microkernel-input-queue kernel2))))
    
    ;; Invalid receiver should fail
    (is-false (send-message-to-kernel "sender" "nonexistent" '(:hello "world")))
    
    ;; Deactivate when done
    (deactivate-microkernel kernel1)
    (deactivate-microkernel kernel2)))

;;;; Performance Metrics Tests
(test microkernel-performance-metrics
  "Test performance metrics calculation"
  (cleanup-test-kernels)
  
  (let ((kernel (make-microkernel "test-metrics" :generic)))
    ;; Initial metrics
    (let ((metrics (calculate-kernel-performance kernel)))
      (is (= 0 (gethash :message-count metrics 0)))
      (is (= 0 (gethash :error-count metrics 0)))
      (is (= 0 (gethash :queue-length metrics 0))))
    
    ;; Activate and process some messages
    (activate-microkernel kernel)
    (process-input kernel '(:test "data1"))
    (process-input kernel '(:test "data2"))
    (process-input kernel '(:test "data3"))
    
    ;; Let processing happen
    (sleep 0.1)
    
    ;; Check updated metrics
    (let ((metrics (calculate-kernel-performance kernel)))
      (is (>= (gethash :message-count metrics 0) 1)
          "Should have processed at least one message")
      (is (> (gethash :processing-time metrics 0) 0)
          "Processing time should be greater than 0")
      (is (>= (gethash :uptime metrics 0) 0)
          "Uptime should be tracked"))
    
    ;; Deactivate when done
    (deactivate-microkernel kernel)))

;;;; System Management Tests
(test microkernel-system-management
  "Test global system management functions"
  ;; Start with clean state
  (shutdown-microkernel-system)
  
  ;; Initialize
  (is-true (initialize-microkernel-system))
  (is-true synth-u-core::*system-running*)
  
  ;; Create some kernels
  (make-microkernel "sys-test-1" :vision-core)
  (make-microkernel "sys-test-2" :linguistic-init)
  
  ;; Check counts
  (is (= 2 (hash-table-count *active-kernels*)))
  
  ;; Shutdown
  (is-true (shutdown-microkernel-system))
  (is-false synth-u-core::*system-running*)
  
  ;; Should be empty after shutdown
  (is (= 0 (hash-table-count *active-kernels*))))

;;;; Role-specific Processing Tests
(test role-specific-processing
  "Test that different roles process differently"
  (cleanup-test-kernels)
  
  (let ((vision-kernel (make-microkernel "vision-test" :vision-core))
        (linguistic-kernel (make-microkernel "linguistic-test" :linguistic-init))
        (network-kernel (make-microkernel "network-test" :network-core))
        (orchestration-kernel (make-microkernel "orchestration-test" :orchestration))
        (generic-kernel (make-microkernel "generic-test" :generic)))
    
    ;; Activate all
    (activate-microkernel vision-kernel)
    (activate-microkernel linguistic-kernel)
    (activate-microkernel network-kernel)
    (activate-microkernel orchestration-kernel)
    (activate-microkernel generic-kernel)
    
    ;; Process same input for all
    (let ((test-input '(:test-data "sample")))
      (process-input vision-kernel test-input)
      (process-input linguistic-kernel test-input)
      (process-input network-kernel test-input)
      (process-input orchestration-kernel test-input)
      (process-input generic-kernel test-input))
    
    ;; Let processing happen
    (sleep 0.1)
    
    ;; Check that outputs are different based on role
    (let ((vision-output (first (microkernel-output-queue vision-kernel)))
          (linguistic-output (first (microkernel-output-queue linguistic-kernel)))
          (network-output (first (microkernel-output-queue network-kernel)))
          (orchestration-output (first (microkernel-output-queue orchestration-kernel)))
          (generic-output (first (microkernel-output-queue generic-kernel))))
      
      (is (getf vision-output :visual-features)
          "Vision kernel should produce visual features")
      
      (is (getf linguistic-output :linguistic-features)
          "Linguistic kernel should produce linguistic features")
      
      (is (getf network-output :network-features)
          "Network kernel should produce network features")
      
      (is (getf orchestration-output :orchestration-result)
          "Orchestration kernel should produce orchestration results")
      
      (is (getf generic-output :generic-result)
          "Generic kernel should produce generic results"))
    
    ;; Deactivate all
    (deactivate-microkernel vision-kernel)
    (deactivate-microkernel linguistic-kernel)
    (deactivate-microkernel network-kernel)
    (deactivate-microkernel orchestration-kernel)
    (deactivate-microkernel generic-kernel)))

;;;; Export a function to run all tests
(defun run-core-tests ()
  "Run all core microkernel tests"
  (run! 'core-microkernel-tests))

;;;; Export the test suite
(export '(run-core-tests))
