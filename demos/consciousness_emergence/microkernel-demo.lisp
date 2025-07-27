;;;; -----------------------------------------------------
;;;; MODULE: microkernel-demo.lisp
;;;; DESCRIPTION: Demonstration script for the Core Microkernel
;;;; DEPENDENCIES: synth-u-core
;;;; -----------------------------------------------------

(defpackage :synth-u-demo
  (:use :cl :synth-u-core)
  (:export #:run-microkernel-demo))

(in-package :synth-u-demo)

(defun run-microkernel-demo ()
  "Run a demonstration of the Synth-U microkernel system.
   This creates a simple system with multiple kernels of different roles
   and shows their interaction."
  
  ;; Initialize the system
  (format t "~%=== SYNTH-U MICROKERNEL SYSTEM DEMONSTRATION ===~%")
  (format t "Initializing microkernel system...~%")
  (initialize-microkernel-system)
  
  ;; Create demonstration kernels
  (format t "Creating demonstration kernels...~%")
  
  (let ((vision-kernel (make-microkernel "vision-demo" :vision-core))
        (linguistic-kernel (make-microkernel "linguistic-demo" :linguistic-init))
        (network-kernel (make-microkernel "network-demo" :network-core))
        (orchestrator (make-microkernel "orchestrator" :orchestration)))
    
    ;; Display created kernels
    (format t "Created kernels:~%")
    (format t "  - ~A: ~A~%" (microkernel-id vision-kernel) (microkernel-role vision-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id linguistic-kernel) (microkernel-role linguistic-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id network-kernel) (microkernel-role network-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id orchestrator) (microkernel-role orchestrator))
    
    ;; Activate all kernels
    (format t "~%Activating kernels...~%")
    (activate-microkernel vision-kernel)
    (activate-microkernel linguistic-kernel)
    (activate-microkernel network-kernel)
    (activate-microkernel orchestrator)
    
    ;; Display status
    (format t "Kernel statuses after activation:~%")
    (format t "  - ~A: ~A~%" (microkernel-id vision-kernel) (microkernel-status vision-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id linguistic-kernel) (microkernel-status linguistic-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id network-kernel) (microkernel-status network-kernel))
    (format t "  - ~A: ~A~%" (microkernel-id orchestrator) (microkernel-status orchestrator))
    
    ;; Send messages to kernels
    (format t "~%Sending input messages to each kernel...~%")
    (process-input vision-kernel '(:visual-data "simulated camera input"))
    (process-input linguistic-kernel '(:text "Hello, Synth-U system!"))
    (process-input network-kernel '(:network-event :connection-request))
    
    ;; Send inter-kernel messages
    (format t "~%Demonstrating inter-kernel communication...~%")
    (send-message-to-kernel "vision-demo" "orchestrator" '(:status :processing))
    (send-message-to-kernel "linguistic-demo" "orchestrator" '(:status :ready))
    (send-message-to-kernel "network-demo" "orchestrator" '(:status :waiting))
    
    ;; Give time for processing
    (format t "Processing messages...~%")
    (sleep 0.5)
    
    ;; Show output queues
    (format t "~%Output queue contents:~%")
    (format t "  - Vision kernel: ~A~%" (microkernel-output-queue vision-kernel))
    (format t "  - Linguistic kernel: ~A~%" (microkernel-output-queue linguistic-kernel))
    (format t "  - Network kernel: ~A~%" (microkernel-output-queue network-kernel))
    (format t "  - Orchestrator: ~A~%" (microkernel-output-queue orchestrator))
    
    ;; Demonstrate role reassignment
    (format t "~%Demonstrating role reassignment...~%")
    (format t "Reassigning network kernel from ~A to :orchestration~%" 
            (microkernel-role network-kernel))
    
    (reassign-role network-kernel :orchestration)
    
    (format t "New role: ~A~%" (microkernel-role network-kernel))
    
    ;; Show performance metrics
    (format t "~%Performance metrics:~%")
    (let ((metrics (calculate-kernel-performance vision-kernel)))
      (format t "Vision kernel metrics:~%")
      (maphash (lambda (k v)
                 (format t "  - ~A: ~A~%" k v))
               metrics))
    
    ;; Deactivate all kernels
    (format t "~%Deactivating kernels...~%")
    (deactivate-microkernel vision-kernel)
    (deactivate-microkernel linguistic-kernel)
    (deactivate-microkernel network-kernel)
    (deactivate-microkernel orchestrator)
    
    ;; Shutdown the system
    (format t "~%Shutting down microkernel system...~%")
    (shutdown-microkernel-system)
    
    (format t "~%=== DEMONSTRATION COMPLETE ===~%")
    
    ;; Return success
    :demo-complete))

;;;; Export the demo function
(export '(run-microkernel-demo))
