;;;; -----------------------------------------------------
;;;; MODULE: genome-demo.lisp
;;;; DESCRIPTION: Demonstration script for the Symbolic Genome
;;;; DEPENDENCIES: synth-u-genome
;;;; -----------------------------------------------------

(defpackage :synth-u-genome-demo
  (:use :cl :synth-u-genome)
  (:export #:run-genome-demo))

(in-package :synth-u-genome-demo)

(defun run-genome-demo ()
  "Run a demonstration of the Synth-U symbolic genome system.
   This creates genomes, mutates them, and shows how they evolve."
  
  (format t "~%=== SYNTH-U SYMBOLIC GENOME DEMONSTRATION ===~%")
  
  ;; Create initial genomes
  (format t "~%Creating base genomes...~%")
  
  (let ((vision-genome (make-genome "vision-genome"
                                   :capabilities '(:visual-processing :pattern-recognition)
                                   :traits (list 
                                            (cons :structural 
                                                  (list :name "visual-structure"
                                                        :strength 0.8
                                                        :flexibility 0.6))
                                            (cons :functional
                                                  (list :name "edge-detection"
                                                        :strength 0.9
                                                        :flexibility 0.4))
                                            (cons :behavioral
                                                  (list :name "focus-attention"
                                                        :strength 0.7
                                                        :flexibility 0.7)))
                                   :mutation-rate 0.2))
        
        (language-genome (make-genome "language-genome"
                                     :capabilities '(:text-processing :language-understanding)
                                     :traits (list
                                              (cons :structural
                                                    (list :name "language-structure"
                                                          :strength 0.7
                                                          :flexibility 0.8))
                                              (cons :functional
                                                    (list :name "token-processing"
                                                          :strength 0.85
                                                          :flexibility 0.5))
                                              (cons :cognitive
                                                    (list :name "semantic-analysis"
                                                          :strength 0.9
                                                          :flexibility 0.3)))
                                     :mutation-rate 0.15)))
    
    ;; Display basic genome information
    (format t "~%Vision Genome:~%")
    (format t "  ID: ~A~%" (genome-id vision-genome))
    (format t "  Capabilities: ~A~%" (genome-capabilities vision-genome))
    (format t "  Mutation Rate: ~A~%" (genome-mutation-rate vision-genome))
    (format t "  Traits:~%")
    (maphash (lambda (category traits)
               (format t "    ~A:~%" category)
               (dolist (trait traits)
                 (format t "      - ~A (strength: ~A, flexibility: ~A)~%" 
                         (getf trait :name)
                         (getf trait :strength)
                         (getf trait :flexibility))))
             (genome-traits vision-genome))
    
    (format t "~%Language Genome:~%")
    (format t "  ID: ~A~%" (genome-id language-genome))
    (format t "  Capabilities: ~A~%" (genome-capabilities language-genome))
    (format t "  Mutation Rate: ~A~%" (genome-mutation-rate language-genome))
    (format t "  Traits:~%")
    (maphash (lambda (category traits)
               (format t "    ~A:~%" category)
               (dolist (trait traits)
                 (format t "      - ~A (strength: ~A, flexibility: ~A)~%" 
                         (getf trait :name)
                         (getf trait :strength)
                         (getf trait :flexibility))))
             (genome-traits language-genome))
    
    ;; Demonstrate genome mutation
    (format t "~%Demonstrating genome mutation...~%")
    (format t "Mutating vision genome three times:~%")
    
    (dotimes (i 3)
      (let ((mutation-type (nth (random (length synth-u-genome::*mutation-types*))
                               synth-u-genome::*mutation-types*)))
        (format t "  Mutation ~A: ~A~%" (1+ i) mutation-type)
        (mutate-genome vision-genome :specific-mutation mutation-type)))
    
    (format t "~%Vision Genome after mutations:~%")
    (format t "  Traits:~%")
    (maphash (lambda (category traits)
               (format t "    ~A:~%" category)
               (dolist (trait traits)
                 (format t "      - ~A (strength: ~A, flexibility: ~A)~%" 
                         (getf trait :name)
                         (getf trait :strength)
                         (getf trait :flexibility))))
             (genome-traits vision-genome))
    
    ;; Demonstrate genome replication
    (format t "~%Demonstrating genome replication...~%")
    (let ((vision-replica (replicate-genome vision-genome 
                                           :mutate t
                                           :mutation-count 1)))
      
      (format t "Created replica of vision genome:~%")
      (format t "  ID: ~A~%" (genome-id vision-replica))
      (format t "  Traits:~%")
      (maphash (lambda (category traits)
                 (format t "    ~A:~%" category)
                 (dolist (trait traits)
                   (format t "      - ~A (strength: ~A, flexibility: ~A)~%" 
                           (getf trait :name)
                           (getf trait :strength)
                           (getf trait :flexibility))))
               (genome-traits vision-replica)))
    
    ;; Demonstrate genome merging
    (format t "~%Demonstrating genome merging...~%")
    (format t "Calculating compatibility between vision and language genomes...~%")
    
    (let ((compatibility (calculate-compatibility vision-genome language-genome)))
      (format t "  Compatibility score: ~,2F~%" compatibility)
      (format t "  Compatible? ~A~%" 
              (if (compatible-p vision-genome language-genome :threshold 0.5)
                  "Yes" "No"))
      
      (format t "~%Merging genomes to create hybrid genome...~%")
      (let ((hybrid-genome (merge-genomes vision-genome language-genome)))
        
        (format t "Hybrid Genome:~%")
        (format t "  ID: ~A~%" (genome-id hybrid-genome))
        (format t "  Capabilities: ~A~%" (genome-capabilities hybrid-genome))
        (format t "  Mutation Rate: ~A~%" (genome-mutation-rate hybrid-genome))
        (format t "  Traits:~%")
        (maphash (lambda (category traits)
                   (format t "    ~A:~%" category)
                   (dolist (trait traits)
                     (format t "      - ~A (strength: ~A, flexibility: ~A)~%" 
                             (getf trait :name)
                             (getf trait :strength)
                             (getf trait :flexibility))))
                 (genome-traits hybrid-genome))
        
        ;; Demonstrate genome analysis
        (format t "~%Analyzing hybrid genome...~%")
        (let ((analysis (analyze-genome hybrid-genome)))
          
          (format t "  ID: ~A~%" (gethash :id analysis))
          (format t "  Age: ~A seconds~%" (gethash :age analysis))
          (format t "  Capabilities: ~A (~A total)~%" 
                  (gethash :capabilities analysis)
                  (gethash :capability-count analysis))
          (format t "  Total traits: ~A~%" (gethash :total-traits analysis))
          
          (format t "  Trait counts by category:~%")
          (maphash (lambda (category count)
                     (format t "    ~A: ~A~%" category count))
                   (gethash :trait-counts analysis))
          
          (format t "  Average trait strengths by category:~%")
          (maphash (lambda (category strength)
                     (format t "    ~A: ~,2F~%" category strength))
                   (gethash :trait-strengths analysis))
          
          (format t "  Evolution events: ~A~%" (gethash :evolution-events analysis))
          
          (let ((last-event (gethash :last-evolution analysis)))
            (format t "  Last evolution event: ~A~%" (getf last-event :event)))))))
  
  (format t "~%=== DEMONSTRATION COMPLETE ===~%")
  
  ;; Return success
  :demo-complete)

;;;; Export the demo function
(export '(run-genome-demo))
