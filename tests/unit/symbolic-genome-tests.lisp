;;;; -----------------------------------------------------
;;;; MODULE: symbolic-genome-tests.lisp
;;;; DESCRIPTION: Tests for the Symbolic Genome system
;;;; DEPENDENCIES: FiveAM, synth-u-genome
;;;; -----------------------------------------------------

(defpackage :synth-u-genome-tests
  (:use :cl :fiveam :synth-u-genome)
  (:export #:run-genome-tests))

(in-package :synth-u-genome-tests)

;;;; Test Suite Setup
(def-suite genome-tests
  :description "Test suite for the Symbolic Genome functionality")

(in-suite genome-tests)

;;;; Basic Genome Creation Tests
(test genome-creation
  "Test basic genome creation and properties"
  
  ;; Create a test genome
  (let ((genome (make-genome "test-genome-1"
                            :capabilities '(:visual-processing :pattern-recognition)
                            :mutation-rate 0.1)))
    ;; Check properties
    (is (string= (genome-id genome) "test-genome-1")
        "Genome ID should match")
    
    (is (string= (genome-version genome) synth-u-genome::+genome-version+)
        "Genome version should match constant")
    
    (is (equal (genome-capabilities genome) '(:visual-processing :pattern-recognition))
        "Genome capabilities should match")
    
    (is (= (genome-mutation-rate genome) 0.1)
        "Genome mutation rate should match")
    
    (is (hash-table-p (genome-traits genome))
        "Genome traits should be a hash table")
    
    (is (listp (genome-evolution-history genome))
        "Genome evolution history should be a list")
    
    (is (= (length (genome-evolution-history genome)) 1)
        "New genome should have one evolution event (creation)")))

(test genome-invalid-creation
  "Test validation during genome creation"
  
  ;; Empty ID should fail
  (signals error
    (make-genome ""))
  
  ;; Invalid mutation rate should fail
  (signals error
    (make-genome "test-invalid" :mutation-rate 1.5)))

;;;; Trait Manipulation Tests
(test trait-manipulation
  "Test adding, removing, and modifying traits"
  
  (let ((genome (make-genome "test-traits")))
    ;; Initially no traits
    (is (= (hash-table-count (genome-traits genome)) 0)
        "New genome should have no traits")
    
    ;; Add a trait
    (is-true (add-trait genome :structural 
                      (list :name "test-trait" :strength 0.8 :flexibility 0.5))
              "Adding a trait should return T")
    
    ;; Verify trait was added
    (is (= (length (gethash :structural (genome-traits genome))) 1)
        "Should have one structural trait")
    
    ;; Extract the trait
    (let ((trait (extract-trait genome :structural "test-trait")))
      (is (not (null trait))
          "Should be able to extract the trait")
      (is (= (getf trait :strength) 0.8)
          "Trait strength should match"))
    
    ;; Modify the trait
    (is-true (modify-trait genome :structural "test-trait" 
                          (list :strength 0.9 :flexibility 0.6))
              "Modifying a trait should return T")
    
    ;; Verify modification
    (let ((trait (extract-trait genome :structural "test-trait")))
      (is (= (getf trait :strength) 0.9)
          "Trait strength should be updated")
      (is (= (getf trait :flexibility) 0.6)
          "Trait flexibility should be updated"))
    
    ;; Add another trait
    (add-trait genome :structural 
              (list :name "second-trait" :strength 0.7 :flexibility 0.4))
    
    ;; Verify two traits
    (is (= (length (gethash :structural (genome-traits genome))) 2)
        "Should have two structural traits")
    
    ;; Remove a trait
    (is-true (remove-trait genome :structural "test-trait")
              "Removing a trait should return T")
    
    ;; Verify removal
    (is (= (length (gethash :structural (genome-traits genome))) 1)
        "Should have one structural trait after removal")
    
    ;; Remove non-existent trait should fail
    (is-false (remove-trait genome :structural "nonexistent-trait")
               "Removing a non-existent trait should return NIL")))

(test trait-category-validation
  "Test validation of trait categories"
  
  (let ((genome (make-genome "test-categories")))
    ;; Valid categories should work
    (is-true (add-trait genome :structural (list :name "struct-trait"))
              "Adding trait to :structural category should work")
    
    (is-true (add-trait genome :functional (list :name "func-trait"))
              "Adding trait to :functional category should work")
    
    (is-true (add-trait genome :behavioral (list :name "behav-trait"))
              "Adding trait to :behavioral category should work")
    
    (is-true (add-trait genome :cognitive (list :name "cog-trait"))
              "Adding trait to :cognitive category should work")
    
    (is-true (add-trait genome :adaptive (list :name "adapt-trait"))
              "Adding trait to :adaptive category should work")
    
    ;; Invalid category should signal error
    (signals error
      (add-trait genome :invalid-category (list :name "invalid-trait")))))

;;;; Mutation Tests
(test genome-mutation
  "Test genome mutation capabilities"
  
  (let ((genome (make-genome "test-mutation")))
    ;; Add some initial traits
    (add-trait genome :structural (list :name "struct-1" :strength 0.5 :flexibility 0.5))
    (add-trait genome :functional (list :name "func-1" :strength 0.5 :flexibility 0.5))
    
    ;; Record initial state
    (let ((initial-struct-traits (length (gethash :structural (genome-traits genome))))
          (initial-func-traits (length (gethash :functional (genome-traits genome))))
          (initial-history-length (length (genome-evolution-history genome))))
      
      ;; Apply a mutation
      (is-true (mutate-genome genome)
                "Mutating a genome should return T")
      
      ;; Verify mutation happened
      (is (> (length (genome-evolution-history genome)) initial-history-length)
          "Evolution history should be updated after mutation")
      
      ;; Apply specific mutations
      
      ;; Add trait mutation
      (is-true (mutate-genome genome :specific-mutation :add-trait)
                "Add trait mutation should return T")
      
      ;; Verify trait count increased somewhere
      (let ((total-traits-after (loop for category in synth-u-genome::*trait-categories*
                                     sum (length (gethash category (genome-traits genome) nil)))))
        (is (> total-traits-after (+ initial-struct-traits initial-func-traits))
            "Total trait count should increase after add-trait mutation"))
      
      ;; Remove trait mutation
      (is-true (mutate-genome genome :specific-mutation :remove-trait)
                "Remove trait mutation should return T")
      
      ;; Modify trait mutation
      (is-true (mutate-genome genome :specific-mutation :modify-trait)
                "Modify trait mutation should return T"))))

(test genome-mutation-complex
  "Test more complex genome mutations"
  
  (let ((genome (make-genome "test-complex-mutation")))
    ;; Add several traits for testing complex mutations
    (add-trait genome :structural (list :name "struct-1" :strength 0.5 :flexibility 0.5))
    (add-trait genome :structural (list :name "struct-2" :strength 0.6 :flexibility 0.4))
    (add-trait genome :functional (list :name "func-1" :strength 0.7 :flexibility 0.3))
    (add-trait genome :functional (list :name "func-2" :strength 0.8 :flexibility 0.2))
    
    ;; Test merge traits mutation
    (is-true (mutate-genome genome :specific-mutation :merge-traits)
              "Merge traits mutation should return T")
    
    ;; Test split trait mutation
    (is-true (mutate-genome genome :specific-mutation :split-trait)
              "Split trait mutation should return T")))

;;;; Replication Tests
(test genome-replication
  "Test genome replication capabilities"
  
  (let* ((original (make-genome "original-genome"
                               :capabilities '(:vision :language)
                               :mutation-rate 0.2))
         ;; Add some traits to original
         (dummy1 (add-trait original :structural 
                           (list :name "struct-trait" :strength 0.7 :flexibility 0.3)))
         (dummy2 (add-trait original :functional 
                           (list :name "func-trait" :strength 0.6 :flexibility 0.4)))
         ;; Create replica without mutation
         (replica1 (replicate-genome original :mutate nil))
         ;; Create replica with mutation
         (replica2 (replicate-genome original :mutate t :mutation-count 2)))
    
    ;; Verify replica without mutation
    (is (string/= (genome-id original) (genome-id replica1))
        "Replica should have different ID")
    
    (is (equal (genome-capabilities original) (genome-capabilities replica1))
        "Replica should have same capabilities")
    
    (is (= (genome-mutation-rate original) (genome-mutation-rate replica1))
        "Replica should have same mutation rate")
    
    ;; Check traits were copied
    (let ((original-struct-traits (gethash :structural (genome-traits original)))
          (replica-struct-traits (gethash :structural (genome-traits replica1))))
      
      (is (= (length original-struct-traits) (length replica-struct-traits))
          "Replica should have same number of structural traits")
      
      (let ((original-trait (first original-struct-traits))
            (replica-trait (first replica-struct-traits)))
        (is (string= (getf original-trait :name) (getf replica-trait :name))
            "Trait names should match")
        (is (= (getf original-trait :strength) (getf replica-trait :strength))
            "Trait strengths should match")))
    
    ;; Verify replica with mutation has evolution events
    (is (> (length (genome-evolution-history replica2))
           (length (genome-evolution-history replica1)))
        "Mutated replica should have more evolution events")))

;;;; Merge Tests
(test genome-merging
  "Test merging of two genomes"
  
  (let* ((genome1 (make-genome "genome-1"
                              :capabilities '(:vision :pattern-recognition)
                              :mutation-rate 0.1))
         (genome2 (make-genome "genome-2"
                              :capabilities '(:language :reasoning)
                              :mutation-rate 0.3))
         ;; Add traits to genome1
         (dummy1 (add-trait genome1 :structural 
                           (list :name "struct-1" :strength 0.7 :flexibility 0.3)))
         (dummy2 (add-trait genome1 :functional 
                           (list :name "common-trait" :strength 0.6 :flexibility 0.4)))
         ;; Add traits to genome2
         (dummy3 (add-trait genome2 :behavioral 
                           (list :name "behav-1" :strength 0.5 :flexibility 0.5)))
         (dummy4 (add-trait genome2 :functional 
                           (list :name "common-trait" :strength 0.8 :flexibility 0.2)))
         ;; Merge the genomes
         (merged (merge-genomes genome1 genome2)))
    
    ;; Verify merged genome properties
    (is (search "merged" (genome-id merged))
        "Merged genome ID should contain 'merged'")
    
    ;; Check capabilities were combined
    (let ((merged-capabilities (genome-capabilities merged)))
      (is (member :vision merged-capabilities)
          "Merged genome should have :vision capability")
      (is (member :language merged-capabilities)
          "Merged genome should have :language capability")
      (is (= (length merged-capabilities) 4)
          "Merged genome should have all 4 capabilities"))
    
    ;; Check mutation rate was averaged
    (is (= (genome-mutation-rate merged) 0.2)
        "Merged genome should have averaged mutation rate")
    
    ;; Check traits were combined
    (is (not (null (extract-trait merged :structural "struct-1")))
        "Merged genome should have structural trait from genome1")
    
    (is (not (null (extract-trait merged :behavioral "behav-1")))
        "Merged genome should have behavioral trait from genome2")
    
    ;; Check common trait was merged
    (let ((common-trait (extract-trait merged :functional "common-trait")))
      (is (not (null common-trait))
          "Merged genome should have common trait")
      
      ;; Strength should be averaged
      (is (= (getf common-trait :strength) 0.7)
          "Common trait strength should be averaged"))
    
    ;; Check evolution history
    (let ((last-event (first (genome-evolution-history merged))))
      (is (eq (getf last-event :event) :genome-merge)
          "Last evolution event should be genome merge")
      
      (is (member (genome-id genome1) (getf last-event :parents) :test #'string=)
          "Parent list should include genome1 ID")
      
      (is (member (genome-id genome2) (getf last-event :parents) :test #'string=)
          "Parent list should include genome2 ID"))))

;;;; Compatibility Tests
(test genome-compatibility
  "Test genome compatibility calculations"
  
  (let* ((genome1 (make-genome "compat-1"
                              :capabilities '(:vision :pattern-recognition)))
         (genome2 (make-genome "compat-2"
                              :capabilities '(:vision :language)))
         (genome3 (make-genome "compat-3"
                              :capabilities '(:reasoning :planning)))
         ;; Add same traits to genome1 and genome2
         (dummy1 (add-trait genome1 :structural 
                           (list :name "common-struct" :strength 0.7)))
         (dummy2 (add-trait genome2 :structural 
                           (list :name "common-struct" :strength 0.6)))
         ;; Add different traits
         (dummy3 (add-trait genome1 :functional 
                           (list :name "unique-1" :strength 0.5)))
         (dummy4 (add-trait genome2 :functional 
                           (list :name "unique-2" :strength 0.8)))
         (dummy5 (add-trait genome3 :cognitive 
                           (list :name "unique-3" :strength 0.9))))
    
    ;; Calculate compatibility scores
    (let ((compat-1-2 (calculate-compatibility genome1 genome2))
          (compat-1-3 (calculate-compatibility genome1 genome3))
          (compat-2-3 (calculate-compatibility genome2 genome3)))
      
      ;; genome1 and genome2 share capabilities and traits
      (is (> compat-1-2 0.5)
          "Genomes with shared capabilities and traits should have high compatibility")
      
      ;; genome1 and genome3 share nothing
      (is (< compat-1-3 0.5)
          "Genomes with no shared capabilities or traits should have low compatibility")
      
      ;; Check compatibility predicate
      (is-true (compatible-p genome1 genome2 :threshold 0.5)
                "Genomes 1 and 2 should be compatible with threshold 0.5")
      
      (is-false (compatible-p genome1 genome3 :threshold 0.5)
                 "Genomes 1 and 3 should not be compatible with threshold 0.5"))))

;;;; Analysis Tests
(test genome-analysis
  "Test genome analysis capabilities"
  
  (let* ((genome (make-genome "analysis-test"
                             :capabilities '(:vision :language :reasoning)))
         ;; Add some traits
         (dummy1 (add-trait genome :structural 
                           (list :name "struct-1" :strength 0.7)))
         (dummy2 (add-trait genome :structural 
                           (list :name "struct-2" :strength 0.8)))
         (dummy3 (add-trait genome :functional 
                           (list :name "func-1" :strength 0.6)))
         (dummy4 (add-trait genome :cognitive 
                           (list :name "cog-1" :strength 0.9)))
         ;; Apply a mutation
         (dummy5 (mutate-genome genome))
         ;; Run analysis
         (analysis (analyze-genome genome)))
    
    ;; Check basic info
    (is (string= (gethash :id analysis) "analysis-test")
        "Analysis should contain correct ID")
    
    (is (= (gethash :capability-count analysis) 3)
        "Analysis should count 3 capabilities")
    
    ;; Check trait counts
    (is (= (gethash :total-traits analysis) 
           (+ (length (gethash :structural (genome-traits genome)))
              (length (gethash :functional (genome-traits genome)))
              (length (gethash :cognitive (genome-traits genome)))))
        "Total trait count should match sum of all traits")
    
    ;; Check trait strengths
    (let ((struct-strength (gethash :structural (gethash :trait-strengths analysis))))
      (is (not (null struct-strength))
          "Analysis should calculate structural trait strength")
      
      (is (<= 0.7 struct-strength 0.8)
          "Structural strength should be between 0.7 and 0.8"))
    
    ;; Check evolution events
    (is (>= (gethash :evolution-events analysis) 2)
        "Analysis should count at least 2 evolution events")
    
    (let ((last-event (gethash :last-evolution analysis)))
      (is (not (null last-event))
          "Analysis should include last evolution event"))))

;;;; Export a function to run all tests
(defun run-genome-tests ()
  "Run all symbolic genome tests"
  (run! 'genome-tests))

;;;; Export the test suite
(export '(run-genome-tests))
