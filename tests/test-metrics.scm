#!/usr/bin/env guile
!#

;;; Test suite for enhanced metrics validation module
;;; Tests both existing and new statistical validation metrics

(add-to-load-path ".")
(use-modules (attribution-graphs validation metrics)
             (attribution-graphs graph structure)
             (attribution-graphs graph attribution)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-43)
             (ice-9 format))

;;; =============================================================================
;;; TEST UTILITIES
;;; =============================================================================

(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define (test-assert name condition)
  "Assert that condition is true"
  (set! *test-count* (+ *test-count* 1))
  (if condition
      (begin
        (set! *test-passed* (+ *test-passed* 1))
        (format #t "✓ PASS: ~a~%" name))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (format #t "✗ FAIL: ~a~%" name))))

(define (test-approximate name actual expected tolerance)
  "Test that actual is within tolerance of expected"
  (test-assert name
    (< (abs (- actual expected)) tolerance)))

(define (print-test-summary)
  "Print final test results"
  (format #t "~%Test Summary: ~a/~a passed (~a failed)~%"
          *test-passed* *test-count* *test-failed*))

;;; =============================================================================
;;; TEST DATA GENERATION
;;; =============================================================================

(define (make-test-graph n-nodes n-edges)
  "Create a test attribution graph"
  (let ((graph (make-attribution-graph)))
    ;; Add nodes
    (do ((i 0 (+ i 1)))
        ((>= i n-nodes))
      (add-node! graph (make-node (string->symbol (format #f "node~a" i))
                                  'feature
                                  (random:uniform)
                                  '())))
    ;; Add edges
    (do ((i 0 (+ i 1)))
        ((>= i n-edges))
      (let* ((nodes (graph-nodes graph))
             (source (node-id (list-ref nodes (random (length nodes)))))
             (target (node-id (list-ref nodes (random (length nodes)))))
             (weight (- (* 2 (random:uniform)) 1)))  ; Weight in [-1, 1]
        (add-edge! graph (make-edge source target weight))))
    graph))

(define (make-sparse-graph)
  "Create a sparse test graph"
  (make-test-graph 10 5))

(define (make-dense-graph)
  "Create a dense test graph"
  (make-test-graph 10 45))

(define (make-empty-graph)
  "Create an empty graph"
  (make-attribution-graph))

;;; =============================================================================
;;; BASIC METRIC TESTS
;;; =============================================================================

(define (test-basic-metrics)
  "Test original metrics functionality"
  (format #t "~%=== Testing Basic Metrics ===~%")
  
  ;; Test sparsity
  (let* ((sparse (make-sparse-graph))
         (dense (make-dense-graph))
         (empty (make-empty-graph))
         (sparse-val (graph-sparsity sparse))
         (dense-val (graph-sparsity dense))
         (empty-val (graph-sparsity empty)))
    (test-assert "Sparse graph has high sparsity" (> sparse-val 0.5))
    (test-assert "Dense graph has lower sparsity" (< dense-val sparse-val))
    (test-assert "Empty graph has sparsity 1.0" (= empty-val 1.0)))
  
  ;; Test concentration
  (let* ((graph (make-test-graph 5 10))
         (concentration (attribution-concentration graph)))
    (test-assert "Concentration is valid ratio" (and (>= concentration 0) (<= concentration 1))))
  
  ;; Test path coverage
  (let* ((graph (make-test-graph 5 8))
         (test-paths '((node0 node1 node2) (node3 node4)))
         (coverage (path-coverage graph test-paths)))
    (test-assert "Coverage is valid ratio" (and (>= coverage 0) (<= coverage 1)))))

;;; =============================================================================
;;; STATISTICAL UTILITY TESTS
;;; =============================================================================

(define (test-statistical-utilities)
  "Test statistical utility functions"
  (format #t "~%=== Testing Statistical Utilities ===~%")
  
  ;; Test confidence interval calculation
  (let* ((samples '(1.0 2.0 3.0 4.0 5.0))
         (ci (confidence-interval samples 0.05))
         (mean (assoc-ref ci 'mean))
         (lower (assoc-ref ci 'lower))
         (upper (assoc-ref ci 'upper)))
    (test-approximate "Confidence interval mean" mean 3.0 0.1)
    (test-assert "Lower bound < upper bound" (< lower upper))
    (test-assert "Mean within bounds" (and (<= lower mean) (<= mean upper))))
  
  ;; Test percentile calculation
  (let* ((values '(1 2 3 4 5 6 7 8 9 10))
         (p50 (percentile values 0.5))
         (p90 (percentile values 0.9)))
    (test-approximate "50th percentile" p50 5.5 1.0)
    (test-assert "90th percentile > 50th percentile" (> p90 p50)))
  
  ;; Test permutation test
  (let* ((observed 0.8)
         (null-values '(0.1 0.2 0.3 0.4 0.5))
         (result (permutation-test-simple observed null-values 0.05))
         (p-value (assoc-ref result 'p-value))
         (significant (assoc-ref result 'significant)))
    (test-assert "P-value is valid probability" (and (>= p-value 0) (<= p-value 1)))
    (test-assert "High observed value is significant" significant)))

;;; =============================================================================
;;; ENHANCED METRIC TESTS
;;; =============================================================================

(define (test-enhanced-metrics)
  "Test new statistical validation metrics"
  (format #t "~%=== Testing Enhanced Metrics ===~%")
  
  ;; Test attribution coherence
  (let* ((graph (make-test-graph 8 15))
         (coherence (attribution-coherence graph)))
    (test-assert "Coherence is valid ratio" (and (>= coherence 0) (<= coherence 1))))
  
  ;; Test circuit stability (simplified test)
  (let* ((graph (make-test-graph 6 10))
         (circuits '((node0 node1 node2) (node3 node4 node5)))
         (test-inputs '("input1" "input2" "input3"))
         (stability (circuit-stability graph circuits test-inputs)))
    (test-assert "Stability is valid ratio" (and (>= stability 0) (<= stability 1))))
  
  ;; Test causal fidelity (with mock model)
  (let* ((graph (make-test-graph 5 8))
         (mock-model 'dummy-model)
         (test-inputs '("test1" "test2"))
         (fidelity (causal-fidelity graph mock-model test-inputs)))
    (test-assert "Fidelity is valid ratio" (and (>= fidelity 0) (<= fidelity 1))))
  
  ;; Test circuit confidence scoring
  (let* ((graph (make-test-graph 7 12))
         (circuits '((node0 node1) (node2 node3 node4)))
         (mock-model 'dummy-model)
         (test-inputs '("input1" "input2"))
         (confidence (circuit-confidence graph circuits mock-model test-inputs))
         (overall (assoc-ref confidence 'overall-confidence)))
    (test-assert "Overall confidence is valid" (and (>= overall 0) (<= overall 1)))
    (test-assert "Confidence has stability component" 
                 (assoc-ref confidence 'stability))
    (test-assert "Confidence has coherence component" 
                 (assoc-ref confidence 'coherence))
    (test-assert "Confidence has fidelity component" 
                 (assoc-ref confidence 'fidelity))))

;;; =============================================================================
;;; SIGNIFICANCE TESTING TESTS
;;; =============================================================================

(define (test-significance-testing)
  "Test significance testing framework"
  (format #t "~%=== Testing Significance Testing ===~%")
  
  ;; Create test graphs
  (let* ((observed-graph (make-test-graph 6 10))
         (null-graphs (map (lambda (_) (make-test-graph 6 2)) (iota 5)))
         (sig-results (significance-tests observed-graph null-graphs 0.05))
         (sparsity-test (assoc-ref sig-results 'sparsity))
         (concentration-test (assoc-ref sig-results 'concentration)))
    
    (test-assert "Sparsity test has p-value" 
                 (assoc-ref sparsity-test 'p-value))
    (test-assert "Concentration test has p-value" 
                 (assoc-ref concentration-test 'p-value))
    (test-assert "Tests have significance flags" 
                 (and (assoc-ref sparsity-test 'significant)
                      (assoc-ref concentration-test 'significant)))))

;;; =============================================================================
;;; METHOD COMPARISON TESTS
;;; =============================================================================

(define (test-method-comparison)
  "Test enhanced method comparison"
  (format #t "~%=== Testing Method Comparison ===~%")
  
  (let* ((graph1 (make-sparse-graph))
         (graph2 (make-dense-graph))
         (graphs (list graph1 graph2))
         (labels '("sparse-method" "dense-method"))
         (test-inputs '("input1" "input2"))
         (comparison (compare-methods-enhanced graphs labels test-inputs)))
    
    (test-assert "Comparison has sparse method results"
                 (hash-ref comparison "sparse-method"))
    (test-assert "Comparison has dense method results"
                 (hash-ref comparison "dense-method"))
    
    ;; Check that results include confidence intervals
    (let ((sparse-results (hash-ref comparison "sparse-method")))
      (test-assert "Sparse method has sparsity CI"
                   (assoc-ref sparse-results 'sparsity-ci))
      (test-assert "Sparse method has concentration CI"
                   (assoc-ref sparse-results 'concentration-ci)))))

;;; =============================================================================
;;; EDGE CASE TESTS
;;; =============================================================================

(define (test-edge-cases)
  "Test edge cases and error handling"
  (format #t "~%=== Testing Edge Cases ===~%")
  
  ;; Empty graph tests
  (let ((empty (make-empty-graph)))
    (test-assert "Empty graph sparsity is 1.0" 
                 (= (graph-sparsity empty) 1.0))
    (test-assert "Empty graph coherence is valid" 
                 (and (>= (attribution-coherence empty) 0)
                      (<= (attribution-coherence empty) 1))))
  
  ;; Single node graph
  (let ((single (make-attribution-graph)))
    (add-node! single (make-node 'lonely 'feature 1.0 '()))
    (test-assert "Single node graph is sparse" 
                 (= (graph-sparsity single) 1.0)))
  
  ;; Stability with single input
  (let* ((graph (make-test-graph 4 6))
         (circuits '((node0 node1)))
         (single-input '("only-input"))
         (stability (circuit-stability graph circuits single-input)))
    (test-assert "Single input is trivially stable" 
                 (= stability 1.0))))

;;; =============================================================================
;;; INTEGRATION TESTS
;;; =============================================================================

(define (test-integration)
  "Test integration between metrics and graph structures"
  (format #t "~%=== Testing Integration ===~%")
  
  ;; Test metrics on various graph topologies
  (let* ((linear-graph (make-test-graph 5 4))  ; Linear chain
         (star-graph (make-test-graph 6 5))     ; Star topology
         (dense-graph (make-dense-graph)))      ; Dense graph
    
    ;; All graphs should produce valid metric values
    (for-each (lambda (graph name)
                (let ((sparsity (graph-sparsity graph))
                      (concentration (attribution-concentration graph))
                      (coherence (attribution-coherence graph)))
                  (test-assert (format #f "~a sparsity valid" name)
                               (and (>= sparsity 0) (<= sparsity 1)))
                  (test-assert (format #f "~a concentration valid" name)
                               (and (>= concentration 0) (<= concentration 1)))
                  (test-assert (format #f "~a coherence valid" name)
                               (and (>= coherence 0) (<= coherence 1)))))
              (list linear-graph star-graph dense-graph)
              '("linear" "star" "dense"))))

;;; =============================================================================
;;; MAIN TEST RUNNER
;;; =============================================================================

(define (run-all-tests)
  "Run all test suites"
  (format #t "Attribution Graphs Metrics Test Suite~%")
  (format #t "=====================================~%")
  
  ;; Initialize random seed for reproducible tests
  (set! *random-state* (seed->random-state 12345))
  
  ;; Run test suites
  (test-basic-metrics)
  (test-statistical-utilities)
  (test-enhanced-metrics)
  (test-significance-testing)
  (test-method-comparison)
  (test-edge-cases)
  (test-integration)
  
  ;; Print summary
  (print-test-summary)
  
  ;; Exit with error code if tests failed
  (if (> *test-failed* 0)
      (exit 1)
      (exit 0)))

;; Run tests if this file is executed directly
(when (and (defined? 'command-line) (not (null? (command-line))))
  (run-all-tests))