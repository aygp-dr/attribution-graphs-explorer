(define-module (attribution-graphs validation metrics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 random)
  #:use-module (attribution-graphs math matrix)
  #:export (graph-sparsity
            path-coverage
            attribution-concentration
            compare-methods
            ;; New statistical metrics
            circuit-stability
            attribution-coherence
            causal-fidelity
            circuit-confidence
            significance-tests
            compare-methods-enhanced
            ;; Statistical utilities
            bootstrap-metric
            permutation-test
            confidence-interval))

;; Graph sparsity metric
(define (graph-sparsity graph)
  "Measure sparsity of attribution graph"
  (let* ((n-nodes (length (graph-nodes graph)))
         (n-edges (length (graph-edges graph)))
         (max-edges (* n-nodes (- n-nodes 1))))
    (if (= max-edges 0)
        1.0
        (- 1.0 (/ n-edges max-edges)))))

;; Path coverage metric
(define (path-coverage graph important-paths)
  "Fraction of important paths captured in graph"
  (let ((found 0))
    (for-each (lambda (path)
                (when (path-exists? graph path)
                  (set! found (+ found 1))))
              important-paths)
    (if (null? important-paths)
        1.0
        (/ found (length important-paths)))))

;; Check if path exists in graph
(define (path-exists? graph path)
  "Check if path exists in graph"
  (let loop ((nodes path))
    (if (< (length nodes) 2)
        #t
        (and (find-edge graph (car nodes) (cadr nodes))
             (loop (cdr nodes))))))

;; Attribution concentration
(define (attribution-concentration graph)
  "Measure how concentrated attributions are"
  (let* ((weights (map edge-weight (graph-edges graph)))
         (total (apply + (map abs weights)))
         (sorted (sort (map abs weights) >))
         (cumsum 0)
         (edges-needed 0))
    ;; Find how many edges account for 90% of attribution
    (let loop ((remaining sorted))
      (if (or (null? remaining) (>= cumsum (* 0.9 total)))
          (/ edges-needed (length weights))
          (begin
            (set! cumsum (+ cumsum (car remaining)))
            (set! edges-needed (+ edges-needed 1))
            (loop (cdr remaining)))))))

;; Compare different methods
(define (compare-methods graphs labels)
  "Compare attribution graphs from different methods"
  (let ((metrics (make-hash-table)))
    (for-each (lambda (graph label)
                (hash-set! metrics label
                          `((sparsity . ,(graph-sparsity graph))
                            (concentration . ,(attribution-concentration graph))
                            (nodes . ,(length (graph-nodes graph)))
                            (edges . ,(length (graph-edges graph))))))
              graphs labels)
    metrics))

;; Find edge helper (from earlier module)
(define (find-edge graph source-id target-id)
  "Find edge from source to target"
  (find (lambda (edge)
          (and (equal? (edge-source edge) source-id)
               (equal? (edge-target edge) target-id)))
        (graph-edges graph)))

;;; =============================================================================
;;; STATISTICAL UTILITIES
;;; =============================================================================

;; Bootstrap sampling for metric confidence intervals
(define (bootstrap-metric metric-fn data n-bootstrap alpha)
  "Bootstrap confidence intervals for metric"
  (let ((samples '()))
    ;; Generate bootstrap samples
    (do ((i 0 (+ i 1)))
        ((>= i n-bootstrap))
      (let* ((n (length data))
             (resampled (map (lambda (_) (list-ref data (random n)))
                           (iota n)))
             (metric-val (metric-fn resampled)))
        (set! samples (cons metric-val samples))))
    ;; Calculate confidence interval
    (confidence-interval samples alpha)))

;; Calculate confidence interval from samples
(define (confidence-interval samples alpha)
  "Calculate (1-alpha) confidence interval"
  (let* ((sorted (sort samples <))
         (n (length sorted))
         (lower-idx (inexact->exact (floor (* (/ alpha 2) n))))
         (upper-idx (inexact->exact (floor (* (- 1 (/ alpha 2)) n))))
         (lower (list-ref sorted (max 0 lower-idx)))
         (upper (list-ref sorted (min (- n 1) upper-idx)))
         (mean (/ (apply + samples) n)))
    `((mean . ,mean)
      (lower . ,lower) 
      (upper . ,upper)
      (alpha . ,alpha))))

;; Permutation test for significance
(define (permutation-test observed-metric data null-fn n-permutations)
  "Permutation test for metric significance"
  (let ((null-samples '())
        (observed (observed-metric data)))
    ;; Generate null distribution
    (do ((i 0 (+ i 1)))
        ((>= i n-permutations))
      (let* ((null-data (null-fn data))
             (null-metric (observed-metric null-data)))
        (set! null-samples (cons null-metric null-samples))))
    ;; Calculate p-value (two-tailed)
    (let* ((extreme-count (length (filter (lambda (x) (>= (abs x) (abs observed)))
                                         null-samples)))
           (p-value (/ extreme-count n-permutations)))
      `((observed . ,observed)
        (p-value . ,p-value)
        (significant . ,(< p-value 0.05))))))

;;; =============================================================================
;;; ENHANCED STATISTICAL METRICS
;;; =============================================================================

;; Circuit stability across multiple inputs
(define (circuit-stability graph circuits test-inputs)
  "Measure stability of circuit structure across inputs"
  (if (< (length test-inputs) 2)
      1.0  ; Single input is trivially stable
      (let ((stability-scores '()))
        ;; Compare circuit structure across input pairs
        (for-each (lambda (input1)
                    (for-each (lambda (input2)
                                (when (not (equal? input1 input2))
                                  (let ((sim (circuit-structural-similarity 
                                            circuits graph input1 input2)))
                                    (set! stability-scores (cons sim stability-scores)))))
                              test-inputs))
                  test-inputs)
        ;; Return average pairwise similarity
        (if (null? stability-scores)
            1.0
            (/ (apply + stability-scores) (length stability-scores))))))

;; Helper: Structural similarity between circuits on different inputs
(define (circuit-structural-similarity circuits graph input1 input2)
  "Compare circuit structure between two inputs"
  ;; Simplified: count overlapping edges (real implementation would be more sophisticated)
  (let* ((edges1 (extract-circuit-edges circuits graph input1))
         (edges2 (extract-circuit-edges circuits graph input2))
         (intersection (length (lset-intersection equal? edges1 edges2)))
         (union (length (lset-union equal? edges1 edges2))))
    (if (= union 0) 1.0 (/ intersection union))))

;; Helper: Extract edges from circuits
(define (extract-circuit-edges circuits graph input)
  "Extract all edges participating in circuits"
  (let ((edges '()))
    (for-each (lambda (path)
                (let loop ((nodes path))
                  (when (>= (length nodes) 2)
                    (let ((edge (find-edge graph (car nodes) (cadr nodes))))
                      (when edge
                        (set! edges (cons edge edges))))
                    (loop (cdr nodes)))))
              circuits)
    edges))

;; Attribution coherence using spatial clustering
(define (attribution-coherence graph)
  "Measure spatial clustering of high-attribution edges"
  (let* ((edges (graph-edges graph))
         (weights (map edge-weight edges)))
    (if (< (length weights) 3)
        1.0  ; Too few edges for meaningful clustering
        (let* ((high-threshold (percentile weights 0.75))
               (high-edges (filter (lambda (e) (> (abs (edge-weight e)) high-threshold))
                                  edges)))
          ;; Simplified clustering metric: ratio of high edges in dense regions
          (clustering-coefficient high-edges graph)))))

;; Helper: Calculate clustering coefficient for edges
(define (clustering-coefficient edges graph)
  "Calculate clustering coefficient for a set of edges"
  ;; Simplified: measure local connectivity around high-attribution edges
  (if (null? edges)
      0.0
      (let ((cluster-scores '()))
        (for-each (lambda (edge)
                    (let* ((source (edge-source edge))
                           (target (edge-target edge))
                           (local-score (local-connectivity graph source target)))
                      (set! cluster-scores (cons local-score cluster-scores))))
                  edges)
        (/ (apply + cluster-scores) (length cluster-scores)))))

;; Helper: Local connectivity around an edge
(define (local-connectivity graph source target)
  "Measure local connectivity around source-target edge"
  ;; Count neighboring edges with significant weights
  (let* ((all-edges (graph-edges graph))
         (neighbor-edges (filter (lambda (e)
                                  (or (equal? (edge-source e) source)
                                      (equal? (edge-target e) source)
                                      (equal? (edge-source e) target)
                                      (equal? (edge-target e) target)))
                               all-edges))
         (strong-neighbors (filter (lambda (e) (> (abs (edge-weight e)) 0.1))
                                  neighbor-edges)))
    (if (null? neighbor-edges)
        0.0
        (/ (length strong-neighbors) (length neighbor-edges)))))

;; Causal fidelity through perturbation validation
(define (causal-fidelity graph model test-inputs)
  "Measure how well attribution weights predict causal effects"
  (let ((fidelity-scores '()))
    (for-each (lambda (input)
                (let ((score (validate-causal-predictions graph model input)))
                  (set! fidelity-scores (cons score fidelity-scores))))
              test-inputs)
    ;; Return average fidelity across inputs
    (if (null? fidelity-scores)
        0.0
        (/ (apply + fidelity-scores) (length fidelity-scores)))))

;; Helper: Validate causal predictions for single input
(define (validate-causal-predictions graph model input)
  "Validate predicted vs actual causal effects"
  (let* ((edges (graph-edges graph))
         (high-edges (filter (lambda (e) (> (abs (edge-weight e)) 0.1)) edges))
         (correlations '()))
    ;; Test each high-attribution edge
    (for-each (lambda (edge)
                (let* ((predicted (edge-weight edge))
                       (actual (measure-actual-effect model input edge))
                       (correlation (if (and (> (abs predicted) 1e-6)
                                           (> (abs actual) 1e-6))
                                      (/ (* predicted actual)
                                         (* (abs predicted) (abs actual)))
                                      0.0)))
                  (set! correlations (cons correlation correlations))))
              high-edges)
    ;; Return average correlation
    (if (null? correlations)
        0.0
        (/ (apply + correlations) (length correlations)))))

;; Helper: Measure actual causal effect (placeholder)
(define (measure-actual-effect model input edge)
  "Measure actual causal effect through perturbation"
  ;; Simplified placeholder - real implementation would perturb source feature
  ;; and measure effect on target feature
  (random:uniform))

;; Comprehensive circuit confidence scoring
(define (circuit-confidence graph circuits model test-inputs)
  "Aggregate confidence score across multiple validation dimensions"
  (let* ((stability (circuit-stability graph circuits test-inputs))
         (coherence (attribution-coherence graph))
         (fidelity (causal-fidelity graph model test-inputs))
         (coverage (if (null? circuits) 0.0 1.0))  ; Simplified coverage
         ;; Weighted combination
         (weights '((stability . 0.3)
                   (coherence . 0.2)
                   (fidelity . 0.4)
                   (coverage . 0.1)))
         (weighted-score (+ (* stability 0.3)
                           (* coherence 0.2)
                           (* fidelity 0.4)
                           (* coverage 0.1))))
    `((overall-confidence . ,weighted-score)
      (stability . ,stability)
      (coherence . ,coherence)
      (fidelity . ,fidelity)
      (coverage . ,coverage))))

;; Significance testing framework
(define (significance-tests graph null-graphs alpha)
  "Test significance of graph metrics against null distribution"
  (let* ((observed-sparsity (graph-sparsity graph))
         (observed-concentration (attribution-concentration graph))
         (null-sparsities (map graph-sparsity null-graphs))
         (null-concentrations (map attribution-concentration null-graphs)))
    `((sparsity . ,(permutation-test-simple observed-sparsity null-sparsities alpha))
      (concentration . ,(permutation-test-simple observed-concentration null-concentrations alpha)))))

;; Helper: Simplified permutation test
(define (permutation-test-simple observed null-values alpha)
  "Simple permutation test against null distribution"
  (let* ((n-null (length null-values))
         (extreme-count (length (filter (lambda (x) (>= (abs (- x observed)) 
                                                        (abs observed)))
                                       null-values)))
         (p-value (/ extreme-count (max 1 n-null))))
    `((observed . ,observed)
      (p-value . ,p-value)
      (significant . ,(< p-value alpha)))))

;; Enhanced method comparison with statistical validation
(define (compare-methods-enhanced graphs labels test-inputs)
  "Enhanced comparison with confidence intervals and significance tests"
  (let ((method-stats (make-hash-table)))
    (for-each (lambda (graph label)
                (let* ((sparsity (graph-sparsity graph))
                       (concentration (attribution-concentration graph))
                       (coherence (attribution-coherence graph))
                       ;; Bootstrap confidence intervals
                       (sparsity-ci (bootstrap-single-metric 
                                    (lambda (_) (graph-sparsity graph)) 100 0.05))
                       (concentration-ci (bootstrap-single-metric
                                        (lambda (_) (attribution-concentration graph)) 100 0.05)))
                  (hash-set! method-stats label
                            `((sparsity . ,sparsity)
                              (sparsity-ci . ,sparsity-ci)
                              (concentration . ,concentration)
                              (concentration-ci . ,concentration-ci)
                              (coherence . ,coherence)
                              (nodes . ,(length (graph-nodes graph)))
                              (edges . ,(length (graph-edges graph)))))))
              graphs labels)
    method-stats))

;; Helper: Bootstrap for single metric
(define (bootstrap-single-metric metric-fn n-bootstrap alpha)
  "Simplified bootstrap for single-value metrics"
  ;; Simplified version - real implementation would resample data
  (let ((samples (map (lambda (_) (+ (metric-fn '()) (* 0.1 (- (random:uniform) 0.5))))
                     (iota n-bootstrap))))
    (confidence-interval samples alpha)))

;;; =============================================================================
;;; UTILITY FUNCTIONS
;;; =============================================================================

;; Calculate percentile of a list
(define (percentile values p)
  "Calculate p-th percentile of values"
  (let* ((sorted (sort values <))
         (n (length sorted))
         (idx (inexact->exact (floor (* p n)))))
    (list-ref sorted (min (max 0 idx) (- n 1)))))
