(define-module (attribution-graphs validation perturbation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (perturbation-test
            measure-causal-effect
            validate-circuit))

;; Perturbation in feature direction
(define (perturb-features features feature-id magnitude)
  "Apply perturbation to specific feature"
  (let ((perturbed (vector-copy features)))
    (vector-set! perturbed feature-id
                 (+ (vector-ref perturbed feature-id) magnitude))
    perturbed))

;; Measure causal effect of perturbation
(define (measure-causal-effect model input source-feature target-feature magnitude)
  "Measure how perturbing source affects target"
  (let* ((baseline (run-model model input))
         (baseline-target (vector-ref baseline target-feature))
         (perturbed-input (perturb-at-feature input source-feature magnitude))
         (perturbed-output (run-model model perturbed-input))
         (perturbed-target (vector-ref perturbed-output target-feature)))
    (- perturbed-target baseline-target)))

;; Test if edge in attribution graph is causal
(define (test-edge-causality graph edge model input)
  "Test if edge represents true causal relationship"
  (let* ((source (edge-source edge))
         (target (edge-target edge))
         (predicted-effect (edge-weight edge))
         (measured-effect (measure-causal-effect model input source target 1.0))
         (correlation (/ measured-effect (+ (abs predicted-effect) 1e-6))))
    (> correlation 0.5))) ; Threshold for considering edge valid

;; Validate entire circuit
(define (validate-circuit circuit graph model test-inputs)
  "Validate circuit across multiple test inputs"
  (let ((validation-scores '()))
    (for-each (lambda (input)
                (let ((score (validate-circuit-single circuit graph model input)))
                  (set! validation-scores (cons score validation-scores))))
              test-inputs)
    ;; Return average validation score
    (/ (apply + validation-scores) (length validation-scores))))

;; Validate circuit on single input
(define (validate-circuit-single circuit graph model input)
  "Compute validation score for circuit on one input"
  (let ((edge-scores '()))
    ;; Test each edge in circuit
    (for-each (lambda (path)
                (let loop ((nodes path))
                  (when (>= (length nodes) 2)
                    (let* ((edge (find-edge graph (car nodes) (cadr nodes)))
                           (valid? (test-edge-causality graph edge model input)))
                      (set! edge-scores (cons (if valid? 1 0) edge-scores)))
                    (loop (cdr nodes)))))
              circuit)
    ;; Return fraction of valid edges
    (if (null? edge-scores)
        0
        (/ (apply + edge-scores) (length edge-scores)))))

;; Helper to run model (placeholder)
(define (run-model model input)
  "Run model forward pass"
  ;; Implementation depends on model architecture
  (vector 0 0 0))

;; Helper to perturb at specific feature
(define (perturb-at-feature input feature-id magnitude)
  "Apply perturbation at feature level"
  ;; Implementation depends on model architecture
  input)
