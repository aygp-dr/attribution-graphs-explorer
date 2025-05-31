(define-module (attribution-graphs validation metrics)
  #:use-module (srfi srfi-1)
  #:export (graph-sparsity
            path-coverage
            attribution-concentration
            compare-methods))

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
