(define-module (attribution-graphs circuits visualization)
  #:use-module (srfi srfi-1)
  #:use-module (attribution-graphs graph structure)
  #:export (graph->mermaid
            circuit->mermaid
            feature->label))

;; Convert attribution graph to Mermaid diagram
(define (graph->mermaid graph)
  "Generate Mermaid diagram from attribution graph"
  (string-append
   "graph TD\n"
   (string-join
    (append
     ;; Node definitions
     (map (lambda (node)
            (format #f "    ~a[~a]"
                    (node-id node)
                    (feature->label node)))
          (graph-nodes graph))
     ;; Edge definitions
     (map (lambda (edge)
            (format #f "    ~a -->|~,2f| ~a"
                    (edge-source edge)
                    (edge-weight edge)
                    (edge-target edge)))
          (graph-edges graph)))
    "\n")
   "\n"
   ;; Styling
   (generate-node-styles graph)))

;; Generate node styling based on type
(define (generate-node-styles graph)
  "Generate Mermaid styles for different node types"
  (string-join
   (map (lambda (type style)
          (let ((nodes (filter (lambda (n) (eq? (node-type n) type))
                              (graph-nodes graph))))
            (if (null? nodes)
                ""
                (format #f "    style ~a ~a"
                        (string-join (map (lambda (n)
                                           (symbol->string (node-id n)))
                                         nodes) ",")
                        style))))
        '(feature token error logit)
        '("fill:#f9f" "fill:#9f9" "fill:#f99" "fill:#99f"))
   "\n"))

;; Convert circuit to focused Mermaid diagram
(define (circuit->mermaid circuit graph)
  "Generate Mermaid diagram highlighting specific circuit"
  (let ((circuit-nodes (make-hash-table))
        (circuit-edges (make-hash-table)))
    ;; Mark circuit nodes and edges
    (for-each (lambda (path)
                (for-each (lambda (node-id)
                            (hash-set! circuit-nodes node-id #t))
                          path)
                ;; Mark edges in path
                (let loop ((nodes path))
                  (when (>= (length nodes) 2)
                    (hash-set! circuit-edges
                               (cons (car nodes) (cadr nodes)) #t)
                    (loop (cdr nodes)))))
              circuit)
    ;; Generate diagram with highlighting
    (graph->mermaid-highlighted graph circuit-nodes circuit-edges)))

;; Feature to human-readable label
(define (feature->label node)
  "Convert feature node to readable label"
  (let ((metadata (node-metadata node)))
    (case (node-type node)
      ((feature)
       (format #f "F~a: ~a"
               (node-id node)
               (hash-ref metadata 'interpretation "?")))
      ((token)
       (format #f "T: ~a"
               (hash-ref metadata 'token "")))
      ((error)
       "Error")
      ((logit)
       (format #f "L: ~a"
               (hash-ref metadata 'token "")))
      (else
       (symbol->string (node-id node))))))

;; Helper for highlighted graph
(define (graph->mermaid-highlighted graph highlight-nodes highlight-edges)
  "Generate Mermaid with highlighted circuit"
  ;; Similar to graph->mermaid but with special styling
  ;; for highlighted elements
  (string-append
   "graph TD\n"
   ;; ... implementation ...
   ))
