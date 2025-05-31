(define-module (attribution-graphs circuits discovery)
  #:use-module (srfi srfi-1)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs graph attribution)
  #:export (find-circuits
            trace-path
            identify-motifs))

;; Find all paths from source to target nodes
(define (find-all-paths graph source-id target-id)
  "Find all paths from source to target node"
  (define (dfs current-id visited path)
    (if (equal? current-id target-id)
        (list (reverse (cons current-id path)))
        (let ((neighbors (get-neighbors graph current-id)))
          (append-map
           (lambda (neighbor)
             (if (member neighbor visited)
                 '()
                 (dfs neighbor
                      (cons current-id visited)
                      (cons current-id path))))
           neighbors))))
  (dfs source-id '() '()))

;; Get neighbor nodes
(define (get-neighbors graph node-id)
  "Get IDs of nodes connected from given node"
  (map edge-target
       (filter (lambda (edge)
                 (equal? (edge-source edge) node-id))
               (graph-edges graph))))

;; Trace strongest path
(define (trace-path graph source-id target-id)
  "Trace path with highest attribution from source to target"
  (let ((paths (find-all-paths graph source-id target-id)))
    (if (null? paths)
        #f
        (max-by path-strength paths))))

;; Compute path strength
(define (path-strength path graph)
  "Compute total attribution strength of path"
  (if (< (length path) 2)
      0
      (let loop ((nodes path)
                 (strength 1))
        (if (null? (cdr nodes))
            strength
            (let* ((edge (find-edge graph (car nodes) (cadr nodes)))
                   (weight (if edge (edge-weight edge) 0)))
              (loop (cdr nodes) (* strength weight)))))))

;; Find edge between two nodes
(define (find-edge graph source-id target-id)
  "Find edge from source to target"
  (find (lambda (edge)
          (and (equal? (edge-source edge) source-id)
               (equal? (edge-target edge) target-id)))
        (graph-edges graph)))

;; Identify common circuit motifs
(define (identify-motifs graph)
  "Identify common patterns in attribution graph"
  (let ((motifs '()))
    ;; Skip connections
    (set! motifs (cons (find-skip-connections graph) motifs))
    ;; Bottlenecks
    (set! motifs (cons (find-bottlenecks graph) motifs))
    ;; Parallel paths
    (set! motifs (cons (find-parallel-paths graph) motifs))
    motifs))

;; Pattern: Skip connections
(define (find-skip-connections graph)
  "Find features that connect across multiple layers"
  ;; Implementation would analyze edge patterns
  '(skip-connections))

;; Pattern: Bottlenecks
(define (find-bottlenecks graph)
  "Find nodes with high in/out degree"
  (let ((in-degree (make-hash-table))
        (out-degree (make-hash-table)))
    ;; Count degrees
    (for-each (lambda (edge)
                (hash-update! in-degree (edge-target edge) 1+ 0)
                (hash-update! out-degree (edge-source edge) 1+ 0))
              (graph-edges graph))
    ;; Find bottlenecks
    (filter (lambda (node)
              (or (> (hash-ref in-degree (node-id node) 0) 10)
                  (> (hash-ref out-degree (node-id node) 0) 10)))
            (graph-nodes graph))))

;; Helper utilities
(define (max-by fn lst)
  "Find element that maximizes fn"
  (if (null? lst)
      #f
      (fold (lambda (x best)
              (if (> (fn x) (fn best)) x best))
            (car lst)
            (cdr lst))))

(define (hash-update! table key fn default)
  "Update hash table value with function"
  (hash-set! table key
             (fn (hash-ref table key default))))
