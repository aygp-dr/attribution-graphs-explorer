(define-module (attribution-graphs interface export)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (json)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs circuits discovery)
  #:export (graph->json
            export-graph-file
            circuit->json
            node->json-object
            edge->json-object))

;; Convert attribution graph to JSON format for web interface
(define (graph->json graph . circuits)
  "Convert attribution graph to JSON object for web visualization"
  (let* ((circuits-list (if (null? circuits) '() (car circuits)))
         (nodes-json (map node->json-object (graph-nodes graph)))
         (edges-json (map edge->json-object (graph-edges graph)))
         (circuits-json (map circuit->json circuits-list))
         (layout-info (compute-layout-positions graph)))
    
    ;; Build JSON object
    `((nodes . ,(list->vector nodes-json))
      (edges . ,(list->vector edges-json))
      (circuits . ,(list->vector circuits-json))
      (layout . ,layout-info)
      (metadata . ,(graph-metadata graph)))))

;; Convert single node to JSON object
(define (node->json-object node)
  "Convert node to JSON-compatible alist"
  (let ((metadata (node-metadata node)))
    `((id . ,(symbol->string (node-id node)))
      (type . ,(symbol->string (node-type node)))
      (activation . ,(node-activation node))
      (layer . ,(hash-ref metadata 'layer 0))
      (position . ((x . ,(hash-ref metadata 'x 0))
                   (y . ,(hash-ref metadata 'y 0))))
      (metadata . ((interpretation . ,(hash-ref metadata 'interpretation ""))
                   (feature_index . ,(hash-ref metadata 'feature_index #f))
                   (token . ,(hash-ref metadata 'token ""))
                   (confidence . ,(hash-ref metadata 'confidence 1.0)))))))

;; Convert single edge to JSON object  
(define (edge->json-object edge)
  "Convert edge to JSON-compatible alist"
  `((source . ,(symbol->string (edge-source edge)))
    (target . ,(symbol->string (edge-target edge)))
    (weight . ,(edge-weight edge))
    (layer_from . ,(edge-layer-from edge))
    (layer_to . ,(edge-layer-to edge))
    (type . ,(symbol->string (edge-type edge)))))

;; Convert circuit to JSON object
(define (circuit->json circuit)
  "Convert circuit (path list) to JSON object"
  `((name . ,(circuit-name circuit))
    (path . ,(list->vector (map symbol->string (circuit-path circuit))))
    (strength . ,(circuit-strength circuit))
    (type . ,(symbol->string (circuit-type circuit)))
    (confidence . ,(circuit-confidence circuit))))

;; Compute layout positions for nodes
(define (compute-layout-positions graph)
  "Compute initial layout positions for graph nodes"
  (let ((nodes (graph-nodes graph))
        (layers (make-hash-table)))
    
    ;; Group nodes by layer
    (for-each (lambda (node)
                (let ((layer (hash-ref (node-metadata node) 'layer 0)))
                  (hash-set! layers layer 
                            (cons node (hash-ref layers layer '())))))
              nodes)
    
    ;; Assign positions within layers
    (let ((layer-keys (sort (hash-map->list (lambda (k v) k) layers) <)))
      (map (lambda (layer-idx layer-key)
             (let ((layer-nodes (hash-ref layers layer-key))
                   (layer-x (* layer-idx 200))) ; 200px between layers
               (map (lambda (node-idx node)
                      (let ((node-y (* node-idx 80))) ; 80px between nodes
                        ;; Update node metadata with position
                        (hash-set! (node-metadata node) 'x layer-x)
                        (hash-set! (node-metadata node) 'y node-y)))
                    (iota (length layer-nodes))
                    layer-nodes)))
           (iota (length layer-keys))
           layer-keys))
    
    ;; Return layout configuration
    `((type . "layered")
      (layer_spacing . 200)
      (node_spacing . 80)
      (layers . ,(length layer-keys)))))

;; Export graph to JSON file
(define (export-graph-file graph filename . circuits)
  "Export attribution graph to JSON file for web interface"
  (let* ((circuits-list (if (null? circuits) '() (car circuits)))
         (json-data (graph->json graph circuits-list))
         (json-string (scm->json json-data)))
    (with-output-to-file filename
      (lambda () 
        (display json-string)
        (newline)))
    (format #t "Exported graph to ~a~%" filename)))

;; Helper: Get graph metadata
(define (graph-metadata graph)
  "Extract metadata from graph for JSON export"
  `((node_count . ,(length (graph-nodes graph)))
    (edge_count . ,(length (graph-edges graph)))
    (max_layer . ,(max-layer graph))
    (attribution_range . ,(attribution-range graph))
    (export_timestamp . ,(current-time))))

;; Helper: Find maximum layer in graph
(define (max-layer graph)
  "Find the highest layer number in the graph"
  (fold (lambda (node max-so-far)
          (max max-so-far 
               (hash-ref (node-metadata node) 'layer 0)))
        0
        (graph-nodes graph)))

;; Helper: Find attribution weight range
(define (attribution-range graph)
  "Find min and max attribution weights in graph"
  (let ((weights (map edge-weight (graph-edges graph))))
    (if (null? weights)
        '((min . 0) (max . 0))
        `((min . ,(apply min weights))
          (max . ,(apply max weights))))))

;; Helper functions for edge metadata
(define (edge-layer-from edge)
  "Get source layer of edge (placeholder implementation)"
  ;; In real implementation, this would look up the source node's layer
  0)

(define (edge-layer-to edge)
  "Get target layer of edge (placeholder implementation)"  
  ;; In real implementation, this would look up the target node's layer
  1)

(define (edge-type edge)
  "Get edge type (placeholder implementation)"
  'attribution)

;; Helper functions for circuit metadata
(define (circuit-name circuit)
  "Get circuit name"
  (if (list? circuit)
      "Unnamed Circuit"
      (car circuit)))

(define (circuit-path circuit)
  "Get circuit path as list of node IDs"
  (if (list? circuit)
      circuit
      (cdr circuit)))

(define (circuit-strength circuit)
  "Calculate circuit strength (placeholder)"
  0.5)

(define (circuit-type circuit)
  "Get circuit type"
  'discovered)

(define (circuit-confidence circuit)
  "Get circuit confidence score"
  0.8)

;; Helper: Current timestamp
(define (current-time)
  "Get current timestamp (simplified)"
  (strftime "%Y-%m-%dT%H:%M:%SZ" (localtime (current-time))))