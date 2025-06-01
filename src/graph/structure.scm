(define-module (attribution-graphs graph structure)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-node
            make-edge
            make-attribution-graph
            add-node!
            add-edge!
            node-id
            node-type
            node-activation
            edge-source
            edge-target
            edge-weight
            graph-nodes
            graph-edges))

;; Node in attribution graph
(define-record-type <node>
  (make-node id type activation metadata)
  node?
  (id node-id)                ; Unique identifier
  (type node-type)            ; 'feature, 'token, 'error, 'logit
  (activation node-activation) ; Activation value
  (metadata node-metadata))    ; Additional info

;; Edge in attribution graph
(define-record-type <edge>
  (make-edge source target weight)
  edge?
  (source edge-source)   ; Source node ID
  (target edge-target)   ; Target node ID
  (weight edge-weight))  ; Attribution weight

;; Attribution graph
(define-record-type <attribution-graph>
  (make-attribution-graph-internal nodes edges)
  attribution-graph?
  (nodes graph-nodes-internal graph-set-nodes!)
  (edges graph-edges-internal graph-set-edges!))

;; Constructor
(define (make-attribution-graph)
  "Create empty attribution graph"
  (make-attribution-graph-internal
   (make-hash-table)  ; nodes by ID
   '()))              ; list of edges

;; Add node to graph
(define (add-node! graph node)
  "Add node to attribution graph"
  (hash-set! (graph-nodes-internal graph)
             (node-id node)
             node))

;; Add edge to graph
(define (add-edge! graph edge)
  "Add edge to attribution graph"
  (graph-set-edges! graph
                    (cons edge (graph-edges-internal graph))))

;; Getters
(define (graph-nodes graph)
  "Get all nodes as list"
  (hash-map->list (lambda (k v) v)
                  (graph-nodes-internal graph)))

(define (graph-edges graph)
  "Get all edges"
  (graph-edges-internal graph))
