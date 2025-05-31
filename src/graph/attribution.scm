(define-module (attribution-graphs graph attribution)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs math matrix)
  #:export (compute-attribution-graph
            backward-attribution
            prune-graph))

;; Compute attribution from source to target features
(define (compute-attribution source-features target-features jacobian-matrix)
  "Compute attribution weights using Jacobian"
  (let* ((n-source (vector-length source-features))
         (n-target (vector-length target-features))
         (attributions (make-matrix n-target n-source)))
    ;; Attribution = Jacobian × source activations
    (do ((i 0 (+ i 1)))
        ((>= i n-target) attributions)
      (do ((j 0 (+ j 1)))
          ((>= j n-source))
        (let ((jacob-ij (vector-ref (vector-ref jacobian-matrix i) j))
              (source-j (vector-ref source-features j)))
          (vector-set! (vector-ref attributions i) j
                      (* jacob-ij source-j)))))))

;; Backward attribution through one layer
(define (backward-attribution current-features prev-features layer-fn)
  "Compute attributions from previous to current layer features"
  (let* ((jacobian-fn (lambda (x) (layer-fn x)))
         (J (jacobian jacobian-fn prev-features)))
    (compute-attribution prev-features current-features J)))

;; Build complete attribution graph
(define (compute-attribution-graph clt prompt target-token)
  "Build attribution graph for target token in prompt"
  (let ((graph (make-attribution-graph))
        (layer-features (make-hash-table))) ; features at each layer
    
    ;; Forward pass to collect features
    (forward-pass-collect-features! clt prompt layer-features)
    
    ;; Backward pass to compute attributions
    (backward-pass-attributions! layer-features graph target-token)
    
    ;; Prune to keep only significant paths
    (prune-graph graph 0.1))) ; threshold = 0.1

;; Graph pruning
(define (prune-graph graph threshold)
  "Remove edges with attribution below threshold"
  (let ((pruned (make-attribution-graph)))
    ;; Copy nodes
    (for-each (lambda (node) (add-node! pruned node))
              (graph-nodes graph))
    ;; Filter edges
    (for-each (lambda (edge)
                (when (> (abs (edge-weight edge)) threshold)
                  (add-edge! pruned edge)))
              (graph-edges graph))
    pruned))

;; Helper: Forward pass to collect features
(define (forward-pass-collect-features! clt prompt layer-features)
  "Run forward pass and store features at each layer"
  ;; Implementation depends on model architecture
  ;; This is a simplified placeholder
  (let ((embeddings (embed-tokens prompt)))
    (hash-set! layer-features 0 embeddings)
    ;; Process through layers...
    ))

;; Helper: Backward attribution pass
(define (backward-pass-attributions! layer-features graph target-token)
  "Compute attributions backward from target"
  ;; Start from output logits
  (let ((n-layers (hash-count layer-features)))
    (do ((l (- n-layers 1) (- l 1)))
        ((< l 1))
      (let ((curr-features (hash-ref layer-features l))
            (prev-features (hash-ref layer-features (- l 1))))
        ;; Compute attributions between layers
        ;; Add nodes and edges to graph
        ))))
