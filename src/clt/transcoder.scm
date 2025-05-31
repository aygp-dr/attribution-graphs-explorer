(define-module (attribution-graphs clt transcoder)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (attribution-graphs math activation)
  #:export (make-clt
            clt-forward
            clt-encode
            clt-decode))

;; Cross-Layer Transcoder structure
(define-record-type <clt>
  (make-clt-internal encoder decoder read-layer write-layers feature-dim)
  clt?
  (encoder clt-encoder)           ; Encoding matrix
  (decoder clt-decoder)           ; Decoding matrices per layer
  (read-layer clt-read-layer)     ; Layer to read from
  (write-layers clt-write-layers) ; Layers to write to
  (feature-dim clt-feature-dim))  ; Feature dimension

;; Constructor
(define (make-clt read-layer write-layers input-dim feature-dim output-dim)
  "Create a new Cross-Layer Transcoder"
  (let ((encoder (make-random-matrix feature-dim input-dim))
        (decoder (make-hash-table)))
    ;; Initialize decoder matrices for each write layer
    (for-each (lambda (layer)
                (hash-set! decoder layer 
                          (make-random-matrix output-dim feature-dim)))
              write-layers)
    (make-clt-internal encoder decoder read-layer write-layers feature-dim)))

;; Encoding: residual stream -> features
(define (clt-encode clt residual-stream)
  "Encode residual stream into sparse features"
  (let* ((encoder (clt-encoder clt))
         (num-features (clt-feature-dim clt))
         (features (make-vector num-features 0)))
    ;; Compute feature activations
    (do ((i 0 (+ i 1)))
        ((>= i num-features) features)
      (let ((encoder-row (matrix-row encoder i))
            (bias 0)) ; Simplified: zero bias
        (vector-set! features i
                    (feature-activation encoder-row residual-stream bias))))))

;; Decoding: features -> MLP outputs for specific layer
(define (clt-decode clt features target-layer)
  "Decode features to MLP output for target layer"
  (let ((decoder-matrix (hash-ref (clt-decoder clt) target-layer)))
    (matrix-vector-multiply decoder-matrix features)))

;; Forward pass through CLT
(define (clt-forward clt residual-stream)
  "Complete forward pass: encode then decode for all layers"
  (let ((features (clt-encode clt residual-stream))
        (outputs (make-hash-table)))
    (for-each (lambda (layer)
                (hash-set! outputs layer
                          (clt-decode clt features layer)))
              (clt-write-layers clt))
    outputs))
