(define-module (attribution-graphs math activation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (feature-activation
            relu
            affine-transform))

;; ReLU activation function
(define (relu x)
  "Apply ReLU activation: max(0, x)"
  (max 0 x))

;; Affine transformation: E_i · x + b_i
(define (affine-transform weights input bias)
  "Compute affine transformation: weights · input + bias"
  (+ (dot-product weights input) bias))

;; Feature activation: ReLU(E_i · x + b_i)
(define (feature-activation encoder-weights residual-stream bias)
  "Compute feature activation as thresholded affine function"
  (relu (affine-transform encoder-weights residual-stream bias)))

;; Helper: dot product
(define (dot-product vec1 vec2)
  "Compute dot product of two vectors"
  (fold + 0 (map * vec1 vec2)))
