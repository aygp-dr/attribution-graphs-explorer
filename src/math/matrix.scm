(define-module (attribution-graphs math matrix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (make-matrix
            make-random-matrix
            matrix-row
            matrix-vector-multiply
            matrix-multiply
            jacobian))

;; Create matrix filled with zeros
(define (make-matrix rows cols)
  "Create a rows×cols matrix filled with zeros"
  (make-vector rows (lambda () (make-vector cols 0))))

;; Create random matrix (simplified)
(define (make-random-matrix rows cols)
  "Create a rows×cols matrix with random values"
  (let ((matrix (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((>= i rows) matrix)
      (vector-set! matrix i 
                   (list->vector 
                    (map (lambda (_) (- (random:uniform) 0.5))
                         (iota cols)))))))

;; Get row from matrix
(define (matrix-row matrix i)
  "Get i-th row of matrix as vector"
  (vector-ref matrix i))

;; Matrix-vector multiplication
(define (matrix-vector-multiply matrix vec)
  "Multiply matrix by vector"
  (let* ((rows (vector-length matrix))
         (result (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((>= i rows) result)
      (vector-set! result i
                   (dot-product (matrix-row matrix i) vec)))))

;; Matrix multiplication
(define (matrix-multiply A B)
  "Multiply two matrices A and B"
  (let* ((m (vector-length A))
         (n (vector-length (vector-ref B 0)))
         (k (vector-length B))
         (C (make-matrix m n)))
    (do ((i 0 (+ i 1)))
        ((>= i m) C)
      (do ((j 0 (+ j 1)))
          ((>= j n))
        (let ((sum 0))
          (do ((l 0 (+ l 1)))
              ((>= l k))
            (set! sum (+ sum (* (vector-ref (vector-ref A i) l)
                               (vector-ref (vector-ref B l) j)))))
          (vector-set! (vector-ref C i) j sum))))))

;; Compute Jacobian matrix
(define (jacobian output-fn input-vec)
  "Compute Jacobian matrix of output-fn w.r.t. input-vec"
  (let* ((n (vector-length input-vec))
         (epsilon 1e-6)
         (base-output (output-fn input-vec))
         (m (vector-length base-output))
         (J (make-matrix m n)))
    ;; Numerical differentiation
    (do ((j 0 (+ j 1)))
        ((>= j n) J)
      (let ((perturbed (vector-copy input-vec)))
        (vector-set! perturbed j (+ (vector-ref perturbed j) epsilon))
        (let ((perturbed-output (output-fn perturbed)))
          (do ((i 0 (+ i 1)))
              ((>= i m))
            (vector-set! (vector-ref J i) j
                        (/ (- (vector-ref perturbed-output i)
                              (vector-ref base-output i))
                           epsilon))))))))

;; Helper for matrix operations
(define (dot-product vec1 vec2)
  "Compute dot product of two vectors"
  (fold + 0 (map * (vector->list vec1) (vector->list vec2))))
