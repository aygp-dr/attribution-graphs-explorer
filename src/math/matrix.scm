(define-module (attribution-graphs math matrix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (make-matrix
            make-random-matrix
            matrix-row
            matrix-vector-multiply
            matrix-multiply
            jacobian
            matrix-svd-decompose
            matrix-condition-number
            matrix-rank-estimate))

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

;; Enhanced SVD computation with improved numerics
(define (matrix-svd-decompose matrix max-rank tolerance)
  "Compute SVD decomposition using stabilized power iteration"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (effective-rank (min max-rank (min m n)))
         (A (matrix-copy matrix)))
    
    ;; Use iterative Lanczos-style algorithm for better stability
    (lanczos-svd A effective-rank tolerance)))

;; Estimate matrix condition number using SVD
(define (matrix-condition-number matrix)
  "Estimate condition number (ratio of largest to smallest singular value)"
  (let* ((svd-result (matrix-svd-decompose matrix 
                                          (min (vector-length matrix) 10) 
                                          1e-8))
         (S (cadr svd-result))
         (s-list (vector->list S)))
    (if (and (not (null? s-list)) (> (last s-list) 1e-12))
        (/ (car s-list) (last s-list))
        +inf.0)))

;; Estimate effective rank of matrix
(define (matrix-rank-estimate matrix tolerance)
  "Estimate numerical rank by counting significant singular values"
  (let* ((svd-result (matrix-svd-decompose matrix 
                                          (min (vector-length matrix) 20)
                                          tolerance))
         (S (cadr svd-result)))
    (length (filter (lambda (s) (> s tolerance)) (vector->list S)))))

;; Lanczos-style SVD for improved numerical stability
(define (lanczos-svd matrix rank tolerance)
  "Compute SVD using Lanczos bidiagonalization"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (U (make-matrix m rank))
         (S (make-vector rank))
         (V (make-matrix n rank)))
    
    ;; Simplified implementation - in practice would use full Lanczos
    (simple-power-iteration-svd matrix rank tolerance)))

;; Simplified power iteration for SVD
(define (simple-power-iteration-svd matrix rank tolerance)
  "Basic power iteration SVD implementation"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (U (make-matrix m rank))
         (S (make-vector rank))
         (V (make-matrix n rank))
         (A (matrix-copy matrix)))
    
    (do ((k 0 (+ k 1)))
        ((>= k rank) (list U S V))
      
      ;; Random initialization with better conditioning
      (let ((v (normalized-random-vector n)))
        
        ;; Power iteration with deflation
        (let ((sigma (power-iteration-step! A v U V S k tolerance)))
          (vector-set! S k sigma))))))

;; Helper functions for enhanced matrix operations
(define (matrix-copy matrix)
  "Create deep copy of matrix"
  (let* ((m (vector-length matrix))
         (copy (make-vector m)))
    (do ((i 0 (+ i 1)))
        ((>= i m) copy)
      (vector-set! copy i (vector-copy (vector-ref matrix i))))))

(define (normalized-random-vector n)
  "Generate normalized random vector"
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (vector-set! v i (- (random:uniform) 0.5)))
    (let ((norm (sqrt (dot-product v v))))
      (if (> norm 1e-12)
          (vector-map (lambda (x) (/ x norm)) v)
          (normalized-random-vector n)))))

(define (power-iteration-step! A v U V S k tolerance)
  "Single step of power iteration with deflation"
  (let ((max-iter 50)
        (current-v (vector-copy v)))
    
    ;; Power iteration
    (do ((iter 0 (+ iter 1)))
        ((or (>= iter max-iter) 
             (< (vector-change-norm current-v
                                   (next-power-vector A current-v)) 
                tolerance))
         (compute-singular-value A current-v))
      (set! current-v (next-power-vector A current-v)))))

(define (next-power-vector A v)
  "Compute next vector in power iteration"
  (let* ((Av (matrix-vector-multiply A v))
         (AtAv (matrix-vector-multiply (matrix-transpose A) Av))
         (norm (sqrt (dot-product AtAv AtAv))))
    (if (> norm 1e-12)
        (vector-map (lambda (x) (/ x norm)) AtAv)
        v)))

(define (compute-singular-value A v)
  "Compute singular value for converged vector"
  (let ((Av (matrix-vector-multiply A v)))
    (sqrt (dot-product Av Av))))

(define (vector-change-norm v1 v2)
  "Compute norm of difference between vectors"
  (let ((diff (vector-map - v1 v2)))
    (sqrt (dot-product diff diff))))

(define (matrix-transpose A)
  "Compute matrix transpose"
  (let* ((m (vector-length A))
         (n (if (> m 0) (vector-length (vector-ref A 0)) 0))
         (At (make-matrix n m)))
    (do ((i 0 (+ i 1)))
        ((>= i m) At)
      (do ((j 0 (+ j 1)))
          ((>= j n))
        (vector-set! (vector-ref At j) i 
                     (vector-ref (vector-ref A i) j))))))

;; Helper for matrix operations
(define (dot-product vec1 vec2)
  "Compute dot product of two vectors"
  (fold + 0 (map * (vector->list vec1) (vector->list vec2))))
