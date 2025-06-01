(define-module (attribution-graphs math global-analysis)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (attribution-graphs math matrix)
  #:export (matrix-svd
            analyze-components
            aggregate-weights
            find-stable-components
            component-similarity
            rank-components
            extract-principal-components))

;; SVD using power iteration method
(define (matrix-svd matrix max-rank tolerance)
  "Compute SVD of matrix using power iteration method"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (rank (min max-rank (min m n)))
         (U (make-matrix m rank))
         (S (make-vector rank))
         (V (make-matrix n rank)))
    
    ;; Create copy of input matrix for modification
    (let ((A (matrix-copy matrix)))
      (do ((k 0 (+ k 1)))
          ((>= k rank) (list U S V))
        
        ;; Power iteration to find k-th singular vector
        (let* ((v (random-unit-vector n))
               (converged #f)
               (iterations 0))
          
          ;; Iterate until convergence
          (while (and (not converged) (< iterations 100))
            (let* ((Av (matrix-vector-multiply A v))
                   (AtAv (matrix-vector-multiply (matrix-transpose A) Av))
                   (norm (vector-norm AtAv))
                   (v-new (vector-scale AtAv (/ 1.0 norm))))
              
              ;; Check convergence
              (set! converged (< (vector-distance v v-new) tolerance))
              (set! v v-new)
              (set! iterations (+ iterations 1))))
          
          ;; Compute singular value and vectors
          (let* ((Av (matrix-vector-multiply A v))
                 (sigma (vector-norm Av))
                 (u (if (> sigma 1e-10) 
                        (vector-scale Av (/ 1.0 sigma))
                        (random-unit-vector m))))
            
            ;; Store results
            (vector-set! S k sigma)
            (matrix-set-column! U k u)
            (matrix-set-column! V k v)
            
            ;; Deflate matrix: A = A - sigma * u * v^T
            (deflate-matrix! A sigma u v)))))))

;; Analyze SVD components for significance
(define (analyze-components U S V threshold)
  "Analyze SVD components and filter by significance"
  (let* ((rank (vector-length S))
         (total-energy (fold + 0 (map (lambda (s) (* s s)) 
                                     (vector->list S))))
         (significant '()))
    
    ;; Find components above threshold
    (do ((i 0 (+ i 1)))
        ((>= i rank) (reverse significant))
      (let* ((sigma (vector-ref S i))
             (energy-ratio (/ (* sigma sigma) total-energy)))
        (when (> energy-ratio threshold)
          (set! significant 
                (cons (list i sigma energy-ratio
                           (matrix-column U i)
                           (matrix-column V i))
                      significant)))))))

;; Aggregate weight matrices across multiple inputs
(define (aggregate-weights matrices method)
  "Combine weight matrices from multiple analyses"
  (case method
    ((mean) (aggregate-mean matrices))
    ((median) (aggregate-median matrices))
    ((robust-mean) (aggregate-robust-mean matrices))
    (else (error "Unknown aggregation method" method))))

;; Mean aggregation
(define (aggregate-mean matrices)
  "Compute element-wise mean of matrices"
  (if (null? matrices)
      #f
      (let* ((m (vector-length (car matrices)))
             (n (vector-length (vector-ref (car matrices) 0)))
             (result (make-matrix m n))
             (count (length matrices)))
        
        ;; Sum all matrices
        (for-each 
         (lambda (matrix)
           (do ((i 0 (+ i 1)))
               ((>= i m))
             (do ((j 0 (+ j 1)))
                 ((>= j n))
               (let ((current (matrix-ref result i j))
                     (value (matrix-ref matrix i j)))
                 (matrix-set! result i j (+ current value))))))
         matrices)
        
        ;; Divide by count
        (do ((i 0 (+ i 1)))
            ((>= i m) result)
          (do ((j 0 (+ j 1)))
              ((>= j n))
            (let ((value (matrix-ref result i j)))
              (matrix-set! result i j (/ value count))))))))

;; Find components that appear consistently across inputs
(define (find-stable-components component-sets stability-threshold)
  "Identify components that appear consistently across analyses"
  (let ((component-map (make-hash-table))
        (n-sets (length component-sets)))
    
    ;; Count component occurrences
    (for-each
     (lambda (components)
       (for-each
        (lambda (comp)
          (let ((key (component-signature comp)))
            (hash-update! component-map key 1+ 0)))
        components))
     component-sets)
    
    ;; Filter by stability threshold
    (hash-fold
     (lambda (key count stable)
       (if (>= (/ count n-sets) stability-threshold)
           (cons key stable)
           stable))
     '()
     component-map)))

;; Component similarity metric
(define (component-similarity comp1 comp2)
  "Compute similarity between two components"
  (let ((v1 (list-ref comp1 4))  ; V vector
        (v2 (list-ref comp2 4))) ; V vector
    (abs (dot-product v1 v2))))

;; Generate component signature for matching
(define (component-signature component)
  "Create a signature for component matching"
  (let ((v-vector (list-ref component 4)))
    ;; Use top features as signature
    (take (sort-by-magnitude v-vector) 5)))

;; Rank components by importance
(define (rank-components components)
  "Sort components by singular value (importance)"
  (sort components
        (lambda (a b)
          (> (list-ref a 1) (list-ref b 1)))))

;; Extract principal components for visualization
(define (extract-principal-components matrices n-components)
  "Extract top principal components across all matrices"
  (let* ((aggregated (aggregate-weights matrices 'mean))
         (svd-result (matrix-svd aggregated n-components 1e-6))
         (U (car svd-result))
         (S (cadr svd-result))
         (V (caddr svd-result)))
    
    (analyze-components U S V 0.01))) ; 1% energy threshold

;; Helper functions

(define (matrix-copy matrix)
  "Create a copy of matrix"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (copy (make-matrix m n)))
    (do ((i 0 (+ i 1)))
        ((>= i m) copy)
      (do ((j 0 (+ j 1)))
          ((>= j n))
        (matrix-set! copy i j (matrix-ref matrix i j))))))

(define (matrix-transpose matrix)
  "Compute matrix transpose"
  (let* ((m (vector-length matrix))
         (n (if (> m 0) (vector-length (vector-ref matrix 0)) 0))
         (result (make-matrix n m)))
    (do ((i 0 (+ i 1)))
        ((>= i m) result)
      (do ((j 0 (+ j 1)))
          ((>= j n))
        (matrix-set! result j i (matrix-ref matrix i j))))))

(define (random-unit-vector n)
  "Generate random unit vector of length n"
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (vector-set! v i (- (random:uniform) 0.5)))
    (let ((norm (vector-norm v)))
      (vector-scale v (/ 1.0 norm)))))

(define (vector-norm v)
  "Compute L2 norm of vector"
  (sqrt (fold + 0 (map (lambda (x) (* x x)) (vector->list v)))))

(define (vector-scale v scale)
  "Scale vector by constant"
  (list->vector (map (lambda (x) (* x scale)) (vector->list v))))

(define (vector-distance v1 v2)
  "Compute distance between vectors"
  (vector-norm (vector-subtract v1 v2)))

(define (vector-subtract v1 v2)
  "Subtract v2 from v1"
  (list->vector (map - (vector->list v1) (vector->list v2))))

(define (matrix-ref matrix i j)
  "Get matrix element"
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set! matrix i j value)
  "Set matrix element"
  (vector-set! (vector-ref matrix i) j value))

(define (matrix-column matrix j)
  "Extract column j from matrix"
  (let* ((m (vector-length matrix))
         (col (make-vector m)))
    (do ((i 0 (+ i 1)))
        ((>= i m) col)
      (vector-set! col i (matrix-ref matrix i j)))))

(define (matrix-set-column! matrix j col)
  "Set column j of matrix"
  (let ((m (vector-length matrix)))
    (do ((i 0 (+ i 1)))
        ((>= i m))
      (matrix-set! matrix i j (vector-ref col i)))))

(define (deflate-matrix! A sigma u v)
  "Remove rank-1 component from matrix: A = A - sigma * u * v^T"
  (let ((m (vector-length A))
        (n (vector-length (vector-ref A 0))))
    (do ((i 0 (+ i 1)))
        ((>= i m))
      (do ((j 0 (+ j 1)))
          ((>= j n))
        (let* ((current (matrix-ref A i j))
               (correction (* sigma (vector-ref u i) (vector-ref v j))))
          (matrix-set! A i j (- current correction)))))))

(define (sort-by-magnitude vector)
  "Sort vector indices by absolute value"
  (let ((indexed (map cons (vector->list vector) (iota (vector-length vector)))))
    (map cdr (sort indexed (lambda (a b) (> (abs (car a)) (abs (car b))))))))

(define (hash-update! table key fn default)
  "Update hash table value with function"
  (hash-set! table key 
             (fn (hash-ref table key default))))