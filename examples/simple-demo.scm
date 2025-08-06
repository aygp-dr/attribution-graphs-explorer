(define-module (attribution-graphs examples simple-demo)
  #:use-module (attribution-graphs clt transcoder)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs graph attribution)
  #:use-module (attribution-graphs circuits discovery)
  #:use-module (attribution-graphs validation perturbation)
  #:use-module (attribution-graphs math matrix)
  #:export (run-simple-demo))

;; Create mock model for demo
(define (create-mock-model)
  "Create a mock model for demonstration"
  (let ((layers (make-vector 4)))
    ;; Create simple layer functions
    (do ((i 0 (+ i 1)))
        ((>= i 4) layers)
      (vector-set! layers i 
                  (lambda (features)
                    (let ((output (make-vector (vector-length features))))
                      (do ((j 0 (+ j 1)))
                          ((>= j (vector-length features)))
                        (vector-set! output j 
                                   (+ (vector-ref features j)
                                      (- (random:uniform) 0.5))))
                      output))))
    ;; Create cross-layer transcoder
    (let ((clt (make-clt 0 '(1 2 3) 768 128 768)))
      `((layers . ,layers)
        (clt . ,clt)))))

;; Mock embedding function
(define (mock-embed-tokens text)
  "Create mock token embeddings from text"
  (let* ((tokens (string-split text #\space))
         (embeddings (make-vector (length tokens))))
    (do ((i 0 (+ i 1)))
        ((>= i (length tokens)) embeddings)
      (vector-set! embeddings i 
                  (make-random-vector 768)))))

;; Create random vector
(define (make-random-vector size)
  "Create vector with random values"
  (let ((vec (make-vector size)))
    (do ((i 0 (+ i 1)))
        ((>= i size) vec)
      (vector-set! vec i (- (random:uniform) 0.5)))))

;; Main demo function
(define (run-simple-demo)
  "Run a simple demonstration of attribution graphs"
  (display "Attribution Graphs Explorer - Simple Demo\n")
  (display "=======================================\n\n")
  
  ;; Create mock model
  (let ((model (create-mock-model)))
    (display "Created mock model with 4 layers\n")
    
    ;; Create sample input
    (let ((sample-text "The attribution graphs explore model circuits"))
      (display "Sample text: ")
      (display sample-text)
      (newline)
      
      ;; Create mock embeddings
      (let ((embeddings (mock-embed-tokens sample-text)))
        (display "Created mock embeddings with shape: ")
        (display (vector-length embeddings))
        (display "x")
        (display (vector-length (vector-ref embeddings 0)))
        (newline)
        
        ;; Create attribution graph
        (let* ((clt (assoc-ref model 'clt))
               (graph (create-demo-attribution-graph clt embeddings)))
          (display "Created attribution graph with ")
          (display (length (graph-nodes graph)))
          (display " nodes and ")
          (display (length (graph-edges graph)))
          (display " edges\n")
          
          ;; Find circuits
          (display "\nDiscovering circuits in attribution graph...\n")
          (let ((circuits (find-demo-circuits graph)))
            (display "Found ")
            (display (length circuits))
            (display " circuits\n\n")
            
            ;; Validate circuits
            (display "Validating circuits...\n")
            (let* ((mock-inputs (make-random-vector 5))
                   (validation-score (validate-circuit (car circuits) 
                                                     graph 
                                                     model 
                                                     (list mock-inputs))))
              (display "Validation score: ")
              (display validation-score)
              (newline)
              
              ;; Global weight analysis
              (display "\nPerforming global weight analysis...\n")
              (let* ((weight-matrices 
                     (list (make-random-matrix 10 10)
                           (make-random-matrix 10 10)
                           (make-random-matrix 10 10)))
                     (analysis (global-weight-analysis weight-matrices)))
                (display "Analysis complete, found ")
                (display (length (cadr analysis)))
                (display " principal components\n\n")
                
                ;; Return results
                `((graph . ,graph)
                  (circuits . ,circuits)
                  (validation . ,validation-score)
                  (analysis . ,analysis)))))))))

;; Create demo attribution graph
(define (create-demo-attribution-graph clt embeddings)
  "Create a simple attribution graph for demo"
  (let ((graph (make-attribution-graph)))
    ;; Create some nodes
    (do ((i 0 (+ i 1)))
        ((>= i 10))
      (let ((node (make-node 
                  (string-append "input-" (number->string i))
                  'token
                  0.5
                  (list (cons 'position i)))))
        (add-node! graph node)))
    
    ;; Create some feature nodes
    (do ((i 0 (+ i 1)))
        ((>= i 15))
      (let ((node (make-node 
                  (string-append "feature-" (number->string i))
                  'feature
                  (- (random:uniform) 0.2)
                  (list (cons 'layer (random 3))
                        (cons 'index i)))))
        (add-node! graph node)))
    
    ;; Create some output nodes
    (do ((i 0 (+ i 1)))
        ((>= i 5))
      (let ((node (make-node 
                  (string-append "output-" (number->string i))
                  'logit
                  (random:uniform)
                  (list (cons 'token i)))))
        (add-node! graph node)))
    
    ;; Create some edges
    (do ((i 0 (+ i 1)))
        ((>= i 50))
      (let* ((source-type (if (< (random:uniform) 0.3) 'input 'feature))
             (target-type (if (< (random:uniform) 0.7) 'feature 'output))
             (source-id (string-append 
                        (if (eq? source-type 'input) "input-" "feature-")
                        (number->string (random (if (eq? source-type 'input) 10 15)))))
             (target-id (string-append 
                        (if (eq? target-type 'feature) "feature-" "output-")
                        (number->string (random (if (eq? target-type 'feature) 15 5)))))
             (edge (make-edge source-id target-id (- (random:uniform) 0.5))))
        (add-edge! graph edge)))
    
    graph))

;; Find demo circuits
(define (find-demo-circuits graph)
  "Find circuits in demo graph"
  (let ((paths '()))
    ;; Try to find paths from inputs to outputs
    (do ((i 0 (+ i 1)))
        ((>= i 5))
      (do ((j 0 (+ j 1)))
          ((>= j 3))
        (let* ((source-id (string-append "input-" (number->string j)))
               (target-id (string-append "output-" (number->string i)))
               (path (trace-path graph source-id target-id)))
          (when path
            (set! paths (cons path paths))))))
    
    ;; Return found paths
    paths))

;; Run the demo when file is executed directly
(when (batch-mode?) (run-simple-demo))