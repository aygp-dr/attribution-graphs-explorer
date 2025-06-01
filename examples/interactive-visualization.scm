#!/usr/bin/env guile3
!#

;; Interactive Visualization Example
;; Demonstrates the interactive attribution graph explorer

(add-to-load-path ".")

(use-modules (attribution-graphs graph structure)
             (attribution-graphs graph attribution)
             (attribution-graphs circuits discovery)
             (attribution-graphs visualization interactive)
             (srfi srfi-1)
             (ice-9 format))

;; Example 1: Basic Interactive Exploration
(define (demo-basic-exploration)
  "Demonstrate basic interactive graph exploration"
  (format #t "=== Basic Interactive Exploration Demo ===~%")
  
  ;; Create a demo attribution graph
  (let* ((prompt "The capital of France is Paris")
         (graph (create-demo-attribution-graph prompt "Paris"))
         (circuits (find-demo-circuits graph)))
    
    (format #t "Created demo graph with ~a nodes and ~a edges~%"
            (length (graph-nodes graph))
            (length (graph-edges graph)))
    
    (format #t "Discovered ~a demonstration circuits~%" (length circuits))
    
    ;; Launch interactive explorer
    (format #t "~%Launching interactive explorer...~%")
    (format #t "The browser should open automatically to http://localhost:8080~%")
    (format #t "Use the interface to:~%")
    (format #t "  - Pan and zoom the graph~%")
    (format #t "  - Click nodes to see details~%")
    (format #t "  - Adjust attribution threshold~%")
    (format #t "  - Toggle node types~%")
    (format #t "  - Highlight circuits~%")
    (format #t "~%Press Ctrl+C to stop the server when done.~%~%")
    
    (start-interactive-explorer graph circuits)))

;; Example 2: Custom Configuration
(define (demo-custom-configuration)
  "Demonstrate explorer with custom configuration"
  (format #t "=== Custom Configuration Demo ===~%")
  
  (let* ((prompt "John went to the store to buy milk")
         (graph (create-demo-attribution-graph prompt "milk"))
         (circuits (find-demo-circuits graph)))
    
    ;; Start on custom port without auto-opening browser
    (format #t "Starting explorer on port 9000~%")
    (format #t "Visit http://localhost:9000 manually~%")
    
    (start-interactive-explorer graph circuits 
                               #:port 9000 
                               #:auto-open #f)))

;; Example 3: Circuit-Focused Exploration
(define (demo-circuit-exploration)
  "Demonstrate focused circuit exploration"
  (format #t "=== Circuit-Focused Exploration Demo ===~%")
  
  (let* ((prompt "Marie Curie won the Nobel Prize for her work on radioactivity")
         (graph (create-demo-attribution-graph prompt "Nobel"))
         (circuits (find-specific-demo-circuits graph)))
    
    (format #t "Created graph focused on factual recall circuits~%")
    (format #t "Circuits found:~%")
    (for-each (lambda (circuit index)
                (format #t "  Circuit ~a: ~a nodes~%" 
                        (+ index 1) 
                        (length (car circuit))))
              circuits
              (iota (length circuits)))
    
    (start-interactive-explorer graph circuits)))

;; Example 4: Batch Export for External Analysis
(define (demo-batch-export)
  "Demonstrate exporting visualization data"
  (format #t "=== Batch Export Demo ===~%")
  
  (let* ((prompt "The quick brown fox jumps over the lazy dog")
         (graph (create-demo-attribution-graph prompt "dog"))
         (circuits (find-demo-circuits graph))
         (output-dir "visualization-export"))
    
    ;; Export without starting server
    (format #t "Exporting visualization data to ~a/~%" output-dir)
    (export-for-web graph circuits output-dir)
    
    (format #t "~%Exported files can be used with external web servers~%")
    (format #t "or loaded into the static visualization interface.~%")))

;; Example 5: Integration with Existing Circuit Discovery
(define (demo-circuit-integration)
  "Demonstrate integration with circuit discovery module"
  (format #t "=== Circuit Discovery Integration Demo ===~%")
  
  (let* ((prompt "Shakespeare wrote Romeo and Juliet")
         (graph (create-demo-attribution-graph prompt "Shakespeare")))
    
    ;; Use circuit discovery module (if available)
    (format #t "Running circuit discovery...~%")
    (let ((discovered-circuits (find-circuits graph)))
      
      (format #t "Found ~a circuits using discovery algorithms~%" 
              (length discovered-circuits))
      
      ;; Launch explorer with discovered circuits
      (explore-discovered-circuits graph discovered-circuits))))

;; Helper: Create more realistic demo circuits
(define (find-specific-demo-circuits graph)
  "Find circuits that demonstrate specific patterns"
  (let ((circuits '())
        (nodes (map node-id (graph-nodes graph))))
    
    ;; Factual recall circuit: token -> features -> logit
    (when (>= (length nodes) 6)
      (let ((token-nodes (filter (lambda (id) 
                                   (string-prefix? "tok_" (symbol->string id))) 
                                 nodes))
            (feature-nodes (filter (lambda (id) 
                                     (string-prefix? "feat_" (symbol->string id))) 
                                   nodes))
            (logit-nodes (filter (lambda (id) 
                                   (string-prefix? "logit_" (symbol->string id))) 
                                 nodes)))
        
        ;; Create factual recall path
        (when (and (>= (length token-nodes) 1)
                   (>= (length feature-nodes) 3)
                   (>= (length logit-nodes) 1))
          (set! circuits 
                (cons (list (list (car token-nodes)
                                  (car feature-nodes)
                                  (cadr feature-nodes)
                                  (caddr feature-nodes)
                                  (car logit-nodes)))
                      circuits)))
        
        ;; Create attention circuit (shorter path)
        (when (>= (length feature-nodes) 2)
          (set! circuits
                (cons (list (list (car token-nodes)
                                  (car feature-nodes)
                                  (car logit-nodes)))
                      circuits)))))
    
    circuits))

;; Helper: Enhanced demo graph with more realistic structure
(define (create-enhanced-demo-graph prompt target-token)
  "Create a more realistic demo attribution graph"
  (let ((graph (make-attribution-graph))
        (tokens (string-split prompt #\space)))
    
    ;; Add input tokens with realistic activations
    (for-each (lambda (token index)
                (let ((metadata (make-hash-table)))
                  (hash-set! metadata 'token token)
                  (hash-set! metadata 'position index)
                  (hash-set! metadata 'type 'input)
                  (add-node! graph 
                             (make-node (string->symbol (format #f "tok_~a" index))
                                        'token
                                        (+ 0.8 (* 0.4 (random:uniform)))
                                        metadata))))
              tokens
              (iota (length tokens)))
    
    ;; Add hierarchical feature layers
    (do ((layer 0 (+ layer 1)))
        ((>= layer 8))
      (let ((features-in-layer (if (< layer 4) 15 10))) ; Fewer features in later layers
        (do ((feature 0 (+ feature 1)))
            ((>= feature features-in-layer))
          (let ((metadata (make-hash-table))
                (activation (* (- 1.2 (* layer 0.1)) (random:uniform))))
            (hash-set! metadata 'layer layer)
            (hash-set! metadata 'feature-index feature)
            (hash-set! metadata 'interpretation 
                       (cond 
                        ((< layer 2) (format #f "Low-level feature ~a.~a" layer feature))
                        ((< layer 5) (format #f "Mid-level concept ~a.~a" layer feature))
                        (else (format #f "High-level abstraction ~a.~a" layer feature))))
            (add-node! graph
                       (make-node (string->symbol (format #f "feat_~a_~a" layer feature))
                                  'feature
                                  activation
                                  metadata))))))
    
    ;; Add output logits
    (let ((output-tokens (list target-token "the" "is" "and" ".")))
      (for-each (lambda (token index)
                  (let ((metadata (make-hash-table)))
                    (hash-set! metadata 'token token)
                    (hash-set! metadata 'rank index)
                    (add-node! graph
                               (make-node (string->symbol (format #f "logit_~a" index))
                                          'logit
                                          (if (= index 0) 
                                              (+ 2.0 (random:uniform)) 
                                              (random:uniform))
                                          metadata))))
                output-tokens
                (iota (length output-tokens))))
    
    ;; Add structured edges with attribution patterns
    (let ((all-nodes (graph-nodes graph)))
      ;; Token to early features
      (for-each (lambda (token-node)
                  (for-each (lambda (feat-node)
                              (when (and (eq? (node-type token-node) 'token)
                                         (eq? (node-type feat-node) 'feature)
                                         (< (hash-ref (node-metadata feat-node) 'layer) 3))
                                (add-edge! graph
                                           (make-edge (node-id token-node)
                                                      (node-id feat-node)
                                                      (* (+ 0.3 (* 0.7 (random:uniform)))
                                                         (if (string-contains 
                                                              (hash-ref (node-metadata token-node) 'token)
                                                              target-token) 1.5 1.0))))))
                            all-nodes))
                (filter (lambda (n) (eq? (node-type n) 'token)) all-nodes))
      
      ;; Feature to feature (layer progression)
      (for-each (lambda (source-node)
                  (when (eq? (node-type source-node) 'feature)
                    (let ((source-layer (hash-ref (node-metadata source-node) 'layer)))
                      (for-each (lambda (target-node)
                                  (when (and (eq? (node-type target-node) 'feature)
                                             (= (hash-ref (node-metadata target-node) 'layer)
                                                (+ source-layer 1)))
                                    (when (< (random:uniform) 0.3) ; Sparse connections
                                      (add-edge! graph
                                                 (make-edge (node-id source-node)
                                                            (node-id target-node)
                                                            (- (* 1.4 (random:uniform)) 0.2))))))
                                all-nodes))))
                all-nodes)
      
      ;; High-level features to logits
      (for-each (lambda (feat-node)
                  (when (and (eq? (node-type feat-node) 'feature)
                             (>= (hash-ref (node-metadata feat-node) 'layer) 6))
                    (for-each (lambda (logit-node)
                                (when (eq? (node-type logit-node) 'logit)
                                  (add-edge! graph
                                             (make-edge (node-id feat-node)
                                                        (node-id logit-node)
                                                        (* (+ 0.5 (* 0.8 (random:uniform)))
                                                           (if (= (hash-ref (node-metadata logit-node) 'rank) 0)
                                                               1.3 0.7))))))
                              all-nodes)))
                all-nodes))
    
    graph))

;; Command-line interface
(define (main args)
  "Main function for command-line usage"
  (let ((demo-type (if (>= (length args) 2) (cadr args) "basic")))
    (case (string->symbol demo-type)
      ((basic) (demo-basic-exploration))
      ((custom) (demo-custom-configuration))
      ((circuits) (demo-circuit-exploration))
      ((export) (demo-batch-export))
      ((integration) (demo-circuit-integration))
      (else 
       (format #t "Usage: ~a [basic|custom|circuits|export|integration]~%" (car args))
       (format #t "~%Available demos:~%")
       (format #t "  basic       - Basic interactive exploration~%")
       (format #t "  custom      - Custom server configuration~%")
       (format #t "  circuits    - Circuit-focused exploration~%")
       (format #t "  export      - Batch export demonstration~%")
       (format #t "  integration - Circuit discovery integration~%")))))

;; Run main if this file is executed directly
(when (batch-mode?)
  (main (command-line)))