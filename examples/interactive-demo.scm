#!/usr/bin/env guile

;; Interactive Attribution Graph Interface Demo
;; This example shows how to use the new interactive web interface

(add-to-load-path ".")

(use-modules (attribution-graphs clt transcoder)
             (attribution-graphs graph attribution)
             (attribution-graphs graph structure)
             (attribution-graphs circuits discovery)
             (attribution-graphs circuits visualization)
             (attribution-graphs interface export)
             (ice-9 format))

;; Create a demo attribution graph for interactive exploration
(define (create-demo-graph)
  "Create a demo attribution graph showing poetry planning circuit"
  (let ((graph (make-attribution-graph)))
    
    ;; Add nodes
    (add-node! graph 
               (make-node 'token_0 'token 1.0 
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'token "Roses")
                           (hash-set! meta 'interpretation "Input token")
                           (hash-set! meta 'layer 0)
                           (hash-set! meta 'x 50)
                           (hash-set! meta 'y 100)
                           meta)))
    
    (add-node! graph
               (make-node 'feature_15 'feature 0.45
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'interpretation "Word boundary detection")
                           (hash-set! meta 'feature_index 15)
                           (hash-set! meta 'layer 5)
                           (hash-set! meta 'x 150)
                           (hash-set! meta 'y 50)
                           meta)))
    
    (add-node! graph
               (make-node 'feature_42 'feature 0.85
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'interpretation "Plans rhyming patterns")
                           (hash-set! meta 'feature_index 42)
                           (hash-set! meta 'layer 6)
                           (hash-set! meta 'x 250)
                           (hash-set! meta 'y 100)
                           meta)))
    
    (add-node! graph
               (make-node 'feature_23 'feature 0.32
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'interpretation "Word choice preferences")
                           (hash-set! meta 'feature_index 23)
                           (hash-set! meta 'layer 6)
                           (hash-set! meta 'x 250)
                           (hash-set! meta 'y 150)
                           meta)))
    
    (add-node! graph
               (make-node 'feature_67 'feature 0.72
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'interpretation "Detects rhyme potential")
                           (hash-set! meta 'feature_index 67)
                           (hash-set! meta 'layer 7)
                           (hash-set! meta 'x 350)
                           (hash-set! meta 'y 100)
                           meta)))
    
    (add-node! graph
               (make-node 'feature_89 'feature 0.58
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'interpretation "Phoneme matching")
                           (hash-set! meta 'feature_index 89)
                           (hash-set! meta 'layer 7)
                           (hash-set! meta 'x 350)
                           (hash-set! meta 'y 50)
                           meta)))
    
    (add-node! graph
               (make-node 'logit_blue 'logit 0.91
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'token "blue")
                           (hash-set! meta 'interpretation "Output prediction")
                           (hash-set! meta 'layer 8)
                           (hash-set! meta 'x 450)
                           (hash-set! meta 'y 100)
                           meta)))
    
    (add-node! graph
               (make-node 'logit_green 'logit 0.23
                         (let ((meta (make-hash-table)))
                           (hash-set! meta 'token "green")
                           (hash-set! meta 'interpretation "Alternative prediction")
                           (hash-set! meta 'layer 8)
                           (hash-set! meta 'x 450)
                           (hash-set! meta 'y 150)
                           meta)))
    
    ;; Add edges
    (add-edge! graph (make-edge 'token_0 'feature_15 0.32))
    (add-edge! graph (make-edge 'token_0 'feature_42 0.75))
    (add-edge! graph (make-edge 'feature_15 'feature_23 0.28))
    (add-edge! graph (make-edge 'feature_42 'feature_67 0.65))
    (add-edge! graph (make-edge 'feature_42 'feature_23 0.43))
    (add-edge! graph (make-edge 'feature_67 'feature_89 0.52))
    (add-edge! graph (make-edge 'feature_67 'logit_blue 0.78))
    (add-edge! graph (make-edge 'feature_89 'logit_blue 0.34))
    (add-edge! graph (make-edge 'feature_23 'logit_green 0.41))
    
    graph))

;; Define circuits for the demo
(define (create-demo-circuits)
  "Define circuits for the demo graph"
  (list
   ;; Primary poetry planning circuit
   '((name . "Poetry Planning Circuit")
     (path . ("token_0" "feature_42" "feature_67" "logit_blue"))
     (strength . 0.78)
     (type . "discovered")
     (confidence . 0.92))
   
   ;; Secondary phoneme matching circuit  
   '((name . "Phoneme Matching Circuit")
     (path . ("feature_42" "feature_67" "feature_89" "logit_blue"))
     (strength . 0.45)
     (type . "discovered")
     (confidence . 0.73))
   
   ;; Word choice circuit
   '((name . "Word Choice Circuit")
     (path . ("token_0" "feature_15" "feature_23" "logit_green"))
     (strength . 0.32)
     (type . "discovered")
     (confidence . 0.68))))

;; Demo function
(define (run-interactive-demo)
  "Run the interactive attribution graph demo"
  (format #t "=== Attribution Graph Explorer - Interactive Demo ===~%")
  (format #t "~%Creating demo graph...~%")
  
  (let ((graph (create-demo-graph))
        (circuits (create-demo-circuits)))
    
    ;; Display basic info
    (format #t "Graph created with ~a nodes and ~a edges~%"
            (length (graph-nodes graph))
            (length (graph-edges graph)))
    
    ;; Show traditional Mermaid output
    (format #t "~%Traditional Mermaid diagram:~%")
    (display (graph->mermaid graph))
    (newline)
    
    ;; Export to JSON for web interface
    (format #t "~%Exporting to JSON for web interface...~%")
    (export-graph-file graph "examples/demo-poetry-circuit.json" circuits)
    
    ;; Launch interactive interface
    (format #t "~%Launching interactive interface...~%")
    (launch-interactive-interface graph circuits 8080)
    
    ;; Instructions for user
    (format #t "~%=== Interactive Demo Instructions ===~%")
    (format #t "1. Open your web browser to: http://localhost:8080~%")
    (format #t "2. Click 'Load Graph' and select: examples/demo-poetry-circuit.json~%")
    (format #t "3. Try these features:~%")
    (format #t "   - Click on Feature 42 to see planning details~%")
    (format #t "   - Adjust the threshold slider to filter connections~%")
    (format #t "   - Check 'Poetry Planning Circuit' to highlight the main path~%")
    (format #t "   - Try different layouts (Force, Hierarchical, Circular)~%")
    (format #t "   - Right-click nodes for context menu options~%")
    (format #t "   - Use zoom controls or mouse wheel to navigate~%")
    (format #t "   - Export SVG or generate analysis reports~%")
    (format #t "~%4. Compare with other circuits using the circuit selector~%")
    (format #t "~%This demo shows how a language model plans rhyming in poetry,~%")
    (format #t "tracing the path from 'Roses' to 'blue' through planning features.~%")))

;; Additional utility: Create comparison graphs
(define (create-comparison-demo)
  "Create multiple graphs for comparison demonstration"
  (let ((graph1 (create-demo-graph))
        (circuits1 (create-demo-circuits)))
    
    ;; Create a second graph with different weights (simulating different method)
    (let ((graph2 (create-demo-graph))
          (circuits2 (create-demo-circuits)))
      
      ;; Modify some edge weights to simulate integrated gradients vs jacobian
      (for-each (lambda (edge)
                  (let ((weight (edge-weight edge)))
                    ;; Simulate integrated gradients showing different attributions
                    (set! (edge-weight edge) (* weight 0.9))))
                (graph-edges graph2))
      
      ;; Export both graphs
      (export-graph-file graph1 "examples/jacobian-poetry-circuit.json" circuits1)
      (export-graph-file graph2 "examples/intgrad-poetry-circuit.json" circuits2)
      
      (format #t "Created comparison graphs:~%")
      (format #t "  - examples/jacobian-poetry-circuit.json~%")
      (format #t "  - examples/intgrad-poetry-circuit.json~%")
      (format #t "Load both in separate browser tabs to compare!~%"))))

;; Run the demo
(define (main args)
  "Main entry point"
  (cond
   ((and (> (length args) 1) (string= (cadr args) "comparison"))
    (create-comparison-demo))
   (else
    (run-interactive-demo))))

;; Export demo functions for use in REPL
(export run-interactive-demo
        create-demo-graph
        create-demo-circuits
        create-comparison-demo)

;; If running as script
(when (defined? 'command-line)
  (main (command-line)))