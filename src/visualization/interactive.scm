(define-module (attribution-graphs visualization interactive)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs graph attribution)
  #:use-module (attribution-graphs circuits discovery)
  #:use-module (attribution-graphs visualization export)
  #:use-module (attribution-graphs visualization server)
  #:export (start-interactive-explorer
            explore-attribution-graph
            create-interactive-session
            export-for-web))

;; Start interactive attribution graph explorer
(define* (start-interactive-explorer graph circuits #:key (port 8080) (host "127.0.0.1") (auto-open #t))
  "Launch interactive web-based attribution graph explorer"
  (let ((server-thread #f))
    
    ;; Create static directory structure if it doesn't exist
    (ensure-static-directory-structure)
    
    ;; Export graph data for web interface
    (export-visualization-data graph circuits "src/visualization/static/data")
    
    ;; Start the visualization server
    (format #t "Starting Attribution Graphs Interactive Explorer...~%")
    (format #t "Server will be available at: http://~a:~a~%" host port)
    (format #t "Press Ctrl+C to stop the server~%~%")
    
    ;; Launch server (this blocks)
    (start-visualization-server graph circuits port host)
    
    ;; Open browser if requested
    (when auto-open
      (open-browser-to-explorer host port))))

;; Create an interactive exploration session
(define (create-interactive-session prompt target-token)
  "Create a complete attribution graph and launch interactive explorer"
  (format #t "Creating interactive session for: ~a~%" prompt)
  (format #t "Target token: ~a~%" target-token)
  
  ;; This would typically involve:
  ;; 1. Loading a language model
  ;; 2. Computing attribution graph
  ;; 3. Finding circuits
  ;; 4. Launching interactive explorer
  
  ;; For now, create a demonstration graph
  (let* ((demo-graph (create-demo-attribution-graph prompt target-token))
         (demo-circuits (find-demo-circuits demo-graph)))
    
    (format #t "Generated demo graph with ~a nodes and ~a edges~%"
            (length (graph-nodes demo-graph))
            (length (graph-edges demo-graph)))
    
    (start-interactive-explorer demo-graph demo-circuits)))

;; Simplified exploration function
(define (explore-attribution-graph graph circuits)
  "Launch interactive explorer for existing graph and circuits"
  (start-interactive-explorer graph circuits))

;; Export graph data for web visualization
(define (export-for-web graph circuits output-dir)
  "Export attribution graph and circuits for web visualization"
  (unless (file-exists? output-dir)
    (mkdir output-dir))
    
  (export-visualization-data graph circuits output-dir)
  
  (format #t "Exported visualization data to: ~a~%" output-dir)
  (format #t "Files created:~%")
  (format #t "  - graph.json (main graph data)~%")
  (format #t "  - graph-cytoscape.json (Cytoscape.js format)~%")
  (format #t "  - circuit-*.json (circuit data files)~%"))

;; Helper: Ensure static directory structure exists
(define (ensure-static-directory-structure)
  "Create necessary static file directories"
  (let ((dirs '("src/visualization/static"
                "src/visualization/static/css"
                "src/visualization/static/js"
                "src/visualization/static/data"
                "src/visualization/static/lib")))
    (for-each (lambda (dir)
                (unless (file-exists? dir)
                  (system (format #f "mkdir -p ~a" dir))))
              dirs)))

;; Helper: Open browser to explorer
(define (open-browser-to-explorer host port)
  "Attempt to open web browser to the explorer interface"
  (let ((url (format #f "http://~a:~a" host port)))
    (catch #t
      (lambda ()
        ;; Try different browser commands
        (or (system* "xdg-open" url)      ; Linux
            (system* "open" url)          ; macOS
            (system* "start" url)         ; Windows
            (begin
              (format #t "Could not auto-open browser. Please visit: ~a~%" url)
              #f)))
      (lambda (key . args)
        (format #t "Could not auto-open browser. Please visit: ~a~%" url)))))

;; Demo graph creation (for testing/demonstration)
(define (create-demo-attribution-graph prompt target-token)
  "Create a demonstration attribution graph for testing"
  (let ((graph (make-attribution-graph)))
    
    ;; Add token nodes
    (let ((tokens (string-split prompt #\space)))
      (for-each (lambda (token index)
                  (let ((metadata (make-hash-table)))
                    (hash-set! metadata 'token token)
                    (hash-set! metadata 'position index)
                    (add-node! graph 
                               (make-node (string->symbol (format #f "tok_~a" index))
                                          'token
                                          (+ 0.5 (* 0.3 (random:uniform)))
                                          metadata))))
                tokens
                (iota (length tokens))))
    
    ;; Add feature nodes (simulated)
    (do ((layer 0 (+ layer 1)))
        ((>= layer 6))
      (do ((feature 0 (+ feature 1)))
          ((>= feature 20))
        (let ((metadata (make-hash-table)))
          (hash-set! metadata 'layer layer)
          (hash-set! metadata 'feature-index feature)
          (hash-set! metadata 'interpretation 
                     (format #f "Feature ~a.~a" layer feature))
          (add-node! graph
                     (make-node (string->symbol (format #f "feat_~a_~a" layer feature))
                                'feature
                                (* (random:uniform) 2.0)
                                metadata)))))
    
    ;; Add logit nodes
    (let ((logit-metadata (make-hash-table)))
      (hash-set! logit-metadata 'token target-token)
      (add-node! graph
                 (make-node 'logit_target
                            'logit
                            (+ 1.0 (random:uniform))
                            logit-metadata)))
    
    ;; Add random edges with attribution weights
    (let ((nodes (graph-nodes graph)))
      (do ((i 0 (+ i 1)))
          ((>= i 200)) ; Create 200 random edges
        (let* ((source (list-ref nodes (random (length nodes))))
               (target (list-ref nodes (random (length nodes))))
               (weight (- (* 2.0 (random:uniform)) 1.0))) ; -1 to 1
          (unless (eq? (node-id source) (node-id target))
            (add-edge! graph 
                       (make-edge (node-id source) 
                                  (node-id target) 
                                  weight))))))
    
    graph))

;; Demo circuit discovery
(define (find-demo-circuits graph)
  "Find demonstration circuits in the graph"
  (let ((circuits '()))
    
    ;; Create some demo circuits
    (let ((nodes (map node-id (graph-nodes graph))))
      ;; Circuit 1: Token to feature to logit
      (when (>= (length nodes) 3)
        (set! circuits 
              (cons (list (list (car nodes) (cadr nodes) (caddr nodes)))
                    circuits)))
      
      ;; Circuit 2: Multi-hop path
      (when (>= (length nodes) 5)
        (set! circuits
              (cons (list (take nodes 5))
                    circuits))))
    
    circuits))

;; Configuration management
(define (load-visualization-config)
  "Load visualization configuration from file"
  (let ((config-file "visualization-config.scm"))
    (if (file-exists? config-file)
        (call-with-input-file config-file read)
        ;; Default configuration
        `((server-port . 8080)
          (server-host . "127.0.0.1")
          (auto-open-browser . #t)
          (theme . "default")
          (layout . "force-directed")
          (node-size . 8)
          (edge-thickness . 2)))))

(define (save-visualization-config config)
  "Save visualization configuration to file"
  (call-with-output-file "visualization-config.scm"
    (lambda (port)
      (write config port))))

;; Session management
(define *current-session* #f)

(define-record-type <visualization-session>
  (make-visualization-session graph circuits config server-info)
  visualization-session?
  (graph session-graph)
  (circuits session-circuits)
  (config session-config)
  (server-info session-server-info))

(define (get-current-session)
  "Get current visualization session"
  *current-session*)

(define (create-session graph circuits . args)
  "Create new visualization session"
  (let ((config (load-visualization-config)))
    (set! *current-session*
          (make-visualization-session graph circuits config #f))
    *current-session*))

;; Export functions for external use
(define (start-explorer-with-model model prompt target-token)
  "High-level function to start explorer with language model"
  ;; This would integrate with actual model loading and computation
  ;; For now, delegate to demo function
  (create-interactive-session prompt target-token))

;; Utility: Check if visualization dependencies are available
(define (check-visualization-dependencies)
  "Check if required dependencies for visualization are available"
  (let ((missing-deps '()))
    
    ;; Check for required Guile modules
    (catch #t
      (lambda () (use-modules (web server)))
      (lambda args (set! missing-deps (cons "web server" missing-deps))))
    
    ;; Check for static file directory
    (unless (file-exists? "src/visualization/static")
      (set! missing-deps (cons "static files directory" missing-deps)))
    
    (if (null? missing-deps)
        #t
        (begin
          (format #t "Missing visualization dependencies: ~a~%" 
                  (string-join missing-deps ", "))
          #f))))

;; Integration with existing circuit discovery
(define (explore-discovered-circuits graph circuits)
  "Launch explorer for circuits discovered by the discovery module"
  (format #t "Launching interactive explorer for ~a discovered circuits~%" 
          (length circuits))
  (start-interactive-explorer graph circuits))

;; Batch processing function
(define (batch-explore-prompts prompts target-tokens)
  "Create interactive sessions for multiple prompts"
  (for-each (lambda (prompt target)
              (format #t "~%=== Exploring: ~a ===~%" prompt)
              (create-interactive-session prompt target))
            prompts
            target-tokens))