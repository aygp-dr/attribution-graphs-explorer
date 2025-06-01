#!/usr/bin/env guile
!#

;; Load and run all examples
(add-to-load-path (dirname (current-filename)))
(add-to-load-path "..")

(use-modules (attribution-graphs examples poetry-circuit)
             (attribution-graphs examples reasoning-circuit)
             (attribution-graphs examples interactive-demo))

(display "Attribution Graphs Explorer\n")
(display "===========================\n\n")

;; Run examples with mock data
(display "Available examples:\n")
(display "1. Poetry Circuit Analysis (traditional)\n")
(display "2. Reasoning Circuit Analysis (traditional)\n")
(display "3. Interactive Web Interface Demo (NEW!)\n\n")

;; Check command line arguments
(let ((args (command-line)))
  (cond
   ((and (> (length args) 1) (string= (cadr args) "interactive"))
    (display "Running interactive web interface demo...\n\n")
    (run-interactive-demo))
   
   ((and (> (length args) 1) (string= (cadr args) "comparison"))
    (display "Creating comparison graphs...\n\n")
    (create-comparison-demo))
   
   (else
    (display "Usage:\n")
    (display "  guile run-all.scm interactive  # Launch interactive demo\n")
    (display "  guile run-all.scm comparison   # Create comparison graphs\n")
    (display "  guile run-all.scm              # Show this help\n\n")
    
    (display "For the interactive interface:\n")
    (display "1. Run: guile run-all.scm interactive\n")
    (display "2. In another terminal: guile -L . src/interface/server.scm\n")
    (display "3. Open browser to: http://localhost:8080\n")
    (display "4. Load the generated JSON file\n\n")
    
    (display "Note: Traditional examples require actual model implementations\n")
    (display "and are not included in this demo.\n"))))

;; Note: These require actual model implementations
;; This is for demonstration purposes
