(define-module (attribution-graphs interface server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (attribution-graphs interface export)
  #:export (start-visualization-server
            serve-static-file
            handle-api-request))

;; Simple HTTP server for serving the web interface
(define (start-visualization-server port)
  "Start HTTP server for web interface on specified port"
  (format #t "Starting Attribution Graph Explorer server on port ~a~%" port)
  (format #t "Open your browser to: http://localhost:~a~%" port)
  (run-server request-handler 'http `(#:port ,port)))

;; Main request handler
(define (request-handler request body)
  "Handle incoming HTTP requests"
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request)))
    
    (format #t "~a ~a~%" method path)
    
    (cond
     ;; API endpoints
     ((string-prefix? "/api/" path)
      (handle-api-request request body))
     
     ;; Root redirect to index.html
     ((string= path "/")
      (serve-static-file "interface/web/index.html"))
     
     ;; Static files
     (else
      (serve-static-file (string-append "interface/web" path))))))

;; Serve static files (HTML, CSS, JS)
(define (serve-static-file file-path)
  "Serve static files from the web interface directory"
  (let ((full-path (string-append (getcwd) "/" file-path)))
    (if (file-exists? full-path)
        (let* ((content (call-with-input-file full-path get-string-all))
               (mime-type (get-mime-type file-path)))
          (values (build-response
                   #:code 200
                   #:headers `((content-type . (,mime-type))))
                  content))
        ;; File not found
        (values (build-response
                 #:code 404
                 #:headers '((content-type . (text/plain))))
                "File not found"))))

;; Get MIME type based on file extension
(define (get-mime-type file-path)
  "Determine MIME type from file extension"
  (cond
   ((string-suffix? ".html" file-path) 'text/html)
   ((string-suffix? ".css" file-path) 'text/css)
   ((string-suffix? ".js" file-path) 'application/javascript)
   ((string-suffix? ".json" file-path) 'application/json)
   ((string-suffix? ".svg" file-path) 'image/svg+xml)
   ((string-suffix? ".png" file-path) 'image/png)
   ((string-suffix? ".jpg" file-path) 'image/jpeg)
   ((string-suffix? ".jpeg" file-path) 'image/jpeg)
   (else 'text/plain)))

;; Handle API requests
(define (handle-api-request request body)
  "Handle API requests for graph data and operations"
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request)))
    
    (cond
     ;; GET /api/graphs - List available graphs
     ((and (eq? method 'GET) (string= path "/api/graphs"))
      (handle-list-graphs))
     
     ;; GET /api/graph/{id} - Get specific graph data
     ((and (eq? method 'GET) (string-prefix? "/api/graph/" path))
      (let ((graph-id (substring path 11)))
        (handle-get-graph graph-id)))
     
     ;; POST /api/export - Export current graph
     ((and (eq? method 'POST) (string= path "/api/export"))
      (handle-export-graph body))
     
     ;; POST /api/discover-circuits - Run circuit discovery
     ((and (eq? method 'POST) (string= path "/api/discover-circuits"))
      (handle-discover-circuits body))
     
     ;; Unknown API endpoint
     (else
      (values (build-response
               #:code 404
               #:headers '((content-type . (application/json))))
              "{\"error\": \"API endpoint not found\"}")))))

;; API endpoint handlers
(define (handle-list-graphs)
  "Return list of available graph files"
  (let ((graph-files (filter (lambda (f) (string-suffix? ".json" f))
                             (scan-directory "examples/"))))
    (values (build-response
             #:code 200
             #:headers '((content-type . (application/json))))
            (format #f "{\"graphs\": [~{\"~a\"~^, ~}]}" graph-files))))

(define (handle-get-graph graph-id)
  "Return specific graph data"
  (let ((file-path (string-append "examples/" graph-id ".json")))
    (if (file-exists? file-path)
        (let ((content (call-with-input-file file-path get-string-all)))
          (values (build-response
                   #:code 200
                   #:headers '((content-type . (application/json))))
                  content))
        (values (build-response
                 #:code 404
                 #:headers '((content-type . (application/json))))
                "{\"error\": \"Graph not found\"}"))))

(define (handle-export-graph body)
  "Handle graph export requests"
  ;; This would integrate with the export module
  (values (build-response
           #:code 200
           #:headers '((content-type . (application/json))))
          "{\"status\": \"Export functionality coming soon\"}"))

(define (handle-discover-circuits body)
  "Handle circuit discovery requests"
  ;; This would integrate with the circuit discovery module
  (values (build-response
           #:code 200
           #:headers '((content-type . (application/json))))
          "{\"status\": \"Circuit discovery integration coming soon\"}"))

;; Utility functions
(define (scan-directory directory)
  "Scan directory for files (simplified)"
  (if (file-exists? directory)
      (let ((files '()))
        (ftw directory
             (lambda (filename statinfo flag)
               (when (eq? flag 'regular)
                 (set! files (cons (basename filename) files)))
               #t))
        files)
      '()))

;; Example: Create sample graph data
(define (create-sample-graph)
  "Create sample graph data for testing"
  (let ((sample-graph 
         `((nodes . #((id . "token_0")
                      (type . "token")
                      (activation . 1.0)
                      (layer . 0)
                      (metadata . ((token . "Roses")
                                   (interpretation . "Input token"))))
                     ((id . "feature_42")
                      (type . "feature")
                      (activation . 0.85)
                      (layer . 6)
                      (metadata . ((interpretation . "Plans rhyming patterns")
                                   (feature_index . 42))))
                     ((id . "feature_67")
                      (type . "feature")
                      (activation . 0.72)
                      (layer . 7)
                      (metadata . ((interpretation . "Detects rhyme potential")
                                   (feature_index . 67))))
                     ((id . "logit_blue")
                      (type . "logit")
                      (activation . 0.91)
                      (layer . 8)
                      (metadata . ((token . "blue")
                                   (interpretation . "Output prediction"))))))
           (edges . #(((source . "token_0")
                       (target . "feature_42")
                       (weight . 0.75))
                      ((source . "feature_42")
                       (target . "feature_67")
                       (weight . 0.65))
                      ((source . "feature_67")
                       (target . "logit_blue")
                       (weight . 0.78))))
           (circuits . #(((name . "Poetry Planning Circuit")
                          (path . #("token_0" "feature_42" "feature_67" "logit_blue"))
                          (strength . 0.78)
                          (type . "discovered"))))
           (metadata . ((method . "Jacobian Attribution")
                        (node_count . 4)
                        (edge_count . 3)
                        (export_timestamp . "2025-06-01T01:00:00Z"))))))
    
    ;; Ensure examples directory exists
    (unless (file-exists? "examples")
      (mkdir "examples"))
    
    ;; Write sample to file
    (with-output-to-file "examples/poetry-circuit.json"
      (lambda ()
        (display (scm->json sample-graph))
        (newline)))
    
    (format #t "Created sample graph: examples/poetry-circuit.json~%")))

;; Server startup script
(define (main args)
  "Main entry point for server script"
  (let ((port (if (> (length args) 1)
                  (string->number (cadr args))
                  8080)))
    
    ;; Create sample data
    (create-sample-graph)
    
    ;; Start server
    (start-visualization-server port)))

;; If running as script
(when (and (defined? 'command-line)
           (not (null? (command-line))))
  (main (command-line)))