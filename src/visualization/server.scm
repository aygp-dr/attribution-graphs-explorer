(define-module (attribution-graphs visualization server)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs visualization export)
  #:export (start-visualization-server
            stop-visualization-server
            make-visualization-handler))

;; Server state
(define *server-port* 8080)
(define *server-host* "127.0.0.1")
(define *static-dir* "src/visualization/static")
(define *current-graph* #f)
(define *current-circuits* '())

;; MIME type mapping
(define mime-types
  '(("html" . "text/html")
    ("css" . "text/css") 
    ("js" . "application/javascript")
    ("json" . "application/json")
    ("png" . "image/png")
    ("svg" . "image/svg+xml")
    ("ico" . "image/x-icon")))

;; Get MIME type from file extension
(define (get-mime-type path)
  "Get MIME type based on file extension"
  (let* ((ext-match (string-match "\\.([^.]+)$" path))
         (ext (if ext-match (match:substring ext-match 1) "")))
    (or (assoc-ref mime-types ext) "text/plain")))

;; Read file safely
(define (read-static-file path)
  "Read static file, return content and success flag"
  (let ((full-path (string-append *static-dir* "/" path)))
    (catch #t
      (lambda ()
        (if (file-exists? full-path)
            (let ((mime-type (get-mime-type path)))
              (if (string-prefix? "text/" mime-type)
                  ;; Text file
                  (values (call-with-input-file full-path get-string-all) mime-type #t)
                  ;; Binary file  
                  (values (call-with-input-file full-path get-bytevector-all) mime-type #t)))
            (values #f #f #f)))
      (lambda (key . args)
        (values #f #f #f)))))

;; Generate index.html with embedded data
(define (generate-index-html graph circuits)
  "Generate main HTML page with embedded graph data"
  (format #f "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Attribution Graphs Explorer - Interactive Visualization</title>
    <link rel=\"stylesheet\" href=\"/css/style.css\">
    <script src=\"https://d3js.org/d3.v7.min.js\"></script>
    <script src=\"https://unpkg.com/cytoscape@3.26.0/dist/cytoscape.min.js\"></script>
    <script src=\"https://unpkg.com/cytoscape-cose-bilkent@4.1.0/cytoscape-cose-bilkent.js\"></script>
</head>
<body>
    <div id=\"app\">
        <header class=\"header\">
            <div class=\"title\">
                <h1>Attribution Graphs Explorer</h1>
                <p>Interactive Visualization</p>
            </div>
            <nav class=\"nav\">
                <button id=\"btn-file\">File</button>
                <button id=\"btn-view\">View</button>
                <button id=\"btn-analysis\">Analysis</button>
                <button id=\"btn-export\">Export</button>
                <button id=\"btn-help\">Help</button>
            </nav>
        </header>

        <main class=\"main-content\">
            <aside class=\"control-panel\">
                <section class=\"filtering\">
                    <h3>Filtering</h3>
                    <div class=\"control-group\">
                        <label for=\"threshold-slider\">Attribution Threshold:</label>
                        <input type=\"range\" id=\"threshold-slider\" min=\"0\" max=\"1\" step=\"0.01\" value=\"0.1\">
                        <span id=\"threshold-value\">0.1</span>
                    </div>
                    <div class=\"control-group\">
                        <label>Node Types:</label>
                        <label><input type=\"checkbox\" id=\"show-features\" checked> Features</label>
                        <label><input type=\"checkbox\" id=\"show-tokens\" checked> Tokens</label>
                        <label><input type=\"checkbox\" id=\"show-logits\" checked> Logits</label>
                        <label><input type=\"checkbox\" id=\"show-errors\"> Errors</label>
                    </div>
                    <div class=\"control-group\">
                        <button id=\"apply-filters\">Apply</button>
                        <button id=\"reset-filters\">Reset</button>
                    </div>
                </section>

                <section class=\"circuits\">
                    <h3>Circuit Analysis</h3>
                    <div class=\"circuit-list\">
                        <label><input type=\"radio\" name=\"circuit\" value=\"all\" checked> Show All</label>
                        <!-- Circuit options will be populated by JavaScript -->
                    </div>
                    <div class=\"control-group\">
                        <button id=\"discover-circuits\">Discover New...</button>
                        <button id=\"compare-circuits\">Compare Circuits...</button>
                    </div>
                </section>
            </aside>

            <div class=\"graph-container\">
                <div id=\"graph-canvas\"></div>
                <div id=\"minimap\"></div>
                <div class=\"graph-controls\">
                    <button id=\"zoom-in\">+</button>
                    <button id=\"zoom-out\">-</button>
                    <button id=\"fit-view\">⌂</button>
                    <button id=\"reset-view\">↻</button>
                </div>
            </div>

            <aside class=\"info-panel\">
                <section class=\"node-details\">
                    <h3>Node Details</h3>
                    <div id=\"node-info\">
                        <p>Select a node to view details</p>
                    </div>
                </section>

                <section class=\"statistics\">
                    <h3>Statistics</h3>
                    <div id=\"stats-info\">
                        <div class=\"stat-item\">
                            <span class=\"stat-label\">Nodes:</span>
                            <span class=\"stat-value\" id=\"node-count\">0</span>
                        </div>
                        <div class=\"stat-item\">
                            <span class=\"stat-label\">Edges:</span>
                            <span class=\"stat-value\" id=\"edge-count\">0</span>
                        </div>
                        <div class=\"stat-item\">
                            <span class=\"stat-label\">Circuits:</span>
                            <span class=\"stat-value\" id=\"circuit-count\">0</span>
                        </div>
                    </div>
                </section>
            </aside>
        </main>

        <footer class=\"status-bar\">
            <span id=\"status\">Ready</span>
            <span id=\"selection-info\"></span>
            <span id=\"zoom-level\">Zoom: 100%</span>
        </footer>
    </div>

    <!-- Embedded graph data -->
    <script>
        window.ATTRIBUTION_GRAPH_DATA = ~a;
        window.CIRCUIT_DATA = ~a;
    </script>
    <script src=\"/js/utils.js\"></script>
    <script src=\"/js/graph.js\"></script>
    <script src=\"/js/ui.js\"></script>
</body>
</html>"
          (if graph (graph->json graph) "{}")
          (if (null? circuits) "[]" 
              (format #f "[~a]" 
                      (string-join 
                       (map (lambda (circuit) (circuit->json circuit graph)) circuits)
                       ", ")))))

;; HTTP request handler
(define (visualization-handler request request-body)
  "Handle HTTP requests for visualization server"
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request)))
    
    (cond
     ;; Root path - serve index.html
     ((or (string=? path "/") (string=? path "/index.html"))
      (values (build-response #:code 200
                              #:headers `((content-type text/html)))
              (generate-index-html *current-graph* *current-circuits*)))
     
     ;; API endpoints
     ((string-prefix? "/api/" path)
      (handle-api-request path method request-body))
     
     ;; Static files
     (else
      (handle-static-file path)))))

;; Handle API requests
(define (handle-api-request path method request-body)
  "Handle API endpoint requests"
  (cond
   ;; Get current graph data
   ((string=? path "/api/graph")
    (if *current-graph*
        (values (build-response #:code 200
                                #:headers `((content-type application/json)))
                (graph->json *current-graph*))
        (values (build-response #:code 404)
                "No graph loaded")))
   
   ;; Get circuit data
   ((string=? path "/api/circuits")
    (let ((circuits-json (format #f "[~a]"
                                 (string-join
                                  (map (lambda (circuit)
                                         (circuit->json circuit *current-graph*))
                                       *current-circuits*)
                                  ", "))))
      (values (build-response #:code 200
                              #:headers `((content-type application/json)))
              circuits-json)))
   
   ;; Update graph filters
   ((and (string=? path "/api/filter") (eq? method 'POST))
    (handle-filter-request request-body))
   
   ;; Export data
   ((string=? path "/api/export")
    (handle-export-request))
   
   ;; Unknown API endpoint
   (else
    (values (build-response #:code 404)
            "API endpoint not found"))))

;; Handle static file requests
(define (handle-static-file path)
  "Serve static files"
  (let-values (((content mime-type success?) (read-static-file (string-drop path 1))))
    (if success?
        (values (build-response #:code 200
                                #:headers `((content-type ,(string->symbol mime-type))))
                content)
        (values (build-response #:code 404)
                "File not found"))))

;; Handle filter requests
(define (handle-filter-request request-body)
  "Process filter updates from client"
  ;; TODO: Parse JSON request body and apply filters
  (values (build-response #:code 200
                          #:headers `((content-type application/json)))
          "{\"status\": \"filters applied\"}"))

;; Handle export requests  
(define (handle-export-request)
  "Handle export data requests"
  (if *current-graph*
      (let ((export-data (export-visualization-data *current-graph* *current-circuits* "/tmp/export")))
        (values (build-response #:code 200
                                #:headers `((content-type application/json)))
                (format #f "{\"status\": \"success\", \"message\": \"~a\"}" export-data)))
      (values (build-response #:code 404)
              "{\"status\": \"error\", \"message\": \"No graph loaded\"}")))

;; Create the request handler
(define (make-visualization-handler graph circuits)
  "Create HTTP handler with specific graph and circuits"
  (set! *current-graph* graph)
  (set! *current-circuits* circuits)
  visualization-handler)

;; Start the visualization server
(define (start-visualization-server graph circuits . args)
  "Start HTTP server for interactive visualization"
  (let ((port (if (null? args) *server-port* (car args)))
        (host (if (or (null? args) (null? (cdr args))) *server-host* (cadr args))))
    
    ;; Set global state
    (set! *current-graph* graph)
    (set! *current-circuits* circuits)
    (set! *server-port* port)
    (set! *server-host* host)
    
    ;; Create static directory if it doesn't exist
    (unless (file-exists? *static-dir*)
      (mkdir *static-dir*))
    
    ;; Start server
    (format #t "Starting visualization server on http://~a:~a~%" host port)
    (format #t "Graph: ~a nodes, ~a edges~%" 
            (if graph (length (graph-nodes graph)) 0)
            (if graph (length (graph-edges graph)) 0))
    (format #t "Circuits: ~a~%" (length circuits))
    
    (run-server visualization-handler
                `(#:host ,host #:port ,port))))

;; Stop the visualization server
(define (stop-visualization-server)
  "Stop the visualization server"
  ;; TODO: Implement server shutdown when Guile supports it
  (format #t "Server shutdown not implemented in this Guile version~%")
  (format #t "Use Ctrl+C to stop the server~%"))