(define-module (attribution-graphs visualization export)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (attribution-graphs graph structure)
  #:export (graph->json
            graph->cytoscape
            circuit->json
            serialize-node
            serialize-edge
            export-visualization-data))

;; JSON serialization helpers
(define (escape-json-string str)
  "Escape string for JSON output"
  (regexp-substitute/global #f "\"" str 'pre "\\\"" 'post))

(define (format-json-value val)
  "Format value for JSON output"
  (cond
   ((string? val) (format #f "\"~a\"" (escape-json-string val)))
   ((number? val) (format #f "~a" val))
   ((boolean? val) (if val "true" "false"))
   ((null? val) "null")
   (else (format #f "\"~a\"" val))))

;; Serialize node to JSON object
(define (serialize-node node)
  "Convert node to JSON object string"
  (let* ((id (node-id node))
         (type (symbol->string (node-type node)))
         (activation (node-activation node))
         (metadata (node-metadata node)))
    (format #f "{
  \"id\": ~a,
  \"type\": ~a,
  \"activation\": ~a,
  \"label\": ~a,
  \"metadata\": ~a
}"
            (format-json-value (symbol->string id))
            (format-json-value type)
            (format-json-value activation)
            (format-json-value (node->label node))
            (serialize-metadata metadata))))

;; Serialize edge to JSON object
(define (serialize-edge edge)
  "Convert edge to JSON object string"
  (let ((source (edge-source edge))
        (target (edge-target edge))
        (weight (edge-weight edge)))
    (format #f "{
  \"source\": ~a,
  \"target\": ~a,
  \"weight\": ~a,
  \"strength\": ~a
}"
            (format-json-value (symbol->string source))
            (format-json-value (symbol->string target))
            (format-json-value weight)
            (format-json-value (classify-edge-strength weight)))))

;; Serialize metadata hash table to JSON
(define (serialize-metadata metadata)
  "Convert metadata hash to JSON object"
  (if (hash-table? metadata)
      (let ((entries (hash-map->list 
                      (lambda (k v) 
                        (format #f "\"~a\": ~a" k (format-json-value v)))
                      metadata)))
        (format #f "{~a}" (string-join entries ", ")))
      "{}"))

;; Helper: Convert node to display label
(define (node->label node)
  "Generate display label for node"
  (let ((type (node-type node))
        (id (node-id node))
        (metadata (node-metadata node)))
    (case type
      ((feature)
       (let ((interpretation (and (hash-table? metadata)
                                  (hash-ref metadata 'interpretation #f))))
         (if interpretation
             (format #f "F~a: ~a" id interpretation)
             (format #f "Feature ~a" id))))
      ((token)
       (let ((token-text (and (hash-table? metadata)
                              (hash-ref metadata 'token #f))))
         (if token-text
             (format #f "Token: ~a" token-text)
             (format #f "Token ~a" id))))
      ((logit)
       (let ((token-text (and (hash-table? metadata)
                              (hash-ref metadata 'token #f))))
         (if token-text
             (format #f "Logit: ~a" token-text)
             (format #f "Logit ~a" id))))
      ((error) "Error")
      (else (symbol->string id)))))

;; Helper: Classify edge strength for visualization
(define (classify-edge-strength weight)
  "Classify edge weight into strength categories"
  (let ((abs-weight (abs weight)))
    (cond
     ((>= abs-weight 0.8) "very-strong")
     ((>= abs-weight 0.6) "strong")
     ((>= abs-weight 0.4) "medium")
     ((>= abs-weight 0.2) "weak")
     (else "very-weak"))))

;; Main JSON export function
(define (graph->json graph)
  "Export attribution graph as JSON for web visualization"
  (let ((nodes (graph-nodes graph))
        (edges (graph-edges graph)))
    (format #f "{
  \"nodes\": [
~a
  ],
  \"edges\": [
~a
  ],
  \"metadata\": {
    \"nodeCount\": ~a,
    \"edgeCount\": ~a,
    \"generatedAt\": \"~a\"
  }
}"
            (string-join (map serialize-node nodes) ",\n")
            (string-join (map serialize-edge edges) ",\n")
            (length nodes)
            (length edges)
            (strftime "%Y-%m-%dT%H:%M:%SZ" (gmtime (current-time))))))

;; Cytoscape.js format export
(define (graph->cytoscape graph)
  "Export attribution graph in Cytoscape.js format"
  (let ((nodes (graph-nodes graph))
        (edges (graph-edges graph)))
    (format #f "{
  \"elements\": {
    \"nodes\": [
~a
    ],
    \"edges\": [
~a
    ]
  },
  \"style\": [
~a
  ],
  \"layout\": {
    \"name\": \"cose\",
    \"idealEdgeLength\": 100,
    \"nodeOverlap\": 20
  }
}"
            (string-join (map serialize-cytoscape-node nodes) ",\n")
            (string-join (map serialize-cytoscape-edge edges) ",\n")
            (generate-cytoscape-styles))))

;; Cytoscape node serialization
(define (serialize-cytoscape-node node)
  "Convert node to Cytoscape.js format"
  (let ((id (symbol->string (node-id node)))
        (type (symbol->string (node-type node)))
        (label (node->label node))
        (activation (node-activation node)))
    (format #f "      {
        \"data\": {
          \"id\": ~a,
          \"label\": ~a,
          \"type\": ~a,
          \"activation\": ~a
        },
        \"classes\": ~a
      }"
            (format-json-value id)
            (format-json-value label)
            (format-json-value type)
            (format-json-value activation)
            (format-json-value type))))

;; Cytoscape edge serialization
(define (serialize-cytoscape-edge edge)
  "Convert edge to Cytoscape.js format"
  (let ((id (format #f "~a-~a" (edge-source edge) (edge-target edge)))
        (source (symbol->string (edge-source edge)))
        (target (symbol->string (edge-target edge)))
        (weight (edge-weight edge)))
    (format #f "      {
        \"data\": {
          \"id\": ~a,
          \"source\": ~a,
          \"target\": ~a,
          \"weight\": ~a
        },
        \"classes\": ~a
      }"
            (format-json-value id)
            (format-json-value source)
            (format-json-value target)
            (format-json-value weight)
            (format-json-value (classify-edge-strength weight)))))

;; Generate Cytoscape styles
(define (generate-cytoscape-styles)
  "Generate Cytoscape.js style definitions"
  "    {
      \"selector\": \"node\",
      \"style\": {
        \"background-color\": \"#666\",
        \"label\": \"data(label)\",
        \"text-valign\": \"center\",
        \"text-halign\": \"center\",
        \"font-size\": \"12px\"
      }
    },
    {
      \"selector\": \"node[type='feature']\",
      \"style\": {
        \"background-color\": \"#3498db\",
        \"shape\": \"ellipse\"
      }
    },
    {
      \"selector\": \"node[type='token']\",
      \"style\": {
        \"background-color\": \"#2ecc71\",
        \"shape\": \"rectangle\"
      }
    },
    {
      \"selector\": \"node[type='logit']\",
      \"style\": {
        \"background-color\": \"#9b59b6\",
        \"shape\": \"diamond\"
      }
    },
    {
      \"selector\": \"edge\",
      \"style\": {
        \"width\": \"mapData(weight, -1, 1, 1, 10)\",
        \"line-color\": \"#ccc\",
        \"target-arrow-color\": \"#ccc\",
        \"target-arrow-shape\": \"triangle\",
        \"curve-style\": \"bezier\"
      }
    },
    {
      \"selector\": \"edge.very-strong\",
      \"style\": {
        \"line-color\": \"#e74c3c\",
        \"target-arrow-color\": \"#e74c3c\",
        \"width\": 8
      }
    },
    {
      \"selector\": \"edge.strong\",
      \"style\": {
        \"line-color\": \"#f39c12\",
        \"target-arrow-color\": \"#f39c12\",
        \"width\": 6
      }
    }")

;; Circuit-specific export
(define (circuit->json circuit graph)
  "Export specific circuit as JSON with highlighted paths"
  (let* ((circuit-nodes (extract-circuit-nodes circuit))
         (circuit-edges (extract-circuit-edges circuit graph))
         (filtered-graph (filter-graph-to-circuit graph circuit-nodes)))
    (format #f "{
  \"circuit\": ~a,
  \"graph\": ~a,
  \"paths\": ~a,
  \"metadata\": {
    \"circuitLength\": ~a,
    \"nodeCount\": ~a,
    \"pathCount\": ~a
  }
}"
            (graph->json filtered-graph)
            (graph->json graph)
            (serialize-circuit-paths circuit)
            (length circuit)
            (length circuit-nodes)
            (length circuit))))

;; Helper: Extract nodes involved in circuit
(define (extract-circuit-nodes circuit)
  "Extract all unique nodes from circuit paths"
  (delete-duplicates (apply append circuit)))

;; Helper: Extract edges in circuit
(define (extract-circuit-edges circuit graph)
  "Extract edges that are part of circuit paths"
  (let ((circuit-edges '()))
    (for-each
     (lambda (path)
       (let loop ((nodes path))
         (when (>= (length nodes) 2)
           (let ((edge (find-edge graph (car nodes) (cadr nodes))))
             (when edge
               (set! circuit-edges (cons edge circuit-edges))))
           (loop (cdr nodes)))))
     circuit)
    (delete-duplicates circuit-edges)))

;; Helper: Find edge between two nodes
(define (find-edge graph source-id target-id)
  "Find edge between source and target nodes"
  (find (lambda (edge)
          (and (eq? (edge-source edge) source-id)
               (eq? (edge-target edge) target-id)))
        (graph-edges graph)))

;; Helper: Filter graph to circuit nodes
(define (filter-graph-to-circuit graph circuit-nodes)
  "Create subgraph containing only circuit nodes"
  (let ((filtered (make-attribution-graph)))
    ;; Add circuit nodes
    (for-each
     (lambda (node-id)
       (let ((node (find (lambda (n) (eq? (node-id n) node-id))
                         (graph-nodes graph))))
         (when node (add-node! filtered node))))
     circuit-nodes)
    ;; Add edges between circuit nodes
    (for-each
     (lambda (edge)
       (when (and (member (edge-source edge) circuit-nodes)
                  (member (edge-target edge) circuit-nodes))
         (add-edge! filtered edge)))
     (graph-edges graph))
    filtered))

;; Helper: Serialize circuit paths
(define (serialize-circuit-paths circuit)
  "Convert circuit paths to JSON array"
  (format #f "[~a]"
          (string-join
           (map (lambda (path)
                  (format #f "[~a]"
                          (string-join
                           (map (lambda (node-id)
                                  (format-json-value (symbol->string node-id)))
                                path)
                           ", ")))
                circuit)
           ",\n    ")))

;; Complete export function for interactive visualization
(define (export-visualization-data graph circuits output-dir)
  "Export all data needed for interactive visualization"
  (let ((graph-json (graph->json graph))
        (cytoscape-json (graph->cytoscape graph))
        (circuits-json (map (lambda (circuit) (circuit->json circuit graph)) circuits)))
    
    ;; Write main graph data
    (call-with-output-file (string-append output-dir "/graph.json")
      (lambda (port) (display graph-json port)))
    
    ;; Write Cytoscape format
    (call-with-output-file (string-append output-dir "/graph-cytoscape.json")
      (lambda (port) (display cytoscape-json port)))
    
    ;; Write circuits data
    (let loop ((circuits-data circuits-json) (index 0))
      (unless (null? circuits-data)
        (call-with-output-file (string-append output-dir "/circuit-" (number->string index) ".json")
          (lambda (port) (display (car circuits-data) port)))
        (loop (cdr circuits-data) (+ index 1))))
    
    ;; Return summary
    (format #f "Exported graph with ~a nodes, ~a edges, and ~a circuits to ~a"
            (length (graph-nodes graph))
            (length (graph-edges graph))
            (length circuits)
            output-dir)))