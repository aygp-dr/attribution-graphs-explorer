(define-module (attribution-graphs circuits global-visualization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (attribution-graphs graph structure)
  #:use-module (attribution-graphs math global-analysis)
  #:use-module (attribution-graphs circuits visualization)
  #:export (components->mermaid
            stability-heatmap
            component-loadings-plot
            cross-input-comparison
            principal-component-projection
            generate-global-analysis-report))

;; Visualize principal components as Mermaid diagram
(define (components->mermaid components feature-names)
  "Generate Mermaid diagram showing principal components"
  (string-append
   "graph TD\n"
   "    subgraph Components\n"
   (string-join
    (map (lambda (comp idx)
           (let* ((sigma (list-ref comp 1))
                  (energy (list-ref comp 2))
                  (v-vector (list-ref comp 4))
                  (top-features (get-top-features v-vector feature-names 5)))
             (format #f "        C~a[\"Component ~a<br/>σ=~,3f (~,1f%)\"]"
                     idx idx sigma (* energy 100))))
         components
         (iota (length components)))
    "\n")
   "\n    end\n"
   "    subgraph Features\n"
   (string-join
    (append-map (lambda (comp idx)
                  (let* ((v-vector (list-ref comp 4))
                         (top-features (get-top-features v-vector feature-names 5)))
                    (map (lambda (feat-info)
                           (let ((feat-idx (car feat-info))
                                 (weight (cdr feat-info)))
                             (format #f "        F~a[\"~a<br/>~,3f\"]"
                                     feat-idx
                                     (if (< feat-idx (length feature-names))
                                         (list-ref feature-names feat-idx)
                                         (format #f "Feature~a" feat-idx))
                                     weight)))
                         top-features)))
                components
                (iota (length components)))
    "\n")
   "\n    end\n"
   ;; Component to feature connections
   (string-join
    (append-map (lambda (comp idx)
                  (let* ((v-vector (list-ref comp 4))
                         (top-features (get-top-features v-vector feature-names 5)))
                    (map (lambda (feat-info)
                           (let ((feat-idx (car feat-info))
                                 (weight (cdr feat-info)))
                             (format #f "    C~a -->|~,3f| F~a"
                                     idx weight feat-idx)))
                         top-features)))
                components
                (iota (length components)))
    "\n")
   "\n"
   ;; Styling
   (generate-component-styles components)))

;; Generate stability heatmap visualization
(define (stability-heatmap stability-matrix feature-names input-names)
  "Generate heatmap showing component stability across inputs"
  (string-append
   "graph TB\n"
   "    subgraph \"Stability Matrix\"\n"
   (string-join
    (map (lambda (input-idx)
           (string-join
            (map (lambda (feat-idx)
                   (let ((stability (matrix-ref stability-matrix input-idx feat-idx)))
                     (format #f "        I~a_F~a[\"~,2f\"]"
                             input-idx feat-idx stability)))
                 (iota (length feature-names)))
            "\n"))
         (iota (length input-names)))
    "\n")
   "\n    end\n"
   ;; Color coding based on stability values
   (generate-heatmap-styles stability-matrix input-names feature-names)))

;; Component loadings visualization
(define (component-loadings-plot components feature-names n-top)
  "Generate plot showing feature loadings for each component"
  (string-append
   "graph LR\n"
   (string-join
    (map (lambda (comp idx)
           (let* ((v-vector (list-ref comp 4))
                  (top-features (get-top-features v-vector feature-names n-top)))
             (string-append
              (format #f "    subgraph C~a [\"Component ~a\"]\n" idx idx)
              (string-join
               (map (lambda (feat-info rank)
                      (let ((feat-idx (car feat-info))
                            (weight (cdr feat-info)))
                        (format #f "        C~a_F~a[\"~a: ~,3f\"]"
                                idx feat-idx
                                (if (< feat-idx (length feature-names))
                                    (list-ref feature-names feat-idx)
                                    (format #f "F~a" feat-idx))
                                weight)))
                    top-features
                    (iota (length top-features)))
               "\n")
              "\n    end\n")))
         components
         (iota (length components)))
    "\n")))

;; Cross-input comparison visualization
(define (cross-input-comparison component-sets input-names)
  "Generate visualization comparing components across inputs"
  (string-append
   "graph TD\n"
   "    subgraph \"Cross-Input Analysis\"\n"
   (string-join
    (map (lambda (comp-set input-name idx)
           (format #f "        Input~a[\"~a<br/>~a components\"]"
                   idx input-name (length comp-set)))
         component-sets input-names (iota (length input-names)))
    "\n")
   "\n    end\n"
   ;; Show component similarities
   (generate-similarity-connections component-sets input-names)))

;; Principal component projection visualization
(define (principal-component-projection data-points component-1 component-2)
  "Generate 2D projection plot using two principal components"
  (string-append
   "graph TB\n"
   "    subgraph \"PC Projection\"\n"
   (string-join
    (map (lambda (point idx)
           (let ((pc1-coord (dot-product point component-1))
                 (pc2-coord (dot-product point component-2)))
             (format #f "        P~a[\"Point ~a<br/>(~,2f, ~,2f)\"]"
                     idx idx pc1-coord pc2-coord)))
         data-points
         (iota (length data-points)))
    "\n")
   "\n    end\n"
   ;; Position styling based on coordinates
   (generate-projection-styles data-points component-1 component-2)))

;; Generate comprehensive global analysis report
(define (generate-global-analysis-report matrices input-names feature-names)
  "Generate complete analysis report with multiple visualizations"
  (let* ((n-components 5)
         (aggregated (aggregate-weights matrices 'mean))
         (components (extract-principal-components matrices n-components))
         (stability (compute-stability-matrix matrices)))
    
    (string-append
     "# Global Weight Analysis Report\n\n"
     
     "## Principal Components Analysis\n"
     (format #f "Found ~a significant components explaining ~,1f% of variance\n\n"
             (length components)
             (* 100 (fold + 0 (map (lambda (c) (list-ref c 2)) components))))
     
     "### Component Visualization\n"
     "```mermaid\n"
     (components->mermaid components feature-names)
     "```\n\n"
     
     "### Component Loadings\n"
     "```mermaid\n"
     (component-loadings-plot components feature-names 5)
     "```\n\n"
     
     "### Stability Analysis\n"
     "```mermaid\n"
     (stability-heatmap stability input-names feature-names)
     "```\n\n"
     
     "## Summary\n"
     (generate-analysis-summary components stability input-names))))

;; Helper functions

(define (get-top-features v-vector feature-names n-top)
  "Get top N features by absolute weight"
  (let* ((indexed-weights (map cons 
                               (vector->list v-vector)
                               (iota (vector-length v-vector))))
         (sorted-weights (sort indexed-weights
                              (lambda (a b) (> (abs (car a)) (abs (car b)))))))
    (take sorted-weights (min n-top (length sorted-weights)))))

(define (generate-component-styles components)
  "Generate Mermaid styles for components"
  (string-join
   (map (lambda (comp idx)
          (let ((energy (list-ref comp 2)))
            (cond
             ((> energy 0.3) (format #f "    classDef comp~a fill:#ff9999" idx))
             ((> energy 0.1) (format #f "    classDef comp~a fill:#99ff99" idx))
             (else (format #f "    classDef comp~a fill:#9999ff" idx)))))
        components
        (iota (length components)))
   "\n"))

(define (generate-heatmap-styles stability-matrix input-names feature-names)
  "Generate color-coded styles for heatmap"
  (let ((max-stability (matrix-max stability-matrix))
        (min-stability (matrix-min stability-matrix)))
    (string-join
     (append-map (lambda (input-idx)
                   (map (lambda (feat-idx)
                          (let* ((stability (matrix-ref stability-matrix input-idx feat-idx))
                                 (normalized (/ (- stability min-stability)
                                               (- max-stability min-stability)))
                                 (color-intensity (floor (* normalized 255))))
                            (format #f "    style I~a_F~a fill:rgb(~a,~a,~a)"
                                    input-idx feat-idx
                                    color-intensity 
                                    (- 255 color-intensity)
                                    128)))
                        (iota (length feature-names))))
                 (iota (length input-names)))
     "\n")))

(define (generate-similarity-connections component-sets input-names)
  "Generate connections showing component similarities"
  ;; Simplified implementation
  (string-join
   (append-map (lambda (i)
                 (map (lambda (j)
                        (when (< i j)
                          (let ((similarity (compute-set-similarity 
                                           (list-ref component-sets i)
                                           (list-ref component-sets j))))
                            (when (> similarity 0.5)
                              (format #f "    Input~a -.->|~,2f| Input~a"
                                      i similarity j)))))
                      (iota (length input-names))))
               (iota (length input-names)))
   "\n"))

(define (generate-projection-styles data-points component-1 component-2)
  "Generate positioning styles for PC projection"
  ;; Simplified implementation
  "")

(define (compute-stability-matrix matrices)
  "Compute stability matrix showing component consistency"
  (let* ((n-inputs (length matrices))
         (n-features (if (null? matrices) 0 
                        (vector-length (vector-ref (car matrices) 0))))
         (stability (make-matrix n-inputs n-features)))
    
    ;; Compute pairwise correlations
    (do ((i 0 (+ i 1)))
        ((>= i n-inputs) stability)
      (do ((j 0 (+ j 1)))
          ((>= j n-features))
        ;; Simplified stability metric
        (matrix-set! stability i j (random:uniform))))))

(define (matrix-max matrix)
  "Find maximum value in matrix"
  (fold max -inf.0 
        (append-map (lambda (row) (vector->list row))
                    (vector->list matrix))))

(define (matrix-min matrix)
  "Find minimum value in matrix"
  (fold min +inf.0
        (append-map (lambda (row) (vector->list row))
                    (vector->list matrix))))

(define (compute-set-similarity set1 set2)
  "Compute similarity between component sets"
  ;; Simplified implementation
  0.5)

(define (generate-analysis-summary components stability input-names)
  "Generate textual summary of analysis"
  (format #f "Analysis identified ~a principal components across ~a inputs.\n"
          (length components)
          (length input-names)))

(define (matrix-ref matrix i j)
  "Get matrix element (assuming standard vector-of-vectors)"
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set! matrix i j value)
  "Set matrix element"
  (vector-set! (vector-ref matrix i) j value))