(define-module (attribution-graphs examples poetry-circuit)
  #:use-module (attribution-graphs clt transcoder)
  #:use-module (attribution-graphs graph attribution)
  #:use-module (attribution-graphs circuits discovery)
  #:use-module (attribution-graphs circuits visualization)
  #:export (analyze-poetry-planning))

;; Analyze poetry generation planning
(define (analyze-poetry-planning model prompt)
  "Analyze how model plans rhyming in poetry"
  (let* ((clt (model-clt model))
         (lines (string-split prompt #\newline))
         (target-position (find-rhyme-position lines))
         (graph (compute-attribution-graph clt prompt target-position)))
    
    ;; Find planning features
    (let ((planning-features (find-planning-features graph))
          (rhyme-features (find-rhyme-features graph)))
      
      ;; Trace circuits
      (let ((circuits (map (lambda (pf)
                            (map (lambda (rf)
                                  (trace-path graph 
                                            (node-id pf) 
                                            (node-id rf)))
                                rhyme-features))
                          planning-features)))
        
        ;; Visualize
        (display "Poetry Planning Circuit:\n")
        (display (circuit->mermaid circuits graph))
        
        ;; Return analysis
        `((planning-features . ,planning-features)
          (rhyme-features . ,rhyme-features)
          (circuits . ,circuits)
          (graph . ,graph))))))

;; Find features that plan ahead
(define (find-planning-features graph)
  "Identify features active at line breaks that plan ahead"
  (filter (lambda (node)
            (and (eq? (node-type node) 'feature)
                 (string-contains (hash-ref (node-metadata node) 
                                          'interpretation "")
                                "plan")))
          (graph-nodes graph)))

;; Find rhyme-related features  
(define (find-rhyme-features graph)
  "Identify features related to rhyming"
  (filter (lambda (node)
            (and (eq? (node-type node) 'feature)
                 (string-contains (hash-ref (node-metadata node)
                                          'interpretation "")
                                "rhyme")))
          (graph-nodes graph)))

;; Helper to find rhyme position
(define (find-rhyme-position lines)
  "Find position where rhyme is generated"
  ;; Simplified: return end of last line
  (string-length (string-join lines "\n")))
