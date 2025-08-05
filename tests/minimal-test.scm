#!/usr/bin/env guile
!#

;; Minimal smoke test
(add-to-load-path ".")

;; Test basic math activation
(use-modules (attribution-graphs math activation))
(display "Testing activation functions...\n")
(if (= (relu 5) 5)
    (display "relu test passed\n")
    (display "relu test failed\n"))

;; Test graph structures
(use-modules (attribution-graphs graph structure))
(display "Testing graph structures...\n")

(let ((graph (make-attribution-graph)))
  (if (attribution-graph? graph)
      (display "graph creation test passed\n")
      (display "graph creation test failed\n")))

(let ((node (make-node 'test 'feature 0.5 (make-hash-table))))
  (if (node? node)
      (display "node creation test passed\n")
      (display "node creation test failed\n")))

(let ((edge (make-edge 'a 'b 0.8)))
  (if (edge? edge)
      (display "edge creation test passed\n")
      (display "edge creation test failed\n")))

(display "Basic tests completed!\n")