#!/usr/bin/env guile
!#

;; Load and run all examples
(add-to-load-path (dirname (current-filename)))
(add-to-load-path "..")

(use-modules (attribution-graphs examples poetry-circuit)
             (attribution-graphs examples reasoning-circuit))

(display "Attribution Graphs in Guile Scheme\n")
(display "==================================\n\n")

;; Run examples with mock data
(display "Running examples with mock data...\n")

;; Note: These require actual model implementations
;; This is for demonstration purposes
