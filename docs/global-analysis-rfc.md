# RFC: Global Weights Analysis Module

## Summary

This RFC proposes enhancements to the attribution graphs explorer to support global weight analysis across multiple prompts and inputs, enabling the identification of common circuit patterns that persist across different contexts.

## Motivation

The current implementation in `src/math/matrix.scm` provides basic matrix operations but lacks:

1. **SVD Analysis**: No singular value decomposition for identifying principal components
2. **Global Analysis**: No capability to analyze weights across multiple inputs
3. **Component Interpretation**: No tools to interpret identified components
4. **Cross-Input Comparison**: No mechanisms to find common patterns across prompts
5. **Advanced Visualization**: Limited visualization beyond basic Mermaid diagrams

## Proposed Architecture

### Core Components

1. **Enhanced Matrix Module** (`src/math/matrix.scm`)
   - Add SVD implementation using iterative algorithms
   - Matrix decomposition utilities
   - Component ranking and selection

2. **Global Analysis Module** (`src/math/global-analysis.scm`) 
   - Cross-prompt weight aggregation
   - Principal component analysis
   - Component stability metrics
   - Pattern clustering across inputs

3. **Interpretation Module** (`src/math/interpretation.scm`)
   - Component semantic analysis
   - Feature importance ranking
   - Circuit pattern identification
   - Statistical significance testing

4. **Enhanced Visualization** (`src/circuits/global-visualization.scm`)
   - Component importance plots
   - Cross-input comparison charts
   - Interactive circuit diagrams
   - Principal component projections

### Data Flow

```
Multiple Inputs → Weight Matrices → SVD Analysis → Global Components → Interpretation → Visualization
```

## Detailed Design

### SVD Implementation

```scheme
;; Power iteration method for large matrices
(define (matrix-svd matrix rank)
  "Compute SVD using power iteration"
  ;; Returns (U S V) where A = U * S * V^T
  )

;; Component analysis
(define (analyze-components U S V threshold)
  "Analyze SVD components for significance"
  ;; Filter by singular value threshold
  ;; Return ranked components
  )
```

### Global Analysis

```scheme
;; Aggregate weights across multiple inputs
(define (aggregate-weights weight-matrices method)
  "Combine weights from multiple analyses"
  ;; Methods: mean, median, robust-mean
  )

;; Find stable components
(define (find-stable-components matrices stability-threshold)
  "Identify components that appear consistently"
  )
```

### Visualization Enhancements

- **Component Importance**: Bar charts showing singular values
- **Loading Plots**: Feature contributions to each component  
- **Stability Maps**: How components vary across inputs
- **Circuit Overlays**: Highlight common patterns in attribution graphs

## Implementation Plan

### Phase 1: Core SVD Implementation
- Extend `src/math/matrix.scm` with SVD algorithms
- Add matrix decomposition utilities
- Implement component ranking

### Phase 2: Global Analysis Framework
- Create `src/math/global-analysis.scm`
- Implement cross-input aggregation
- Add stability metrics

### Phase 3: Interpretation Tools
- Create `src/math/interpretation.scm` 
- Component semantic analysis
- Statistical significance tests

### Phase 4: Enhanced Visualization
- Extend visualization module
- Interactive component plots
- Cross-input comparison views

## Benefits

1. **Circuit Discovery**: Identify patterns that persist across different inputs
2. **Robustness**: Distinguish stable circuit components from noise
3. **Interpretability**: Better understanding of what components represent
4. **Scalability**: Handle analysis across large sets of prompts/inputs
5. **Validation**: Compare predicted vs. measured causal effects globally

## Testing Strategy

- Unit tests for SVD implementation accuracy
- Integration tests with existing attribution graph pipeline
- Performance benchmarks for large matrix operations
- Validation against known circuit patterns

## Migration Plan

This is an additive enhancement that extends existing functionality without breaking changes to the current API.

## Open Questions

1. Should we implement full SVD or focus on top-k components?
2. What clustering algorithms work best for circuit patterns?
3. How to balance computation cost vs. analysis depth?
4. Integration strategy with existing perturbation validation?