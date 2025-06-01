# Interactive Visualization Guide

## Overview

The Attribution Graphs Explorer includes a powerful interactive visualization system that allows researchers to explore attribution graphs, discover circuits, and analyze computational pathways in transformer models through a web-based interface.

## Quick Start

### Basic Usage

```scheme
;; Load the interactive visualization module
(use-modules (attribution-graphs visualization interactive))

;; Create an attribution graph (or use existing)
(define graph (compute-attribution-graph clt "The cat sat on the mat" 'last-token))
(define circuits (find-circuits graph))

;; Launch interactive explorer
(start-interactive-explorer graph circuits)
;; Opens browser to http://localhost:8080
```

### Running Examples

```bash
# Basic exploration demo
guile3 examples/interactive-visualization.scm basic

# Circuit-focused exploration
guile3 examples/interactive-visualization.scm circuits

# Export data for external analysis
guile3 examples/interactive-visualization.scm export
```

## Interface Overview

### Main Components

1. **Control Panel** (Left): Filtering and circuit selection tools
2. **Graph Canvas** (Center): Interactive graph visualization
3. **Information Panel** (Right): Node details and statistics
4. **Status Bar** (Bottom): Current status and zoom level

### Navigation

- **Pan**: Click and drag the background
- **Zoom**: Mouse wheel or +/- buttons
- **Select**: Click nodes (Ctrl+click for multi-select)
- **Focus**: Double-click node to extract subgraph
- **Fit View**: Press 'F' or click home button

## Features

### Graph Exploration

#### Node Selection and Details
- Click any node to view its properties
- See incoming and outgoing connections
- View activation values and metadata
- Multi-select with Ctrl+click

#### Interactive Filtering
- **Attribution Threshold**: Filter edges by weight strength
- **Node Types**: Toggle visibility of features, tokens, logits
- **Layer Filtering**: Focus on specific transformer layers

#### Circuit Visualization
- Highlight discovered computational circuits
- Compare multiple circuits side-by-side
- Trace paths from selected nodes to outputs
- Adjust circuit opacity for clarity

### Analysis Tools

#### Graph Statistics
- Real-time node and edge counts
- Attribution weight distributions
- Network connectivity metrics
- Circuit complexity measures

#### Path Discovery
- Find paths between any two nodes
- Trace computational flows
- Identify bottlenecks and critical paths
- Analyze circuit composition

#### Export Capabilities
- Save graphs as PNG, SVG images
- Export data as JSON for external analysis
- Generate Mermaid diagrams
- Download filtered views

## API Reference

### Core Functions

#### `start-interactive-explorer`
Launch the interactive visualization server.

```scheme
(start-interactive-explorer graph circuits
                           #:port 8080
                           #:host "127.0.0.1" 
                           #:auto-open #t)
```

**Parameters:**
- `graph`: Attribution graph object
- `circuits`: List of discovered circuits
- `port`: Server port (default: 8080)
- `host`: Server host (default: "127.0.0.1")
- `auto-open`: Auto-open browser (default: #t)

#### `export-for-web`
Export graph data for web visualization.

```scheme
(export-for-web graph circuits "output-directory")
```

**Parameters:**
- `graph`: Attribution graph object
- `circuits`: List of circuits
- `output-dir`: Directory for exported files

#### `create-interactive-session`
Create a complete session with demo data.

```scheme
(create-interactive-session "The capital of France is Paris" "Paris")
```

### Data Export Functions

#### JSON Export
```scheme
(use-modules (attribution-graphs visualization export))

;; Export graph as JSON
(define json-data (graph->json graph))

;; Export in Cytoscape.js format
(define cytoscape-data (graph->cytoscape graph))
```

#### Multiple Formats
```scheme
;; Export complete visualization data
(export-visualization-data graph circuits "output-dir")
```

Generated files:
- `graph.json`: Main graph data
- `graph-cytoscape.json`: Cytoscape.js format
- `circuit-N.json`: Individual circuit files

## Configuration

### Server Configuration
```scheme
;; Custom server settings
(start-interactive-explorer graph circuits
                           #:port 9000
                           #:host "0.0.0.0"  ; Allow external access
                           #:auto-open #f)   ; Don't auto-open browser
```

### Visualization Settings
The interface supports customization through URL parameters:

- `?theme=dark`: Dark color scheme
- `?layout=hierarchical`: Hierarchical layout
- `?zoom=0.5`: Initial zoom level

## Integration Examples

### With Existing Attribution System

```scheme
(use-modules (attribution-graphs graph attribution)
             (attribution-graphs circuits discovery)
             (attribution-graphs visualization interactive))

;; Compute attribution graph
(define clt (make-clt 5 '(6 7 8) 768 128 768))
(define graph (compute-attribution-graph clt prompt 'last-token))

;; Discover circuits
(define circuits (find-circuits graph))

;; Launch explorer
(start-interactive-explorer graph circuits)
```

### Batch Analysis

```scheme
;; Analyze multiple prompts
(define prompts '("The capital of France is"
                  "Shakespeare wrote"
                  "E=mc² was discovered by"))

(for-each (lambda (prompt)
            (let* ((graph (compute-attribution-graph clt prompt 'last-token))
                   (circuits (find-circuits graph))
                   (export-dir (format #f "analysis-~a" (string-hash prompt))))
              (export-for-web graph circuits export-dir)))
          prompts)
```

### Custom Circuit Discovery

```scheme
;; Define custom circuit discovery
(define (find-attention-circuits graph)
  "Find circuits that primarily use attention mechanisms"
  ;; Custom implementation
  ...)

;; Use with visualization
(define attention-circuits (find-attention-circuits graph))
(start-interactive-explorer graph attention-circuits)
```

## Advanced Features

### Performance Optimization

For large graphs (>1000 nodes):

1. **Use Filtering**: Apply threshold filters to reduce visible elements
2. **Focus Mode**: Extract subgraphs around nodes of interest
3. **Layer Filtering**: Show only relevant transformer layers
4. **Circuit Selection**: Focus on specific computational paths

### Collaborative Analysis

```scheme
;; Start server accessible from network
(start-interactive-explorer graph circuits
                           #:host "0.0.0.0"
                           #:port 8080)
;; Now accessible at http://your-ip:8080
```

### Programmatic Control

```scheme
;; Export specific views
(define filtered-graph (filter-graph graph #:threshold 0.5))
(export-for-web filtered-graph circuits "high-attribution-view")

;; Generate reports
(define stats (calculate-graph-statistics graph))
(generate-analysis-report stats circuits "report.html")
```

## Troubleshooting

### Common Issues

**Server won't start:**
- Check if port is already in use
- Verify Guile web modules are installed
- Ensure static files directory exists

**Browser doesn't open:**
- Check firewall settings
- Try manual navigation to http://localhost:8080
- Verify auto-open parameter

**Large graphs slow:**
- Apply attribution threshold filtering
- Use subgraph extraction
- Increase system memory allocation

**Export fails:**
- Check output directory permissions
- Verify disk space availability
- Ensure graph data is valid

### Performance Tips

1. **Filter Early**: Apply threshold filters before visualization
2. **Use Subgraphs**: Focus on relevant portions of large graphs
3. **Batch Processing**: Export data for offline analysis
4. **Progressive Loading**: Start with overview, drill down as needed

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| F | Fit graph to view |
| R | Reset view to default |
| Esc | Clear selection |
| Ctrl+E | Open export dialog |
| Space+Drag | Pan view |
| +/- | Zoom in/out |
| Tab | Next UI panel |

## Integration with Research Workflows

### Paper Generation
```scheme
;; Export high-quality figures
(start-interactive-explorer graph circuits)
;; Use interface to:
;; 1. Filter to interesting circuits
;; 2. Export as SVG for papers
;; 3. Save configuration for reproducibility
```

### Hypothesis Testing
```scheme
;; Compare different attribution methods
(define method1-graph (compute-attribution-graph clt prompt 'integrated-gradients))
(define method2-graph (compute-attribution-graph clt prompt 'attention-rollout))

;; Analyze differences interactively
(start-interactive-explorer method1-graph circuits #:port 8080)
(start-interactive-explorer method2-graph circuits #:port 8081)
```

### Educational Use
```scheme
;; Create teaching materials
(define simple-graph (create-demo-attribution-graph "simple example" "example"))
(export-for-web simple-graph '() "teaching-materials")
;; Deploy to web server for student access
```

This interactive visualization system transforms the Attribution Graphs Explorer from a computational tool into a comprehensive research platform, enabling intuitive exploration and understanding of complex transformer model circuits.