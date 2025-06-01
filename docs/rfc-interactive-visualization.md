# RFC: Interactive Visualization Module for Attribution Graphs

## Summary

This RFC proposes an interactive visualization module that extends the existing Attribution Graphs Explorer framework with rich, web-based interactive interfaces for exploring attribution graphs and circuits.

## Background

The current system provides basic static Mermaid diagram generation via `circuits/visualization.scm`. While useful for documentation, this approach lacks the interactivity needed for researchers to effectively explore complex attribution graphs with hundreds or thousands of nodes and edges.

## Goals

1. **Interactive Graph Navigation**: Pan, zoom, and explore large attribution graphs
2. **Dynamic Filtering**: Filter nodes/edges by attribution strength, layer, or feature type  
3. **Circuit Highlighting**: Visually trace and highlight specific computational circuits
4. **Comparison Views**: Side-by-side comparison of multiple attribution methods
5. **Research Workflow Integration**: Export findings, save exploration states, generate reports

## Technical Approach

### Architecture Overview

```
Scheme Core ──→ JSON Export ──→ Web Interface ──→ Interactive Visualization
    │               │               │                      │
    │               │               │                      ▼
    │               │               │             ┌─────────────────┐
    │               │               │             │   D3.js/Cytoscape │
    │               │               │             │   Graph Renderer  │
    │               │               │             └─────────────────┘
    │               │               │                      │
    │               │               │                      ▼
    │               │               │             ┌─────────────────┐
    │               │               │             │  Interactive UI  │
    │               │               │             │   Components     │
    │               │               │             └─────────────────┘
    │               │               │                      │
    │               │               └──────────────────────┘
    │               │
    │               └── HTTP Server (Scheme-based)
    │
    └── Attribution Graph Computation
```

### Component Design

#### 1. Data Export Layer (`src/visualization/export.scm`)

Extends existing graph structures to export interactive-ready formats:

```scheme
(define (graph->json graph)
  "Export attribution graph as JSON for web visualization")

(define (graph->cytoscape graph)  
  "Export in Cytoscape.js format for network visualization")
```

#### 2. Web Server Layer (`src/visualization/server.scm`)

Lightweight HTTP server to serve the interactive interface:

```scheme
(define (start-visualization-server graph port)
  "Start web server for interactive graph exploration")
```

#### 3. Interactive Interface (`src/visualization/static/`)

Web-based interface using modern JavaScript libraries:

- **D3.js** for custom graph layouts and interactions
- **Cytoscape.js** for network visualization  
- **React/Vue** for UI components and state management
- **WebGL** acceleration for large graphs

### Key Features

#### Graph Navigation
- **Pan & Zoom**: Smooth navigation of large graphs
- **Level-of-Detail**: Show/hide nodes based on zoom level
- **Minimap**: Overview navigation for large graphs
- **Search**: Find specific nodes, features, or circuits

#### Interactive Filtering  
- **Attribution Threshold**: Slider to filter by edge weights
- **Node Types**: Toggle visibility of features, tokens, logits
- **Layer Filtering**: Focus on specific transformer layers
- **Circuit Masks**: Show only nodes in discovered circuits

#### Circuit Exploration
- **Path Highlighting**: Trace specific computational paths
- **Circuit Comparison**: Overlay multiple circuits
- **Feature Interpretation**: Hover/click for feature details
- **Subgraph Extraction**: Focus on circuit neighborhoods

#### Analysis Tools
- **Attribution Heatmaps**: Color-coded edge weights
- **Circuit Statistics**: Metrics panel for selected circuits  
- **Export Options**: Save views, generate reports
- **State Persistence**: Bookmark exploration states

## Implementation Plan

### Phase 1: Core Infrastructure
1. **JSON Export**: Extend `graph/structure.scm` with JSON serialization
2. **Basic Server**: HTTP server in Scheme to serve static files
3. **Simple Viewer**: Basic D3.js graph rendering

### Phase 2: Interactive Features  
1. **Graph Navigation**: Pan, zoom, search functionality
2. **Basic Filtering**: Attribution threshold filtering
3. **Node Selection**: Click to inspect node details

### Phase 3: Advanced Visualization
1. **Circuit Highlighting**: Visual path tracing
2. **Comparison Views**: Side-by-side graph comparison
3. **Advanced Filtering**: Multi-dimensional filtering

### Phase 4: Research Integration
1. **Export Workflows**: Save findings, generate reports
2. **State Persistence**: Bookmark and share explorations
3. **Performance Optimization**: Handle large graphs efficiently

## File Structure

```
src/visualization/
├── export.scm          # Data export to JSON/other formats
├── server.scm          # HTTP server for web interface  
├── layouts.scm         # Graph layout algorithms
├── static/
│   ├── index.html      # Main interface
│   ├── js/
│   │   ├── graph.js    # Graph rendering logic
│   │   ├── ui.js       # UI components
│   │   └── utils.js    # Utilities
│   ├── css/
│   │   └── style.css   # Interface styling
│   └── lib/            # Third-party libraries
└── templates/          # HTML templates for different views
```

## Integration Points

### With Existing System
- **Graph Structures**: Extend `graph/structure.scm` records
- **Attribution Computing**: Use existing `graph/attribution.scm`
- **Circuit Discovery**: Integrate with `circuits/discovery.scm`
- **Visualization**: Replace/extend `circuits/visualization.scm`

### Configuration
- **Visualization Settings**: Theme, layout preferences
- **Server Configuration**: Port, static file paths  
- **Export Options**: Supported formats, quality settings

## Example Usage

```scheme
;; Basic visualization
(use-modules (attribution-graphs visualization interactive))

(define graph (compute-attribution-graph clt "Example input" 'last-token))
(define circuits (find-circuits graph))

;; Start interactive server
(start-visualization-server graph 8080)
;; Opens browser to localhost:8080

;; Export for external tools
(graph->json graph "output.json")
(circuits->json circuits graph "circuits.json")
```

## Alternative Approaches Considered

1. **Pure Scheme GUI**: Using Guile-GNOME or similar
   - *Pros*: Native integration, no web dependency
   - *Cons*: Limited modern UI capabilities, installation complexity

2. **Jupyter Integration**: Python wrapper with Jupyter widgets
   - *Pros*: Familiar research environment
   - *Cons*: Language bridge complexity, performance overhead

3. **Desktop Application**: Electron-based standalone app
   - *Pros*: Rich desktop integration
   - *Cons*: Heavy dependency, distribution complexity

**Selected Approach**: Web-based with embedded HTTP server provides the best balance of functionality, accessibility, and integration with the existing Scheme codebase.

## Security Considerations

- **Local-only Server**: Default to localhost binding
- **File Access**: Restrict server to designated static directories
- **Input Validation**: Sanitize all user inputs for filtering/search
- **CORS Policy**: Strict same-origin policy for API endpoints

## Performance Considerations

- **Large Graphs**: Implement level-of-detail rendering
- **Memory Usage**: Stream large graph data, avoid loading entire graphs
- **Network Transfer**: Compress JSON exports, incremental loading
- **Rendering**: WebGL acceleration for complex visualizations

## Future Extensions

- **Real-time Updates**: Live graph updates during computation
- **Collaborative Features**: Shared exploration sessions
- **Plugin System**: Custom visualization components
- **ML Integration**: Automatic circuit discovery suggestions
- **3D Visualization**: Immersive graph exploration

## Success Metrics

- **Usability**: Researchers can explore graphs >1000 nodes smoothly
- **Discovery**: Interface enables identification of new circuit patterns
- **Performance**: <1s load time for typical attribution graphs
- **Adoption**: Integration into standard research workflows

## Conclusion

This interactive visualization module will transform Attribution Graphs Explorer from a computational tool into a comprehensive research platform, enabling researchers to intuitively explore and understand the complex circuits within transformer models.