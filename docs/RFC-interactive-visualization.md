# RFC: Interactive Attribution Graph Visualization Interface

**Status:** Draft  
**Author:** Claude Code  
**Date:** 2025-06-01

## Abstract

This RFC proposes an interactive web-based visualization interface for exploring attribution graphs in transformer models. The interface will integrate with the existing Scheme-based attribution graph system while providing modern interactive graph exploration capabilities.

## Background

The current attribution graphs explorer provides static Mermaid diagram generation, which is useful for documentation but limited for interactive research. Researchers need to:

- Dynamically explore large attribution graphs
- Filter nodes and edges by attribution strength
- Highlight specific circuits and paths
- Compare multiple attribution methods
- Export findings for papers

## Technical Approach

### Hybrid Architecture

Given Scheme's limited web visualization ecosystem, we propose a hybrid approach:

1. **Scheme Backend**: Continue using existing attribution graph computation
2. **Web Frontend**: Modern JavaScript interface using D3.js/Cytoscape.js
3. **JSON Bridge**: Export graphs from Scheme to web-compatible format
4. **Local Server**: Simple HTTP server for serving the interface

### Data Flow

```
Scheme Code → JSON Export → Web Interface → Interactive Visualization
     ↑                                              ↓
Attribution Graph                            User Interactions
Computation                                  (filter, highlight, etc.)
```

## Architecture Components

### 1. JSON Export Module (`src/interface/export.scm`)

Convert attribution graphs to web-compatible JSON format:

```json
{
  "nodes": [
    {
      "id": "node_id",
      "type": "feature|token|error|logit",
      "activation": 0.85,
      "layer": 6,
      "position": {"x": 100, "y": 200},
      "metadata": {
        "interpretation": "plans rhyming",
        "feature_index": 42
      }
    }
  ],
  "edges": [
    {
      "source": "node1",
      "target": "node2", 
      "weight": 0.65,
      "layer_from": 5,
      "layer_to": 6
    }
  ],
  "circuits": [
    {
      "name": "Poetry Planning Circuit",
      "path": ["token_start", "feature_42", "feature_67", "logit_end"],
      "strength": 0.89
    }
  ]
}
```

### 2. Web Interface (`interface/web/`)

Modern single-page application with:

- **Graph Canvas**: Interactive node-link diagram with zoom/pan
- **Control Panel**: Filters, layout options, circuit selection
- **Inspector Panel**: Detailed node/edge information
- **Comparison View**: Side-by-side graph comparison
- **Export Tools**: Save visualizations, generate figures

### 3. Local HTTP Server (`src/interface/server.scm`)

Simple server to bridge Scheme and web interface:

- Serve static web files
- Provide API endpoints for graph data
- Handle real-time graph updates
- Support multiple graph formats

## User Interface Design

### Main Components

```
┌─────────────────────────────────────────────────────┐
│ Attribution Graph Explorer                          │
├─────────────────┬───────────────────────────────────┤
│ Control Panel   │ Graph Visualization Canvas        │
│                 │                                   │
│ □ Filter by     │    ○ Token                        │
│   Attribution   │     ↓                             │
│   [0.1] ────    │    ○ Feature 42 ──→ ○ Feature 67  │
│                 │     ↓                ↓            │
│ □ Layer Range   │    ○ Logit                        │
│   [5] to [8]    │                                   │
│                 │                                   │
│ ◉ Show Circuit  │                                   │
│   Poetry Plan   │                                   │
│                 │                                   │
├─────────────────┼───────────────────────────────────┤
│ Inspector       │ Export & Tools                    │
│                 │                                   │
│ Selected: F42   │ [Save SVG] [Generate Report]      │
│ Type: Feature   │ [Compare Graphs] [Circuit Trace]  │
│ Activation: 0.85│                                   │
│ Interp: "plans" │                                   │
└─────────────────┴───────────────────────────────────┘
```

### Interactive Features

1. **Node Interaction**:
   - Click: Select and show details
   - Hover: Show tooltip with basic info
   - Right-click: Context menu (trace paths, highlight connections)

2. **Edge Interaction**:
   - Hover: Show attribution weight
   - Click: Highlight path
   - Thickness proportional to weight

3. **Graph Navigation**:
   - Zoom and pan
   - Minimap for large graphs
   - Search nodes by ID or interpretation

4. **Circuit Exploration**:
   - Highlight discovered circuits
   - Trace paths between selected nodes
   - Show/hide different circuit types

## Implementation Plan

### Phase 1: Foundation
- JSON export module
- Basic web interface with D3.js
- Simple HTTP server
- Static graph display

### Phase 2: Interactivity  
- Node/edge interaction
- Basic filtering (attribution threshold)
- Circuit highlighting
- Layout algorithms

### Phase 3: Advanced Features
- Real-time graph updates
- Multiple graph comparison
- Export functionality
- Performance optimization

### Phase 4: Research Tools
- Circuit discovery UI
- Perturbation analysis interface  
- Batch graph processing
- Research workflow integration

## Technology Stack

### Frontend
- **D3.js**: Core visualization and interaction
- **Cytoscape.js**: Alternative for large graphs
- **Vanilla JS**: Keep dependencies minimal
- **CSS Grid/Flexbox**: Layout
- **Web Workers**: Performance for large graphs

### Backend Integration
- **Guile HTTP**: Simple server implementation
- **JSON**: Data exchange format
- **WebSockets**: Real-time updates (future)

### File Structure
```
interface/
├── web/
│   ├── index.html
│   ├── css/
│   │   └── style.css
│   ├── js/
│   │   ├── graph-viz.js
│   │   ├── controls.js
│   │   └── export.js
│   └── assets/
└── server.scm
src/interface/
├── export.scm
├── server.scm  
└── layouts.scm
```

## Benefits

1. **Usability**: Modern interactive interface familiar to researchers
2. **Performance**: Web technologies optimized for large graph visualization
3. **Integration**: Maintains existing Scheme computation pipeline
4. **Extensibility**: Easy to add new visualization features
5. **Portability**: Runs in any web browser

## Alternatives Considered

1. **Pure Scheme GUI**: Limited by available GUI libraries
2. **Desktop Application**: Would require GUI framework, less portable
3. **Jupyter Integration**: Could be future addition, not core interface
4. **Terminal-based TUI**: Too limited for complex graph interaction

## Implementation Details

The implementation will extend the existing visualization module while maintaining backward compatibility with Mermaid output for documentation.

## Future Extensions

- Jupyter notebook integration
- Collaborative graph exploration
- Cloud deployment options
- Integration with model training pipelines
- Automated circuit discovery UI

## Questions and Considerations

1. **Performance**: Large graphs (>1000 nodes) may need optimization
2. **Layout**: Automatic layout algorithms for readable attribution graphs
3. **User Experience**: Research workflow integration patterns
4. **Deployment**: Local vs. cloud deployment options

This RFC provides the foundation for implementing a modern, interactive interface while leveraging the existing robust Scheme-based attribution graph computation system.