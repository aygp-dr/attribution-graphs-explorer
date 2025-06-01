// Attribution Graphs Explorer - UI Controller

class AttributionGraphUI {
  constructor() {
    this.visualization = null;
    this.currentFilters = {
      threshold: 0.1,
      nodeTypes: ['feature', 'token', 'logit', 'error'],
      layerRange: { min: 0, max: 100 },
      selectedCircuit: 'all'
    };
    
    this.init();
  }

  init() {
    this.initializeVisualization();
    this.setupEventListeners();
    this.loadInitialData();
    this.updateUI();
  }

  initializeVisualization() {
    // Initialize the graph visualization
    this.visualization = new AttributionGraphVisualization('graph-canvas', {
      width: document.getElementById('graph-canvas').clientWidth,
      height: document.getElementById('graph-canvas').clientHeight
    });

    // Set up visualization callbacks
    this.visualization.onNodeClick = (node, selectedNodes) => {
      this.updateNodeDetails(node);
      this.updateSelectionInfo(selectedNodes);
    };

    this.visualization.onSelectionChange = (selectedNodes) => {
      this.updateSelectionInfo(selectedNodes);
    };

    this.visualization.onNodeHover = (node) => {
      // Optional: Update info panel on hover
    };
  }

  setupEventListeners() {
    // Threshold slider
    const thresholdSlider = document.getElementById('threshold-slider');
    const thresholdValue = document.getElementById('threshold-value');
    
    thresholdSlider.addEventListener('input', AttributionGraphUtils.debounce((e) => {
      this.currentFilters.threshold = parseFloat(e.target.value);
      thresholdValue.textContent = this.currentFilters.threshold.toFixed(2);
      this.applyFilters();
    }, 300));

    // Node type checkboxes
    const nodeTypeCheckboxes = ['show-features', 'show-tokens', 'show-logits', 'show-errors'];
    const nodeTypeMap = ['feature', 'token', 'logit', 'error'];
    
    nodeTypeCheckboxes.forEach((id, index) => {
      const checkbox = document.getElementById(id);
      if (checkbox) {
        checkbox.addEventListener('change', () => {
          this.updateNodeTypeFilters();
          this.applyFilters();
        });
      }
    });

    // Filter buttons
    document.getElementById('apply-filters')?.addEventListener('click', () => {
      this.applyFilters();
    });

    document.getElementById('reset-filters')?.addEventListener('click', () => {
      this.resetFilters();
    });

    // Graph controls
    document.getElementById('zoom-in')?.addEventListener('click', () => {
      this.visualization.zoomIn();
    });

    document.getElementById('zoom-out')?.addEventListener('click', () => {
      this.visualization.zoomOut();
    });

    document.getElementById('fit-view')?.addEventListener('click', () => {
      this.visualization.fitToView();
    });

    document.getElementById('reset-view')?.addEventListener('click', () => {
      this.visualization.resetView();
    });

    // Circuit analysis buttons
    document.getElementById('discover-circuits')?.addEventListener('click', () => {
      this.showDiscoverCircuitsDialog();
    });

    document.getElementById('compare-circuits')?.addEventListener('click', () => {
      this.showCompareCircuitsDialog();
    });

    // Navigation menu
    document.getElementById('btn-file')?.addEventListener('click', () => {
      this.showFileMenu();
    });

    document.getElementById('btn-view')?.addEventListener('click', () => {
      this.showViewMenu();
    });

    document.getElementById('btn-analysis')?.addEventListener('click', () => {
      this.showAnalysisMenu();
    });

    document.getElementById('btn-export')?.addEventListener('click', () => {
      this.showExportDialog();
    });

    document.getElementById('btn-help')?.addEventListener('click', () => {
      this.showHelpDialog();
    });

    // Window resize
    window.addEventListener('resize', AttributionGraphUtils.debounce(() => {
      this.handleResize();
    }, 250));

    // Keyboard shortcuts
    document.addEventListener('keydown', (e) => {
      this.handleKeyboard(e);
    });

    // Clear selection on background click
    document.getElementById('graph-canvas').addEventListener('click', (e) => {
      if (e.target.id === 'graph-canvas') {
        this.visualization.selectNodes([]);
        this.clearNodeDetails();
      }
    });
  }

  loadInitialData() {
    // Load embedded graph data
    if (window.ATTRIBUTION_GRAPH_DATA) {
      const success = this.visualization.loadData(window.ATTRIBUTION_GRAPH_DATA);
      if (success) {
        this.setupCircuitList();
        setTimeout(() => {
          this.visualization.fitToView();
        }, 1000); // Allow simulation to stabilize
      }
    } else {
      this.showLoadingMessage();
    }
  }

  setupCircuitList() {
    const circuitContainer = document.querySelector('.circuit-list');
    if (!circuitContainer || !window.CIRCUIT_DATA) return;

    // Clear existing circuits (except "Show All")
    const existingCircuits = circuitContainer.querySelectorAll('label:not(:first-child)');
    existingCircuits.forEach(el => el.remove());

    // Add circuit options
    window.CIRCUIT_DATA.forEach((circuit, index) => {
      const label = document.createElement('label');
      label.innerHTML = `
        <input type="radio" name="circuit" value="circuit-${index}">
        Circuit ${index + 1} (${this.getCircuitDescription(circuit)})
      `;
      
      const radio = label.querySelector('input');
      radio.addEventListener('change', () => {
        if (radio.checked) {
          this.selectCircuit(circuit);
          this.currentFilters.selectedCircuit = `circuit-${index}`;
        }
      });
      
      circuitContainer.appendChild(label);
    });

    // Setup "Show All" radio
    const showAllRadio = circuitContainer.querySelector('input[value="all"]');
    if (showAllRadio) {
      showAllRadio.addEventListener('change', () => {
        if (showAllRadio.checked) {
          this.visualization.highlightCircuit(null);
          this.currentFilters.selectedCircuit = 'all';
        }
      });
    }
  }

  getCircuitDescription(circuit) {
    if (circuit.paths && circuit.paths.length > 0) {
      const pathCount = circuit.paths.length;
      const avgLength = circuit.paths.reduce((sum, path) => sum + path.length, 0) / pathCount;
      return `${pathCount} paths, avg length: ${avgLength.toFixed(1)}`;
    }
    return 'Unknown circuit structure';
  }

  selectCircuit(circuit) {
    if (circuit.paths) {
      this.visualization.highlightCircuit(circuit.paths);
    }
  }

  updateNodeTypeFilters() {
    const nodeTypes = [];
    
    if (document.getElementById('show-features')?.checked) nodeTypes.push('feature');
    if (document.getElementById('show-tokens')?.checked) nodeTypes.push('token');
    if (document.getElementById('show-logits')?.checked) nodeTypes.push('logit');
    if (document.getElementById('show-errors')?.checked) nodeTypes.push('error');
    
    this.currentFilters.nodeTypes = nodeTypes;
  }

  applyFilters() {
    this.visualization.applyFilters(this.currentFilters);
  }

  resetFilters() {
    // Reset filter values
    this.currentFilters = {
      threshold: 0.1,
      nodeTypes: ['feature', 'token', 'logit', 'error'],
      layerRange: { min: 0, max: 100 },
      selectedCircuit: 'all'
    };

    // Update UI controls
    document.getElementById('threshold-slider').value = 0.1;
    document.getElementById('threshold-value').textContent = '0.1';
    
    document.getElementById('show-features').checked = true;
    document.getElementById('show-tokens').checked = true;
    document.getElementById('show-logits').checked = true;
    document.getElementById('show-errors').checked = false;

    // Reset circuit selection
    document.querySelector('input[value="all"]').checked = true;
    this.visualization.highlightCircuit(null);

    // Apply filters
    this.applyFilters();
  }

  updateNodeDetails(node) {
    const nodeInfo = document.getElementById('node-info');
    if (!nodeInfo) return;

    nodeInfo.innerHTML = `
      <div class="node-property">
        <span class="property-label">ID:</span>
        <span class="property-value">${node.id}</span>
      </div>
      <div class="node-property">
        <span class="property-label">Type:</span>
        <span class="property-value">${node.type}</span>
      </div>
      <div class="node-property">
        <span class="property-label">Activation:</span>
        <span class="property-value">${AttributionGraphUtils.formatNumber(node.activation)}</span>
      </div>
      <div class="node-property">
        <span class="property-label">Label:</span>
        <span class="property-value">${node.label || 'N/A'}</span>
      </div>
      ${this.generateNodeConnectionsHTML(node)}
      <div style="margin-top: 1rem;">
        <button onclick="attributionUI.traceFromNode('${node.id}')">Trace Paths</button>
        <button onclick="attributionUI.focusOnNode('${node.id}')">Focus View</button>
      </div>
    `;
  }

  generateNodeConnectionsHTML(node) {
    const graphData = this.visualization.filteredData;
    if (!graphData) return '';

    const incomingEdges = graphData.edges.filter(e => 
      (e.target.id || e.target) === node.id
    );
    const outgoingEdges = graphData.edges.filter(e => 
      (e.source.id || e.source) === node.id
    );

    let html = '';
    
    if (incomingEdges.length > 0) {
      html += `
        <div class="node-property">
          <span class="property-label">Incoming (${incomingEdges.length}):</span>
        </div>
        <div style="max-height: 100px; overflow-y: auto; margin-left: 1rem;">
      `;
      incomingEdges.slice(0, 5).forEach(edge => {
        const sourceId = edge.source.id || edge.source;
        html += `
          <div style="font-size: 0.8rem; margin-bottom: 0.25rem;">
            ${sourceId} (${AttributionGraphUtils.formatNumber(edge.weight)})
          </div>
        `;
      });
      if (incomingEdges.length > 5) {
        html += `<div style="font-size: 0.8rem; color: #666;">... and ${incomingEdges.length - 5} more</div>`;
      }
      html += '</div>';
    }

    if (outgoingEdges.length > 0) {
      html += `
        <div class="node-property">
          <span class="property-label">Outgoing (${outgoingEdges.length}):</span>
        </div>
        <div style="max-height: 100px; overflow-y: auto; margin-left: 1rem;">
      `;
      outgoingEdges.slice(0, 5).forEach(edge => {
        const targetId = edge.target.id || edge.target;
        html += `
          <div style="font-size: 0.8rem; margin-bottom: 0.25rem;">
            ${targetId} (${AttributionGraphUtils.formatNumber(edge.weight)})
          </div>
        `;
      });
      if (outgoingEdges.length > 5) {
        html += `<div style="font-size: 0.8rem; color: #666;">... and ${outgoingEdges.length - 5} more</div>`;
      }
      html += '</div>';
    }

    return html;
  }

  clearNodeDetails() {
    const nodeInfo = document.getElementById('node-info');
    if (nodeInfo) {
      nodeInfo.innerHTML = '<p>Select a node to view details</p>';
    }
  }

  updateSelectionInfo(selectedNodes) {
    const selectionInfo = document.getElementById('selection-info');
    if (!selectionInfo) return;

    if (selectedNodes.length === 0) {
      selectionInfo.textContent = '';
    } else if (selectedNodes.length === 1) {
      selectionInfo.textContent = `Selected: ${selectedNodes[0]}`;
    } else {
      selectionInfo.textContent = `Selected: ${selectedNodes.length} nodes`;
    }
  }

  updateUI() {
    // Update any UI elements that need periodic refresh
    this.updateStatistics();
  }

  updateStatistics() {
    // This is handled by the visualization class
    this.visualization.updateStatistics();
  }

  // Action methods
  traceFromNode(nodeId) {
    // Find paths from this node
    const graphData = this.visualization.graphData;
    const outputNodes = graphData.nodes.filter(n => n.type === 'logit');
    
    if (outputNodes.length > 0) {
      const paths = AttributionGraphUtils.findPaths(graphData, nodeId, outputNodes[0].id);
      if (paths.length > 0) {
        this.visualization.highlightCircuit(paths.slice(0, 3)); // Show top 3 paths
        this.visualization.updateStatus(`Found ${paths.length} paths from ${nodeId}`);
      } else {
        this.visualization.updateStatus(`No paths found from ${nodeId}`);
      }
    }
  }

  focusOnNode(nodeId) {
    const subgraph = AttributionGraphUtils.extractSubgraph(
      this.visualization.graphData, 
      nodeId, 
      2
    );
    this.visualization.filteredData = subgraph;
    this.visualization.updateVisualization();
    setTimeout(() => {
      this.visualization.fitToView();
    }, 500);
  }

  // Dialog methods
  showLoadingMessage() {
    const graphCanvas = document.getElementById('graph-canvas');
    graphCanvas.innerHTML = `
      <div class="loading">
        <p>Loading attribution graph data...</p>
      </div>
    `;
  }

  showDiscoverCircuitsDialog() {
    this.showModal('Discover Circuits', `
      <p>Circuit discovery functionality would analyze the current graph to find interesting computational paths.</p>
      <p>This feature is not yet implemented in this prototype.</p>
      <div style="margin-top: 1rem;">
        <button onclick="attributionUI.closeModal()">Close</button>
      </div>
    `);
  }

  showCompareCircuitsDialog() {
    this.showModal('Compare Circuits', `
      <p>Circuit comparison allows side-by-side analysis of different computational paths.</p>
      <p>This feature is not yet implemented in this prototype.</p>
      <div style="margin-top: 1rem;">
        <button onclick="attributionUI.closeModal()">Close</button>
      </div>
    `);
  }

  showFileMenu() {
    this.showModal('File Operations', `
      <div style="display: flex; flex-direction: column; gap: 1rem;">
        <button onclick="attributionUI.loadGraphFile()">Load Graph File</button>
        <button onclick="attributionUI.saveCurrentView()">Save Current View</button>
        <button onclick="attributionUI.exportGraphData()">Export Graph Data</button>
        <button onclick="attributionUI.closeModal()">Close</button>
      </div>
    `);
  }

  showViewMenu() {
    this.showModal('View Options', `
      <div style="display: flex; flex-direction: column; gap: 1rem;">
        <button onclick="attributionUI.visualization.fitToView(); attributionUI.closeModal();">Fit to View</button>
        <button onclick="attributionUI.visualization.resetView(); attributionUI.closeModal();">Reset View</button>
        <button onclick="attributionUI.resetFilters(); attributionUI.closeModal();">Reset Filters</button>
        <button onclick="attributionUI.toggleMinimap()">Toggle Minimap</button>
        <button onclick="attributionUI.closeModal()">Close</button>
      </div>
    `);
  }

  showAnalysisMenu() {
    this.showModal('Analysis Tools', `
      <div style="display: flex; flex-direction: column; gap: 1rem;">
        <button onclick="attributionUI.showStatisticsDialog()">Graph Statistics</button>
        <button onclick="attributionUI.showDiscoverCircuitsDialog()">Discover Circuits</button>
        <button onclick="attributionUI.showCompareCircuitsDialog()">Compare Circuits</button>
        <button onclick="attributionUI.closeModal()">Close</button>
      </div>
    `);
  }

  showExportDialog() {
    this.showModal('Export Options', `
      <h3>Export Graph</h3>
      <div style="display: flex; flex-direction: column; gap: 1rem; margin: 1rem 0;">
        <button onclick="attributionUI.visualization.exportImage('png'); attributionUI.closeModal();">Export as PNG</button>
        <button onclick="attributionUI.visualization.exportImage('svg'); attributionUI.closeModal();">Export as SVG</button>
        <button onclick="attributionUI.exportAsJSON()">Export as JSON</button>
        <button onclick="attributionUI.exportAsMermaid()">Export as Mermaid</button>
      </div>
      <button onclick="attributionUI.closeModal()">Close</button>
    `);
  }

  showHelpDialog() {
    this.showModal('Help', `
      <h3>Attribution Graphs Explorer</h3>
      <p>Interactive visualization for exploring transformer model circuits.</p>
      
      <h4>Navigation:</h4>
      <ul>
        <li>Pan: Click and drag background</li>
        <li>Zoom: Mouse wheel or +/- buttons</li>
        <li>Select: Click node (Ctrl+click for multi-select)</li>
        <li>Focus: Double-click node</li>
      </ul>
      
      <h4>Filtering:</h4>
      <ul>
        <li>Threshold: Hide weak attribution edges</li>
        <li>Node Types: Show/hide different node types</li>
        <li>Circuits: Highlight specific computational paths</li>
      </ul>
      
      <h4>Keyboard Shortcuts:</h4>
      <ul>
        <li>F: Fit to view</li>
        <li>R: Reset view</li>
        <li>Esc: Clear selection</li>
        <li>Ctrl+E: Export</li>
      </ul>
      
      <button onclick="attributionUI.closeModal()" style="margin-top: 1rem;">Close</button>
    `);
  }

  showStatisticsDialog() {
    const stats = AttributionGraphUtils.calculateGraphStats(this.visualization.filteredData);
    
    this.showModal('Graph Statistics', `
      <h3>Current Graph Statistics</h3>
      <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin: 1rem 0;">
        <div>
          <h4>Nodes</h4>
          <p>Total: ${stats.nodeCount}</p>
          ${Object.entries(stats.nodesByType).map(([type, count]) => 
            `<p>${type}: ${count}</p>`
          ).join('')}
        </div>
        <div>
          <h4>Edges</h4>
          <p>Total: ${stats.edgeCount}</p>
          <p>Max Weight: ${AttributionGraphUtils.formatNumber(stats.maxWeight)}</p>
          <p>Min Weight: ${AttributionGraphUtils.formatNumber(stats.minWeight)}</p>
          <p>Avg Weight: ${AttributionGraphUtils.formatNumber(stats.averageWeight)}</p>
        </div>
      </div>
      <button onclick="attributionUI.closeModal()">Close</button>
    `);
  }

  showModal(title, content) {
    const modal = document.createElement('div');
    modal.className = 'modal';
    modal.innerHTML = `
      <div class="modal-content">
        <div class="modal-header">
          <h2>${title}</h2>
          <button class="modal-close" onclick="attributionUI.closeModal()">&times;</button>
        </div>
        <div class="modal-body">
          ${content}
        </div>
      </div>
    `;
    
    document.body.appendChild(modal);
    this.currentModal = modal;
    
    // Close on background click
    modal.addEventListener('click', (e) => {
      if (e.target === modal) {
        this.closeModal();
      }
    });
  }

  closeModal() {
    if (this.currentModal) {
      document.body.removeChild(this.currentModal);
      this.currentModal = null;
    }
  }

  // Export methods
  exportAsJSON() {
    const data = this.visualization.filteredData;
    const jsonString = AttributionGraphUtils.exportGraphData(data, 'json');
    this.downloadText(jsonString, 'attribution-graph.json', 'application/json');
    this.closeModal();
  }

  exportAsMermaid() {
    const data = this.visualization.filteredData;
    const mermaidString = AttributionGraphUtils.exportGraphData(data, 'mermaid');
    this.downloadText(mermaidString, 'attribution-graph.mmd', 'text/plain');
    this.closeModal();
  }

  downloadText(text, filename, mimeType) {
    const blob = new Blob([text], { type: mimeType });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  // Event handlers
  handleResize() {
    const newWidth = document.getElementById('graph-canvas').clientWidth;
    const newHeight = document.getElementById('graph-canvas').clientHeight;
    this.visualization.resize(newWidth, newHeight);
  }

  handleKeyboard(event) {
    // Handle keyboard shortcuts
    if (event.target.tagName === 'INPUT' || event.target.tagName === 'TEXTAREA') {
      return; // Don't handle shortcuts when typing in inputs
    }

    switch (event.key.toLowerCase()) {
      case 'f':
        event.preventDefault();
        this.visualization.fitToView();
        break;
      case 'r':
        event.preventDefault();
        this.visualization.resetView();
        break;
      case 'escape':
        event.preventDefault();
        this.visualization.selectNodes([]);
        this.clearNodeDetails();
        if (this.currentModal) {
          this.closeModal();
        }
        break;
      case 'e':
        if (event.ctrlKey || event.metaKey) {
          event.preventDefault();
          this.showExportDialog();
        }
        break;
    }
  }

  // Utility methods
  toggleMinimap() {
    const minimap = document.getElementById('minimap');
    if (minimap) {
      minimap.style.display = minimap.style.display === 'none' ? 'block' : 'none';
    }
    this.closeModal();
  }
}

// Initialize UI when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
  window.attributionUI = new AttributionGraphUI();
});