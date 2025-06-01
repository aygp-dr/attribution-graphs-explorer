// Attribution Graphs Explorer - Graph Visualization

class AttributionGraphVisualization {
  constructor(containerId, options = {}) {
    this.containerId = containerId;
    this.container = document.getElementById(containerId);
    this.options = {
      width: options.width || this.container.clientWidth,
      height: options.height || this.container.clientHeight,
      nodeRadius: options.nodeRadius || 8,
      linkDistance: options.linkDistance || 100,
      charge: options.charge || -300,
      zoomExtent: options.zoomExtent || [0.1, 10],
      ...options
    };

    this.graphData = null;
    this.filteredData = null;
    this.selectedNodes = new Set();
    this.highlightedCircuit = null;
    this.isInitialized = false;

    // Event callbacks
    this.onNodeClick = null;
    this.onNodeHover = null;
    this.onEdgeClick = null;
    this.onSelectionChange = null;

    this.init();
  }

  init() {
    // Clear container
    this.container.innerHTML = '';

    // Create SVG
    this.svg = d3.select(`#${this.containerId}`)
      .append('svg')
      .attr('width', this.options.width)
      .attr('height', this.options.height);

    // Create groups for different elements
    this.g = this.svg.append('g').attr('class', 'graph-container');
    this.linksGroup = this.g.append('g').attr('class', 'links');
    this.nodesGroup = this.g.append('g').attr('class', 'nodes');

    // Setup zoom behavior
    this.zoom = d3.zoom()
      .scaleExtent(this.options.zoomExtent)
      .on('zoom', (event) => {
        this.g.attr('transform', event.transform);
        this.updateZoomLevel(event.transform.k);
      });

    this.svg.call(this.zoom);

    // Setup force simulation
    this.simulation = d3.forceSimulation()
      .force('link', d3.forceLink().id(d => d.id).distance(this.options.linkDistance))
      .force('charge', d3.forceManyBody().strength(this.options.charge))
      .force('center', d3.forceCenter(this.options.width / 2, this.options.height / 2))
      .force('collision', d3.forceCollide().radius(this.options.nodeRadius + 5));

    // Create tooltip
    this.tooltip = d3.select('body')
      .append('div')
      .attr('class', 'tooltip')
      .style('position', 'absolute')
      .style('visibility', 'hidden');

    this.isInitialized = true;
    this.updateStatus('Visualization initialized');
  }

  loadData(graphData) {
    try {
      // Validate data
      const errors = AttributionGraphUtils.validateGraphData(graphData);
      if (errors.length > 0) {
        throw new Error(`Invalid graph data: ${errors.join(', ')}`);
      }

      this.graphData = graphData;
      this.filteredData = { ...graphData };
      
      this.updateVisualization();
      this.updateStatistics();
      this.updateStatus(`Loaded graph with ${graphData.nodes.length} nodes and ${graphData.edges.length} edges`);
      
      return true;
    } catch (error) {
      console.error('Error loading graph data:', error);
      this.updateStatus(`Error: ${error.message}`);
      return false;
    }
  }

  updateVisualization() {
    if (!this.filteredData) return;

    // Update simulation data
    this.simulation
      .nodes(this.filteredData.nodes)
      .force('link')
      .links(this.filteredData.edges);

    // Update links
    this.updateLinks();
    
    // Update nodes
    this.updateNodes();

    // Restart simulation
    this.simulation.alpha(1).restart();
  }

  updateLinks() {
    const links = this.linksGroup
      .selectAll('.link')
      .data(this.filteredData.edges, d => `${d.source.id || d.source}-${d.target.id || d.target}`);

    // Remove old links
    links.exit().remove();

    // Add new links
    const linkEnter = links.enter()
      .append('line')
      .attr('class', d => `link edge-${AttributionGraphUtils.classifyEdgeStrength(d.weight)}`)
      .style('stroke-width', d => AttributionGraphUtils.getEdgeWidth(d.weight))
      .style('stroke', d => AttributionGraphUtils.getAttributionColor(d.weight))
      .style('stroke-opacity', 0.7)
      .on('mouseover', (event, d) => this.showEdgeTooltip(event, d))
      .on('mouseout', () => this.hideTooltip())
      .on('click', (event, d) => this.handleEdgeClick(event, d));

    // Merge and update
    this.links = linkEnter.merge(links);
  }

  updateNodes() {
    const nodes = this.nodesGroup
      .selectAll('.node')
      .data(this.filteredData.nodes, d => d.id);

    // Remove old nodes
    nodes.exit().remove();

    // Add new nodes
    const nodeEnter = nodes.enter()
      .append('g')
      .attr('class', 'node')
      .call(this.createDragBehavior());

    // Add node shapes
    nodeEnter.append('circle')
      .attr('r', this.options.nodeRadius)
      .style('fill', d => AttributionGraphUtils.getNodeColor(d.type))
      .style('stroke', '#fff')
      .style('stroke-width', 2);

    // Add node labels
    nodeEnter.append('text')
      .attr('dx', this.options.nodeRadius + 5)
      .attr('dy', 4)
      .style('font-size', '10px')
      .style('fill', '#333')
      .text(d => d.label || d.id);

    // Add event handlers
    nodeEnter
      .on('mouseover', (event, d) => this.showNodeTooltip(event, d))
      .on('mouseout', () => this.hideTooltip())
      .on('click', (event, d) => this.handleNodeClick(event, d))
      .on('dblclick', (event, d) => this.handleNodeDoubleClick(event, d));

    // Merge and update
    this.nodes = nodeEnter.merge(nodes);

    // Update selection styles
    this.updateSelectionStyles();

    // Update simulation tick
    this.simulation.on('tick', () => {
      this.links
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);

      this.nodes
        .attr('transform', d => `translate(${d.x},${d.y})`);
    });
  }

  createDragBehavior() {
    return d3.drag()
      .on('start', (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on('drag', (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on('end', (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });
  }

  applyFilters(filters) {
    if (!this.graphData) return;

    this.filteredData = AttributionGraphUtils.filterGraph(this.graphData, filters);
    this.updateVisualization();
    this.updateStatistics();
    
    const stats = this.filteredData.metadata;
    this.updateStatus(`Filtered: ${stats.filteredNodeCount}/${stats.originalNodeCount} nodes, ${stats.filteredEdgeCount}/${stats.originalEdgeCount} edges`);
  }

  highlightCircuit(circuit) {
    this.highlightedCircuit = circuit;
    
    if (!circuit) {
      // Clear highlighting
      this.nodes.style('opacity', 1);
      this.links.style('opacity', 0.7);
      return;
    }

    // Get circuit node IDs
    const circuitNodeIds = new Set(circuit.flatMap(path => path));
    
    // Dim non-circuit elements
    this.nodes.style('opacity', d => circuitNodeIds.has(d.id) ? 1 : 0.3);
    
    // Highlight circuit edges
    this.links.style('opacity', d => {
      const isCircuitEdge = circuitNodeIds.has(d.source.id || d.source) && 
                           circuitNodeIds.has(d.target.id || d.target);
      return isCircuitEdge ? 1 : 0.2;
    });

    this.updateStatus(`Highlighted circuit with ${circuitNodeIds.size} nodes`);
  }

  selectNodes(nodeIds) {
    this.selectedNodes = new Set(nodeIds);
    this.updateSelectionStyles();
    
    if (this.onSelectionChange) {
      this.onSelectionChange(Array.from(this.selectedNodes));
    }
  }

  updateSelectionStyles() {
    if (!this.nodes) return;

    this.nodes.select('circle')
      .style('stroke', d => this.selectedNodes.has(d.id) ? '#e74c3c' : '#fff')
      .style('stroke-width', d => this.selectedNodes.has(d.id) ? 4 : 2);
  }

  handleNodeClick(event, node) {
    event.stopPropagation();
    
    if (event.ctrlKey || event.metaKey) {
      // Multi-select
      if (this.selectedNodes.has(node.id)) {
        this.selectedNodes.delete(node.id);
      } else {
        this.selectedNodes.add(node.id);
      }
    } else {
      // Single select
      this.selectedNodes.clear();
      this.selectedNodes.add(node.id);
    }
    
    this.updateSelectionStyles();
    
    if (this.onNodeClick) {
      this.onNodeClick(node, Array.from(this.selectedNodes));
    }
    
    if (this.onSelectionChange) {
      this.onSelectionChange(Array.from(this.selectedNodes));
    }
  }

  handleNodeDoubleClick(event, node) {
    // Focus on node's subgraph
    const subgraph = AttributionGraphUtils.extractSubgraph(this.graphData, node.id, 2);
    this.filteredData = subgraph;
    this.updateVisualization();
    this.updateStatus(`Focused on subgraph around ${node.id}`);
  }

  handleEdgeClick(event, edge) {
    event.stopPropagation();
    
    if (this.onEdgeClick) {
      this.onEdgeClick(edge);
    }
  }

  showNodeTooltip(event, node) {
    this.tooltip
      .style('visibility', 'visible')
      .html(AttributionGraphUtils.createNodeTooltip(node))
      .style('left', (event.pageX + 10) + 'px')
      .style('top', (event.pageY - 10) + 'px');
      
    if (this.onNodeHover) {
      this.onNodeHover(node);
    }
  }

  showEdgeTooltip(event, edge) {
    this.tooltip
      .style('visibility', 'visible')
      .html(AttributionGraphUtils.createEdgeTooltip(edge))
      .style('left', (event.pageX + 10) + 'px')
      .style('top', (event.pageY - 10) + 'px');
  }

  hideTooltip() {
    this.tooltip.style('visibility', 'hidden');
  }

  fitToView() {
    if (!this.filteredData || this.filteredData.nodes.length === 0) return;

    // Calculate bounds
    const bounds = this.calculateBounds();
    const padding = 50;
    
    const dx = bounds.maxX - bounds.minX + padding * 2;
    const dy = bounds.maxY - bounds.minY + padding * 2;
    const x = (bounds.minX + bounds.maxX) / 2;
    const y = (bounds.minY + bounds.maxY) / 2;
    
    const scale = Math.min(this.options.width / dx, this.options.height / dy, 2);
    const translate = [this.options.width / 2 - scale * x, this.options.height / 2 - scale * y];
    
    this.svg.transition()
      .duration(750)
      .call(this.zoom.transform, d3.zoomIdentity.translate(translate[0], translate[1]).scale(scale));
  }

  calculateBounds() {
    const nodes = this.filteredData.nodes;
    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
    
    nodes.forEach(node => {
      if (node.x !== undefined && node.y !== undefined) {
        minX = Math.min(minX, node.x);
        maxX = Math.max(maxX, node.x);
        minY = Math.min(minY, node.y);
        maxY = Math.max(maxY, node.y);
      }
    });
    
    return { minX, maxX, minY, maxY };
  }

  zoomIn() {
    this.svg.transition().duration(200).call(
      this.zoom.scaleBy, 1.5
    );
  }

  zoomOut() {
    this.svg.transition().duration(200).call(
      this.zoom.scaleBy, 1 / 1.5
    );
  }

  resetView() {
    this.svg.transition().duration(500).call(
      this.zoom.transform,
      d3.zoomIdentity
    );
  }

  updateZoomLevel(scale) {
    const zoomPercent = Math.round(scale * 100);
    document.getElementById('zoom-level').textContent = `Zoom: ${zoomPercent}%`;
  }

  updateStatistics() {
    if (!this.filteredData) return;

    const stats = AttributionGraphUtils.calculateGraphStats(this.filteredData);
    
    document.getElementById('node-count').textContent = stats.nodeCount;
    document.getElementById('edge-count').textContent = stats.edgeCount;
    
    // Update circuit count if available
    const circuitCount = window.CIRCUIT_DATA ? window.CIRCUIT_DATA.length : 0;
    document.getElementById('circuit-count').textContent = circuitCount;
  }

  updateStatus(message) {
    document.getElementById('status').textContent = message;
  }

  exportImage(format = 'png') {
    const svgElement = this.svg.node();
    const svgString = new XMLSerializer().serializeToString(svgElement);
    
    if (format === 'svg') {
      // Direct SVG download
      const blob = new Blob([svgString], { type: 'image/svg+xml' });
      this.downloadBlob(blob, 'attribution-graph.svg');
    } else {
      // Convert to raster format
      const canvas = document.createElement('canvas');
      canvas.width = this.options.width;
      canvas.height = this.options.height;
      const ctx = canvas.getContext('2d');
      
      const img = new Image();
      img.onload = () => {
        ctx.drawImage(img, 0, 0);
        canvas.toBlob(blob => {
          this.downloadBlob(blob, `attribution-graph.${format}`);
        }, `image/${format}`);
      };
      
      img.src = 'data:image/svg+xml;base64,' + btoa(svgString);
    }
  }

  downloadBlob(blob, filename) {
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  resize(width, height) {
    this.options.width = width || this.container.clientWidth;
    this.options.height = height || this.container.clientHeight;
    
    this.svg
      .attr('width', this.options.width)
      .attr('height', this.options.height);
    
    this.simulation
      .force('center', d3.forceCenter(this.options.width / 2, this.options.height / 2))
      .restart();
  }

  destroy() {
    if (this.simulation) {
      this.simulation.stop();
    }
    
    if (this.tooltip) {
      this.tooltip.remove();
    }
    
    if (this.container) {
      this.container.innerHTML = '';
    }
  }
}

// Make class globally available
window.AttributionGraphVisualization = AttributionGraphVisualization;