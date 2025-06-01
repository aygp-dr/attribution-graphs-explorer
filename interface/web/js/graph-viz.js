// Attribution Graph Visualization using D3.js

class AttributionGraphViz {
    constructor(containerId) {
        this.container = d3.select(containerId);
        this.width = 0;
        this.height = 0;
        this.svg = null;
        this.g = null;
        this.zoom = null;
        this.simulation = null;
        
        // Data
        this.graph = { nodes: [], edges: [], circuits: [] };
        this.filteredGraph = { nodes: [], edges: [] };
        
        // State
        this.selectedNode = null;
        this.selectedEdge = null;
        this.highlightedCircuit = null;
        this.filters = {
            threshold: 0.1,
            layerMin: 0,
            layerMax: 10,
            activeCircuits: new Set()
        };
        
        this.initSVG();
        this.setupEventHandlers();
    }
    
    initSVG() {
        // Get container dimensions
        const rect = this.container.node().getBoundingClientRect();
        this.width = rect.width;
        this.height = rect.height;
        
        // Create SVG
        this.svg = this.container.append('svg')
            .attr('width', this.width)
            .attr('height', this.height);
            
        // Create zoom behavior
        this.zoom = d3.zoom()
            .scaleExtent([0.1, 10])
            .on('zoom', (event) => {
                this.g.attr('transform', event.transform);
                this.updateZoomLevel(event.transform.k);
            });
            
        this.svg.call(this.zoom);
        
        // Create main group for graph elements
        this.g = this.svg.append('g').attr('class', 'graph-group');
        
        // Create groups for different elements (order matters for z-index)
        this.g.append('g').attr('class', 'edges');
        this.g.append('g').attr('class', 'nodes');
        this.g.append('g').attr('class', 'labels');
    }
    
    setupEventHandlers() {
        // Window resize
        window.addEventListener('resize', () => {
            this.handleResize();
        });
        
        // Clear selection on canvas click
        this.svg.on('click', (event) => {
            if (event.target === this.svg.node()) {
                this.clearSelection();
            }
        });
        
        // Context menu
        this.svg.on('contextmenu', (event) => {
            event.preventDefault();
            this.hideContextMenu();
        });
    }
    
    loadGraph(graphData) {
        console.log('Loading graph data:', graphData);
        
        this.graph = {
            nodes: graphData.nodes || [],
            edges: graphData.edges || [],
            circuits: graphData.circuits || []
        };
        
        // Process data
        this.processNodes();
        this.processEdges();
        
        // Apply initial filters
        this.applyFilters();
        
        // Create visualization
        this.createVisualization();
        
        // Update UI
        this.updateCircuitList();
        this.updateGraphStats();
    }
    
    processNodes() {
        this.graph.nodes.forEach((node, i) => {
            // Ensure required properties
            node.id = node.id || `node_${i}`;
            node.type = node.type || 'feature';
            node.activation = node.activation || 0;
            node.layer = node.layer || 0;
            
            // Set initial position if not provided
            if (!node.x && !node.y) {
                node.x = node.position?.x || (node.layer * 200 + Math.random() * 50);
                node.y = node.position?.y || (i * 60 + Math.random() * 50);
            }
            
            // Add display properties
            node.radius = this.getNodeRadius(node);
            node.color = this.getNodeColor(node);
            node.label = this.getNodeLabel(node);
        });
    }
    
    processEdges() {
        this.graph.edges.forEach((edge, i) => {
            // Ensure required properties
            edge.id = edge.id || `edge_${i}`;
            edge.source = edge.source;
            edge.target = edge.target;
            edge.weight = edge.weight || 0;
            
            // Add display properties
            edge.strokeWidth = this.getEdgeWidth(edge);
            edge.color = this.getEdgeColor(edge);
            edge.opacity = this.getEdgeOpacity(edge);
        });
    }
    
    applyFilters() {
        // Filter nodes
        this.filteredGraph.nodes = this.graph.nodes.filter(node => {
            return node.layer >= this.filters.layerMin && 
                   node.layer <= this.filters.layerMax;
        });
        
        // Filter edges
        this.filteredGraph.edges = this.graph.edges.filter(edge => {
            const sourceExists = this.filteredGraph.nodes.some(n => n.id === edge.source);
            const targetExists = this.filteredGraph.nodes.some(n => n.id === edge.target);
            const aboveThreshold = Math.abs(edge.weight) >= this.filters.threshold;
            
            return sourceExists && targetExists && aboveThreshold;
        });
        
        console.log(`Filtered to ${this.filteredGraph.nodes.length} nodes, ${this.filteredGraph.edges.length} edges`);
    }
    
    createVisualization() {
        this.createSimulation();
        this.renderEdges();
        this.renderNodes();
        this.renderLabels();
    }
    
    createSimulation() {
        // Stop existing simulation
        if (this.simulation) {
            this.simulation.stop();
        }
        
        // Create force simulation
        this.simulation = d3.forceSimulation(this.filteredGraph.nodes)
            .force('link', d3.forceLink(this.filteredGraph.edges)
                .id(d => d.id)
                .distance(100)
                .strength(0.5))
            .force('charge', d3.forceManyBody().strength(-300))
            .force('center', d3.forceCenter(this.width / 2, this.height / 2))
            .force('collision', d3.forceCollide().radius(d => d.radius + 5))
            .on('tick', () => this.tick());
    }
    
    renderNodes() {
        const nodes = this.g.select('.nodes')
            .selectAll('.node')
            .data(this.filteredGraph.nodes, d => d.id);
            
        // Remove old nodes
        nodes.exit().remove();
        
        // Add new nodes
        const nodeEnter = nodes.enter()
            .append('circle')
            .attr('class', 'node')
            .attr('r', d => d.radius)
            .style('fill', d => d.color)
            .style('stroke', d => d3.rgb(d.color).darker(1))
            .call(this.createDragBehavior())
            .on('click', (event, d) => this.handleNodeClick(event, d))
            .on('contextmenu', (event, d) => this.handleNodeContextMenu(event, d))
            .on('mouseover', (event, d) => this.showTooltip(event, d))
            .on('mouseout', () => this.hideTooltip());
            
        // Update all nodes
        nodes.merge(nodeEnter)
            .classed('feature', d => d.type === 'feature')
            .classed('token', d => d.type === 'token')
            .classed('logit', d => d.type === 'logit')
            .classed('error', d => d.type === 'error');
    }
    
    renderEdges() {
        const edges = this.g.select('.edges')
            .selectAll('.edge')
            .data(this.filteredGraph.edges, d => d.id);
            
        // Remove old edges
        edges.exit().remove();
        
        // Add new edges
        const edgeEnter = edges.enter()
            .append('line')
            .attr('class', 'edge')
            .style('stroke-width', d => d.strokeWidth)
            .style('stroke', d => d.color)
            .style('opacity', d => d.opacity)
            .on('click', (event, d) => this.handleEdgeClick(event, d))
            .on('mouseover', (event, d) => this.showEdgeTooltip(event, d))
            .on('mouseout', () => this.hideTooltip());
            
        // Update all edges
        edges.merge(edgeEnter)
            .classed('strong', d => Math.abs(d.weight) > 0.7)
            .classed('medium', d => Math.abs(d.weight) > 0.3 && Math.abs(d.weight) <= 0.7)
            .classed('weak', d => Math.abs(d.weight) <= 0.3);
    }
    
    renderLabels() {
        const labels = this.g.select('.labels')
            .selectAll('.node-label')
            .data(this.filteredGraph.nodes, d => d.id);
            
        // Remove old labels
        labels.exit().remove();
        
        // Add new labels
        const labelEnter = labels.enter()
            .append('text')
            .attr('class', 'node-label')
            .text(d => d.label);
            
        // Update all labels
        labels.merge(labelEnter);
    }
    
    tick() {
        // Update edge positions
        this.g.select('.edges').selectAll('.edge')
            .attr('x1', d => d.source.x)
            .attr('y1', d => d.source.y)
            .attr('x2', d => d.target.x)
            .attr('y2', d => d.target.y);
            
        // Update node positions
        this.g.select('.nodes').selectAll('.node')
            .attr('cx', d => d.x)
            .attr('cy', d => d.y);
            
        // Update label positions
        this.g.select('.labels').selectAll('.node-label')
            .attr('x', d => d.x)
            .attr('y', d => d.y + d.radius + 12);
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
    
    handleNodeClick(event, node) {
        event.stopPropagation();
        this.selectNode(node);
        this.updateInspector(node);
    }
    
    handleEdgeClick(event, edge) {
        event.stopPropagation();
        this.selectEdge(edge);
        this.updateInspector(edge);
    }
    
    handleNodeContextMenu(event, node) {
        event.preventDefault();
        event.stopPropagation();
        this.showContextMenu(event, node);
    }
    
    selectNode(node) {
        this.clearSelection();
        this.selectedNode = node;
        
        this.g.selectAll('.node')
            .classed('selected', d => d.id === node.id);
    }
    
    selectEdge(edge) {
        this.clearSelection();
        this.selectedEdge = edge;
        
        this.g.selectAll('.edge')
            .classed('selected', d => d.id === edge.id);
    }
    
    clearSelection() {
        this.selectedNode = null;
        this.selectedEdge = null;
        
        this.g.selectAll('.node').classed('selected', false);
        this.g.selectAll('.edge').classed('selected', false);
    }
    
    // Utility methods for node/edge styling
    getNodeRadius(node) {
        const baseRadius = 8;
        const activationScale = Math.abs(node.activation) * 6;
        return Math.max(baseRadius + activationScale, 6);
    }
    
    getNodeColor(node) {
        const colors = {
            feature: '#f59e0b',
            token: '#10b981',
            logit: '#3b82f6',
            error: '#ef4444'
        };
        return colors[node.type] || '#64748b';
    }
    
    getNodeLabel(node) {
        if (node.type === 'token') {
            return node.metadata?.token || `T:${node.id}`;
        } else if (node.type === 'logit') {
            return node.metadata?.token || `L:${node.id}`;
        } else {
            return `F:${node.id}`;
        }
    }
    
    getEdgeWidth(edge) {
        const weight = Math.abs(edge.weight);
        if (weight > 0.7) return 3;
        if (weight > 0.3) return 2;
        return 1;
    }
    
    getEdgeColor(edge) {
        const weight = Math.abs(edge.weight);
        if (weight > 0.7) return '#2563eb';
        if (weight > 0.3) return '#64748b';
        return '#cbd5e1';
    }
    
    getEdgeOpacity(edge) {
        return 0.6 + Math.abs(edge.weight) * 0.4;
    }
    
    // Filter methods
    setThreshold(threshold) {
        this.filters.threshold = threshold;
        this.applyFilters();
        this.updateVisualization();
    }
    
    setLayerRange(min, max) {
        this.filters.layerMin = min;
        this.filters.layerMax = max;
        this.applyFilters();
        this.updateVisualization();
    }
    
    highlightCircuit(circuitName) {
        const circuit = this.graph.circuits.find(c => c.name === circuitName);
        if (!circuit) return;
        
        this.clearHighlights();
        this.highlightedCircuit = circuit;
        
        // Highlight nodes in circuit
        circuit.path.forEach(nodeId => {
            this.g.selectAll('.node')
                .filter(d => d.id === nodeId)
                .classed('highlighted', true);
        });
        
        // Highlight edges in circuit
        for (let i = 0; i < circuit.path.length - 1; i++) {
            const source = circuit.path[i];
            const target = circuit.path[i + 1];
            
            this.g.selectAll('.edge')
                .filter(d => d.source.id === source && d.target.id === target)
                .classed('circuit', true);
        }
    }
    
    clearHighlights() {
        this.highlightedCircuit = null;
        this.g.selectAll('.node').classed('highlighted', false);
        this.g.selectAll('.edge').classed('circuit', false);
    }
    
    updateVisualization() {
        this.renderNodes();
        this.renderEdges();
        this.renderLabels();
        this.simulation.nodes(this.filteredGraph.nodes);
        this.simulation.force('link').links(this.filteredGraph.edges);
        this.simulation.alpha(0.3).restart();
    }
    
    // Tooltip methods
    showTooltip(event, node) {
        const tooltip = d3.select('#tooltip');
        const interpretation = node.metadata?.interpretation || '';
        
        tooltip.style('display', 'block')
            .style('left', (event.pageX + 10) + 'px')
            .style('top', (event.pageY - 10) + 'px')
            .html(`
                <strong>${node.label}</strong><br>
                Type: ${node.type}<br>
                Activation: ${node.activation.toFixed(3)}<br>
                Layer: ${node.layer}<br>
                ${interpretation ? `<em>${interpretation}</em>` : ''}
            `);
    }
    
    showEdgeTooltip(event, edge) {
        const tooltip = d3.select('#tooltip');
        
        tooltip.style('display', 'block')
            .style('left', (event.pageX + 10) + 'px')
            .style('top', (event.pageY - 10) + 'px')
            .html(`
                <strong>Edge</strong><br>
                From: ${edge.source.id}<br>
                To: ${edge.target.id}<br>
                Weight: ${edge.weight.toFixed(3)}<br>
                Click to highlight path
            `);
    }
    
    hideTooltip() {
        d3.select('#tooltip').style('display', 'none');
    }
    
    // Context menu
    showContextMenu(event, node) {
        const menu = d3.select('#context-menu');
        menu.style('display', 'block')
            .style('left', event.pageX + 'px')
            .style('top', event.pageY + 'px');
            
        // Store the context node for menu actions
        this.contextNode = node;
    }
    
    hideContextMenu() {
        d3.select('#context-menu').style('display', 'none');
        this.contextNode = null;
    }
    
    // Layout methods
    applyLayout(layoutType) {
        switch (layoutType) {
            case 'force':
                this.applyForceLayout();
                break;
            case 'layered':
                this.applyLayeredLayout();
                break;
            case 'circular':
                this.applyCircularLayout();
                break;
        }
    }
    
    applyForceLayout() {
        this.createSimulation();
    }
    
    applyLayeredLayout() {
        // Group nodes by layer
        const layerGroups = d3.group(this.filteredGraph.nodes, d => d.layer);
        const layers = Array.from(layerGroups.keys()).sort((a, b) => a - b);
        
        layers.forEach((layer, layerIndex) => {
            const nodesInLayer = layerGroups.get(layer);
            const layerX = (layerIndex + 1) * (this.width / (layers.length + 1));
            
            nodesInLayer.forEach((node, nodeIndex) => {
                node.fx = layerX;
                node.fy = (nodeIndex + 1) * (this.height / (nodesInLayer.length + 1));
            });
        });
        
        this.simulation.alpha(0.3).restart();
    }
    
    applyCircularLayout() {
        const nodes = this.filteredGraph.nodes;
        const centerX = this.width / 2;
        const centerY = this.height / 2;
        const radius = Math.min(this.width, this.height) * 0.35;
        
        nodes.forEach((node, i) => {
            const angle = (2 * Math.PI * i) / nodes.length;
            node.fx = centerX + radius * Math.cos(angle);
            node.fy = centerY + radius * Math.sin(angle);
        });
        
        this.simulation.alpha(0.3).restart();
    }
    
    // Zoom and pan methods
    zoomIn() {
        this.svg.transition().call(this.zoom.scaleBy, 1.5);
    }
    
    zoomOut() {
        this.svg.transition().call(this.zoom.scaleBy, 1 / 1.5);
    }
    
    fitToView() {
        const bounds = this.g.node().getBBox();
        const parent = this.svg.node();
        const fullWidth = parent.clientWidth || parent.parentNode.clientWidth;
        const fullHeight = parent.clientHeight || parent.parentNode.clientHeight;
        const width = bounds.width;
        const height = bounds.height;
        const midX = bounds.x + width / 2;
        const midY = bounds.y + height / 2;
        
        if (width == 0 || height == 0) return;
        
        const scale = 0.85 / Math.max(width / fullWidth, height / fullHeight);
        const translate = [fullWidth / 2 - scale * midX, fullHeight / 2 - scale * midY];
        
        this.svg.transition()
            .duration(750)
            .call(this.zoom.transform, d3.zoomIdentity.translate(translate[0], translate[1]).scale(scale));
    }
    
    resetView() {
        this.svg.transition()
            .duration(750)
            .call(this.zoom.transform, d3.zoomIdentity);
    }
    
    updateZoomLevel(scale) {
        document.getElementById('zoom-level').textContent = Math.round(scale * 100) + '%';
    }
    
    // UI update methods
    updateCircuitList() {
        const container = document.getElementById('circuit-list');
        container.innerHTML = '';
        
        this.graph.circuits.forEach(circuit => {
            const item = document.createElement('div');
            item.className = 'circuit-item';
            item.innerHTML = `
                <input type="checkbox" id="circuit-${circuit.name}" value="${circuit.name}">
                <label for="circuit-${circuit.name}">${circuit.name}</label>
            `;
            
            const checkbox = item.querySelector('input');
            checkbox.addEventListener('change', (e) => {
                if (e.target.checked) {
                    this.highlightCircuit(circuit.name);
                } else {
                    this.clearHighlights();
                }
            });
            
            container.appendChild(item);
        });
    }
    
    updateGraphStats() {
        document.getElementById('visible-nodes').textContent = this.filteredGraph.nodes.length;
    }
    
    updateInspector(item) {
        const container = document.getElementById('inspector-content');
        
        if (item.type) {
            // It's a node
            this.updateNodeInspector(container, item);
        } else {
            // It's an edge
            this.updateEdgeInspector(container, item);
        }
    }
    
    updateNodeInspector(container, node) {
        const metadata = node.metadata || {};
        
        container.innerHTML = `
            <div class="node-details">
                <h3>Node Inspector</h3>
                
                <div class="detail-section">
                    <h4>Basic Info</h4>
                    <div class="detail-row">
                        <span>ID:</span>
                        <span>${node.id}</span>
                    </div>
                    <div class="detail-row">
                        <span>Type:</span>
                        <span>${node.type}</span>
                    </div>
                    <div class="detail-row">
                        <span>Layer:</span>
                        <span>${node.layer}</span>
                    </div>
                    <div class="detail-row">
                        <span>Activation:</span>
                        <span>${node.activation.toFixed(3)}</span>
                    </div>
                </div>
                
                ${metadata.interpretation ? `
                <div class="detail-section">
                    <h4>Interpretation</h4>
                    <p>${metadata.interpretation}</p>
                </div>
                ` : ''}
                
                <div class="detail-section">
                    <h4>Connections</h4>
                    ${this.getConnectionInfo(node)}
                </div>
                
                <div class="detail-section">
                    <h4>Actions</h4>
                    <button class="btn btn-secondary" onclick="graphViz.tracePaths('${node.id}')">Trace Paths</button>
                    <button class="btn btn-secondary" onclick="graphViz.highlightNeighbors('${node.id}')">Highlight</button>
                    <button class="btn btn-secondary" onclick="graphViz.hideNode('${node.id}')">Hide Node</button>
                </div>
            </div>
        `;
    }
    
    updateEdgeInspector(container, edge) {
        container.innerHTML = `
            <div class="edge-details">
                <h3>Edge Inspector</h3>
                
                <div class="detail-section">
                    <h4>Connection</h4>
                    <div class="detail-row">
                        <span>From:</span>
                        <span>${edge.source.id || edge.source}</span>
                    </div>
                    <div class="detail-row">
                        <span>To:</span>
                        <span>${edge.target.id || edge.target}</span>
                    </div>
                    <div class="detail-row">
                        <span>Weight:</span>
                        <span>${edge.weight.toFixed(3)}</span>
                    </div>
                </div>
                
                <div class="detail-section">
                    <h4>Actions</h4>
                    <button class="btn btn-secondary" onclick="graphViz.highlightPath('${edge.source.id || edge.source}', '${edge.target.id || edge.target}')">Highlight Path</button>
                </div>
            </div>
        `;
    }
    
    getConnectionInfo(node) {
        const incoming = this.filteredGraph.edges.filter(e => 
            (e.target.id || e.target) === node.id
        );
        const outgoing = this.filteredGraph.edges.filter(e => 
            (e.source.id || e.source) === node.id
        );
        
        return `
            <div class="detail-row">
                <span>Incoming:</span>
                <span>${incoming.length}</span>
            </div>
            <div class="detail-row">
                <span>Outgoing:</span>
                <span>${outgoing.length}</span>
            </div>
        `;
    }
    
    handleResize() {
        const rect = this.container.node().getBoundingClientRect();
        this.width = rect.width;
        this.height = rect.height;
        
        this.svg
            .attr('width', this.width)
            .attr('height', this.height);
            
        if (this.simulation) {
            this.simulation.force('center', d3.forceCenter(this.width / 2, this.height / 2));
            this.simulation.alpha(0.1).restart();
        }
    }
    
    // Action methods (called from inspector buttons)
    tracePaths(nodeId) {
        console.log('Tracing paths from:', nodeId);
        // Implementation for path tracing
    }
    
    highlightNeighbors(nodeId) {
        console.log('Highlighting neighbors of:', nodeId);
        // Implementation for neighbor highlighting
    }
    
    hideNode(nodeId) {
        console.log('Hiding node:', nodeId);
        // Implementation for hiding nodes
    }
    
    highlightPath(sourceId, targetId) {
        console.log('Highlighting path from', sourceId, 'to', targetId);
        // Implementation for path highlighting
    }
}