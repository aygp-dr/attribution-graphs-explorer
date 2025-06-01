// Controls for the Attribution Graph Explorer interface

class GraphControls {
    constructor(graphViz) {
        this.graphViz = graphViz;
        this.setupControls();
        this.setupModals();
    }
    
    setupControls() {
        // Threshold slider
        const thresholdSlider = document.getElementById('threshold-slider');
        const thresholdValue = document.getElementById('threshold-value');
        
        thresholdSlider.addEventListener('input', (e) => {
            const value = parseFloat(e.target.value);
            thresholdValue.textContent = value.toFixed(2);
            this.graphViz.setThreshold(value);
            this.updateVisibleNodes();
        });
        
        // Layer range sliders
        const layerMin = document.getElementById('layer-min');
        const layerMax = document.getElementById('layer-max');
        const layerMinVal = document.getElementById('layer-min-val');
        const layerMaxVal = document.getElementById('layer-max-val');
        
        layerMin.addEventListener('input', (e) => {
            const min = parseInt(e.target.value);
            const max = parseInt(layerMax.value);
            
            if (min <= max) {
                layerMinVal.textContent = min;
                this.graphViz.setLayerRange(min, max);
                this.updateVisibleNodes();
            }
        });
        
        layerMax.addEventListener('input', (e) => {
            const max = parseInt(e.target.value);
            const min = parseInt(layerMin.value);
            
            if (max >= min) {
                layerMaxVal.textContent = max;
                this.graphViz.setLayerRange(min, max);
                this.updateVisibleNodes();
            }
        });
        
        // Layout controls
        const layoutRadios = document.querySelectorAll('input[name="layout"]');
        layoutRadios.forEach(radio => {
            radio.addEventListener('change', (e) => {
                if (e.target.checked) {
                    this.graphViz.applyLayout(e.target.value);
                }
            });
        });
        
        document.getElementById('apply-layout').addEventListener('click', () => {
            const selectedLayout = document.querySelector('input[name="layout"]:checked').value;
            this.graphViz.applyLayout(selectedLayout);
        });
        
        document.getElementById('reset-layout').addEventListener('click', () => {
            this.graphViz.applyLayout('force');
        });
        
        // Zoom controls
        document.getElementById('zoom-in').addEventListener('click', () => {
            this.graphViz.zoomIn();
        });
        
        document.getElementById('zoom-out').addEventListener('click', () => {
            this.graphViz.zoomOut();
        });
        
        document.getElementById('zoom-fit').addEventListener('click', () => {
            this.graphViz.fitToView();
        });
        
        document.getElementById('reset-view').addEventListener('click', () => {
            this.graphViz.resetView();
        });
        
        // Circuit discovery
        document.getElementById('discover-circuits').addEventListener('click', () => {
            this.discoverCircuits();
        });
        
        // Context menu actions
        this.setupContextMenu();
    }
    
    setupModals() {
        // File input modal
        const modal = document.getElementById('file-modal');
        const loadBtn = document.getElementById('load-graph');
        const closeBtn = modal.querySelector('.close');
        const fileInput = document.getElementById('file-input');
        
        loadBtn.addEventListener('click', () => {
            modal.style.display = 'block';
        });
        
        closeBtn.addEventListener('click', () => {
            modal.style.display = 'none';
        });
        
        window.addEventListener('click', (e) => {
            if (e.target === modal) {
                modal.style.display = 'none';
            }
        });
        
        fileInput.addEventListener('change', (e) => {
            this.loadGraphFile(e.target.files[0]);
            modal.style.display = 'none';
        });
    }
    
    setupContextMenu() {
        const contextMenu = document.getElementById('context-menu');
        
        // Hide context menu when clicking elsewhere
        document.addEventListener('click', () => {
            this.graphViz.hideContextMenu();
        });
        
        // Handle context menu actions
        contextMenu.addEventListener('click', (e) => {
            const action = e.target.dataset.action;
            if (action && this.graphViz.contextNode) {
                this.handleContextMenuAction(action, this.graphViz.contextNode);
            }
            this.graphViz.hideContextMenu();
        });
    }
    
    handleContextMenuAction(action, node) {
        switch (action) {
            case 'inspect':
                this.graphViz.selectNode(node);
                this.graphViz.updateInspector(node);
                break;
                
            case 'trace-paths':
                this.tracePathsFromNode(node);
                break;
                
            case 'highlight':
                this.highlightNodeCircuit(node);
                break;
                
            case 'hide':
                this.hideNode(node);
                break;
                
            case 'export-info':
                this.exportNodeInfo(node);
                break;
        }
    }
    
    loadGraphFile(file) {
        if (!file) return;
        
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const graphData = JSON.parse(e.target.result);
                this.graphViz.loadGraph(graphData);
                this.updateGraphInfo(file.name, graphData);
            } catch (error) {
                alert('Error loading graph file: ' + error.message);
                console.error('Graph loading error:', error);
            }
        };
        reader.readAsText(file);
    }
    
    updateGraphInfo(filename, graphData) {
        document.getElementById('graph-name').textContent = filename;
        
        if (graphData.metadata) {
            const method = graphData.metadata.method || 'Unknown';
            document.getElementById('method-name').textContent = `Method: ${method}`;
        }
        
        // Update layer range sliders based on data
        if (graphData.layout && graphData.layout.layers) {
            const maxLayer = graphData.layout.layers - 1;
            const layerMin = document.getElementById('layer-min');
            const layerMax = document.getElementById('layer-max');
            
            layerMin.max = maxLayer;
            layerMax.max = maxLayer;
            layerMax.value = maxLayer;
            
            document.getElementById('layer-max-val').textContent = maxLayer;
        }
    }
    
    updateVisibleNodes() {
        this.graphViz.updateGraphStats();
    }
    
    discoverCircuits() {
        // This would integrate with the Scheme circuit discovery
        // For now, show a placeholder
        alert('Circuit discovery integration with Scheme backend coming soon!');
    }
    
    tracePathsFromNode(node) {
        console.log('Tracing paths from node:', node.id);
        
        // Find all paths from this node
        const paths = this.findPathsFromNode(node);
        
        // Highlight the strongest path
        if (paths.length > 0) {
            const strongestPath = paths.reduce((strongest, current) => 
                current.strength > strongest.strength ? current : strongest
            );
            
            this.highlightPath(strongestPath.nodes);
        }
    }
    
    findPathsFromNode(startNode) {
        // Simple pathfinding algorithm
        const paths = [];
        const visited = new Set();
        
        const dfs = (currentNode, path, strength) => {
            if (visited.has(currentNode.id)) return;
            if (path.length > 6) return; // Limit path length
            
            visited.add(currentNode.id);
            path.push(currentNode);
            
            // Find outgoing edges
            const outgoingEdges = this.graphViz.filteredGraph.edges.filter(e => 
                (e.source.id || e.source) === currentNode.id
            );
            
            if (outgoingEdges.length === 0) {
                // End of path
                if (path.length > 1) {
                    paths.push({
                        nodes: [...path],
                        strength: strength / path.length
                    });
                }
            } else {
                outgoingEdges.forEach(edge => {
                    const targetNode = this.graphViz.filteredGraph.nodes.find(n => 
                        n.id === (edge.target.id || edge.target)
                    );
                    if (targetNode) {
                        dfs(targetNode, path, strength + Math.abs(edge.weight));
                    }
                });
            }
            
            path.pop();
            visited.delete(currentNode.id);
        };
        
        dfs(startNode, [], 0);
        return paths;
    }
    
    highlightPath(nodeList) {
        // Clear previous highlights
        this.graphViz.clearHighlights();
        
        // Highlight nodes in path
        nodeList.forEach(node => {
            this.graphViz.g.selectAll('.node')
                .filter(d => d.id === node.id)
                .classed('highlighted', true);
        });
        
        // Highlight edges in path
        for (let i = 0; i < nodeList.length - 1; i++) {
            const source = nodeList[i].id;
            const target = nodeList[i + 1].id;
            
            this.graphViz.g.selectAll('.edge')
                .filter(d => 
                    (d.source.id || d.source) === source && 
                    (d.target.id || d.target) === target
                )
                .classed('circuit', true);
        }
    }
    
    highlightNodeCircuit(node) {
        // Find circuits containing this node
        const containingCircuits = this.graphViz.graph.circuits.filter(circuit => 
            circuit.path.includes(node.id)
        );
        
        if (containingCircuits.length > 0) {
            this.graphViz.highlightCircuit(containingCircuits[0].name);
        } else {
            // Highlight immediate neighbors
            this.highlightNeighbors(node);
        }
    }
    
    highlightNeighbors(node) {
        this.graphViz.clearHighlights();
        
        // Highlight the node itself
        this.graphViz.g.selectAll('.node')
            .filter(d => d.id === node.id)
            .classed('highlighted', true);
        
        // Find and highlight connected nodes
        const connectedNodes = new Set();
        
        this.graphViz.filteredGraph.edges.forEach(edge => {
            const sourceId = edge.source.id || edge.source;
            const targetId = edge.target.id || edge.target;
            
            if (sourceId === node.id) {
                connectedNodes.add(targetId);
            } else if (targetId === node.id) {
                connectedNodes.add(sourceId);
            }
        });
        
        // Highlight connected nodes
        this.graphViz.g.selectAll('.node')
            .filter(d => connectedNodes.has(d.id))
            .classed('highlighted', true);
        
        // Highlight connecting edges
        this.graphViz.g.selectAll('.edge')
            .filter(d => {
                const sourceId = d.source.id || d.source;
                const targetId = d.target.id || d.target;
                return sourceId === node.id || targetId === node.id;
            })
            .classed('circuit', true);
    }
    
    hideNode(node) {
        // Remove node from filtered graph
        const nodeIndex = this.graphViz.filteredGraph.nodes.findIndex(n => n.id === node.id);
        if (nodeIndex !== -1) {
            this.graphViz.filteredGraph.nodes.splice(nodeIndex, 1);
        }
        
        // Remove edges connected to this node
        this.graphViz.filteredGraph.edges = this.graphViz.filteredGraph.edges.filter(edge => {
            const sourceId = edge.source.id || edge.source;
            const targetId = edge.target.id || edge.target;
            return sourceId !== node.id && targetId !== node.id;
        });
        
        // Update visualization
        this.graphViz.updateVisualization();
        this.updateVisibleNodes();
    }
    
    exportNodeInfo(node) {
        const info = {
            id: node.id,
            type: node.type,
            layer: node.layer,
            activation: node.activation,
            metadata: node.metadata,
            connections: {
                incoming: this.graphViz.filteredGraph.edges.filter(e => 
                    (e.target.id || e.target) === node.id
                ).length,
                outgoing: this.graphViz.filteredGraph.edges.filter(e => 
                    (e.source.id || e.source) === node.id
                ).length
            }
        };
        
        const blob = new Blob([JSON.stringify(info, null, 2)], { 
            type: 'application/json' 
        });
        const url = URL.createObjectURL(blob);
        
        const a = document.createElement('a');
        a.href = url;
        a.download = `node_${node.id}_info.json`;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }
    
    // Demo data loader for testing
    loadDemoData() {
        const demoData = {
            nodes: [
                { id: 'token_0', type: 'token', activation: 1.0, layer: 0, 
                  metadata: { token: 'Roses', interpretation: 'Input token' }},
                { id: 'feature_42', type: 'feature', activation: 0.85, layer: 6,
                  metadata: { interpretation: 'Plans rhyming patterns', feature_index: 42 }},
                { id: 'feature_67', type: 'feature', activation: 0.72, layer: 7,
                  metadata: { interpretation: 'Detects rhyme potential', feature_index: 67 }},
                { id: 'logit_blue', type: 'logit', activation: 0.91, layer: 8,
                  metadata: { token: 'blue', interpretation: 'Output prediction' }}
            ],
            edges: [
                { source: 'token_0', target: 'feature_42', weight: 0.75 },
                { source: 'feature_42', target: 'feature_67', weight: 0.65 },
                { source: 'feature_67', target: 'logit_blue', weight: 0.78 }
            ],
            circuits: [
                { 
                    name: 'Poetry Planning Circuit',
                    path: ['token_0', 'feature_42', 'feature_67', 'logit_blue'],
                    strength: 0.78,
                    type: 'discovered'
                }
            ],
            metadata: {
                method: 'Jacobian Attribution',
                node_count: 4,
                edge_count: 3
            }
        };
        
        this.graphViz.loadGraph(demoData);
        this.updateGraphInfo('demo-poetry-circuit.json', demoData);
    }
}