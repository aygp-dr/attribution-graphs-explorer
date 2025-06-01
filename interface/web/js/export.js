// Export functionality for Attribution Graph Explorer

class GraphExporter {
    constructor(graphViz) {
        this.graphViz = graphViz;
        this.setupExportHandlers();
    }
    
    setupExportHandlers() {
        // SVG export
        document.getElementById('export-svg-btn').addEventListener('click', () => {
            this.exportSVG();
        });
        
        // JSON export
        document.getElementById('export-json-btn').addEventListener('click', () => {
            this.exportJSON();
        });
        
        // Screenshot
        document.getElementById('screenshot-btn').addEventListener('click', () => {
            this.takeScreenshot();
        });
        
        // Generate report
        document.getElementById('generate-report').addEventListener('click', () => {
            this.generateReport();
        });
        
        // Header export button
        document.getElementById('export-svg').addEventListener('click', () => {
            this.exportSVG();
        });
    }
    
    exportSVG() {
        try {
            const svg = this.graphViz.svg.node();
            const serializer = new XMLSerializer();
            let svgString = serializer.serializeToString(svg);
            
            // Add CSS styles inline
            svgString = this.inlineStyles(svgString);
            
            // Create blob and download
            const blob = new Blob([svgString], { type: 'image/svg+xml' });
            this.downloadBlob(blob, 'attribution-graph.svg');
            
        } catch (error) {
            console.error('SVG export error:', error);
            alert('Error exporting SVG: ' + error.message);
        }
    }
    
    inlineStyles(svgString) {
        // Add embedded CSS styles for proper rendering
        const styles = `
        <style>
        .node.feature { fill: #f59e0b; stroke: #d97706; }
        .node.token { fill: #10b981; stroke: #059669; }
        .node.logit { fill: #3b82f6; stroke: #2563eb; }
        .node.error { fill: #ef4444; stroke: #dc2626; }
        .node.selected { stroke-width: 4px; filter: drop-shadow(0 0 6px rgba(59, 130, 246, 0.6)); }
        .node.highlighted { stroke-width: 3px; filter: drop-shadow(0 0 4px rgba(245, 158, 11, 0.6)); }
        .edge { stroke: #64748b; stroke-width: 1px; fill: none; }
        .edge.strong { stroke-width: 3px; stroke: #2563eb; }
        .edge.medium { stroke-width: 2px; stroke: #64748b; }
        .edge.weak { stroke-width: 1px; stroke: #cbd5e1; stroke-dasharray: 3,3; }
        .edge.selected { stroke: #f59e0b; stroke-width: 4px; }
        .edge.circuit { stroke: #10b981; stroke-width: 3px; }
        .node-label { font-size: 10px; fill: #1e293b; text-anchor: middle; font-family: Arial, sans-serif; }
        </style>
        `;
        
        // Insert styles after the opening <svg> tag
        return svgString.replace('<svg', styles + '<svg');
    }
    
    exportJSON() {
        try {
            const exportData = {
                graph: this.graphViz.graph,
                filtered: this.graphViz.filteredGraph,
                filters: this.graphViz.filters,
                selection: {
                    selectedNode: this.graphViz.selectedNode?.id || null,
                    selectedEdge: this.graphViz.selectedEdge?.id || null,
                    highlightedCircuit: this.graphViz.highlightedCircuit?.name || null
                },
                export_metadata: {
                    timestamp: new Date().toISOString(),
                    tool: 'Attribution Graph Explorer',
                    version: '1.0.0'
                }
            };
            
            const jsonString = JSON.stringify(exportData, null, 2);
            const blob = new Blob([jsonString], { type: 'application/json' });
            this.downloadBlob(blob, 'attribution-graph-export.json');
            
        } catch (error) {
            console.error('JSON export error:', error);
            alert('Error exporting JSON: ' + error.message);
        }
    }
    
    takeScreenshot() {
        try {
            // Use html2canvas if available, otherwise provide fallback
            if (typeof html2canvas !== 'undefined') {
                const graphContainer = document.querySelector('.graph-container');
                
                html2canvas(graphContainer, {
                    backgroundColor: '#ffffff',
                    scale: 2 // Higher resolution
                }).then(canvas => {
                    canvas.toBlob(blob => {
                        this.downloadBlob(blob, 'attribution-graph-screenshot.png');
                    });
                });
            } else {
                // Fallback: Convert SVG to PNG using canvas
                this.svgToCanvas();
            }
            
        } catch (error) {
            console.error('Screenshot error:', error);
            alert('Error taking screenshot: ' + error.message);
        }
    }
    
    svgToCanvas() {
        const svg = this.graphViz.svg.node();
        const bbox = svg.getBBox();
        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        
        // Set canvas size
        canvas.width = bbox.width + 40;
        canvas.height = bbox.height + 40;
        
        // White background
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        
        // Convert SVG to image
        const svgData = new XMLSerializer().serializeToString(svg);
        const svgBlob = new Blob([this.inlineStyles(svgData)], { type: 'image/svg+xml' });
        const url = URL.createObjectURL(svgBlob);
        
        const img = new Image();
        img.onload = () => {
            ctx.drawImage(img, 20, 20);
            canvas.toBlob(blob => {
                this.downloadBlob(blob, 'attribution-graph-screenshot.png');
            });
            URL.revokeObjectURL(url);
        };
        img.src = url;
    }
    
    generateReport() {
        try {
            const report = this.createAnalysisReport();
            const blob = new Blob([report], { type: 'text/markdown' });
            this.downloadBlob(blob, 'attribution-graph-analysis.md');
            
        } catch (error) {
            console.error('Report generation error:', error);
            alert('Error generating report: ' + error.message);
        }
    }
    
    createAnalysisReport() {
        const graph = this.graphViz.graph;
        const filtered = this.graphViz.filteredGraph;
        const timestamp = new Date().toLocaleString();
        
        // Calculate statistics
        const stats = this.calculateGraphStatistics();
        
        const report = `# Attribution Graph Analysis Report

*Generated on ${timestamp}*

## Graph Overview

- **Total Nodes:** ${graph.nodes.length}
- **Total Edges:** ${graph.edges.length}
- **Visible Nodes:** ${filtered.nodes.length}
- **Visible Edges:** ${filtered.edges.length}
- **Circuits Identified:** ${graph.circuits.length}

## Filter Settings

- **Attribution Threshold:** ${this.graphViz.filters.threshold}
- **Layer Range:** ${this.graphViz.filters.layerMin} to ${this.graphViz.filters.layerMax}

## Node Analysis

### By Type
${this.getNodeTypeBreakdown()}

### By Layer
${this.getLayerBreakdown()}

## Edge Analysis

### Attribution Strength Distribution
- **Strong (>0.7):** ${stats.strongEdges} edges
- **Medium (0.3-0.7):** ${stats.mediumEdges} edges  
- **Weak (<0.3):** ${stats.weakEdges} edges

### Attribution Range
- **Maximum:** ${stats.maxAttribution.toFixed(3)}
- **Minimum:** ${stats.minAttribution.toFixed(3)}
- **Average:** ${stats.avgAttribution.toFixed(3)}

## Circuit Analysis

${this.getCircuitAnalysis()}

## Key Findings

${this.generateKeyFindings()}

## Methodology

This analysis was generated using the Attribution Graph Explorer interface, which visualizes computational circuits in transformer models using attribution-based methods. The underlying attribution computation uses Jacobian-based techniques to trace information flow through the network.

## Appendix

### Selected Nodes
${this.getSelectedNodeInfo()}

### Export Configuration
- Tool: Attribution Graph Explorer v1.0.0
- Export Format: Markdown Report
- Timestamp: ${timestamp}
`;

        return report;
    }
    
    calculateGraphStatistics() {
        const edges = this.graphViz.filteredGraph.edges;
        const weights = edges.map(e => Math.abs(e.weight));
        
        return {
            strongEdges: edges.filter(e => Math.abs(e.weight) > 0.7).length,
            mediumEdges: edges.filter(e => Math.abs(e.weight) > 0.3 && Math.abs(e.weight) <= 0.7).length,
            weakEdges: edges.filter(e => Math.abs(e.weight) <= 0.3).length,
            maxAttribution: weights.length > 0 ? Math.max(...weights) : 0,
            minAttribution: weights.length > 0 ? Math.min(...weights) : 0,
            avgAttribution: weights.length > 0 ? weights.reduce((a, b) => a + b, 0) / weights.length : 0
        };
    }
    
    getNodeTypeBreakdown() {
        const nodes = this.graphViz.filteredGraph.nodes;
        const types = {};
        
        nodes.forEach(node => {
            types[node.type] = (types[node.type] || 0) + 1;
        });
        
        return Object.entries(types)
            .map(([type, count]) => `- **${type}:** ${count} nodes`)
            .join('\n');
    }
    
    getLayerBreakdown() {
        const nodes = this.graphViz.filteredGraph.nodes;
        const layers = {};
        
        nodes.forEach(node => {
            layers[node.layer] = (layers[node.layer] || 0) + 1;
        });
        
        return Object.entries(layers)
            .sort(([a], [b]) => parseInt(a) - parseInt(b))
            .map(([layer, count]) => `- **Layer ${layer}:** ${count} nodes`)
            .join('\n');
    }
    
    getCircuitAnalysis() {
        const circuits = this.graphViz.graph.circuits;
        
        if (circuits.length === 0) {
            return "No circuits identified in the current graph.";
        }
        
        return circuits.map(circuit => `
### ${circuit.name}
- **Path Length:** ${circuit.path ? circuit.path.length : 'Unknown'} nodes
- **Strength:** ${circuit.strength || 'Unknown'}
- **Type:** ${circuit.type || 'Unknown'}
`).join('\n');
    }
    
    generateKeyFindings() {
        const findings = [];
        const stats = this.calculateGraphStatistics();
        
        // Attribution strength findings
        if (stats.strongEdges > 0) {
            findings.push(`- Found ${stats.strongEdges} strong attribution connections (>0.7)`);
        }
        
        // Circuit findings
        const circuits = this.graphViz.graph.circuits;
        if (circuits.length > 0) {
            findings.push(`- Identified ${circuits.length} computational circuit(s)`);
        }
        
        // Layer distribution
        const layers = new Set(this.graphViz.filteredGraph.nodes.map(n => n.layer));
        findings.push(`- Information flows across ${layers.size} layer(s)`);
        
        // Node type analysis
        const tokenNodes = this.graphViz.filteredGraph.nodes.filter(n => n.type === 'token');
        const featureNodes = this.graphViz.filteredGraph.nodes.filter(n => n.type === 'feature');
        const logitNodes = this.graphViz.filteredGraph.nodes.filter(n => n.type === 'logit');
        
        if (featureNodes.length > 0) {
            findings.push(`- ${featureNodes.length} intermediate feature(s) mediate the computation`);
        }
        
        if (findings.length === 0) {
            findings.push("- Graph requires further analysis to identify key patterns");
        }
        
        return findings.join('\n');
    }
    
    getSelectedNodeInfo() {
        if (this.graphViz.selectedNode) {
            const node = this.graphViz.selectedNode;
            return `
**Selected Node:** ${node.id}
- Type: ${node.type}
- Layer: ${node.layer}
- Activation: ${node.activation.toFixed(3)}
${node.metadata?.interpretation ? `- Interpretation: ${node.metadata.interpretation}` : ''}
`;
        }
        
        return "No node currently selected.";
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
    
    // Export current view configuration
    exportViewConfiguration() {
        const config = {
            filters: this.graphViz.filters,
            layout: document.querySelector('input[name="layout"]:checked')?.value || 'force',
            zoom: this.graphViz.zoom.transform(),
            selectedNode: this.graphViz.selectedNode?.id || null,
            highlightedCircuit: this.graphViz.highlightedCircuit?.name || null,
            timestamp: new Date().toISOString()
        };
        
        const blob = new Blob([JSON.stringify(config, null, 2)], { 
            type: 'application/json' 
        });
        this.downloadBlob(blob, 'graph-view-config.json');
    }
    
    // Import view configuration
    importViewConfiguration(configFile) {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const config = JSON.parse(e.target.result);
                this.applyViewConfiguration(config);
            } catch (error) {
                alert('Error loading view configuration: ' + error.message);
            }
        };
        reader.readAsText(configFile);
    }
    
    applyViewConfiguration(config) {
        // Apply filters
        if (config.filters) {
            if (config.filters.threshold !== undefined) {
                document.getElementById('threshold-slider').value = config.filters.threshold;
                document.getElementById('threshold-value').textContent = config.filters.threshold;
                this.graphViz.setThreshold(config.filters.threshold);
            }
            
            if (config.filters.layerMin !== undefined && config.filters.layerMax !== undefined) {
                document.getElementById('layer-min').value = config.filters.layerMin;
                document.getElementById('layer-max').value = config.filters.layerMax;
                document.getElementById('layer-min-val').textContent = config.filters.layerMin;
                document.getElementById('layer-max-val').textContent = config.filters.layerMax;
                this.graphViz.setLayerRange(config.filters.layerMin, config.filters.layerMax);
            }
        }
        
        // Apply layout
        if (config.layout) {
            const layoutRadio = document.querySelector(`input[name="layout"][value="${config.layout}"]`);
            if (layoutRadio) {
                layoutRadio.checked = true;
                this.graphViz.applyLayout(config.layout);
            }
        }
        
        // Apply zoom and pan
        if (config.zoom) {
            this.graphViz.svg.call(this.graphViz.zoom.transform, config.zoom);
        }
        
        // Highlight circuit
        if (config.highlightedCircuit) {
            this.graphViz.highlightCircuit(config.highlightedCircuit);
        }
    }
}