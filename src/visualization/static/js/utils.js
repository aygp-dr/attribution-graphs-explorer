// Attribution Graphs Explorer - Utility Functions

class AttributionGraphUtils {
  /**
   * Debounce function to limit frequent calls
   */
  static debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
      const later = () => {
        clearTimeout(timeout);
        func(...args);
      };
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
    };
  }

  /**
   * Throttle function to limit call frequency
   */
  static throttle(func, limit) {
    let inThrottle;
    return function() {
      const args = arguments;
      const context = this;
      if (!inThrottle) {
        func.apply(context, args);
        inThrottle = true;
        setTimeout(() => inThrottle = false, limit);
      }
    };
  }

  /**
   * Format numbers for display
   */
  static formatNumber(num, decimals = 3) {
    if (typeof num !== 'number') return 'N/A';
    if (Math.abs(num) < 0.001) return '< 0.001';
    return num.toFixed(decimals);
  }

  /**
   * Calculate color based on attribution strength
   */
  static getAttributionColor(weight, minWeight = -1, maxWeight = 1) {
    const normalized = Math.abs(weight) / Math.max(Math.abs(minWeight), Math.abs(maxWeight));
    const intensity = Math.min(normalized, 1);
    
    if (weight >= 0) {
      // Positive attribution: blue to red gradient
      const red = Math.floor(intensity * 220 + 35);
      const green = Math.floor((1 - intensity) * 180 + 75);
      const blue = Math.floor((1 - intensity) * 220 + 35);
      return `rgb(${red}, ${green}, ${blue})`;
    } else {
      // Negative attribution: blue gradient
      const blue = Math.floor(intensity * 180 + 75);
      return `rgb(75, 75, ${blue})`;
    }
  }

  /**
   * Get edge width based on attribution strength
   */
  static getEdgeWidth(weight, minWeight = -1, maxWeight = 1) {
    const normalized = Math.abs(weight) / Math.max(Math.abs(minWeight), Math.abs(maxWeight));
    return Math.max(1, Math.min(8, normalized * 8));
  }

  /**
   * Classify edge strength
   */
  static classifyEdgeStrength(weight) {
    const absWeight = Math.abs(weight);
    if (absWeight >= 0.8) return 'very-strong';
    if (absWeight >= 0.6) return 'strong';
    if (absWeight >= 0.4) return 'medium';
    if (absWeight >= 0.2) return 'weak';
    return 'very-weak';
  }

  /**
   * Get node color based on type
   */
  static getNodeColor(nodeType) {
    const colors = {
      'feature': '#3498db',
      'token': '#2ecc71',
      'logit': '#9b59b6',
      'error': '#e74c3c'
    };
    return colors[nodeType] || '#95a5a6';
  }

  /**
   * Get node shape based on type
   */
  static getNodeShape(nodeType) {
    const shapes = {
      'feature': 'ellipse',
      'token': 'rectangle',
      'logit': 'diamond',
      'error': 'triangle'
    };
    return shapes[nodeType] || 'ellipse';
  }

  /**
   * Create tooltip content for node
   */
  static createNodeTooltip(node) {
    return `
      <div class="tooltip">
        <strong>ID:</strong> ${node.id}<br>
        <strong>Type:</strong> ${node.type}<br>
        <strong>Activation:</strong> ${this.formatNumber(node.activation)}<br>
        <strong>Label:</strong> ${node.label || 'N/A'}
      </div>
    `;
  }

  /**
   * Create tooltip content for edge
   */
  static createEdgeTooltip(edge) {
    return `
      <div class="tooltip">
        <strong>From:</strong> ${edge.source}<br>
        <strong>To:</strong> ${edge.target}<br>
        <strong>Weight:</strong> ${this.formatNumber(edge.weight)}<br>
        <strong>Strength:</strong> ${edge.strength || this.classifyEdgeStrength(edge.weight)}
      </div>
    `;
  }

  /**
   * Filter graph data based on criteria
   */
  static filterGraph(graphData, filters) {
    const filteredNodes = graphData.nodes.filter(node => {
      // Type filter
      if (filters.nodeTypes && !filters.nodeTypes.includes(node.type)) {
        return false;
      }
      
      // Layer filter (if applicable)
      if (filters.layerRange && node.metadata && node.metadata.layer !== undefined) {
        const layer = node.metadata.layer;
        if (layer < filters.layerRange.min || layer > filters.layerRange.max) {
          return false;
        }
      }
      
      return true;
    });

    const nodeIds = new Set(filteredNodes.map(n => n.id));
    const filteredEdges = graphData.edges.filter(edge => {
      // Node connectivity filter
      if (!nodeIds.has(edge.source) || !nodeIds.has(edge.target)) {
        return false;
      }
      
      // Attribution threshold filter
      if (filters.threshold !== undefined && Math.abs(edge.weight) < filters.threshold) {
        return false;
      }
      
      return true;
    });

    return {
      nodes: filteredNodes,
      edges: filteredEdges,
      metadata: {
        ...graphData.metadata,
        filtered: true,
        originalNodeCount: graphData.nodes.length,
        originalEdgeCount: graphData.edges.length,
        filteredNodeCount: filteredNodes.length,
        filteredEdgeCount: filteredEdges.length
      }
    };
  }

  /**
   * Calculate graph statistics
   */
  static calculateGraphStats(graphData) {
    const nodesByType = {};
    const edgesByStrength = {};
    let maxWeight = 0;
    let minWeight = 0;

    // Analyze nodes
    graphData.nodes.forEach(node => {
      nodesByType[node.type] = (nodesByType[node.type] || 0) + 1;
    });

    // Analyze edges
    graphData.edges.forEach(edge => {
      const strength = this.classifyEdgeStrength(edge.weight);
      edgesByStrength[strength] = (edgesByStrength[strength] || 0) + 1;
      maxWeight = Math.max(maxWeight, edge.weight);
      minWeight = Math.min(minWeight, edge.weight);
    });

    return {
      nodeCount: graphData.nodes.length,
      edgeCount: graphData.edges.length,
      nodesByType,
      edgesByStrength,
      maxWeight,
      minWeight,
      averageWeight: graphData.edges.reduce((sum, edge) => sum + Math.abs(edge.weight), 0) / graphData.edges.length
    };
  }

  /**
   * Find paths between nodes
   */
  static findPaths(graphData, sourceId, targetId, maxLength = 10) {
    const adjacencyList = new Map();
    
    // Build adjacency list
    graphData.edges.forEach(edge => {
      if (!adjacencyList.has(edge.source)) {
        adjacencyList.set(edge.source, []);
      }
      adjacencyList.get(edge.source).push({
        target: edge.target,
        weight: edge.weight
      });
    });

    // BFS to find paths
    const paths = [];
    const queue = [[sourceId]];
    
    while (queue.length > 0 && paths.length < 100) { // Limit to 100 paths
      const currentPath = queue.shift();
      const currentNode = currentPath[currentPath.length - 1];
      
      if (currentPath.length > maxLength) continue;
      
      if (currentNode === targetId) {
        paths.push(currentPath);
        continue;
      }
      
      const neighbors = adjacencyList.get(currentNode) || [];
      neighbors.forEach(neighbor => {
        if (!currentPath.includes(neighbor.target)) {
          queue.push([...currentPath, neighbor.target]);
        }
      });
    }
    
    return paths;
  }

  /**
   * Extract subgraph around a node
   */
  static extractSubgraph(graphData, nodeId, radius = 2) {
    const connectedNodes = new Set([nodeId]);
    const connectedEdges = [];
    
    // Find nodes within radius
    for (let r = 0; r < radius; r++) {
      const currentNodes = Array.from(connectedNodes);
      graphData.edges.forEach(edge => {
        if (currentNodes.includes(edge.source)) {
          connectedNodes.add(edge.target);
          if (!connectedEdges.find(e => e.source === edge.source && e.target === edge.target)) {
            connectedEdges.push(edge);
          }
        }
        if (currentNodes.includes(edge.target)) {
          connectedNodes.add(edge.source);
          if (!connectedEdges.find(e => e.source === edge.source && e.target === edge.target)) {
            connectedEdges.push(edge);
          }
        }
      });
    }
    
    // Filter nodes and edges
    const subgraphNodes = graphData.nodes.filter(node => connectedNodes.has(node.id));
    
    return {
      nodes: subgraphNodes,
      edges: connectedEdges,
      metadata: {
        ...graphData.metadata,
        subgraph: true,
        centerNode: nodeId,
        radius: radius
      }
    };
  }

  /**
   * Export graph data in various formats
   */
  static exportGraphData(graphData, format = 'json') {
    switch (format) {
      case 'json':
        return JSON.stringify(graphData, null, 2);
      
      case 'csv-nodes':
        const nodeHeaders = ['id', 'type', 'activation', 'label'];
        const nodeRows = graphData.nodes.map(node => 
          nodeHeaders.map(header => node[header] || '').join(',')
        );
        return [nodeHeaders.join(','), ...nodeRows].join('\n');
      
      case 'csv-edges':
        const edgeHeaders = ['source', 'target', 'weight', 'strength'];
        const edgeRows = graphData.edges.map(edge => 
          edgeHeaders.map(header => edge[header] || '').join(',')
        );
        return [edgeHeaders.join(','), ...edgeRows].join('\n');
      
      case 'mermaid':
        return this.exportToMermaid(graphData);
      
      default:
        throw new Error(`Unsupported export format: ${format}`);
    }
  }

  /**
   * Export to Mermaid diagram format
   */
  static exportToMermaid(graphData) {
    let mermaid = 'graph TD\n';
    
    // Add nodes
    graphData.nodes.forEach(node => {
      const shape = node.type === 'token' ? '[]' : node.type === 'logit' ? '{}' : '()';
      mermaid += `    ${node.id}${shape[0]}${node.label || node.id}${shape[1]}\n`;
    });
    
    // Add edges
    graphData.edges.forEach(edge => {
      const weight = this.formatNumber(edge.weight, 2);
      mermaid += `    ${edge.source} -->|${weight}| ${edge.target}\n`;
    });
    
    // Add styling
    mermaid += '\n';
    const nodeTypes = [...new Set(graphData.nodes.map(n => n.type))];
    nodeTypes.forEach((type, index) => {
      const color = this.getNodeColor(type);
      mermaid += `    style ${type} fill:${color}\n`;
    });
    
    return mermaid;
  }

  /**
   * Load data from URL or file
   */
  static async loadData(source) {
    try {
      if (typeof source === 'string') {
        // URL or file path
        const response = await fetch(source);
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return await response.json();
      } else if (source instanceof File) {
        // File object
        return new Promise((resolve, reject) => {
          const reader = new FileReader();
          reader.onload = e => {
            try {
              resolve(JSON.parse(e.target.result));
            } catch (error) {
              reject(error);
            }
          };
          reader.onerror = () => reject(new Error('File reading failed'));
          reader.readAsText(source);
        });
      } else {
        // Direct object
        return source;
      }
    } catch (error) {
      console.error('Error loading data:', error);
      throw error;
    }
  }

  /**
   * Validate graph data structure
   */
  static validateGraphData(data) {
    const errors = [];
    
    if (!data || typeof data !== 'object') {
      errors.push('Graph data must be an object');
      return errors;
    }
    
    if (!Array.isArray(data.nodes)) {
      errors.push('Graph data must have a nodes array');
    } else {
      data.nodes.forEach((node, index) => {
        if (!node.id) errors.push(`Node ${index} missing required id field`);
        if (!node.type) errors.push(`Node ${index} missing required type field`);
        if (typeof node.activation !== 'number') {
          errors.push(`Node ${index} activation must be a number`);
        }
      });
    }
    
    if (!Array.isArray(data.edges)) {
      errors.push('Graph data must have an edges array');
    } else {
      const nodeIds = new Set(data.nodes?.map(n => n.id) || []);
      data.edges.forEach((edge, index) => {
        if (!edge.source) errors.push(`Edge ${index} missing required source field`);
        if (!edge.target) errors.push(`Edge ${index} missing required target field`);
        if (typeof edge.weight !== 'number') {
          errors.push(`Edge ${index} weight must be a number`);
        }
        if (!nodeIds.has(edge.source)) {
          errors.push(`Edge ${index} source node '${edge.source}' not found in nodes`);
        }
        if (!nodeIds.has(edge.target)) {
          errors.push(`Edge ${index} target node '${edge.target}' not found in nodes`);
        }
      });
    }
    
    return errors;
  }
}

// Make utilities globally available
window.AttributionGraphUtils = AttributionGraphUtils;