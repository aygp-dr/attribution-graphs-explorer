// Main application initialization and coordination

class AttributionGraphApp {
    constructor() {
        this.graphViz = null;
        this.controls = null;
        this.exporter = null;
        
        this.init();
    }
    
    init() {
        // Wait for DOM to be fully loaded
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', () => this.initializeApp());
        } else {
            this.initializeApp();
        }
    }
    
    initializeApp() {
        console.log('Initializing Attribution Graph Explorer...');
        
        try {
            // Initialize main components
            this.graphViz = new AttributionGraphViz('#graph-canvas');
            this.controls = new GraphControls(this.graphViz);
            this.exporter = new GraphExporter(this.graphViz);
            
            // Set up global references for console access
            window.graphViz = this.graphViz;
            window.graphControls = this.controls;
            window.graphExporter = this.exporter;
            
            // Initialize UI state
            this.initializeUI();
            
            // Load demo data for testing
            this.loadDemoIfRequested();
            
            console.log('Attribution Graph Explorer initialized successfully');
            
        } catch (error) {
            console.error('Failed to initialize Attribution Graph Explorer:', error);
            this.showErrorMessage('Failed to initialize the application. Please refresh and try again.');
        }
    }
    
    initializeUI() {
        // Update initial UI state
        document.getElementById('threshold-value').textContent = '0.10';
        document.getElementById('layer-min-val').textContent = '0';
        document.getElementById('layer-max-val').textContent = '10';
        document.getElementById('visible-nodes').textContent = '0';
        
        // Set initial graph info
        document.getElementById('graph-name').textContent = 'No graph loaded';
        document.getElementById('method-name').textContent = '';
        
        // Show initial instructions
        this.showWelcomeMessage();
    }
    
    showWelcomeMessage() {
        const inspector = document.getElementById('inspector-content');
        inspector.innerHTML = `
            <div class="welcome-message">
                <h3>Welcome to Attribution Graph Explorer</h3>
                <p>An interactive interface for exploring computational circuits in transformer models.</p>
                
                <div class="getting-started">
                    <h4>Getting Started</h4>
                    <ol>
                        <li>Click <strong>"Load Graph"</strong> to import an attribution graph JSON file</li>
                        <li>Use the <strong>filters</strong> to adjust what's visible</li>
                        <li>Click <strong>nodes and edges</strong> to inspect details</li>
                        <li>Explore <strong>circuits</strong> to understand information flow</li>
                    </ol>
                </div>
                
                <div class="demo-section">
                    <h4>Try the Demo</h4>
                    <p>Don't have a graph file? Try our demo:</p>
                    <button id="load-demo" class="btn">Load Demo Data</button>
                </div>
                
                <div class="help-section">
                    <h4>Controls</h4>
                    <ul>
                        <li><strong>Mouse:</strong> Click and drag to pan</li>
                        <li><strong>Scroll:</strong> Zoom in/out</li>
                        <li><strong>Click node:</strong> Select and inspect</li>
                        <li><strong>Right-click:</strong> Context menu</li>
                        <li><strong>Hover:</strong> Quick info tooltip</li>
                    </ul>
                </div>
            </div>
        `;
        
        // Add demo button handler
        const demoButton = document.getElementById('load-demo');
        if (demoButton) {
            demoButton.addEventListener('click', () => {
                this.controls.loadDemoData();
                this.showDemoLoadedMessage();
            });
        }
    }
    
    showDemoLoadedMessage() {
        const inspector = document.getElementById('inspector-content');
        inspector.innerHTML = `
            <div class="demo-loaded">
                <h3>Demo Loaded</h3>
                <p>You're now viewing a sample poetry planning circuit from a transformer model.</p>
                
                <div class="demo-features">
                    <h4>Try These Features</h4>
                    <ul>
                        <li>Click on <strong>Feature 42</strong> to see how it plans rhyming</li>
                        <li>Adjust the <strong>threshold slider</strong> to filter weak connections</li>
                        <li>Check the <strong>"Poetry Planning Circuit"</strong> to highlight the path</li>
                        <li>Try different <strong>layouts</strong> (Force, Hierarchical, Circular)</li>
                        <li>Use <strong>zoom controls</strong> or mouse wheel to navigate</li>
                        <li><strong>Right-click</strong> nodes for more options</li>
                    </ul>
                </div>
                
                <div class="demo-explanation">
                    <h4>What You're Seeing</h4>
                    <p>This demo shows how a language model plans rhyming in poetry. The path from "Roses" (token) through Feature 42 (planning) to Feature 67 (rhyme detection) to "blue" (logit) represents the model's computation for generating rhyming words.</p>
                </div>
            </div>
        `;
    }
    
    loadDemoIfRequested() {
        // Check URL parameters for demo mode
        const urlParams = new URLSearchParams(window.location.search);
        if (urlParams.get('demo') === 'true') {
            setTimeout(() => {
                this.controls.loadDemoData();
                this.showDemoLoadedMessage();
            }, 1000);
        }
    }
    
    showErrorMessage(message) {
        const inspector = document.getElementById('inspector-content');
        inspector.innerHTML = `
            <div class="error-message">
                <h3>Error</h3>
                <p>${message}</p>
                <button onclick="location.reload()" class="btn">Reload Page</button>
            </div>
        `;
    }
    
    // Keyboard shortcuts
    setupKeyboardShortcuts() {
        document.addEventListener('keydown', (e) => {
            // Only handle shortcuts when not in an input field
            if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') {
                return;
            }
            
            switch (e.key) {
                case 'Escape':
                    this.graphViz.clearSelection();
                    this.graphViz.clearHighlights();
                    this.graphViz.hideContextMenu();
                    break;
                    
                case 'f':
                    e.preventDefault();
                    this.graphViz.fitToView();
                    break;
                    
                case 'r':
                    e.preventDefault();
                    this.graphViz.resetView();
                    break;
                    
                case '+':
                case '=':
                    e.preventDefault();
                    this.graphViz.zoomIn();
                    break;
                    
                case '-':
                    e.preventDefault();
                    this.graphViz.zoomOut();
                    break;
                    
                case 'h':
                    e.preventDefault();
                    this.showKeyboardHelp();
                    break;
            }
        });
    }
    
    showKeyboardHelp() {
        const inspector = document.getElementById('inspector-content');
        inspector.innerHTML = `
            <div class="keyboard-help">
                <h3>Keyboard Shortcuts</h3>
                <div class="shortcut-list">
                    <div class="shortcut-item">
                        <kbd>Esc</kbd>
                        <span>Clear selection and highlights</span>
                    </div>
                    <div class="shortcut-item">
                        <kbd>F</kbd>
                        <span>Fit graph to view</span>
                    </div>
                    <div class="shortcut-item">
                        <kbd>R</kbd>
                        <span>Reset view</span>
                    </div>
                    <div class="shortcut-item">
                        <kbd>+</kbd>
                        <span>Zoom in</span>
                    </div>
                    <div class="shortcut-item">
                        <kbd>-</kbd>
                        <span>Zoom out</span>
                    </div>
                    <div class="shortcut-item">
                        <kbd>H</kbd>
                        <span>Show this help</span>
                    </div>
                </div>
                
                <div class="mouse-controls">
                    <h4>Mouse Controls</h4>
                    <div class="shortcut-item">
                        <span><strong>Click & Drag</strong></span>
                        <span>Pan the graph</span>
                    </div>
                    <div class="shortcut-item">
                        <span><strong>Mouse Wheel</strong></span>
                        <span>Zoom in/out</span>
                    </div>
                    <div class="shortcut-item">
                        <span><strong>Click Node</strong></span>
                        <span>Select and inspect</span>
                    </div>
                    <div class="shortcut-item">
                        <span><strong>Right Click</strong></span>
                        <span>Context menu</span>
                    </div>
                    <div class="shortcut-item">
                        <span><strong>Hover</strong></span>
                        <span>Show tooltip</span>
                    </div>
                </div>
            </div>
        `;
    }
    
    // Performance monitoring
    monitorPerformance() {
        let frameCount = 0;
        let lastTime = performance.now();
        
        const measureFPS = () => {
            frameCount++;
            const currentTime = performance.now();
            
            if (currentTime - lastTime >= 1000) {
                const fps = Math.round((frameCount * 1000) / (currentTime - lastTime));
                console.log(`Graph rendering FPS: ${fps}`);
                
                // Warn if performance is poor
                if (fps < 30 && this.graphViz.filteredGraph.nodes.length > 100) {
                    console.warn('Performance warning: Consider reducing node count or using simplified layout');
                }
                
                frameCount = 0;
                lastTime = currentTime;
            }
            
            requestAnimationFrame(measureFPS);
        };
        
        // Start monitoring if in development mode
        if (window.location.hostname === 'localhost' || window.location.search.includes('debug=true')) {
            measureFPS();
        }
    }
    
    // Handle browser compatibility
    checkBrowserCompatibility() {
        const features = {
            svg: !!document.createElementNS,
            d3: typeof d3 !== 'undefined',
            es6: typeof Map !== 'undefined',
            fileAPI: typeof FileReader !== 'undefined'
        };
        
        const missingFeatures = Object.entries(features)
            .filter(([feature, supported]) => !supported)
            .map(([feature]) => feature);
            
        if (missingFeatures.length > 0) {
            this.showErrorMessage(`Your browser is missing required features: ${missingFeatures.join(', ')}. Please use a modern browser.`);
            return false;
        }
        
        return true;
    }
}

// Additional CSS for welcome message and help
const additionalStyles = `
<style>
.welcome-message, .demo-loaded, .error-message, .keyboard-help {
    padding: 1rem;
    line-height: 1.6;
}

.welcome-message h3, .demo-loaded h3, .error-message h3, .keyboard-help h3 {
    color: var(--primary-color);
    margin-bottom: 1rem;
}

.welcome-message h4, .demo-loaded h4, .keyboard-help h4 {
    color: var(--text-color);
    margin: 1.5rem 0 0.5rem 0;
    font-size: 0.9rem;
    font-weight: 600;
}

.welcome-message ol, .demo-loaded ul {
    margin: 0.5rem 0 0 1.5rem;
    font-size: 0.875rem;
}

.welcome-message li, .demo-loaded li {
    margin: 0.25rem 0;
}

.demo-section, .help-section, .demo-features, .demo-explanation {
    margin-top: 1.5rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border-color);
}

.shortcut-list, .mouse-controls {
    margin: 1rem 0;
}

.shortcut-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.25rem 0;
    font-size: 0.875rem;
}

kbd {
    background: var(--bg-color);
    border: 1px solid var(--border-color);
    border-radius: 3px;
    padding: 0.2rem 0.4rem;
    font-family: monospace;
    font-size: 0.8rem;
}

.error-message {
    text-align: center;
    color: var(--error-color);
}

.demo-explanation {
    background: var(--bg-color);
    padding: 1rem;
    border-radius: 6px;
    font-size: 0.875rem;
    font-style: italic;
}
</style>
`;

// Add styles to document head
document.head.insertAdjacentHTML('beforeend', additionalStyles);

// Initialize the application
const app = new AttributionGraphApp();

// Export for console access
window.app = app;