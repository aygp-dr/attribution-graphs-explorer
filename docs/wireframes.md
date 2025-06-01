# Interactive Visualization Interface Wireframes

## Main Interface Layout

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Attribution Graphs Explorer - Interactive Visualization                │
├─────────────────────────────────────────────────────────────────────────┤
│ [File] [View] [Analysis] [Export] [Help]                              │
├──────────────┬──────────────────────────────────────┬─────────────────┤
│              │                                      │                 │
│  Control     │                                      │   Information   │
│  Panel       │            Graph Canvas              │   Panel         │
│              │                                      │                 │
│ ┌──────────┐ │  ┌─────────────────────────────────┐ │ ┌─────────────┐ │
│ │Filtering │ │  │                                 │ │ │ Node Detail │ │
│ │          │ │  │     ● ──→ ● ──→ ●              │ │ │             │ │
│ │Threshold │ │  │     │      │      │              │ │ │ ID: feat_42 │ │
│ │[====●  ] │ │  │     ▼      ▼      ▼              │ │ │ Type: feature│ │
│ │          │ │  │     ● ──→ ● ──→ ●              │ │ │ Activation: │ │
│ │Layers    │ │  │               ▲                  │ │ │   0.734     │ │
│ │☑ 0-5     │ │  │               │                  │ │ │             │ │
│ │☑ 6-11    │ │  │               ●                  │ │ │ Connected:  │ │
│ │☐ 12-17   │ │  │                                 │ │ │ • feat_23   │ │
│ │          │ │  │         [Minimap]               │ │ │ • feat_67   │ │
│ │Node Types│ │  │                                 │ │ │ • logit_5   │ │
│ │☑ Features│ │  └─────────────────────────────────┘ │ │             │ │
│ │☑ Tokens  │ │                                      │ └─────────────┘ │
│ │☑ Logits  │ │                                      │                 │
│ │          │ │                                      │ ┌─────────────┐ │
│ │Circuits  │ │                                      │ │ Statistics  │ │
│ │○ All     │ │                                      │ │             │ │
│ │○ Circuit1│ │                                      │ │ Nodes: 247  │ │
│ │○ Circuit2│ │                                      │ │ Edges: 892  │ │
│ │          │ │                                      │ │ Circuits: 3 │ │
│ │[Discover]│ │                                      │ │ Max Path: 7 │ │
│ └──────────┘ │                                      │ │             │ │
│              │                                      │ └─────────────┘ │
└──────────────┴──────────────────────────────────────┴─────────────────┘
│ Status: Ready │ Circuit highlighted: reasoning-path │ Zoom: 100%      │
└─────────────────────────────────────────────────────────────────────────┘
```

## Graph Canvas Interactions

### Node Visualization States

```
Default Node        Selected Node        Highlighted Node     Filtered Node
    ●                   ●●●                    ●               ○ (faded)
  Feature            (glowing)             (bright red)      (low opacity)

  Token               Logit                 Error Node
    □                   ◇                     ⚠
(rectangle)         (diamond)             (warning icon)
```

### Edge Visualization

```
Strong Attribution    Medium Attribution    Weak Attribution     Circuit Path
       ═════                ───                 ∙∙∙              ═══►
    (thick line)         (normal)            (dotted)         (colored arrow)
    
Attribution Weight Encoding:
│ ─────────────────────────────────────────────── │
│ 0.0    0.2    0.4    0.6    0.8    1.0         │
│ ∙∙∙     ──     ──     ══     ██     ████        │
│ (increasing thickness and opacity)              │
```

## Control Panel Details

### Filtering Section
```
┌─────────────────────────┐
│ Filtering               │
├─────────────────────────┤
│ Attribution Threshold   │
│ ┌─────────────────────┐ │
│ │ [====●──────] 0.35  │ │
│ └─────────────────────┘ │
│                         │
│ Layer Range             │
│ From: [0  ▼] To: [12▼]  │
│                         │
│ Node Types              │
│ ☑ Features              │
│ ☑ Tokens                │
│ ☑ Logits                │
│ ☐ Error Terms           │
│                         │
│ [Apply] [Reset]         │
└─────────────────────────┘
```

### Circuit Discovery Section
```
┌─────────────────────────┐
│ Circuit Analysis        │
├─────────────────────────┤
│ Active Circuits:        │
│ ○ Show All              │
│ ● reasoning-path        │
│ ○ attention-circuit     │
│ ○ factual-recall        │
│                         │
│ [Discover New...]       │
│ [Compare Circuits...]   │
│                         │
│ Circuit Metrics:        │
│ • Length: 5 hops        │
│ • Strength: 0.742       │
│ • Coverage: 23 nodes    │
└─────────────────────────┘
```

## Information Panel Details

### Node Detail View
```
┌─────────────────────────┐
│ Node Details            │
├─────────────────────────┤
│ ID: feature_layer5_42   │
│ Type: Feature           │
│ Layer: 5                │
│ Activation: 0.734       │
│                         │
│ Interpretation:         │
│ "Entity recognition -   │
│  proper nouns"          │
│                         │
│ Connections:            │
│ Incoming (3):           │
│ • tok_"John" (0.92)     │
│ • feat_lay4_23 (0.67)   │
│ • feat_lay3_15 (0.45)   │
│                         │
│ Outgoing (2):           │
│ • feat_lay6_78 (0.81)   │
│ • logit_"person" (0.34) │
│                         │
│ [Trace Paths]           │
│ [Add to Circuit]        │
└─────────────────────────┘
```

### Statistics Panel
```
┌─────────────────────────┐
│ Graph Statistics        │
├─────────────────────────┤
│ Current View:           │
│ • Visible Nodes: 89     │
│ • Visible Edges: 342    │
│ • Hidden by Filter: 158 │
│                         │
│ Full Graph:             │
│ • Total Nodes: 247      │
│ • Total Edges: 892      │
│ • Layers: 0-12          │
│ • Max Attribution: 0.97 │
│                         │
│ Active Circuit:         │
│ • Nodes in Path: 7      │
│ • Total Attribution: 2.1│
│ • Avg. Strength: 0.68   │
│                         │
│ Performance:            │
│ • Render Time: 23ms     │
│ • Memory Usage: 45MB    │
└─────────────────────────┘
```

## Modal Dialogs

### Circuit Comparison View
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Circuit Comparison                                    [×] Close          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ┌─────────────────────────┐   ┌─────────────────────────┐               │
│ │ Reasoning Circuit       │   │ Attention Circuit       │               │
│ │                         │   │                         │               │
│ │  ● ──→ ● ──→ ●         │   │    ● ←─── ●             │               │
│ │  │      │      │         │   │    │       │             │               │
│ │  ▼      ▼      ▼         │   │    ▼       ▼             │               │
│ │  ● ──→ ● ──→ ●         │   │    ● ──→ ●             │               │
│ │                         │   │                         │               │
│ │ Length: 5 hops          │   │ Length: 3 hops          │               │
│ │ Strength: 0.742         │   │ Strength: 0.834         │               │
│ │                         │   │                         │               │
│ └─────────────────────────┘   └─────────────────────────┘               │
│                                                                         │
│ Shared Nodes: feature_layer3_45, feature_layer7_23                     │
│ Divergence Point: layer 4                                              │
│ Convergence Point: logit output                                        │
│                                                                         │
│ [Export Comparison] [Overlay View] [Detailed Analysis]                 │
└─────────────────────────────────────────────────────────────────────────┘
```

### Export Options Dialog
```
┌─────────────────────────────────────────┐
│ Export Options                [×] Close │
├─────────────────────────────────────────┤
│                                         │
│ Export Format:                          │
│ ○ PNG Image (High Resolution)           │
│ ● SVG Vector Graphics                   │
│ ○ JSON Data                             │
│ ○ Mermaid Diagram                       │
│ ○ PDF Report                            │
│                                         │
│ Include:                                │
│ ☑ Current View                          │
│ ☑ Node Details                          │
│ ☑ Circuit Annotations                   │
│ ☐ Full Graph (unfiltered)               │
│                                         │
│ Quality:                                │
│ ○ Draft    ● Standard    ○ High         │
│                                         │
│ Filename: [attribution_graph_export]    │
│                                         │
│ [Cancel]              [Export]          │
└─────────────────────────────────────────┘
```

## Mobile/Responsive View

### Tablet Layout (Portrait)
```
┌─────────────────────────────────┐
│ Attribution Graphs Explorer     │
├─────────────────────────────────┤
│ [☰] [View] [Analysis] [Export]  │
├─────────────────────────────────┤
│                                 │
│         Graph Canvas            │
│                                 │
│    ● ──→ ● ──→ ●               │
│    │      │      │               │
│    ▼      ▼      ▼               │
│    ● ──→ ● ──→ ●               │
│                                 │
│         [Minimap]               │
│                                 │
├─────────────────────────────────┤
│ [Filter] [Circuits] [Details]   │
├─────────────────────────────────┤
│ Quick Controls:                 │
│ Threshold: [====●──] Layout: ▼  │
└─────────────────────────────────┘
```

## Keyboard Shortcuts

```
Navigation:
├─ Arrow Keys: Pan graph
├─ +/-: Zoom in/out
├─ Space + Drag: Pan
├─ Home: Fit to view
└─ F: Focus on selected

Selection:
├─ Click: Select node/edge
├─ Ctrl+Click: Multi-select
├─ Shift+Click: Select path
└─ Esc: Clear selection

Analysis:
├─ T: Trace from selected
├─ C: Add to circuit
├─ H: Hide/show selected
└─ R: Reset filters

Interface:
├─ Tab: Next panel
├─ Ctrl+F: Search
├─ Ctrl+E: Export
└─ F11: Fullscreen
```

These wireframes provide a comprehensive blueprint for implementing the interactive visualization interface, balancing functionality with usability for research workflows.