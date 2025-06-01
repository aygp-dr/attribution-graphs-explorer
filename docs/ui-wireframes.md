# Interactive Attribution Graph Interface - UI Wireframes

## Main Interface Layout

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ Attribution Graph Explorer - Interactive Interface                              │
├─────────────────────────────────────────────────────────────────────────────────┤
│ File: poetry-circuit.json | Graph: Poetry Planning | Method: Jacobian          │
├──────────────┬──────────────────────────────────────────────┬─────────────────────┤
│ Controls     │ Graph Visualization Canvas                   │ Inspector Panel     │
│ (250px)      │ (Flexible)                                   │ (300px)            │
│              │                                              │                     │
│ Filters      │    Token: "Roses"                           │ Selected Node       │
│ ┌──────────┐ │         ○                                   │                     │
│ │Threshold │ │         │ 0.75                              │ ID: feature_42      │
│ │[0.1]──■──│ │         ▼                                   │ Type: Feature       │
│ └──────────┘ │    Feature 42: "plans rhyme"               │ Layer: 6            │
│              │         ○ ──────────┐                       │ Activation: 0.852   │
│ Layers       │         │ 0.65      │ 0.43                 │                     │
│ ┌──────────┐ │         ▼           ▼                       │ Interpretation:     │
│ │[5]──■──[8]│ │    Feature 67  Feature 23                 │ "Plans ahead for    │
│ └──────────┘ │         ○           ○                       │  rhyming patterns"  │
│              │         │ 0.78      │ 0.32                 │                     │
│ Circuits     │         ▼           ▼                       │ Connections:        │
│ ☑ Poetry     │    Logit: "blue"                           │ • 3 incoming        │
│ ☐ Reasoning  │         ○                                   │ • 5 outgoing        │
│ ☐ All Paths  │                                             │                     │
│              │                                             │ Actions:            │
│ Layout       │ ┌─────────────────────────────────────────┐ │ [Trace Paths]       │
│ ○ Force      │ │ Minimap                              │▲│ │ [Highlight]         │
│ ● Layered    │ │ ┌─────┐                             ││ │ [Hide Node]         │
│ ○ Circular   │ │ │ ■   │ ←── You are here            ││ │                     │
│              │ │ └─────┘                             │▼│ │                     │
│ [Reset View] │ └─────────────────────────────────────────┘ │                     │
│ [Export]     │                                             │                     │
└──────────────┴──────────────────────────────────────────────┴─────────────────────┘
```

## Graph Visualization Detailed View

```
Graph Canvas Area:
┌─────────────────────────────────────────────────────────────────────────────────┐
│ Zoom: 85% | Center on Selected | Auto-Layout: Layered                          │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                 │
│ Layer 5              Layer 6              Layer 7              Layer 8         │
│                                                                                 │
│   Token              Feature               Feature              Logit           │
│ ┌─────────┐        ┌─────────────┐       ┌─────────────┐      ┌─────────┐       │
│ │"Roses"  │ ────── │F42: plans   │ ───── │F67: rhyme   │ ──── │"blue"   │       │
│ │  ○      │  0.75  │rhyming  ●   │ 0.65  │detect   ●   │ 0.78 │  ○      │       │
│ │ [T:0]   │        │ [F:42]      │       │ [F:67]      │      │ [L:blue]│       │
│ └─────────┘        └─────────────┘       └─────────────┘      └─────────┘       │
│                            │                     │                             │
│                            │ 0.43               │ 0.32                        │
│                            ▼                     ▼                             │
│                    ┌─────────────┐       ┌─────────────┐                       │
│                    │F23: word    │       │F89: phoneme │                       │
│                    │choice   ○   │       │match    ○   │                       │
│                    │ [F:23]      │       │ [F:89]      │                       │
│                    └─────────────┘       └─────────────┘                       │
│                                                                                 │
│ Legend:                                                                         │
│ ● = Selected/Highlighted     ○ = Normal Node     ──── = Strong Edge (>0.5)     │
│ - - - = Weak Edge (<0.5)     [Type:ID] = Node Identifier                      │
└─────────────────────────────────────────────────────────────────────────────────┘
```

## Control Panel Details

```
┌─────────────────┐
│ FILTERS         │
├─────────────────┤
│ Attribution     │
│ Threshold       │
│ ┌─────────────┐ │
│ │ 0.1 ■ 0.5   │ │ ← Slider
│ │ └─────────┘ │ │
│ │ Show: 247   │ │ ← Node count
│ └─────────────┘ │
│                 │
│ Layer Range     │
│ ┌─────────────┐ │
│ │ [5] ■─■ [8] │ │ ← Range slider
│ │ └─────────┘ │ │
│ └─────────────┘ │
├─────────────────┤
│ CIRCUITS        │
├─────────────────┤
│ ☑ Poetry Plan   │ ← Checkboxes
│ ☐ Rhyme Detect  │
│ ☐ Word Choice   │
│ ☐ All Paths     │
│                 │
│ [Discover New]  │ ← Button
├─────────────────┤
│ LAYOUT          │
├─────────────────┤
│ ● Force-Direct  │ ← Radio buttons
│ ○ Hierarchical  │
│ ○ Circular      │
│ ○ Manual        │
│                 │
│ [Apply] [Reset] │
├─────────────────┤
│ ACTIONS         │
├─────────────────┤
│ [Export SVG]    │
│ [Export JSON]   │
│ [Screenshot]    │
│ [Print Report]  │
└─────────────────┘
```

## Inspector Panel Details

```
┌──────────────────────┐
│ NODE INSPECTOR       │
├──────────────────────┤
│ Feature 42           │
│ Type: Feature        │
│ Layer: 6             │
│ Activation: 0.852    │
│                      │
│ Position:            │
│ • Layer: 6           │
│ • Index: 42          │
│                      │
│ Interpretation:      │
│ "Plans ahead for     │
│  rhyming patterns    │
│  in poetry"          │
│                      │
│ Attribution Stats:   │
│ • Total In: 0.75     │
│ • Total Out: 1.08    │
│ • Max Single: 0.65   │
│                      │
│ Connections:         │
│ Incoming (3):        │
│ • T:0 "Roses" (0.75) │
│ • F:15 (0.23)        │
│ • F:8 (0.12)         │
│                      │
│ Outgoing (5):        │
│ • F:67 "rhyme" (0.65)│
│ • F:23 "choice"(0.43)│
│ • F:89 "phone"(0.32) │
│ • ... (show all)     │
│                      │
│ [Trace All Paths]    │
│ [Highlight Circuit]  │
│ [Hide This Node]     │
└──────────────────────┘
```

## Comparison View Layout

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ Graph Comparison: Poetry Planning Circuit                                       │
├─────────────────────────────────────────────────────────────────────────────────┤
│                    Method A: Jacobian          Method B: Integrated Gradients   │
├────────────────────────────────────────────────────────────────────────────────┤
│ ┌─────────────────────────────────────────┐ ┌──────────────────────────────────┐ │
│ │ Graph A: Jacobian Attribution           │ │ Graph B: IntGrad Attribution     │ │
│ │                                         │ │                                  │ │
│ │   T:0 ──0.75──→ F:42 ──0.65──→ F:67    │ │   T:0 ──0.68──→ F:42 ──0.71──→   │ │
│ │    ↓             ↓        ↓             │ │    ↓             ↓        ↓     │ │
│ │   ...           F:23     L:blue         │ │   ...           F:23     L:blue  │ │
│ │                                         │ │                                  │ │
│ │ Strong Path: T:0→F:42→F:67→L:blue      │ │ Strong Path: T:0→F:42→F:67→L:blue│ │
│ │ Circuit Strength: 0.78                  │ │ Circuit Strength: 0.82           │ │
│ └─────────────────────────────────────────┘ └──────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────────────────────────┤
│ Comparison Analysis:                                                            │
│ • Shared Nodes: 15/18 (83% overlap)                                           │
│ • Shared Edges: 12/23 (52% overlap)                                           │
│ • Core Circuit: Both methods identify T:0→F:42→F:67→L:blue path                │
│ • Differences: IntGrad shows stronger F:42→F:67 connection (+0.06)            │
│                                                                                 │
│ [Export Comparison] [Generate Report] [Align Views]                            │
└─────────────────────────────────────────────────────────────────────────────────┘
```

## Context Menus and Interactions

### Node Right-Click Menu
```
┌─────────────────┐
│ Feature 42      │
├─────────────────┤
│ → Inspect       │
│ → Trace Paths   │
│   • To Selected │
│   • From Here   │
│   • All Paths   │
│ → Highlight     │
│   • Node Only   │
│   • + Neighbors │
│   • + Circuit   │
│ → Filter        │
│   • Hide Node   │
│   • Hide <0.1   │
│   • Show Only   │
│ ─────────────── │
│ → Export Info   │
└─────────────────┘
```

### Edge Hover Tooltip
```
┌─────────────────────────┐
│ Edge Information        │
├─────────────────────────┤
│ From: Feature 42        │
│ To: Feature 67          │
│ Weight: 0.65            │
│ Layer: 6 → 7            │
│ Circuit: Poetry Planning│
│                         │
│ Click to highlight path │
└─────────────────────────┘
```

## Responsive Design Considerations

### Tablet/Mobile Layout
```
┌─────────────────────────┐
│ ☰ Attribution Explorer │ ← Hamburger menu
├─────────────────────────┤
│                         │
│ [Filters▼] [Circuits▼] │ ← Collapsible sections
│                         │
│ ┌─────────────────────┐ │
│ │   Graph Canvas      │ │ ← Full width
│ │                     │ │
│ │   Touch-optimized   │ │
│ │   controls          │ │
│ └─────────────────────┘ │
│                         │
│ [Inspector] [Export]    │ ← Bottom toolbar
└─────────────────────────┘
```

This wireframe design prioritizes:
1. **Clarity**: Clear visual hierarchy and information organization
2. **Efficiency**: Quick access to common research tasks
3. **Flexibility**: Adaptable to different graph sizes and research workflows
4. **Integration**: Seamless connection with existing Scheme computation