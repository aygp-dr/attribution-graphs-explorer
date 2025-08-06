@claude Please take a look at this issue and help implement a proper global weights analysis module that can analyze weights across multiple prompts and inputs. We need to identify common circuit patterns that appear across different inputs.

Specifically, I'd like you to:

1. Review the current implementation in `src/math/matrix.scm`
2. Provide an RFC with proposed improvements to the SVD analysis
3. Create a diagram showing how components interact in the global analysis
4. Implement additional visualization capabilities for the identified components

The current implementation has basic SVD but lacks proper visualization and interpretation tools. We need better ways to analyze the identified components.