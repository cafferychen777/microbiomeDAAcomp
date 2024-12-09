# microbiomeDAAcomp

## Description
microbiomeDAAcomp is a comprehensive R package designed for comparing and evaluating Differential Abundance Analysis (DAA) methods in microbiome studies. It provides a unified framework to assess various DAA methods' performance, helping researchers select the most appropriate method based on their specific data characteristics and research needs.

## Installation
You can install the development version of microbiomeDAAcomp from GitHub:
```r
# install.packages("devtools")
devtools::install_github("cafferychen777/microbiomeDAAcomp")
```

## Documentation
- **Package Documentation**: Comprehensive function documentation is available through R's help system
- **Vignettes**: Detailed tutorials and examples are available:
  ```r
  # View the introduction vignette
  vignette("introduction", package = "microbiomeDAAcomp")
  
  # List all available vignettes
  vignette(package = "microbiomeDAAcomp")
  ```

## Key Features

### 1. DAA Method Integration
- Supports multiple popular DAA methods:
  - DESeq2
  - ALDEx2
  - ANCOM-BC
- Unified interface for method execution and comparison

### 2. Performance Evaluation
- Comprehensive metrics calculation:
  - Sensitivity
  - Specificity
  - Precision
  - F1 score
  - MCC (Matthews Correlation Coefficient)
- Confidence interval estimation
- Performance ranking across methods

### 3. Statistical Analysis
- Power analysis for experimental design
- Sensitivity analysis for parameter tuning
- Statistical comparison between methods:
  - Friedman test
  - Post-hoc analysis (Nemenyi test)

### 4. Visualization
- Performance visualization options:
  - Heatmaps
  - Box plots
  - Violin plots
- Method comparison plots
- Interactive plotting support (via plotly)

## Quick Start

For detailed examples and tutorials, please refer to our vignettes:
```r
# View the introduction vignette with complete examples
vignette("introduction", package = "microbiomeDAAcomp")
```

Basic usage:
```r
library(microbiomeDAAcomp)

# Run multiple DAA methods
# See vignette("introduction") for complete examples with real data
results <- run_daa_methods(
  data = your_data,
  methods = c("DESeq2", "ALDEx2", "ANCOM-BC"),
  alpha = 0.05
)

# Evaluate performance
performance <- evaluate_performance(
  results = results,
  true_status = true_differential_status,
  metrics = c("sensitivity", "specificity", "precision")
)

# Visualize results
plot_performance(performance, plot_type = "heatmap")
```

For more examples and detailed usage instructions, please check our comprehensive vignettes:
```r
# List all available vignettes
vignette(package = "microbiomeDAAcomp")
```

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.

## Citation
If you use this package in your research, please cite:
```
Yang, C. (2024). microbiomeDAAcomp: A Comprehensive Toolkit for Microbiome 
Differential Abundance Analysis Method Comparison. R package version 1.0.0.
```

## Support and Resources
- **Documentation**: Complete package documentation and vignettes
- **Issues**: [GitHub Issues](https://github.com/cafferychen777/microbiomeDAAcomp/issues)
- **Discussions**: [GitHub Discussions](https://github.com/cafferychen777/microbiomeDAAcomp/discussions)
- **Email**: [support](mailto:cafferychen777@tamu.edu)

## FAQ

### Q: How to handle sparse data?
It's recommended to preprocess the data before running DAA analysis:

```r
# Handle sparse data using DESeq2's approach
dds <- DESeqDataSetFromMatrix(
  countData = counts + 1,  # Add pseudocount
  colData = data.frame(group = groups),
  design = ~ group
)
```

### Q: How to choose the most suitable DAA method?
You can compare the performance of different methods:

```r
# Run multiple methods and compare performance
results <- run_daa_methods(
  data = your_data,
  methods = c("DESeq2", "ALDEx2", "ANCOM-BC")
)

# Evaluate performance
performance <- evaluate_performance(
  results = results,
  true_status = true_differential_status,
  metrics = c("sensitivity", "specificity", "precision")
)

# Compare method performance
comparison <- compare_methods(
  performance_results = performance,
  comparison_type = "comprehensive"
)
```

## Troubleshooting

Before reporting issues, please check:

1. R version >= 4.1.0
2. All dependencies are properly installed
3. Input data format meets requirements

## Changelog

### v1.0.0 (2024-12)

- Initial release
- Implemented core DAA comparison functionality
- Added basic visualization tools

### v0.9.0 (2024-11)

- Beta release
- Completed major functionality testing
- Performance optimization

## Roadmap

Future plans:

- [ ] Add support for more DAA methods
- [ ] Enhance visualization capabilities
- [ ] Add interactive analysis interface
- [ ] Optimize computational performance
- [ ] Expand documentation and tutorials

## Related Projects

- [phyloseq](https://github.com/joey711/phyloseq)
- [DESeq2](https://github.com/mikelove/DESeq2)
- [ALDEx2](https://github.com/ggloor/ALDEx2)
