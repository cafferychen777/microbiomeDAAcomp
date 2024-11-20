# microbiomeDAAcomp

## Description
microbiomeDAAcomp is a comprehensive R package designed for comparing and evaluating Differential Abundance Analysis (DAA) methods in microbiome studies. It provides a unified framework to assess various DAA methods' performance, helping researchers select the most appropriate method based on their specific data characteristics and research needs.

## Installation
You can install the development version of microbiomeDAAcomp from GitHub:
```r
# install.packages("devtools")
devtools::install_github("cafferychen777/microbiomeDAAcomp")
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

## Package Structure
```
microbiomeDAAcomp/
├── R/
│   ├── run_daa_methods.R     # Core DAA execution
│   ├── evaluate_performance.R # Performance evaluation
│   ├── power_analysis.R      # Statistical power analysis
│   ├── sensitivity_analysis.R # Parameter sensitivity
│   ├── compare_methods.R     # Method comparison
│   └── plot_performance.R    # Visualization
├── tests/
│   └── testthat/            # Unit tests
├── vignettes/
│   └── introduction.Rmd     # Package documentation
├── DESCRIPTION
└── README.md
```

## Usage Examples

### Running DAA Analysis
```r
# Run multiple DAA methods
results <- run_daa_methods(
  data = your_data,
  methods = c("DESeq2", "ALDEx2", "ANCOM-BC"),
  alpha = 0.05
)
```

### Evaluating Performance
```r
# Evaluate method performance
performance <- evaluate_performance(
  results = daa_results,
  true_status = true_differential_status,
  metrics = c("sensitivity", "specificity", "precision")
)
```

### Power Analysis
```r
# Conduct power analysis
power_results <- power_analysis(
  effect_sizes = seq(0.5, 2, 0.5),
  sample_sizes = c(50, 100, 200),
  alpha = 0.05
)
```

### Method Comparison
```r
# Compare methods
comparison <- compare_methods(
  performance_results = performance_data,
  comparison_type = "comprehensive"
)
```

## Development Status
This package is under active development. All core functionalities have been implemented and thoroughly tested. Future developments will focus on:
- Adding more DAA methods
- Enhancing visualization capabilities
- Optimizing computational performance
- Expanding documentation and vignettes

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.

## License
MIT License

## Documentation
Visit our [online documentation](https://cafferychen777.github.io/microbiomeDAAcomp/) for complete guides and API references.

## Dependencies
```r
# Required packages
dependencies <- c(
  "phyloseq",    # for microbiome data handling
  "DESeq2",      # for differential analysis
  "ALDEx2",      # for differential analysis
  "ANCOMBC",     # for differential analysis
  "ggplot2",     # for visualization
  "dplyr",       # for data manipulation
  "tidyr",       # for data tidying
  "stats"        # for statistical computations
)
```

## Citation
If you use this package in your research, please cite:
```
Yang, C. (2024). microbiomeDAAcomp: A Comprehensive Toolkit for Microbiome 
Differential Abundance Analysis Method Comparison. R package version 1.0.0.
```

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

## Community Support

- Report issues: [GitHub Issues](https://github.com/cafferychen777/microbiomeDAAcomp/issues)
- Discussions: [GitHub Discussions](https://github.com/cafferychen777/microbiomeDAAcomp/discussions)
- Email support: [support@example.com](mailto:cafferychen777@tamu.edu)

## Changelog

### v1.0.0 (2024-03)
- Initial release
- Implemented core DAA comparison functionality
- Added basic visualization tools

### v0.9.0 (2024-02)
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