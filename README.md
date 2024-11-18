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