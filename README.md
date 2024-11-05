# microbiomeDAAcomp

## Description
microbiomeDAAcomp is a comprehensive R package designed for comparing and evaluating Differential Abundance Analysis (DAA) methods in microbiome studies. It provides a unified framework to assess various DAA methods' performance, helping researchers select the most appropriate method based on their specific data characteristics and research needs.

## Installation
You can install the development version of microbiomeDAAcomp from GitHub:
```r
# install.packages("devtools")
devtools::install_github("cafferychen777/microbiomeDAAcomp")
```

## Project Development Plan
For the remainder of the semester, the following key developments are planned:
- Complete implementation of core DAA comparison functions (DESeq2, ALDEx2 integration)
- Develop comprehensive method evaluation metrics
- Add data simulation capabilities for benchmarking
- Create detailed vignettes with usage examples
- Implement unit tests for all functions
- Optimize performance for large-scale datasets

## Package Structure
```
microbiomeDAAcomp/
├── R/
│   ├── import_data.R
│   ├── simulate_data.R
│   └── run_daa_methods.R
├── tests/
│   └── testthat/
├── vignettes/
│   └── introduction.Rmd
├── DESCRIPTION
└── README.md
```

## Development Status
This package is under active development. Current features include:
- Basic data import functionality
- DAA method execution framework
- Data simulation utilities

All functions include roxygen documentation and follow standard R coding style guidelines.