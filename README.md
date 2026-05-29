# PRIO-Now CEA

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20447940.svg)](https://doi.org/10.5281/zenodo.20447940)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Cost-effectiveness model of a two-staged coronary artery calcium (CAC)-guided
statin screening strategy (**PRIO-Now**) versus standard of care in a Swedish
population of 60-year-olds.

The analysis is an R/[`heemod`](https://cran.r-project.org/package=heemod) Markov
cohort model run over a 20-year horizon (ages 60–79), from a healthcare-sector
perspective with 3% annual discounting. It produces deterministic base-case
results, one-way deterministic sensitivity analysis (tornado, hazard-ratio
threshold), probabilistic sensitivity analysis (PSA, cost-effectiveness plane,
CEAC), and a subgroup analysis by neighborhood socioeconomic status.

## Repository structure

```
.
├── run_master.R              # Top-level script: runs the full analysis and writes outputs/
├── scripts/                  # Model modules sourced by run_master.R
│   ├── config_basecase.R     # Base-case configuration (horizon, discounting, currency display)
│   ├── read_inputs.R         # Reads model input tables
│   ├── helpers.R             # Discounting and ICER utilities
│   ├── build_models.R        # Builds SoC and intervention Markov models
│   ├── extract_outputs.R     # Extracts discounted totals, costs/cycle, state membership
│   ├── psa.R                 # Probabilistic sensitivity analysis
│   ├── dsa.R                 # One-way DSA + tornado + HR-threshold plots
│   ├── plots.R               # Cost-effectiveness plane, CEAC, state-membership plots
│   ├── subgroup_analysis.R   # Socioeconomic subgroup analysis
│   ├── export.R              # Writes Excel workbooks / PNGs
│   └── plot_model_structure.R# Model-structure diagram
├── data/                     # Model input data
└── CITATION.cff              # Citation metadata
```

## Requirements

- R (developed under R 4.5)
- R packages: `dplyr`, `heemod`, `tibble`, `tidyr`, `ggplot2`, `readxl`,
  `writexl`, `stringr`, `geomtextpath`

Missing packages are installed automatically by `run_master.R`.

## How to run

From the repository root:

```r
source("run_master.R")
```

This regenerates all figures and result tables into an `outputs/` folder.

> **Note:** Monetary results are computed internally in SEK and displayed in EUR
> (1 EUR = 10.8 SEK); the display currency is configurable in
> `scripts/config_basecase.R`.

## Citation

If you use this software, please cite it using the metadata in
[`CITATION.cff`](CITATION.cff), or:

> Svensson, M. (2026). *PRIO-Now CEA: Cost-effectiveness model of two-staged
> CAC-guided statin screening in Sweden* (v1.0.0). Zenodo.
> https://doi.org/10.5281/zenodo.20447940

- Concept DOI (all versions): [10.5281/zenodo.20447940](https://doi.org/10.5281/zenodo.20447940)
- Version DOI (v1.0.0): [10.5281/zenodo.20447941](https://doi.org/10.5281/zenodo.20447941)

## License

Released under the [MIT License](LICENSE).
