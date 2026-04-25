# Intrinsic Data Liquidity Replication Materials

This repository contains the data and code needed to reproduce the empirical
analysis for the paper on intrinsic data liquidity and realized liquidity in
B2B data marketplaces.

Paper link: to be added after publication.

## Contents

| Path | Purpose |
| --- | --- |
| `data/updated_datarade_data_scored_copy.jsonl` | Listing-level replication data used in the analysis. |
| `ch2_empirical_report.Rmd` | Main reproducible report; generates tables, figures, regressions, and fsQCA outputs. |
| `datarade_helpers.R` | Helper functions for parsing listing metadata and constructing measures. |
| `extract_results.R` | Console script for the regression results and descriptive outputs. |
| `extract_fsqca.R` | Console script for fsQCA outputs and cross-check regressions. |
| `install_packages.R` | Installs required R packages from CRAN. |
| `reproduce.R` | One-command reproduction script. |
| `DATA_DICTIONARY.md` | Variable definitions and operationalization notes. |

## Reproduce

Install R 4.3 or later. Then run:

```r
source("install_packages.R")
source("reproduce.R")
```

From a terminal:

```bash
Rscript install_packages.R
Rscript reproduce.R
```

The main HTML report is written to `outputs/ch2_empirical_report.html`.

## Expected Key Results

The checked replication run uses 4,552 unique marketplace listings from 408
providers and 503 retained use-case controls.

| Result | Estimate |
| --- | --- |
| Main Poisson PML, IDL index | beta = 0.2486, IRR = 1.2822, 95% CI [1.2439, 1.3216], p < .001 |
| OLS robustness | beta = 0.5016, p < .001 |
| Ordered logit robustness | beta = 0.4815, OR = 1.6185, p < .001 |
| Provider fixed-effects Poisson | beta = 0.0670, IRR = 1.0692, p < .001 |
| fsQCA parsimonious solution | K + C*T -> OUT, consistency = 0.745, coverage = 0.692 |

## Software

The analysis was last checked with R 4.5.2 on 2026-04-25. Required packages are
listed in `install_packages.R`.

## Citation

Please cite the paper once it is published. Repository citation metadata are
provided in `CITATION.cff`.
