# Replication Materials: Chapter 2 (Data Liquidity and Market)

Empirical analysis code for Study 2: a cross-sectional analysis of 4,525
marketplace listings from 407 identified Datarade providers.

## Files

| File | Purpose |
|------|---------|
| `ch2_empirical_report.Rmd` | **Main reproducible report.** Generates all tables and figures (descriptives, Poisson regressions, robustness checks, fsQCA). |
| `datarade_helpers.R` | Helper functions for data extraction from JSONL listings (delivery parsing, cadence ranking, key counting, etc.). |
| `extract_results.R` | Standalone script that prints regression results and diagnostics to the console. |
| `extract_fsqca.R` | Standalone script that prints fsQCA calibration, necessity, sufficiency, and truth-table results. |
| `robustness_extra.R` | Productization-intensity and missing-rating robustness checks reported in the chapter. |
| `reproduce.R` | One-command execution of regressions, robustness checks, fsQCA, and the rendered report. |
| `install_packages.R` | Installs the R package dependencies used by the replication workflow. |
| `DATA_DICTIONARY.md` | Definitions and operationalization notes for analysis variables. |
| `DATA_AVAILABILITY.md` | Redistribution and access note for the listing-level data. |

## Data

The input file `data/updated_datarade_data_scored_copy.jsonl` contains one JSON
object per line (~40 MB): scraped Datarade listing metadata augmented with
LLM-based semantic-classification scores. The scripts first look for the file
beside the scripts (the manuscript-workspace layout) and then under `data/`
(the public-repository layout). See `DATA_AVAILABILITY.md` before redistributing
the listing-level data.

## Reproduce

1.  Install R (≥ 4.3) and the following packages:

    ```r
    install.packages(c("jsonlite", "dplyr", "tidyr", "stringr", "readr",
                        "ggplot2", "scales", "QCA", "kableExtra", "MASS",
                        "sandwich", "clubSandwich", "rmarkdown", "knitr"))
    ```

2.  Confirm that the JSONL data file is available either in this directory or
    under `data/`.

3.  Knit the report:

    ```r
    setwd("<path-to-this-folder>")
    rmarkdown::render("ch2_empirical_report.Rmd")
    ```

    Or run the standalone scripts:

    ```r
    setwd("<path-to-this-folder>")
    source("extract_results.R")
    source("extract_fsqca.R")
    ```

## Key Results (summary)

- **Main Poisson (provider-clustered):** DL (std.) β = 0.164, IRR = 1.179, 95% CI [1.114, 1.247], *p* < .001
- **OLS (provider-clustered):** β = 0.358, *p* < .001
- **Ordered Logit (provider-clustered):** β = 0.443, OR = 1.557, *p* = .002
- **Provider FE Poisson (provider-clustered):** β = 0.112, IRR = 1.119, *p* < .001
- **Provider FE CR2/Satterthwaite:** SE = 0.0267, df = 38.8, *p* < .001
- **Use-case controls:** 62 tags appearing in at least 2% of the estimation sample; nearby thresholds yield IRRs from 1.138 to 1.231
- **fsQCA parsimonious solution:** K + C·T → OUT (consistency = 0.745, coverage = 0.692)


## Use-case controls

The marketplace use-case field is multilabel: one listing can carry several
tags. `make_prevalent_use_case_controls()` trims whitespace, converts tags to
lower case, removes empty values, and expands every retained tag into a binary
listing-level indicator. The baseline retains tags observed on at least 2% of
the 4,525-listing estimation sample (at least 91 listings). The code checks for
exact duplicate indicator profiles before estimation, without consulting either
the data-liquidity index or the outcome. In the baseline sample, 62 tags pass
the prevalence threshold and no duplicate profile is found, so all 62 indicators
enter jointly as nuisance controls for intended application. Tags below the
threshold are not pooled into an `other` category. Tags are sorted alphabetically
before duplicate profiles are checked; if profiles coincided, the first tag would
be kept and later identical profiles removed. Regression columns receive stable, sequential ASCII identifiers (`uc_001`,
etc.), and the returned `tag_map` preserves their correspondence with the
original tags. Sensitivity checks repeat the estimation at thresholds
from 1% to 5% and without use-case controls.
