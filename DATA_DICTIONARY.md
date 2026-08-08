# Data Dictionary

The input file is `data/updated_datarade_data_scored_copy.jsonl`. Each line is
one JSON object representing a Datarade marketplace listing after scoring.

## Core fields and constructed variables

| Variable | Source field or construction | Description |
| --- | --- | --- |
| `url` | `url` | Listing URL; used for URL-level deduplication. |
| `provider_url` | `provider_url` | Provider identifier; defines provider counts, fixed effects, and the standard-error clustering level. |
| `C_score` | `semantic_score` | Continuous classifier probability used as the proxy for representational transparency. |
| `c_semantic` | `semantic_pred` | Binary classifier prediction retained for comparison in the exploratory fsQCA. |
| `T_rank` | parsed from `delivery`, `details`, and `history` | Update-frequency rank from 0 (historical) to 9 (real-time). |
| `K_log` | parsed from `data_dictionary` | `log(1 + hard_key_count)`, where hard keys are disclosed deterministic join identifiers. |
| `payment_modality` | `pricing_plans` | Indicator for a displayed plan, subscription, or package structure. |
| `price_displayed` | `pricing_plans` | Indicator for a displayed numeric price. |
| `free_sample_available` | `details`, `history`, pricing, and delivery fields | Indicator for a free sample, preview, or extract. |
| `api_method` | `delivery` | Indicator for API delivery availability. |
| `liquidity_count` | constructed | Market-exchange count: sum of the four exchange signals, ranging from 0 to 4. |
| `IDL_std` | constructed | Internal legacy code name for the standardized data-liquidity index (`DL` in the chapter), formed from z-scored `C_score`, `T_rank`, and `K_log`. |
| `provider_n` | constructed | Number of listings supplied by the provider. |
| `provider_rating` / `provider_rating_0` | `provider__rating-summary-score` | Displayed provider rating; baseline missing values are set to zero after flagging. |
| `rating_missing` | constructed | Indicator distinguishing an undisclosed rating from an observed rating. |
| `uc_*` | `use_cases` | Binary multilabel use-case indicators retained at the chosen prevalence threshold. |

## Use-case control construction

Use cases are not forced into one mutually exclusive category. The code trims
whitespace, converts tag strings to lower case, removes empty values, and
expands each retained tag into a binary listing-level indicator. The baseline
retains tags appearing on at least 2% of the provider-identified estimation
sample (at least 91 of 4,525 listings). Exact duplicate indicator profiles are
removed deterministically without consulting the outcome or data-liquidity
index. The resulting 62 indicators enter jointly as nuisance controls for
intended application. Rare tags are not pooled into an `other` category.
Sensitivity checks vary the threshold from 1% to 5% and omit the controls
entirely.

## Productization robustness variables

| Variable | Construction | Interpretation |
| --- | --- | --- |
| `desc_len_log` | `log(1 + description length)` | Amount of seller-supplied narrative information. |
| `meta_fields` | Count of populated neutral metadata blocks | Breadth and completeness of structured listing information outside the focal outcomes. |
| `dd_attr_log` | `log(1 + data-dictionary attribute count)` | Extent of disclosed schema documentation. |
| `prod_index` | Standardized sum of the three preceding measures | General productization-intensity proxy used in a robustness specification. |
| `provider_rating_mean` | Mean-imputed observed rating | Alternative missing-rating treatment. |
| `provider_rating_med` | Median-imputed observed rating | Alternative missing-rating treatment used with the missingness indicator. |

## fsQCA calibration

The exploratory fsQCA script calibrates the main conditions with three
qualitative anchors:

| Set | Fully out | Crossover | Fully in |
| --- | ---: | ---: | ---: |
| Representational transparency, `C` | 0.30 | 0.52 | 0.73 |
| Update frequency, `T` | 0 | 4 | 9 |
| Composability, `K` | 0 | 1 | 5 |
| High market-exchange signaling, `OUT` | 0 | 2 | 4 |
