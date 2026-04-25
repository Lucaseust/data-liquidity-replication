# Data Dictionary

The input file is `data/updated_datarade_data_scored_copy.jsonl`. Each line is
one JSON object representing a Datarade marketplace listing after scoring.

## Core Fields Used

| Variable | Source field or construction | Description |
| --- | --- | --- |
| `url` | `url` | Listing URL; used for deduplication. |
| `provider_url` | `provider_url` | Provider identifier used for provider counts and fixed effects. |
| `C_score` | `semantic_score` | Continuous semantic-clarity score from the classifier. |
| `c_semantic` | `semantic_pred` | Binary semantic-clarity prediction. |
| `T_rank` | parsed from `delivery`, `details`, `history` | Temporal-intensity cadence rank from 0 to 9. |
| `K_log` | parsed from `data_dictionary` | `log(1 + hard_key_count)`, where hard keys are joinable identifiers. |
| `payment_modality` | `pricing_plans` | Indicator for displayed pricing modality. |
| `price_displayed` | `pricing_plans` | Indicator for displayed numeric price. |
| `free_sample_available` | `details`, `history`, pricing and delivery fields | Indicator for free sample or extract availability. |
| `api_method` | `delivery` | Indicator for API delivery availability. |
| `liquidity_count` | constructed | Sum of the four realized-liquidity indicators, range 0 to 4. |
| `IDL_std` | constructed | Standardized sum of z-scored `C_score`, `T_rank`, and `K_log`. |
| `provider_n` | constructed | Number of listings by provider. |
| `provider_rating` | `provider__rating-summary-score` | Provider rating, with missing values set to 0 after flagging. |
| `rating_missing` | constructed | Indicator for missing provider rating. |
| `uc_*` | `use_cases` | Use-case dummies retained when appearing in at least two listings. |

## fsQCA Calibration

The fsQCA script calibrates the main conditions with three qualitative anchors:

| Set | Fully out | Crossover | Fully in |
| --- | ---: | ---: | ---: |
| Semantic clarity, `C` | 0.30 | 0.52 | 0.73 |
| Temporal intensity, `T` | 0 | 4 | 9 |
| Composability, `K` | 0 | 1 | 5 |
| High realized liquidity, `OUT` | 0 | 2 | 4 |
