# Helper functions for data extraction from Datarade JSONL
# Extracted from regressions_fe_datarade.R for use in Rmd reports.

suppressPackageStartupMessages(library(stringr))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

clean_scalar <- function(x) {
  x <- as.character(x %||% "")
  x <- str_squish(x)
  ifelse(is.na(x), "", x)
}

normalize_tokens <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  vals <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
  vals <- vals[!is.na(vals)]
  vals <- str_to_lower(str_squish(vals))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

flatten_values <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (is.atomic(x)) return(as.character(x))
  if (is.data.frame(x)) return(as.character(unlist(x, use.names = FALSE)))
  if (is.list(x)) return(unlist(lapply(x, flatten_values), use.names = FALSE))
  as.character(x)
}

# ---- Delivery extraction ----
extract_delivery_values_by_type <- function(delivery, target_type) {
  out <- character(0)
  target_type <- str_to_lower(target_type)
  if (is.null(delivery) || length(delivery) == 0) return(out)
  if (is.data.frame(delivery)) {
    if (!"type" %in% names(delivery)) return(out)
    types <- str_to_lower(str_squish(as.character(delivery$type)))
    idx <- which(types == target_type)
    if (length(idx) == 0) return(out)
    if ("values" %in% names(delivery)) {
      for (k in idx) out <- c(out, flatten_values(delivery$values[[k]]))
    } else if ("value" %in% names(delivery)) {
      for (k in idx) out <- c(out, flatten_values(delivery$value[[k]]))
    }
  } else if (is.list(delivery)) {
    for (block in delivery) {
      if (is.null(block)) next
      type_val <- str_to_lower(str_squish(as.character(block$type %||% "")))
      if (type_val != target_type) next
      out <- c(out, flatten_values(block$values %||% block$value))
    }
  }
  out <- out[!is.na(out)]
  out <- str_to_lower(str_squish(out))
  out <- out[nzchar(out)]
  unique(out)
}

extract_delivery_text <- function(delivery) {
  vals <- character(0)
  if (is.null(delivery) || length(delivery) == 0) return(vals)
  if (is.data.frame(delivery)) {
    vals <- c(vals, as.character(unlist(delivery, recursive = TRUE, use.names = FALSE)))
  } else if (is.list(delivery)) {
    vals <- c(vals, as.character(unlist(delivery, recursive = TRUE, use.names = FALSE)))
  } else {
    vals <- c(vals, as.character(delivery))
  }
  vals <- vals[!is.na(vals)]
  vals <- str_to_lower(str_squish(vals))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

# ---- API method ----
has_api_method <- function(delivery) {
  methods <- extract_delivery_values_by_type(delivery, "methods")
  as.integer(any(str_detect(methods, "api")))
}

# ---- Temporal intensity ----
extract_cadence_tokens <- function(text) {
  s <- clean_scalar(text)
  if (!nzchar(s)) return(character(0))
  s <- str_to_lower(s)
  out <- character(0)
  if (str_detect(s, "real\\s*-?time|realtime|\\blive\\b|streaming|\\bstream\\b|\\btick\\b")) out <- c(out, "real-time")
  if (str_detect(s, "\\bintraday\\b|\\bsecondly\\b")) out <- c(out, "intraday")
  if (str_detect(s, "\\bminutely\\b|every\\s+minute")) out <- c(out, "minutely")
  if (str_detect(s, "\\bhourly\\b|every\\s+hour")) out <- c(out, "hourly")
  if (str_detect(s, "\\bdaily\\b|every\\s+day")) out <- c(out, "daily")
  if (str_detect(s, "\\bweekly\\b")) out <- c(out, "weekly")
  if (str_detect(s, "\\bmonthly\\b")) out <- c(out, "monthly")
  if (str_detect(s, "\\bquarterly\\b")) out <- c(out, "quarterly")
  if (str_detect(s, "\\byearly\\b|\\bannual\\b")) out <- c(out, "yearly")
  if (str_detect(s, "historical|archive|\\bstatic\\b")) out <- c(out, "historical")
  unique(out)
}

cadence_rank <- c(
  "historical" = 0, "yearly" = 1, "quarterly" = 2, "monthly" = 3,
  "weekly" = 4, "daily" = 5, "hourly" = 6, "minutely" = 7,
  "intraday" = 8, "real-time" = 9
)

temporal_rank_full <- function(delivery, details, history) {
  freq_vals <- extract_delivery_values_by_type(delivery, "frequency")
  delivery_tokens <- unique(unlist(lapply(freq_vals, extract_cadence_tokens), use.names = FALSE))
  tokens <- character(0)
  if (length(delivery_tokens) > 0) {
    tokens <- delivery_tokens
  } else {
    text_tokens <- extract_cadence_tokens(paste(clean_scalar(details), clean_scalar(history)))
    if (length(text_tokens) > 0) tokens <- text_tokens
  }
  if (length(tokens) == 0) return(-1L)
  ranks <- unname(cadence_rank[tokens])
  ranks <- ranks[!is.na(ranks)]
  if (length(ranks) == 0) return(-1L)
  as.integer(max(ranks))
}

# ---- Data dictionary ----
extract_dd_field <- function(dd, field_name) {
  out <- character(0)
  if (is.null(dd) || length(dd) == 0) return(out)
  field_name <- as.character(field_name)
  read_field_from_block <- function(block) {
    vals <- character(0)
    if (is.null(block) || length(block) == 0) return(vals)
    if (is.data.frame(block)) {
      if (field_name %in% names(block)) vals <- c(vals, as.character(block[[field_name]]))
      if ("data" %in% names(block)) {
        for (sub in block$data) {
          if (is.data.frame(sub) && field_name %in% names(sub)) {
            vals <- c(vals, as.character(sub[[field_name]]))
          } else if (is.list(sub) && !is.null(sub[[field_name]])) {
            vals <- c(vals, as.character(sub[[field_name]]))
          }
        }
      }
      return(vals)
    }
    if (is.list(block)) {
      if (!is.null(block[[field_name]])) vals <- c(vals, as.character(block[[field_name]]))
      if (!is.null(block$data)) vals <- c(vals, read_field_from_block(block$data))
      return(vals)
    }
    vals
  }
  out <- read_field_from_block(dd)
  out <- out[!is.na(out)]
  out <- str_squish(as.character(out))
  out <- out[nzchar(out)]
  out
}

normalize_attr_name <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  x <- str_replace_all(x, "\\s+", "_")
  x
}

hard_key_regex_full <- "(^|[_\\W])(id|uuid|guid|user_id|customer_id|account_id|order_id|transaction_id|device_id|adid|idfa|gaid|email|phone|ip|isin|cusip|lei|vin|imei|imsi|mmsi|imo|fips|geoid|osm_id|imdb_id)($|[_\\W])"

is_hard_key <- function(attr_name) {
  if (!nzchar(attr_name)) return(FALSE)
  if (str_ends(attr_name, "_id")) return(TRUE)
  if (attr_name %in% c("id", "email", "phone", "ip")) return(TRUE)
  str_detect(attr_name, hard_key_regex_full)
}

hard_key_count_from_dd <- function(dd) {
  attrs <- extract_dd_field(dd, "attribute")
  if (length(attrs) == 0) return(0L)
  attrs <- normalize_attr_name(attrs)
  attrs <- unique(attrs)
  as.integer(sum(vapply(attrs, is_hard_key, logical(1))))
}

# ---- Free sample ----
has_dictionary_example <- function(dd) {
  examples <- extract_dd_field(dd, "example")
  as.integer(length(examples) > 0)
}

has_dictionary_sample_title <- function(dd) {
  titles <- extract_dd_field(dd, "title")
  if (length(titles) == 0) return(0L)
  titles <- str_to_lower(str_squish(titles))
  titles <- titles[!is.na(titles)]
  titles <- titles[nzchar(titles)]
  if (length(titles) == 0) return(0L)
  as.integer(any(str_detect(titles, "\\bsample\\b|\\bpreview\\b|\\bdemo\\b")))
}

has_free_sample_text <- function(details_text) {
  txt <- as.character(unlist(details_text, recursive = TRUE, use.names = FALSE))
  txt <- txt[!is.na(txt)]
  txt <- paste(txt, collapse = " ")
  txt <- clean_scalar(txt)
  if (!nzchar(txt)) return(0L)
  txt <- str_to_lower(txt)
  as.integer(any(str_detect(
    txt,
    "free sample|sample available|sample data|data sample|sample dataset|request sample|sample file|example dataset|preview|demo data"
  )))
}

has_free_sample_offer <- function(details_text, history_text, pricing_info_text, delivery) {
  collapse_any <- function(x) {
    vals <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
    vals <- vals[!is.na(vals)]
    clean_scalar(paste(vals, collapse = " "))
  }
  txt <- paste(
    collapse_any(details_text),
    collapse_any(history_text),
    collapse_any(pricing_info_text),
    paste(extract_delivery_text(delivery), collapse = " ")
  )
  if (!nzchar(str_squish(txt))) return(0L)
  txt <- str_to_lower(txt)
  as.integer(any(str_detect(
    txt,
    "free sample|sample available|sample data|data sample|sample dataset|request sample|sample file|example dataset|preview|demo data|free trial|trial access|sandbox"
  )))
}

# ---- Pricing ----
extract_starts_at <- function(pricing_plans) {
  vals <- character(0)
  if (is.null(pricing_plans) || length(pricing_plans) == 0) return(vals)
  flatten_local <- function(x) {
    if (is.null(x) || length(x) == 0) return(character(0))
    if (is.atomic(x)) return(as.character(x))
    if (is.data.frame(x)) return(as.character(unlist(x, use.names = FALSE)))
    if (is.list(x)) return(unlist(lapply(x, flatten_local), use.names = FALSE))
    as.character(x)
  }
  price_fields <- c("starts_at", "price", "amount", "cost", "value")
  collect_from_df <- function(df_obj) {
    out <- character(0)
    for (nm in intersect(names(df_obj), price_fields)) out <- c(out, as.character(df_obj[[nm]]))
    out
  }
  if (is.data.frame(pricing_plans)) {
    vals <- c(vals, collect_from_df(pricing_plans))
  } else if (is.list(pricing_plans)) {
    for (plan in pricing_plans) {
      if (is.data.frame(plan)) vals <- c(vals, collect_from_df(plan))
      else if (is.list(plan)) {
        for (nm in price_fields) {
          if (!is.null(plan[[nm]])) vals <- c(vals, flatten_local(plan[[nm]]))
        }
      }
    }
  }
  vals <- vals[!is.na(vals)]
  vals <- str_to_lower(str_squish(vals))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

contains_monetary_value <- function(values) {
  if (is.null(values) || length(values) == 0) return(0L)
  vals <- str_to_lower(str_squish(as.character(values)))
  vals <- vals[!is.na(vals)]
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(0L)
  non_price_markers <- c(
    "available", "not available", "contact for pricing",
    "contact us", "on request", "request pricing", "quote on request"
  )
  vals <- vals[!(vals %in% non_price_markers)]
  if (length(vals) == 0) return(0L)
  parsed_vals <- suppressWarnings(readr::parse_number(vals))
  as.integer(any(!is.na(parsed_vals)))
}

price_displayed_from_metadata <- function(pricing_plans, pricing_info = NULL) {
  starts <- extract_starts_at(pricing_plans)
  contains_monetary_value(starts)
}

payment_modality_displayed <- function(pricing_plans) {
  starts <- extract_starts_at(pricing_plans)
  if (length(starts) == 0) return(0L)
  non_price_markers <- c("not available")
  useful <- starts[!(starts %in% non_price_markers)]
  as.integer(length(useful) > 0)
}

# ---- Clarity ----
normalize_binary_col <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  if (is.logical(x)) return(ifelse(is.na(x), NA_integer_, as.integer(x)))
  vals <- str_to_lower(str_squish(as.character(x)))
  out <- rep(NA_integer_, length(vals))
  vals_num <- suppressWarnings(as.integer(vals))
  numeric_mask <- !is.na(vals_num) & vals_num %in% c(0L, 1L)
  out[numeric_mask] <- vals_num[numeric_mask]
  out[is.na(out) & vals %in% c("true", "t", "yes", "y")] <- 1L
  out[is.na(out) & vals %in% c("false", "f", "no", "n")] <- 0L
  out
}

# ---- Study 2 inference helpers ----

# Build a parsimonious multilabel use-case control set. Tags must occur in at
# least `min_share` of the provider-identified estimation sample. Exact duplicate
# indicator profiles (for example, synonymous tags always assigned together)
# are removed deterministically before estimation.
make_prevalent_use_case_controls <- function(df, min_share = 0.02) {
  stopifnot("use_cases" %in% names(df), min_share > 0, min_share < 1)

  min_n <- ceiling(min_share * nrow(df))
  uc_long <- df |>
    dplyr::mutate(.uc_row = dplyr::row_number()) |>
    tidyr::unnest(use_cases) |>
    dplyr::mutate(use_cases = stringr::str_to_lower(stringr::str_squish(use_cases))) |>
    dplyr::filter(nzchar(use_cases))

  retained <- uc_long |>
    dplyr::count(use_cases) |>
    dplyr::filter(n >= min_n) |>
    dplyr::arrange(use_cases)

  uc_wide <- uc_long |>
    dplyr::filter(use_cases %in% retained$use_cases) |>
    dplyr::mutate(value = 1L) |>
    tidyr::pivot_wider(
      id_cols = .uc_row,
      names_from = use_cases,
      values_from = value,
      values_fill = 0L,
      names_prefix = "uc_"
    )

  out <- df |>
    dplyr::mutate(.uc_row = dplyr::row_number()) |>
    dplyr::left_join(uc_wide, by = ".uc_row") |>
    dplyr::select(-.uc_row)

  uc_cols <- grep("^uc_", names(out), value = TRUE)
  out <- out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(uc_cols), ~tidyr::replace_na(., 0L)))

  # Remove exact duplicate columns without consulting either IDL or the outcome.
  profiles <- vapply(uc_cols, function(v) paste0(out[[v]], collapse = ""), character(1))
  duplicate_cols <- uc_cols[duplicated(profiles)]
  if (length(duplicate_cols)) {
    out <- out |> dplyr::select(-dplyr::all_of(duplicate_cols))
    uc_cols <- setdiff(uc_cols, duplicate_cols)
  }

  # Formula-safe names must be portable across operating-system locales. The
  # original, alphabetically ordered tags remain available in `tag_map`, while
  # regression columns use deterministic sequential ASCII identifiers.
  safe_names <- sprintf("uc_%03d", seq_along(uc_cols))
  tag_map <- data.frame(
    source_tag = sub("^uc_", "", uc_cols),
    column = safe_names,
    stringsAsFactors = FALSE
  )
  names(out)[match(uc_cols, names(out))] <- safe_names

  list(
    data = out,
    columns = safe_names,
    min_share = min_share,
    min_n = min_n,
    retained_tags = nrow(retained),
    duplicate_profiles_removed = sub("^uc_", "", duplicate_cols),
    tag_map = tag_map
  )
}

# Provider-clustered sandwich inference with the finite-cluster multiplier and
# t reference distribution using G - 1 degrees of freedom.
cluster_coeftable <- function(model, cluster, type = "HC1") {
  cluster <- as.character(cluster)
  vc <- sandwich::vcovCL(model, cluster = cluster, type = type, cadjust = TRUE)
  # vcovCL omits aliased coefficients; align estimates to its estimable rows.
  estimates <- stats::coef(model)[rownames(vc)]
  se <- sqrt(diag(vc))
  statistic <- estimates / se
  clusters <- length(unique(cluster[!is.na(cluster)]))
  df <- clusters - 1L
  p <- 2 * stats::pt(-abs(statistic), df = df)
  out <- cbind(
    Estimate = estimates,
    `Std. Error` = se,
    `t value` = statistic,
    `Pr(>|t|)` = p
  )
  attr(out, "vcov") <- vc
  attr(out, "cluster_df") <- df
  out
}

cluster_confint <- function(coeftable, level = 0.95) {
  critical <- stats::qt(1 - (1 - level) / 2, df = attr(coeftable, "cluster_df"))
  cbind(
    lower = coeftable[, "Estimate"] - critical * coeftable[, "Std. Error"],
    upper = coeftable[, "Estimate"] + critical * coeftable[, "Std. Error"]
  )
}
