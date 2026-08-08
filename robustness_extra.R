## Robustness extensions requested in revision:
##  (A) Productization-intensity / listing-completeness control (co-determination)
##  (B) Missing-rating sensitivity (drop rating-missing; alternative imputation)
## Replicates the exact dedup + variable pipeline of extract_results.R, then adds
## new controls/specifications. Prints a compact, paste-ready results block.

suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(stringr)
  library(MASS); library(sandwich)
})
`%||%` <- function(a,b) if(is.null(a)||length(a)==0) b else a
source("datarade_helpers.R")

data_path <- if (file.exists("updated_datarade_data_scored_copy.jsonl")) {
  "updated_datarade_data_scored_copy.jsonl"
} else {
  file.path("data", "updated_datarade_data_scored_copy.jsonl")
}
lines <- readLines(data_path, warn=FALSE)
raw <- lapply(lines, function(l) tryCatch(fromJSON(l), error=function(e) NULL))
raw <- raw[!sapply(raw,is.null)]
urls <- sapply(raw, function(r) r[["url"]] %||% NA_character_)
raw <- raw[!duplicated(urls)]   # same dedup as extract_results.R
provider_ids <- sapply(raw, function(r) {
  v <- r[["provider_url"]]
  if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
})
raw <- raw[!is.na(provider_ids) & nzchar(provider_ids)]

## ---- core variables (identical to extract_results.R) ----
df <- tibble(
  url = sapply(raw, function(r) r[["url"]] %||% NA_character_),
  provider_url = sapply(raw, function(r) { v<-r[["provider_url"]]; if(is.null(v)||length(v)==0) NA_character_ else as.character(v[1]) }),
  C_score = sapply(raw, function(r) as.numeric(r[["semantic_score"]] %||% NA_real_)),
  c_semantic = sapply(raw, function(r) as.integer(r[["semantic_pred"]] %||% NA_integer_)),
  t_rank = sapply(raw, function(r) temporal_rank_full(r[["delivery"]], r[["details"]], r[["history"]])),
  k_hard_key_count = sapply(raw, function(r) hard_key_count_from_dd(r[["data_dictionary"]])),
  payment_modality = sapply(raw, function(r) payment_modality_displayed(r[["pricing_plans"]])),
  price_displayed = sapply(raw, function(r) price_displayed_from_metadata(r[["pricing_plans"]])),
  free_sample_available = sapply(raw, function(r) has_free_sample_offer(r[["details"]], r[["history"]], r[["product-content__pricing-info"]], r[["delivery"]])),
  api_method = sapply(raw, function(r) has_api_method(r[["delivery"]])),
  provider_rating = sapply(raw, function(r) { v<-r[["provider__rating-summary-score"]]; if(is.null(v)||length(v)==0||!nzchar(as.character(v))) return(NA_real_); as.numeric(v[1]) }),
  use_cases = lapply(raw, function(r) { uc<-r[["use_cases"]]; if(is.null(uc)||length(uc)==0) return(character(0)); as.character(unlist(uc)) })
)
df <- df %>% mutate(
  T_rank = pmax(t_rank,0L), K_log = log1p(k_hard_key_count),
  across(c(payment_modality,price_displayed,free_sample_available,api_method), ~replace_na(.,0L)),
  liquidity_count = payment_modality + price_displayed + free_sample_available + api_method
)
df <- df %>% mutate(IDL_raw = scale(C_score)[,1] + scale(T_rank)[,1] + scale(K_log)[,1], IDL_std = as.numeric(scale(IDL_raw)))
prov_n <- df %>% count(provider_url, name="provider_n")
df <- left_join(df, prov_n, by="provider_url") %>%
  mutate(provider_n=replace_na(provider_n,1L), rating_missing=as.integer(is.na(provider_rating)), provider_rating_0=replace_na(provider_rating,0))

## ---- NEW (A): productization-intensity / listing-completeness measures ----
is_filled <- function(x) {
  if (is.null(x) || length(x)==0) return(FALSE)
  if (is.data.frame(x)) return(nrow(x)>0)
  if (is.list(x)) return(length(x)>0)
  v <- as.character(x); v <- v[!is.na(v)]; any(nzchar(str_squish(v)))
}
desc_len <- function(r) {
  txt <- as.character(unlist(r[["details"]], recursive=TRUE, use.names=FALSE))
  txt <- paste(txt[!is.na(txt)], collapse=" "); nchar(str_squish(txt))
}
# Count of populated *neutral* metadata fields (NOT the outcome sources pricing_plans/delivery,
# NOT the IDL-proxy sources details/data_dictionary): a generic "listing effort" index.
neutral_fields <- c("categories","suitable_company_sizes","geographical_coverage",
                    "dataset__fact","quality","history")
meta_count <- function(r) sum(vapply(neutral_fields, function(f) is_filled(r[[f]]), logical(1)))
dd_attr_n  <- function(r) length(extract_dd_field(r[["data_dictionary"]], "attribute"))

df$desc_len      <- sapply(raw, desc_len)
df$desc_len_log  <- log1p(df$desc_len)
df$meta_fields   <- sapply(raw, meta_count)
df$dd_attr_log   <- log1p(sapply(raw, dd_attr_n))
# standardized single productization index (mean of z-scores)
df$prod_index <- as.numeric(scale(scale(df$desc_len_log)[,1] + scale(df$meta_fields)[,1] + scale(df$dd_attr_log)[,1]))

## ---- parsimonious use-case controls (tags present in >=2% of listings) ----
uc_design <- make_prevalent_use_case_controls(df, min_share=0.02)
df <- uc_design$data
uc_cols <- uc_design$columns
UC <- paste(uc_cols, collapse=" + ")

cat("N =", nrow(df), " Providers =", n_distinct(df$provider_url), " UC =", length(uc_cols), "\n")
cat("\n=== Productization-control descriptives ===\n")
cat("desc_len: median=", median(df$desc_len), " mean=", round(mean(df$desc_len),1), "\n")
cat("meta_fields (0-6): mean=", round(mean(df$meta_fields),3), " sd=", round(sd(df$meta_fields),3), "\n")
cat("dd_attr count: median=", median(expm1(df$dd_attr_log)), " mean=", round(mean(expm1(df$dd_attr_log)),1), "\n")
cat("cor(IDL_std, prod_index)=", round(cor(df$IDL_std, df$prod_index),3), "\n")
cat("cor(IDL_std, desc_len_log)=", round(cor(df$IDL_std, df$desc_len_log),3), "\n")

irr_line <- function(m, var, label, cluster) {
  ct <- cluster_coeftable(m, cluster); ci <- cluster_confint(ct)
  b <- ct[var,"Estimate"]; se <- ct[var,"Std. Error"]
  p <- ct[var,"Pr(>|t|)"]
  cat(sprintf("%-26s b=%+.4f se=%.4f IRR=%.4f  CI=[%.3f,%.3f] p=%.4g\n",
              label, b, se, exp(b), exp(ci[var,"lower"]), exp(ci[var,"upper"]), p))
}

base_rhs <- "provider_n + provider_rating_0 + rating_missing"

cat("\n=== (0) Baseline replication (composite, no prod control) ===\n")
m0 <- glm(as.formula(paste("liquidity_count ~ IDL_std +", base_rhs, "+", UC)), data=df, family=poisson(link="log"))
irr_line(m0, "IDL_std", "IDL_std (baseline)", df$provider_url)

cat("\n=== (A1) Composite IDL + productization controls (prevalent use-case controls) ===\n")
mA1 <- glm(as.formula(paste("liquidity_count ~ IDL_std + desc_len_log + meta_fields + dd_attr_log +", base_rhs, "+", UC)), data=df, family=poisson(link="log"))
irr_line(mA1, "IDL_std", "IDL_std (+prod ctrls)", df$provider_url)
irr_line(mA1, "desc_len_log", "  desc_len_log", df$provider_url)
irr_line(mA1, "meta_fields", "  meta_fields", df$provider_url)
irr_line(mA1, "dd_attr_log", "  dd_attr_log", df$provider_url)

cat("\n=== (A1b) Composite IDL + single standardized productization index ===\n")
mA1b <- glm(as.formula(paste("liquidity_count ~ IDL_std + prod_index +", base_rhs, "+", UC)), data=df, family=poisson(link="log"))
irr_line(mA1b, "IDL_std", "IDL_std (+prod_index)", df$provider_url)
irr_line(mA1b, "prod_index", "  prod_index", df$provider_url)

cat("\n=== (A2) Composite IDL + prod controls + PROVIDER FE + use-case controls (most conservative; Table II.8 col 3) ===\n")
df_pfe <- df %>% group_by(provider_url) %>% filter(n() > 1, sum(liquidity_count) > 0) %>% ungroup()
mA2 <- glm(as.formula(paste("liquidity_count ~ IDL_std + desc_len_log + meta_fields + dd_attr_log +", UC, "+ factor(provider_url)")), data=df_pfe, family=poisson(link="log"))
irr_line(mA2, "IDL_std", "IDL_std (prodctrl+provFE)", df_pfe$provider_url)

cat("\n=== (A3) Disaggregated components + productization controls (prevalent use-case controls) ===\n")
mA3 <- glm(as.formula(paste("liquidity_count ~ C_score + T_rank + K_log + desc_len_log + meta_fields + dd_attr_log +", base_rhs, "+", UC)), data=df, family=poisson(link="log"))
for(v in c("C_score","T_rank","K_log")) irr_line(mA3, v, paste0("  ",v," (+prod ctrls)"), df$provider_url)

cat("\n=== (B) MISSING-RATING SENSITIVITY ===\n")
cat("rating_missing share =", round(mean(df$rating_missing),4), "  N with rating =", sum(df$rating_missing==0), "\n")

cat("\n--- (B-i) Drop rating-missing listings; main composite spec ---\n")
dfr <- df %>% filter(rating_missing==0)
mBi <- glm(as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating_0 +",
   "", paste(uc_cols, collapse=" + "))), data=dfr, family=poisson(link="log"))
irr_line(mBi, "IDL_std", paste0("IDL_std (rated only, N=", nrow(dfr), ")"), dfr$provider_url)

cat("\n--- (B-ii) Alternative imputation: mean-impute rating among observed (no indicator) ---\n")
mean_rating <- mean(df$provider_rating[df$rating_missing==0], na.rm=TRUE)
df$provider_rating_mean <- ifelse(df$rating_missing==1, mean_rating, df$provider_rating_0)
cat("mean observed rating used for imputation =", round(mean_rating,3), "\n")
mBii <- glm(as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating_mean +", UC)), data=df, family=poisson(link="log"))
irr_line(mBii, "IDL_std", "IDL_std (mean-imputed)", df$provider_url)

cat("\n--- (B-iii) Alternative imputation: median-impute rating + keep indicator ---\n")
med_rating <- median(df$provider_rating[df$rating_missing==0], na.rm=TRUE)
df$provider_rating_med <- ifelse(df$rating_missing==1, med_rating, df$provider_rating_0)
cat("median observed rating used for imputation =", round(med_rating,3), "\n")
mBiii <- glm(as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating_med + rating_missing +", UC)), data=df, family=poisson(link="log"))
irr_line(mBiii, "IDL_std", "IDL_std (median-imputed)", df$provider_url)

cat("\nDONE\n")
