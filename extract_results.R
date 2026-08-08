suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(stringr)
  library(MASS); library(sandwich); library(clubSandwich)
})
source("datarade_helpers.R")

data_path <- if (file.exists("updated_datarade_data_scored_copy.jsonl")) {
  "updated_datarade_data_scored_copy.jsonl"
} else {
  file.path("data", "updated_datarade_data_scored_copy.jsonl")
}
lines <- readLines(data_path, warn = FALSE)
raw <- lapply(lines, function(l) tryCatch(fromJSON(l), error = function(e) NULL))
raw <- raw[!sapply(raw, is.null)]
urls <- sapply(raw, function(r) r[["url"]] %||% NA_character_)
raw <- raw[!duplicated(urls)]

df <- tibble(
  url = sapply(raw, function(r) r[["url"]] %||% NA_character_),
  provider_url = sapply(raw, function(r) {
    v <- r[["provider_url"]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
  }),
  C_score = sapply(raw, function(r) as.numeric(r[["semantic_score"]] %||% NA_real_)),
  c_semantic = sapply(raw, function(r) as.integer(r[["semantic_pred"]] %||% NA_integer_)),
  t_rank = sapply(raw, function(r) temporal_rank_full(r[["delivery"]], r[["details"]], r[["history"]])),
  k_hard_key_count = sapply(raw, function(r) hard_key_count_from_dd(r[["data_dictionary"]])),
  payment_modality = sapply(raw, function(r) payment_modality_displayed(r[["pricing_plans"]])),
  price_displayed = sapply(raw, function(r) price_displayed_from_metadata(r[["pricing_plans"]])),
  free_sample_available = sapply(raw, function(r) has_free_sample_offer(
    r[["details"]], r[["history"]], r[["product-content__pricing-info"]], r[["delivery"]]
  )),
  api_method = sapply(raw, function(r) has_api_method(r[["delivery"]])),
  provider_rating = sapply(raw, function(r) {
    v <- r[["provider__rating-summary-score"]]
    if (is.null(v) || length(v) == 0 || !nzchar(as.character(v))) return(NA_real_)
    as.numeric(v[1])
  }),
  use_cases = lapply(raw, function(r) {
    uc <- r[["use_cases"]]
    if (is.null(uc) || length(uc) == 0) return(character(0))
    as.character(unlist(uc))
  })
) %>%
  filter(!is.na(provider_url), nzchar(provider_url)) %>%
  mutate(
    T_rank = pmax(t_rank, 0L),
    K_log = log1p(k_hard_key_count),
    across(c(payment_modality, price_displayed, free_sample_available, api_method),
           ~replace_na(., 0L)),
    liquidity_count = payment_modality + price_displayed + free_sample_available + api_method,
    J_i = as.integer(c_semantic == 1 & T_rank >= 5 & k_hard_key_count >= 1),
    IDL_raw = scale(C_score)[,1] + scale(T_rank)[,1] + scale(K_log)[,1],
    IDL_std = as.numeric(scale(IDL_raw))
  )

prov_n <- df %>% count(provider_url, name = "provider_n")
df <- left_join(df, prov_n, by = "provider_url") %>%
  mutate(
    rating_missing = as.integer(is.na(provider_rating)),
    provider_rating = replace_na(provider_rating, 0)
  )
df_base <- df

uc_design <- make_prevalent_use_case_controls(df, min_share = 0.02)
df <- uc_design$data
uc_cols <- uc_design$columns
UC <- paste(uc_cols, collapse = " + ")

cat("N =", nrow(df), ", provider clusters =", n_distinct(df$provider_url),
    ", prevalent use-case controls =", length(uc_cols),
    "(minimum", uc_design$min_n, "listings)\n")

report <- function(label, model, cluster, term, exponentiate = TRUE) {
  ct <- cluster_coeftable(model, cluster)
  ci <- cluster_confint(ct)
  b <- ct[term, "Estimate"]
  se <- ct[term, "Std. Error"]
  p <- ct[term, "Pr(>|t|)"]
  if (exponentiate) {
    cat(sprintf("%-28s b=%+.4f  clustered SE=%.4f  ratio=%.4f  95%% CI=[%.3f, %.3f]  p=%.4g  N=%d\n",
                label, b, se, exp(b), exp(ci[term,"lower"]), exp(ci[term,"upper"]), p, nobs(model)))
  } else {
    cat(sprintf("%-28s b=%+.4f  clustered SE=%.4f  95%% CI=[%.3f, %.3f]  p=%.4g  N=%d\n",
                label, b, se, ci[term,"lower"], ci[term,"upper"], p, nobs(model)))
  }
}

fml <- as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing +", UC))
m <- glm(fml, data = df, family = poisson(link = "log"))
cat("\n--- MAIN POISSON: PROVIDER-CLUSTERED ---\n")
report("IDL_std", m, df$provider_url, "IDL_std")

fml2 <- as.formula(paste("liquidity_count ~ C_score + T_rank + K_log + provider_n + provider_rating + rating_missing +", UC))
m2 <- glm(fml2, data = df, family = poisson(link = "log"))
cat("\n--- DISAGGREGATED: PROVIDER-CLUSTERED ---\n")
for (v in c("C_score", "T_rank", "K_log")) report(v, m2, df$provider_url, v)

cat("\n--- USE-CASE THRESHOLD SENSITIVITY ---\n")
for (threshold in c(0.01, 0.015, 0.02, 0.03, 0.05)) {
  design <- make_prevalent_use_case_controls(df_base, min_share = threshold)
  d <- design$data
  f <- as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing +",
                        paste(design$columns, collapse = " + ")))
  mt <- glm(f, data = d, family = poisson(link = "log"))
  label <- sprintf("Threshold %.1f%% (%d controls)", 100 * threshold, length(design$columns))
  report(label, mt, d$provider_url, "IDL_std")
}

cat("\n--- ALTERNATIVE SPECIFICATIONS ---\n")
m_ols <- lm(fml, data = df)
report("OLS", m_ols, df$provider_url, "IDL_std", exponentiate = FALSE)

m_ol <- polr(ordered(liquidity_count) ~ IDL_std + provider_n + provider_rating + rating_missing,
             data = df, method = "logistic", Hess = TRUE)
report("Ordered logit", m_ol, df$provider_url, "IDL_std")

df_pfe <- df %>% group_by(provider_url) %>%
  filter(n() > 1, sum(liquidity_count) > 0) %>% ungroup()
fml_pfe <- as.formula(paste("liquidity_count ~ IDL_std +", UC, "+ factor(provider_url)"))
m_pfe <- glm(fml_pfe, data = df_pfe, family = poisson(link = "log"))
report("Provider FE Poisson", m_pfe, df_pfe$provider_url, "IDL_std")

cr2 <- coef_test(m_pfe, vcov = "CR2", cluster = df_pfe$provider_url, test = "Satterthwaite")
cr2_idl <- cr2[rownames(cr2) == "IDL_std", ]
cat(sprintf("Provider FE CR2/Satt.       b=%+.4f  CR2 SE=%.4f  df=%.1f  p=%.4g\n",
            cr2_idl$beta, cr2_idl$SE, cr2_idl$df_Satt, cr2_idl$p_Satt))

m_nouc <- glm(liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing,
              data = df, family = poisson(link = "log"))
report("No use-case controls", m_nouc, df$provider_url, "IDL_std")

fml_ji <- as.formula(paste("liquidity_count ~ J_i + provider_n + provider_rating + rating_missing +", UC))
m_ji <- glm(fml_ji, data = df, family = poisson(link = "log"))
report("Legacy J_i", m_ji, df$provider_url, "J_i")

df$IDL_high <- as.integer(df$IDL_std > 0)
fml_bin <- as.formula(paste("liquidity_count ~ IDL_high + provider_n + provider_rating + rating_missing +", UC))
m_bin <- glm(fml_bin, data = df, family = poisson(link = "log"))
report("Above-median IDL", m_bin, df$provider_url, "IDL_high")

df$IDL_Q <- cut(df$IDL_std, breaks = quantile(df$IDL_std, probs = 0:5/5),
                include.lowest = TRUE, labels = paste0("Q", 1:5))
df$IDL_Q2 <- relevel(factor(df$IDL_Q), ref = "Q1")
fml_q <- as.formula(paste("liquidity_count ~ IDL_Q2 + provider_n + provider_rating + rating_missing +", UC))
m_q <- glm(fml_q, data = df, family = poisson(link = "log"))
for (v in paste0("IDL_Q2Q", 2:5)) report(v, m_q, df$provider_url, v)
