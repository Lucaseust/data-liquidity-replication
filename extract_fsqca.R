library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(QCA)
library(MASS)
library(sandwich)
library(clubSandwich)

# ── Load (same logic as Rmd) ──
source("datarade_helpers.R")

data_path <- if (file.exists("updated_datarade_data_scored_copy.jsonl")) {
  "updated_datarade_data_scored_copy.jsonl"
} else {
  file.path("data", "updated_datarade_data_scored_copy.jsonl")
}
lines <- readLines(data_path, warn = FALSE)
raw   <- lapply(lines, function(l) tryCatch(fromJSON(l), error = function(e) NULL))
raw   <- raw[!sapply(raw, is.null)]
urls  <- sapply(raw, function(r) r[["url"]] %||% NA_character_)
raw   <- raw[!duplicated(urls)]
provider_ids <- sapply(raw, function(r) {
  v <- r[["provider_url"]]
  if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
})
raw <- raw[!is.na(provider_ids) & nzchar(provider_ids)]
urls <- sapply(raw, function(r) r[["url"]] %||% NA_character_)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

df <- tibble(
  url = sapply(raw, function(r) r[["url"]] %||% NA_character_),
  provider_url = sapply(raw, function(r) {
    v <- r[["provider_url"]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
  }),
  C_score   = sapply(raw, function(r) as.numeric(r[["semantic_score"]] %||% NA_real_)),
  c_semantic = sapply(raw, function(r) as.integer(r[["semantic_pred"]] %||% NA_integer_)),
  t_rank = sapply(raw, function(r)
    temporal_rank_full(r[["delivery"]], r[["details"]], r[["history"]])),
  k_hard_key_count = sapply(raw, function(r)
    hard_key_count_from_dd(r[["data_dictionary"]])),
  payment_modality      = sapply(raw, function(r)
    payment_modality_displayed(r[["pricing_plans"]])),
  price_displayed       = sapply(raw, function(r)
    price_displayed_from_metadata(r[["pricing_plans"]])),
  free_sample_available = sapply(raw, function(r)
    has_free_sample_offer(r[["details"]], r[["history"]],
                          r[["product-content__pricing-info"]], r[["delivery"]])),
  api_method            = sapply(raw, function(r) has_api_method(r[["delivery"]])),
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
)

df <- df %>% mutate(
  T_rank   = pmax(t_rank, 0L),
  K_log    = log1p(k_hard_key_count),
  across(c(payment_modality, price_displayed, free_sample_available, api_method),
         ~replace_na(., 0L)),
  liquidity_count = payment_modality + price_displayed +
    free_sample_available + api_method
)
df <- df %>% mutate(
  IDL_raw = scale(C_score)[,1] + scale(T_rank)[,1] + scale(K_log)[,1],
  IDL_std = as.numeric(scale(IDL_raw))
)
prov_n <- df %>% count(provider_url, name = "provider_n")
df <- left_join(df, prov_n, by = "provider_url") %>%
  mutate(
    rating_missing  = as.integer(is.na(provider_rating)),
    provider_rating = replace_na(provider_rating, 0)
  )

cat("N =", nrow(df), "\n\n")

# ── Fuzzy calibration ──
df$fs_C_new <- calibrate(df$C_score, type = "fuzzy", thresholds = c(0.30, 0.52, 0.73))
df$fs_T     <- calibrate(df$T_rank,  type = "fuzzy", thresholds = c(0, 4, 9))
df$fs_K     <- calibrate(df$k_hard_key_count, type = "fuzzy", thresholds = c(0, 1, 5))
df$fs_OUT   <- calibrate(df$liquidity_count,  type = "fuzzy", thresholds = c(0, 2, 4))

# Nudge 0.5 → 0.501
nudge <- function(x) ifelse(x == 0.5, x + 0.001, x)
df$fs_C_new <- nudge(df$fs_C_new)
df$fs_T     <- nudge(df$fs_T)
df$fs_K     <- nudge(df$fs_K)
df$fs_OUT   <- nudge(df$fs_OUT)

# Calibration summary
cat("=== Calibration Summary ===\n")
cat(sprintf("fs_C mean=%.3f, sd=%.3f\n", mean(df$fs_C_new), sd(df$fs_C_new)))
cat(sprintf("fs_T mean=%.3f, sd=%.3f\n", mean(df$fs_T), sd(df$fs_T)))
cat(sprintf("fs_K mean=%.3f, sd=%.3f\n", mean(df$fs_K), sd(df$fs_K)))
cat(sprintf("fs_OUT mean=%.3f, sd=%.3f\n\n", mean(df$fs_OUT), sd(df$fs_OUT)))

# ── Necessity Analysis ──
cat("=== Necessity Analysis ===\n")
nec_df <- data.frame(
  C     = df$fs_C_new,
  notC  = 1 - df$fs_C_new,
  T     = df$fs_T,
  notT  = 1 - df$fs_T,
  K     = df$fs_K,
  notK  = 1 - df$fs_K,
  CorT  = pmax(df$fs_C_new, df$fs_T),
  CorK  = pmax(df$fs_C_new, df$fs_K),
  TorK  = pmax(df$fs_T, df$fs_K),
  CorTorK = pmax(df$fs_C_new, df$fs_T, df$fs_K),
  OUT   = df$fs_OUT
)
res_nec <- pof(setms = nec_df[, 1:10], outcome = "OUT", data = nec_df, relation = "necessity")
nec_out <- res_nec$incl.cov
cat("Expression | Consistency | Coverage\n")
for (i in seq_len(nrow(nec_out))) {
  cat(sprintf("%-10s | %.4f      | %.4f\n",
              rownames(nec_out)[i], nec_out[i, "inclN"], nec_out[i, "covN"]))
}

# ── Sufficiency Analysis ──
cat("\n=== Sufficiency Analysis ===\n")
configs <- list(
  "C*T*K" = pmin(df$fs_C_new, df$fs_T, df$fs_K),
  "C*T"   = pmin(df$fs_C_new, df$fs_T),
  "C*K"   = pmin(df$fs_C_new, df$fs_K),
  "T*K"   = pmin(df$fs_T, df$fs_K),
  "C"     = df$fs_C_new,
  "T"     = df$fs_T,
  "K"     = df$fs_K
)
cat("Configuration | Consistency | Coverage\n")
for (nm in names(configs)) {
  s <- configs[[nm]]
  cons <- sum(pmin(s, df$fs_OUT)) / sum(s)
  cov  <- sum(pmin(s, df$fs_OUT)) / sum(df$fs_OUT)
  cat(sprintf("%-12s  | %.4f      | %.4f\n", nm, cons, cov))
}

# ── Truth Table ──
cat("\n=== Truth Table ===\n")
qca_new <- data.frame(C = df$fs_C_new, T = df$fs_T, K = df$fs_K, OUT = df$fs_OUT)
tt_new <- truthTable(qca_new, outcome = "OUT", conditions = c("C", "T", "K"),
                     incl.cut = 0.80, n.cut = 30,
                     sort.by = c("incl", "n"), complete = FALSE)
print(tt_new)

# ── Minimisation ──
cat("\n=== Minimisation (Parsimonious Solution) ===\n")
sol_new <- tryCatch(minimize(tt_new, details = TRUE), error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})
if (!is.null(sol_new)) print(sol_new)

# ── OLS and Ordered Logit (for main spec table) ──
cat("\n=== OLS Regression ===\n")
# Build parsimonious use-case controls (tags present in at least 2% of listings).
uc_design <- make_prevalent_use_case_controls(df, min_share = 0.02)
df <- uc_design$data
uc_keep <- uc_design$columns

fml <- as.formula(paste("liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing +",
                        paste(uc_keep, collapse = " + ")))

# Main Poisson
cat("\n=== Main Poisson PML ===\n")
m_pois <- glm(fml, data = df, family = poisson(link = "log"))
ct_pois <- cluster_coeftable(m_pois, df$provider_url)
cat("Coeftable columns:", paste(colnames(ct_pois), collapse=", "), "\n")
p_col <- ncol(ct_pois)  # last column is p-value
cat(sprintf("Poisson: b=%.4f, se=%.4f, IRR=%.4f, p=%.6f\n",
            coef(m_pois)["IDL_std"], ct_pois["IDL_std","Std. Error"],
            exp(coef(m_pois)["IDL_std"]), ct_pois["IDL_std", p_col]))
ci <- cluster_confint(ct_pois)["IDL_std",]
cat(sprintf("95%% CI for IRR: [%.4f, %.4f]\n", exp(ci[1]), exp(ci[2])))
null_pois <- glm(liquidity_count ~ 1, data = df, family = poisson(link = "log"))
cat(sprintf("Pseudo R2: %.4f\n", 1 - as.numeric(logLik(m_pois)) / as.numeric(logLik(null_pois))))

# Disaggregated Poisson
cat("\n=== Disaggregated Poisson ===\n")
fml_dis <- as.formula(paste("liquidity_count ~ C_score + T_rank + K_log + provider_n + provider_rating + rating_missing +",
                            paste(uc_keep, collapse = " + ")))
m_dis <- glm(fml_dis, data = df, family = poisson(link = "log"))
ct_dis <- cluster_coeftable(m_dis, df$provider_url)
p_col <- ncol(ct_dis)
for (v in c("C_score", "T_rank", "K_log")) {
  cat(sprintf("  %s: b=%.4f, se=%.4f, IRR=%.4f, p=%.6f\n", v,
              coef(m_dis)[v], ct_dis[v,"Std. Error"],
              exp(coef(m_dis)[v]), ct_dis[v, p_col]))
}

m_ols <- lm(fml, data = df)
ct_ols <- cluster_coeftable(m_ols, df$provider_url)
cat(sprintf("OLS: b=%.4f, se=%.4f, p=%.6f\n", coef(m_ols)["IDL_std"],
            ct_ols["IDL_std","Std. Error"], ct_ols["IDL_std", "Pr(>|t|)"]))

cat("\n=== Ordered Logit ===\n")
library(MASS)
df$Y_ord <- factor(df$liquidity_count, ordered = TRUE)
m_ologit <- polr(Y_ord ~ IDL_std + provider_n + provider_rating + rating_missing,
                 data = df, Hess = TRUE, method = "logistic")
ct_ol <- cluster_coeftable(m_ologit, df$provider_url)
b_ol <- ct_ol["IDL_std", "Estimate"]
se_ol <- ct_ol["IDL_std", "Std. Error"]
p_ol <- ct_ol["IDL_std", "Pr(>|t|)"]
cat(sprintf("OLogit: b=%.4f, se=%.4f, p=%.6f, OR=%.4f\n", b_ol, se_ol, p_ol, exp(b_ol)))

# Provider FE Poisson
cat("\n=== Provider FE Poisson ===\n")
df_fe <- df %>% group_by(provider_url) %>% filter(n() > 1, sum(liquidity_count) > 0) %>% ungroup()
fml_fe <- as.formula(paste("liquidity_count ~ IDL_std +",
                           paste(uc_keep, collapse = " + "), "+ factor(provider_url)"))
m_fe <- tryCatch(glm(fml_fe, data = df_fe, family = poisson(link = "log")), error = function(e) NULL)
if (!is.null(m_fe)) {
  ct_fe <- cluster_coeftable(m_fe, df_fe$provider_url)
  p_col_fe <- ncol(ct_fe)
  cat(sprintf("FE Poisson: b=%.4f, se=%.4f, IRR=%.4f, p=%.6f\n",
              coef(m_fe)["IDL_std"], ct_fe["IDL_std","Std. Error"],
              exp(coef(m_fe)["IDL_std"]), ct_fe["IDL_std", p_col_fe]))
  cr2 <- clubSandwich::coef_test(m_fe, vcov = "CR2", cluster = df_fe$provider_url,
                                 test = "Satterthwaite")
  print(cr2[rownames(cr2) == "IDL_std", ])
}

# Quintile model
cat("\n=== Quintile Model ===\n")
df$IDL_Q <- cut(df$IDL_std, breaks = quantile(df$IDL_std, probs = 0:5/5),
                include.lowest = TRUE, labels = paste0("Q", 1:5))
fml_q <- as.formula(paste("liquidity_count ~ IDL_Q + provider_n + provider_rating + rating_missing +",
                          paste(uc_keep, collapse = " + ")))
m_q <- glm(fml_q, data = df, family = poisson(link = "log"))
cat("Quintile IRRs:\n")
ct_q <- cluster_coeftable(m_q, df$provider_url)
p_col_q <- ncol(ct_q)
for (q in paste0("IDL_QQ", 2:5)) {
  cat(sprintf("  %s: b=%.4f, IRR=%.4f, p=%.6f\n", q,
              coef(m_q)[q], exp(coef(m_q)[q]), ct_q[q, p_col_q]))
}
q_means <- tapply(df$liquidity_count, df$IDL_Q, mean)
cat("Quintile means:\n")
for (q in names(q_means)) cat(sprintf("  %s: %.2f\n", q, q_means[q]))
