suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(stringr)
  library(fixest); library(MASS)
})
`%||%` <- function(a,b) if(is.null(a)||length(a)==0) b else a
source("datarade_helpers.R")

data_path <- file.path("data", "updated_datarade_data_scored_copy.jsonl")
if (!file.exists(data_path)) data_path <- "updated_datarade_data_scored_copy.jsonl"
if (!file.exists(data_path)) stop("Data file not found: updated_datarade_data_scored_copy.jsonl")

lines <- readLines(data_path, warn=FALSE)
raw <- lapply(lines, function(l) tryCatch(fromJSON(l), error=function(e) NULL))
raw <- raw[!sapply(raw,is.null)]
urls <- sapply(raw, function(r) r[["url"]] %||% NA_character_)
raw <- raw[!duplicated(urls)]

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
  liquidity_count = payment_modality + price_displayed + free_sample_available + api_method,
  J_i = as.integer(c_semantic==1 & T_rank>=5 & k_hard_key_count>=1)
)
df <- df %>% mutate(IDL_raw = scale(C_score)[,1] + scale(T_rank)[,1] + scale(K_log)[,1], IDL_std = as.numeric(scale(IDL_raw)))
prov_n <- df %>% count(provider_url, name="provider_n")
df <- left_join(df, prov_n, by="provider_url") %>%
  mutate(provider_n=replace_na(provider_n,1L), rating_missing=as.integer(is.na(provider_rating)), provider_rating=replace_na(provider_rating,0))

uc_long <- df %>% mutate(row=row_number()) %>% unnest(use_cases) %>% mutate(use_cases=str_to_lower(str_squish(use_cases))) %>% filter(nzchar(use_cases))
uc_freq <- uc_long %>% count(use_cases) %>% filter(n>=2)
uc_wide <- uc_long %>% filter(use_cases %in% uc_freq[["use_cases"]]) %>% mutate(v=1L) %>% pivot_wider(id_cols=row, names_from=use_cases, values_from=v, values_fill=0L, names_prefix="uc_")
df <- df %>% mutate(row=row_number()) %>% left_join(uc_wide, by="row")
df$row <- NULL
uc_cols <- grep("^uc_", names(df), value=TRUE)
df <- df %>% mutate(across(all_of(uc_cols), ~replace_na(.,0L)))
safe_names <- make.names(uc_cols)
names(df)[names(df) %in% uc_cols] <- safe_names
uc_cols <- safe_names

cat("N =", nrow(df), ", Providers =", n_distinct(df[["provider_url"]]), ", UC =", length(uc_cols), "\n")

# Descriptive
cat("\n--- Descriptive ---\n")
for(v in c("C_score","T_rank","K_log","IDL_std","liquidity_count")) {
  cat(v, ": mean=", round(mean(df[[v]],na.rm=T),4), ", sd=", round(sd(df[[v]],na.rm=T),4),
      ", min=", round(min(df[[v]],na.rm=T),4), ", max=", round(max(df[[v]],na.rm=T),4), "\n")
}

# MAIN POISSON
cat("\n--- MAIN POISSON ---\n")
fml <- as.formula(paste0("liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing + ", paste(uc_cols, collapse=" + ")))
m <- fepois(fml, data=df, vcov="hetero")
ct <- coeftable(m)
b <- ct["IDL_std","Estimate"]; se <- ct["IDL_std","Std. Error"]
cat("IDL_std: b=", round(b,4), ", se=", round(se,4), ", z=", round(ct["IDL_std","z value"],4), ", p=", ct["IDL_std","Pr(>|z|)"], "\n")
cat("IRR=", round(exp(b),4), ", CI=[", round(exp(b-1.96*se),4), ",", round(exp(b+1.96*se),4), "]\n")
for(v in c("provider_n","provider_rating","rating_missing")) {
  cat(v, ": b=", round(ct[v,"Estimate"],4), ", se=", round(ct[v,"Std. Error"],4), ", p=", ct[v,"Pr(>|z|)"], "\n")
}
ll <- logLik(m); ll0 <- logLik(fepois(liquidity_count~1, data=df))
cat("Pseudo R2=", round(1-as.numeric(ll)/as.numeric(ll0),4), "\n")

# DISAGGREGATED
cat("\n--- DISAGGREGATED ---\n")
fml2 <- as.formula(paste0("liquidity_count ~ C_score + T_rank + K_log + provider_n + provider_rating + rating_missing + ", paste(uc_cols, collapse=" + ")))
m2 <- fepois(fml2, data=df, vcov="hetero")
ct2 <- coeftable(m2)
for(v in c("C_score","T_rank","K_log")) {
  cat(v, ": b=", round(ct2[v,"Estimate"],4), ", se=", round(ct2[v,"Std. Error"],4), ", IRR=", round(exp(ct2[v,"Estimate"]),4), ", p=", ct2[v,"Pr(>|z|)"], "\n")
}

# ROBUSTNESS
cat("\n--- ROBUSTNESS ---\n")
m_ols <- feols(fml, data=df, vcov="hetero")
ct_ols <- coeftable(m_ols)
cat("OLS: b=", round(ct_ols["IDL_std","Estimate"],4), ", se=", round(ct_ols["IDL_std","Std. Error"],4), ", p=", ct_ols["IDL_std","Pr(>|t|)"], "\n")

m_ol <- polr(ordered(liquidity_count) ~ IDL_std + provider_n + provider_rating + rating_missing, data=df, method="logistic", Hess=TRUE)
ol_c <- summary(m_ol)$coefficients; ol_t <- ol_c["IDL_std","t value"]
cat("Ordered Logit: b=", round(ol_c["IDL_std","Value"],4), ", se=", round(ol_c["IDL_std","Std. Error"],4), ", p=", 2*pnorm(-abs(ol_t)), "\n")

m_pfe <- fepois(liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing | provider_url, data=df, vcov="hetero")
ct_pfe <- coeftable(m_pfe)
cat("Provider FE: b=", round(ct_pfe["IDL_std","Estimate"],4), ", se=", round(ct_pfe["IDL_std","Std. Error"],4), ", p=", ct_pfe["IDL_std","Pr(>|z|)"], "\n")

m_nouc <- fepois(liquidity_count ~ IDL_std + provider_n + provider_rating + rating_missing, data=df, vcov="hetero")
ct_nouc <- coeftable(m_nouc)
cat("No UC: b=", round(ct_nouc["IDL_std","Estimate"],4), ", se=", round(ct_nouc["IDL_std","Std. Error"],4), ", p=", ct_nouc["IDL_std","Pr(>|z|)"], "\n")

fml_ji <- as.formula(paste0("liquidity_count ~ J_i + provider_n + provider_rating + rating_missing + ", paste(uc_cols, collapse=" + ")))
m_ji <- fepois(fml_ji, data=df, vcov="hetero")
ct_ji <- coeftable(m_ji)
cat("J_i: b=", round(ct_ji["J_i","Estimate"],4), ", se=", round(ct_ji["J_i","Std. Error"],4), ", IRR=", round(exp(ct_ji["J_i","Estimate"]),4), ", p=", ct_ji["J_i","Pr(>|z|)"], "\n")

df$IDL_high <- as.integer(df$IDL_std > 0)
fml_bin <- as.formula(paste0("liquidity_count ~ IDL_high + provider_n + provider_rating + rating_missing + ", paste(uc_cols, collapse=" + ")))
m_bin <- fepois(fml_bin, data=df, vcov="hetero")
ct_bin <- coeftable(m_bin)
cat("Above-median: b=", round(ct_bin["IDL_high","Estimate"],4), ", se=", round(ct_bin["IDL_high","Std. Error"],4), ", IRR=", round(exp(ct_bin["IDL_high","Estimate"]),4), ", p=", ct_bin["IDL_high","Pr(>|z|)"], "\n")

df$IDL_Q <- cut(df$IDL_std, breaks=quantile(df$IDL_std, probs=0:5/5, na.rm=TRUE), include.lowest=TRUE, labels=paste0("Q",1:5))
df$IDL_Q2 <- relevel(factor(df$IDL_Q), ref="Q1")
fml_q <- as.formula(paste0("liquidity_count ~ IDL_Q2 + provider_n + provider_rating + rating_missing + ", paste(uc_cols, collapse=" + ")))
m_q <- fepois(fml_q, data=df, vcov="hetero")
ct_q <- coeftable(m_q)
for(qq in paste0("IDL_Q2Q",2:5)) {
  cat(qq, ": b=", round(ct_q[qq,"Estimate"],4), ", se=", round(ct_q[qq,"Std. Error"],4), ", IRR=", round(exp(ct_q[qq,"Estimate"]),4), ", p=", ct_q[qq,"Pr(>|z|)"], "\n")
}

# Quintile means
cat("\n--- Quintile means ---\n")
q_m <- df %>% filter(!is.na(IDL_Q)) %>% group_by(IDL_Q) %>% summarise(mean_liq=round(mean(liquidity_count),3), n=n(), .groups="drop")
print(q_m)

cat("\n--- Outcome prevalences ---\n")
cat("payment_modality:", round(mean(df$payment_modality),4), "\n")
cat("price_displayed:", round(mean(df$price_displayed),4), "\n")
cat("free_sample:", round(mean(df$free_sample_available),4), "\n")
cat("api_method:", round(mean(df$api_method),4), "\n")

# C_score distribution info
cat("\n--- C_score percentiles ---\n")
cat("5th:", round(quantile(df$C_score, 0.05, na.rm=TRUE),4), "\n")
cat("25th:", round(quantile(df$C_score, 0.25, na.rm=TRUE),4), "\n")
cat("50th:", round(quantile(df$C_score, 0.50, na.rm=TRUE),4), "\n")
cat("75th:", round(quantile(df$C_score, 0.75, na.rm=TRUE),4), "\n")
cat("95th:", round(quantile(df$C_score, 0.95, na.rm=TRUE),4), "\n")
cat("c_semantic share:", round(mean(df$c_semantic, na.rm=TRUE),4), "\n")
