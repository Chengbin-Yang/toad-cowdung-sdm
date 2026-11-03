# ARCHIVE  adds source-group random effect, assumption tests, ANCOVA for initial egg number, quasi-binomial model for the day-60 rate

# ==============================================================================
# Cow Dung Frog Experiment — Analysis v2
# 相对 data_FIGURE4_revised.R 的增量（对应 AB2 批注）
#   #34 卵来源地（5 个采集点）作为分层因子/随机效应纳入
#   #35 全部模型的假设检验（正态性 / 方差齐性 / PH）与样本量
#   #40 主模型统一：连续浓度为唯一主模型，分类模型降为补充
#   #70 初始卵数不平衡：主分析改 ANCOVA（以 init_eggs 为协变量）
#   比例数据（Day-60 幼蛙率）改拟二项 GLM
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(DescTools); library(patchwork); library(survival)
})

file_path  <- "../../data/01_experiment_daily_records.xlsx"
output_dir <- "../../results/fig4_v2"
fig_dir    <- file.path(output_dir, "Figures")
tab_dir    <- file.path(output_dir, "Tables")
for (d in c(output_dir, fig_dir, tab_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

END_DAY <- 60; ALPHA <- 0.05; DUNNETT_ALTERNATIVE <- "two.sided"
group_order <- c("Control", "Low", "Medium", "High", "Ultra-High")
concentration_lookup <- c(Control = 0, Low = 0.8, Medium = 1.2, High = 2.5, `Ultra-High` = 3.5)

# ------------------------------- helpers --------------------------------------
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[,，]", "")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x[x %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(x))
}
first_positive_day <- function(day, value) { i <- which(!is.na(value) & value > 0); if (!length(i)) NA_real_ else min(day[i], na.rm = TRUE) }
first_true_day     <- function(day, cond)  { i <- which(!is.na(cond) & cond);          if (!length(i)) NA_real_ else min(day[i], na.rm = TRUE) }
fmt_p <- function(p) ifelse(is.na(p), NA_character_, ifelse(p < 1e-4, formatC(p, format = "e", digits = 2), formatC(p, digits = 3, format = "f")))

# ------------------------------- 1. read --------------------------------------
all_sheets <- excel_sheets(file_path)
day_sheets <- all_sheets[str_detect(all_sheets, "^Day-[0-9]+$")]
safe_read <- function(sheet_name) {
  tryCatch({
    raw <- read_excel(file_path, sheet = sheet_name, range = cell_cols(1:8),
                      col_names = TRUE, col_types = "text")
    names(raw)[1:8] <- c("sample_id","eggs","tadpoles","limbs","froglets","adults","alive_recorded","dead")
    raw %>% mutate(day = as.numeric(str_remove(sheet_name, "^Day-")))
  }, error = function(e) { warning("skip ", sheet_name, ": ", e$message); NULL })
}
count_columns <- c("eggs","tadpoles","limbs","froglets","adults","alive_recorded","dead")

combined <- map(day_sheets, safe_read) %>% compact() %>% bind_rows() %>%
  mutate(sample_id = str_trim(as.character(sample_id)),
         across(all_of(count_columns), clean_numeric)) %>%
  filter(!is.na(sample_id), sample_id != "") %>%
  mutate(group_code = substr(sample_id, 1, 1),
         group_name = case_when(group_code == "D" ~ "Control", group_code == "E" ~ "Low",
                                group_code == "F" ~ "Medium", group_code == "G" ~ "High",
                                group_code == "H" ~ "Ultra-High", TRUE ~ NA_character_),
         group = factor(group_name, levels = group_order),
         concentration = unname(concentration_lookup[group_name]),
         replicate = clean_numeric(str_extract(sample_id, "[0-9]+"))) %>%
  filter(!is.na(group)) %>%
  # 空缺阶段计数 = 结构性零
  mutate(across(all_of(count_columns), ~ replace_na(.x, 0))) %>%
  arrange(sample_id, day)

# 卵来源地（主文 2.2.1：每处理 15 桶，1-3 河山沟1 / 4-6 河山沟2 / 7-9 三道沟 / 10-12 春化1 / 13-15 春化2）
origin_levels <- c("Heshangou1","Heshangou2","Sandaogou","Chunhua1","Chunhua2")
combined <- combined %>%
  mutate(origin = factor(case_when(replicate <= 3 ~ "Heshangou1", replicate <= 6 ~ "Heshangou2",
                                   replicate <= 9 ~ "Sandaogou", replicate <= 12 ~ "Chunhua1",
                                   TRUE ~ "Chunhua2"), levels = origin_levels))

dup <- combined %>% count(sample_id, day) %>% filter(n > 1)
if (nrow(dup)) stop("Duplicate sample-day rows detected.")

# ------------------------------- 2. derive ------------------------------------
day0 <- combined %>% filter(day == 0) %>%
  transmute(sample_id, group, origin, concentration, replicate, init_eggs = eggs) %>%
  distinct(sample_id, .keep_all = TRUE)

combined <- combined %>% left_join(day0 %>% select(sample_id, init_eggs), by = "sample_id") %>%
  group_by(sample_id) %>% arrange(day, .by_group = TRUE) %>%
  mutate(cumulative_dead = cumsum(dead)) %>% ungroup()

event_times <- combined %>%
  group_by(sample_id, group, origin, concentration, replicate, init_eggs) %>%
  summarise(first_tadpole = first_positive_day(day, tadpoles),
            first_limb    = first_positive_day(day, limbs),
            first_froglet = first_positive_day(day, froglets),
            all_dead_day  = first_true_day(day, cumulative_dead >= init_eggs),
            .groups = "drop") %>%
  mutate(froglet_event = if_else(!is.na(first_froglet) & (is.na(all_dead_day) | first_froglet <= all_dead_day), 1L, 0L),
         followup_time = case_when(froglet_event == 1L ~ first_froglet,
                                   !is.na(all_dead_day) ~ pmin(all_dead_day, END_DAY),
                                   TRUE ~ END_DAY),
         event_status = case_when(froglet_event == 1L ~ "First froglet",
                                  !is.na(all_dead_day) & all_dead_day <= END_DAY ~ "Complete mortality",
                                  TRUE ~ "Censored at day 60"),
         tadpole_to_limb = if_else(!is.na(first_tadpole) & !is.na(first_limb), first_limb - first_tadpole, NA_real_),
         limb_to_froglet = if_else(!is.na(first_limb) & !is.na(first_froglet), first_froglet - first_limb, NA_real_))

timing_observed <- event_times %>% filter(froglet_event == 1L)

d60 <- combined %>% filter(day == END_DAY) %>%
  transmute(sample_id, group, origin, concentration, init_eggs, eggs, tadpoles, limbs, froglets, adults,
            final_stage_count = eggs + tadpoles + limbs + froglets + adults,
            froglet_rate = froglets / init_eggs,
            survival_rate = final_stage_count / init_eggs) %>%
  mutate(succ = round(froglet_rate * init_eggs), fail = init_eggs - succ)

death_summary <- combined %>% group_by(sample_id) %>% summarise(total_dead = sum(dead, na.rm = TRUE), .groups = "drop")
balance_check <- d60 %>% left_join(death_summary, by = "sample_id") %>%
  mutate(balance_difference = init_eggs - final_stage_count - total_dead,
         balanced = abs(balance_difference) < 1e-8)
write.csv(balance_check, file.path(tab_dir, "S0_data_check_day60_balance.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(day0,          file.path(tab_dir, "S0_design_day0.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ==============================================================================
# 3. 假设检验（#35）
# ==============================================================================
shapiro_tab <- function(dat, var) {
  dat %>% group_by(group) %>%
    summarise(n = sum(!is.na(.data[[var]])),
              W = shapiro.test(.data[[var]])$statistic,
              p_shapiro = shapiro.test(.data[[var]])$p.value, .groups = "drop") %>%
    mutate(outcome = var, .before = 1)
}
assumptions <- bind_rows(
  shapiro_tab(timing_observed, "first_froglet") %>% mutate(outcome = "First froglet (days)"),
  shapiro_tab(d60, "froglet_rate")             %>% mutate(outcome = "Day-60 froglet rate")
)
levene_t <- function(dat, f, label) {
  lt <- LeveneTest(as.formula(paste(f, "~ group")), data = dat)
  bt <- bartlett.test(as.formula(paste(f, "~ group")), data = dat)
  tibble(outcome = label, test = c("Levene (median-centred)", "Bartlett"),
         statistic = c(lt[["F value"]][1], unname(bt$statistic)),
         df1 = c(lt$Df[1], unname(bt$parameter)), df2 = c(lt$Df[2], NA),
         p = c(lt[["Pr(>F)"]][1], bt$p.value))
}
variance_tests <- bind_rows(
  levene_t(timing_observed, "first_froglet", "First froglet (days)"),
  levene_t(d60, "froglet_rate", "Day-60 froglet rate")
)
m_anova_raw <- aov(first_froglet ~ group, data = timing_observed)
resid_norm <- tibble(outcome = "Residuals of one-way ANOVA (first froglet)",
                     test = "Shapiro-Wilk", statistic = shapiro.test(resid(m_anova_raw))$statistic,
                     df1 = NA, df2 = NA, p = shapiro.test(resid(m_anova_raw))$p.value)

# ==============================================================================
# 4. 初始卵数平衡（#70）+ ANCOVA 主分析
# ==============================================================================
init_egg_tab <- broom_like <- NULL
mm <- lm(init_eggs ~ group, data = day0); a_init <- anova(mm)
init_egg_anova <- tibble(outcome = "Initial egg count", test = "One-way ANOVA",
                         df1 = a_init$Df[1], df2 = a_init$Residuals[1],
                         statistic = a_init$`F value`[1], p = a_init$`Pr(>F)`[1])

anc <- aov(first_froglet ~ group + init_eggs, data = timing_observed)
anc_tab <- as_tibble(summary(anc)[[1]], rownames = "term") %>%
  transmute(term, df = Df, F = `F value`, p = `Pr(>F)`) %>% mutate(outcome = "First froglet (ANCOVA)", .before = 1)
anc_lm <- lm(first_froglet ~ group + init_eggs, data = timing_observed)
co <- summary(anc_lm)$coefficients
idx <- grep("^group", rownames(co))
anc_adj_contrasts <- tibble(outcome = "First froglet, adjusted for initial eggs",
                            comparison = paste0(str_remove(rownames(co)[idx], "group"), " - Control"),
                            difference_days = co[idx, 1],
                            lower95 = co[idx, 1] - qt(0.975, df.residual(anc_lm)) * co[idx, 2],
                            upper95 = co[idx, 1] + qt(0.975, df.residual(anc_lm)) * co[idx, 2],
                            p_raw = co[idx, 4]) %>%
  mutate(p_holm = p.adjust(p_raw, "holm"))

# 来源地作为区组
anc_origin <- aov(first_froglet ~ group + init_eggs + origin, data = timing_observed)
anc_origin_tab <- as_tibble(summary(anc_origin)[[1]], rownames = "term") %>%
  transmute(term, df = Df, F = `F value`, p = `Pr(>F)`) %>% mutate(outcome = "First froglet (group + init_eggs + origin)", .before = 1)

# 非参数稳健性：Kruskal-Wallis + 两两 Wilcoxon（Holm）
kw <- kruskal.test(first_froglet ~ group, data = timing_observed)
pw <- pairwise.wilcox.test(timing_observed$first_froglet, timing_observed$group,
                           p.adjust.method = "holm", exact = FALSE)
kw_tab <- tibble(outcome = "First froglet (days)", test = "Kruskal-Wallis",
                 statistic = unname(kw$statistic), df = unname(kw$parameter), p = kw$p.value)
pw_tab <- as_tibble(as.data.frame(pw$p.value), rownames = "comparison") %>%
  pivot_longer(-comparison, names_to = "vs", values_to = "p_holm") %>%
  filter(vs == "Control") %>%
  transmute(outcome = "First froglet (Wilcoxon vs Control, Holm)", comparison = paste0(comparison, " - Control"), p_holm)

# ==============================================================================
# 5. 生存分析（#40：单一主模型 = 连续浓度；分类模型列补充）
# ==============================================================================
cox_main <- coxph(Surv(followup_time, froglet_event) ~ concentration + init_eggs + frailty(origin),
                  data = event_times, ties = "efron")
cox_cat  <- coxph(Surv(followup_time, froglet_event) ~ group + init_eggs + frailty(origin),
                  data = event_times, ties = "efron")
cox_fixed <- coxph(Surv(followup_time, froglet_event) ~ concentration + init_eggs + origin,
                   data = event_times, ties = "efron")
ph <- as_tibble(as.data.frame(cox.zph(cox_fixed)$table), rownames = "term") %>%
  transmute(outcome = "PH assumption (continuous + origin fixed)", term, chisq, df, p)
grp_p <- summary(coxph(Surv(followup_time, froglet_event) ~ group + init_eggs, data = event_times))$coefficients

cox_tab <- function(fit, label) {
  s <- summary(fit); cm <- s$coefficients; ci <- s$conf.int
  terms <- rownames(cm)
  grab <- function(mat, col) {
    v <- rep(NA_real_, length(terms)); i <- match(terms, rownames(mat)); ok <- !is.na(i)
    v[ok] <- mat[i[ok], col]; v
  }
  tibble(model = label, term = terms,
         HR = grab(ci, "exp(coef)"),
         lower95 = if ("lower .95" %in% colnames(ci)) grab(ci, "lower .95") else NA_real_,
         upper95 = if ("upper .95" %in% colnames(ci)) grab(ci, "upper .95") else NA_real_,
         stat = if ("z" %in% colnames(cm)) cm[, "z"] else cm[, "Chisq"],
         p = if ("Pr(>|z|)" %in% colnames(cm)) cm[, "Pr(>|z|)"] else cm[, "p"])
}
cox_all <- bind_rows(cox_tab(cox_main, "Main: continuous concentration + frailty(origin)"),
                     cox_tab(cox_cat,  "Supplementary: categorical treatment + frailty(origin)"))

# ==============================================================================
# 6. Day-60 比例数据：拟二项 GLM
# ==============================================================================
g_rate <- glm(cbind(succ, fail) ~ group, data = d60, family = quasibinomial)
g_rate_tab <- anova(g_rate, test = "F") %>%
  as_tibble(rownames = "term") %>% filter(term == "group") %>%
  transmute(outcome = "Day-60 froglet rate (quasi-binomial GLM)", df = Df, dev = `Deviance`,
            F = .data[["F"]], p = `Pr(>F)`, dispersion = summary(g_rate)$dispersion)

# ==============================================================================
# 7. 导出
# ==============================================================================
write.csv(assumptions,        file.path(tab_dir, "S1_assumption_tests_normality.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(variance_tests %>% mutate(df1 = as.numeric(df1), p = as.numeric(p)), resid_norm),
          file.path(tab_dir, "S2_assumption_tests_variance_residuals.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(init_egg_anova, anc_tab, anc_origin_tab, kw_tab),
          file.path(tab_dir, "S3_omnibus_ANCOVA_origin_KW.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(anc_adj_contrasts,  file.path(tab_dir, "S4_adjusted_contrasts_holm.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pw_tab,             file.path(tab_dir, "S5_wilcoxon_vs_control_holm.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cox_all,            file.path(tab_dir, "S6_cox_models_with_origin.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ph,                 file.path(tab_dir, "S7_PH_assumption.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(g_rate_tab,         file.path(tab_dir, "S8_day60_rate_quasibinomial.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(event_times,        file.path(tab_dir, "S0_derived_event_times.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\n================ KEY NUMBERS (v2) ================\n")
cat("n (day0 / day60 / events / censored):", nrow(day0), nrow(d60), sum(event_times$froglet_event == 1L), sum(event_times$froglet_event == 0L), "\n")
cat("\nInitial eggs one-way ANOVA: F(", init_egg_anova$df1, ",", init_egg_anova$df2, ") =", round(init_egg_anova$statistic, 3), ", p =", fmt_p(init_egg_anova$p), "\n")
cat("\nANCOVA group effect: F(", anc_tab$df[1], ",", anc_tab$df[3], ") =", round(anc_tab$F[1], 3), ", p =", fmt_p(anc_tab$p[1]), "\n")
cat("ANCOVA origin effect: F =", round(anc_origin_tab$F[3], 2), ", p =", fmt_p(anc_origin_tab$p[3]), "\n")
cm_main <- cox_all %>% filter(str_detect(model, "^Main"), term == "concentration")
cat("\nCox main (continuous) concentration: HR =", round(cm_main$HR, 3), "(", round(cm_main$lower95, 3), "-",
    round(cm_main$upper95, 3), ") p =", fmt_p(cm_main$p), "\n")
fr <- cox_all %>% filter(str_detect(model, "^Main"), str_detect(term, "frailty"))
cat("Cox main frailty(origin) p =", fmt_p(fr$p[1]), "\n")
cat("Cox categorical (supplementary) group terms:\n")
print(cox_all %>% filter(str_detect(model, "^Supplementary"), str_detect(term, "^group")) %>%
        select(term, HR, lower95, upper95, p))
cat("Cox categorical group p (orig, no origin):", paste(fmt_p(grp_p[grep("^group", rownames(grp_p)), "Pr(>|z|)"]), collapse = ", "), "\n")
cat("\nKruskal-Wallis p =", fmt_p(kw$p.value), ";quasi-binomial group F =", round(g_rate_tab$F, 3), "p =", fmt_p(g_rate_tab$p), "\n")
cat("Balance check failures:", sum(!balance_check$balanced), "\n")
cat("Outputs:", output_dir, "\n")
