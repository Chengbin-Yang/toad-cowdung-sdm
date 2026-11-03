# PURPOSE  Figure 4a (box plots) and 4b (forest plot); Cox models and Dunnett contrasts
# INPUT    ../data/01_experiment_daily_records.xlsx
# OUTPUT   ../results/fig4/ (15 csv tables, png and pdf figures)
# RUN      Rscript fig4_metamorphosis.R   (working directory: code/)
# PACKAGES tidyverse, readxl, DescTools, survival, patchwork, eoffice (optional)

# -------------------------------
# 0. Packages and user settings

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(DescTools)
  library(patchwork)
  library(survival)
  library(knitr)
})

# eoffice is optional. PNG/PDF export still works without it.
has_eoffice <- requireNamespace("eoffice", quietly = TRUE)

# ---- Change only these paths ----
file_path <- "../data/01_experiment_daily_records.xlsx"
output_dir <- "../results/fig4"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

END_DAY <- 60
ALPHA <- 0.05

# Default to two-sided Dunnett tests. Change only if a one-sided hypothesis was
# prospectively specified before inspecting the data.
DUNNETT_ALTERNATIVE <- "two.sided"

# Concentrations used in the experiment (mL/L)
group_order <- c("Control", "Low", "Medium", "High", "Ultra-High")
concentration_lookup <- c(
  "Control" = 0,
  "Low" = 0.8,
  "Medium" = 1.2,
  "High" = 2.5,
  "Ultra-High" = 3.5
)

# -------------------------------
# 1. Helper functions
# -------------------------------
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[,，]", "")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x[x %in% c("", "NA", "N/A", "na", "n/a", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

first_positive_day <- function(day, value) {
  idx <- which(!is.na(value) & value > 0)
  if (length(idx) == 0) NA_real_ else min(day[idx], na.rm = TRUE)
}

first_true_day <- function(day, condition) {
  idx <- which(!is.na(condition) & condition)
  if (length(idx) == 0) NA_real_ else min(day[idx], na.rm = TRUE)
}

mean_se <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    mean = if (length(x) == 0) NA_real_ else mean(x),
    sd = if (length(x) <= 1) NA_real_ else sd(x),
    se = if (length(x) <= 1) NA_real_ else sd(x) / sqrt(length(x)),
    n = length(x)
  )
}

extract_dunnett <- function(dunnett_object, outcome_name) {
  mat <- dunnett_object[[1]]
  tibble(
    outcome = outcome_name,
    comparison = rownames(mat),
    difference = as.numeric(mat[, "diff"]),
    conf_low = as.numeric(mat[, "lwr.ci"]),
    conf_high = as.numeric(mat[, "upr.ci"]),
    p_adjusted = as.numeric(mat[, "pval"])
  )
}

run_anova_dunnett <- function(data, value_col, outcome_name,
                              alternative = DUNNETT_ALTERNATIVE) {
  analysis_data <- data %>%
    select(group, all_of(value_col)) %>%
    filter(!is.na(.data[[value_col]])) %>%
    droplevels()

  formula_obj <- reformulate("group", response = value_col)
  fit <- aov(formula_obj, data = analysis_data)
  anova_summary <- summary(fit)[[1]]

  descriptive <- analysis_data %>%
    group_by(group) %>%
    summarise(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = sd(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome_name, .before = 1)

  dunnett_obj <- DescTools::DunnettTest(
    formula_obj,
    data = analysis_data,
    control = "Control",
    alternative = alternative,
    conf.level = 1 - ALPHA
  )

  dunnett_table <- extract_dunnett(dunnett_obj, outcome_name)

  omnibus <- tibble(
    outcome = outcome_name,
    df_between = anova_summary[["Df"]][1],
    df_within = anova_summary[["Df"]][2],
    F = anova_summary[["F value"]][1],
    p = anova_summary[["Pr(>F)"]][1],
    test = "One-way ANOVA"
  )

  cat("\n--- ", outcome_name, " ---\n", sep = "")
  cat(sprintf(
    "ANOVA: F(%d, %d) = %.3f, p = %.4f\n",
    omnibus$df_between, omnibus$df_within, omnibus$F, omnibus$p
  ))
  print(descriptive)
  print(dunnett_table)

  invisible(list(
    fit = fit,
    omnibus = omnibus,
    descriptive = descriptive,
    dunnett_object = dunnett_obj,
    dunnett = dunnett_table
  ))
}

welch_vs_control <- function(data, value_col, outcome_name) {
  analysis_data <- data %>%
    select(group, all_of(value_col)) %>%
    filter(!is.na(.data[[value_col]])) %>%
    droplevels()

  control_values <- analysis_data %>%
    filter(group == "Control") %>%
    pull(all_of(value_col))

  treatment_levels <- setdiff(levels(analysis_data$group), "Control")

  pairwise <- purrr::map_dfr(treatment_levels, function(trt) {
    treatment_values <- analysis_data %>%
      filter(group == trt) %>%
      pull(all_of(value_col))

    test_result <- t.test(
      treatment_values,
      control_values,
      alternative = "two.sided",
      var.equal = FALSE,
      conf.level = 1 - ALPHA
    )

    tibble(
      outcome = outcome_name,
      comparison = paste0(trt, "-Control"),
      difference = mean(treatment_values, na.rm = TRUE) -
        mean(control_values, na.rm = TRUE),
      conf_low = unname(test_result$conf.int[1]),
      conf_high = unname(test_result$conf.int[2]),
      p_raw = test_result$p.value
    )
  }) %>%
    mutate(p_holm = p.adjust(p_raw, method = "holm"))

  welch_fit <- oneway.test(
    reformulate("group", response = value_col),
    data = analysis_data,
    var.equal = FALSE
  )

  descriptive <- analysis_data %>%
    group_by(group) %>%
    summarise(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = sd(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome_name, .before = 1)

  omnibus <- tibble(
    outcome = outcome_name,
    numerator_df = unname(welch_fit$parameter[1]),
    denominator_df = unname(welch_fit$parameter[2]),
    F = unname(welch_fit$statistic),
    p = welch_fit$p.value,
    test = "Welch one-way ANOVA"
  )

  cat("\n--- Exploratory: ", outcome_name, " ---\n", sep = "")
  cat(sprintf(
    "Welch ANOVA: F(%.2f, %.2f) = %.3f, p = %.4f\n",
    omnibus$numerator_df, omnibus$denominator_df, omnibus$F, omnibus$p
  ))
  print(descriptive)
  print(pairwise)

  invisible(list(
    omnibus = omnibus,
    descriptive = descriptive,
    pairwise = pairwise
  ))
}

cox_to_table <- function(fit, model_name) {
  s <- summary(fit)
  coef_mat <- s$coefficients
  ci_mat <- s$conf.int

  tibble(
    model = model_name,
    term = rownames(coef_mat),
    beta = coef_mat[, "coef"],
    hazard_ratio = ci_mat[, "exp(coef)"],
    conf_low = ci_mat[, "lower .95"],
    conf_high = ci_mat[, "upper .95"],
    z = coef_mat[, "z"],
    p = coef_mat[, "Pr(>|z|)"]
  )
}

save_plot <- function(plot_object, stem, width, height) {
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))

  ggsave(
    filename = png_path,
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  ggsave(
    filename = pdf_path,
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )

  if (has_eoffice) {
    pptx_path <- file.path(output_dir, paste0(stem, ".pptx"))
    tryCatch(
      eoffice::topptx(
        figure = plot_object,
        filename = pptx_path,
        title = "",
        width = width,
        height = height,
        units = "in",
        append = FALSE
      ),
      error = function(e) warning("PPTX export failed for ", stem, ": ", e$message)
    )
  }
}

# -------------------------------
# 2. Read and clean the workbook
# -------------------------------
all_sheets <- excel_sheets(file_path)
day_sheets <- all_sheets[stringr::str_detect(all_sheets, "^Day-[0-9]+$")]

if (length(day_sheets) == 0) {
  stop("No sheets matching 'Day-<number>' were found in the workbook.")
}

safe_read <- function(sheet_name) {
  tryCatch({
    raw <- read_excel(
      path = file_path,
      sheet = sheet_name,
      range = cell_cols(1:8),
      col_names = TRUE,
      col_types = "text"
    )

    if (ncol(raw) < 8) {
      stop("Expected at least 8 columns in sheet ", sheet_name)
    }

    names(raw)[1:8] <- c(
      "sample_id", "eggs", "tadpoles", "limbs",
      "froglets", "adults", "alive_recorded", "dead"
    )

    raw %>%
      select(sample_id, eggs, tadpoles, limbs, froglets,
             adults, alive_recorded, dead) %>%
      mutate(day = as.numeric(stringr::str_remove(sheet_name, "^Day-")))
  }, error = function(e) {
    warning("Skipping sheet ", sheet_name, ": ", e$message)
    NULL
  })
}

combined_raw <- purrr::map(day_sheets, safe_read) %>%
  purrr::compact() %>%
  bind_rows()

count_columns <- c(
  "eggs", "tadpoles", "limbs", "froglets",
  "adults", "alive_recorded", "dead"
)

combined <- combined_raw %>%
  mutate(
    sample_id = stringr::str_trim(as.character(sample_id)),
    across(all_of(count_columns), clean_numeric)
  ) %>%
  filter(!is.na(sample_id), sample_id != "") %>%
  mutate(
    group_code = substr(sample_id, 1, 1),
    group_name = case_when(
      group_code == "D" ~ "Control",
      group_code == "E" ~ "Low",
      group_code == "F" ~ "Medium",
      group_code == "G" ~ "High",
      group_code == "H" ~ "Ultra-High",
      TRUE ~ NA_character_
    ),
    group = factor(group_name, levels = group_order),
    concentration = unname(concentration_lookup[group_name]),
    replicate = clean_numeric(stringr::str_extract(sample_id, "[0-9]+"))
  ) %>%
  filter(!is.na(group)) %>%
  # In these daily census sheets, blank stage-count cells mean zero individuals
  # in that stage. This correction keeps D-8 and E-15 in Day-60 analyses.
  mutate(across(all_of(count_columns), ~ tidyr::replace_na(.x, 0))) %>%
  arrange(sample_id, day)

# Stop if sample-day duplicates exist.
duplicates <- combined %>% count(sample_id, day) %>% filter(n > 1)
if (nrow(duplicates) > 0) {
  write.csv(
    duplicates,
    file.path(output_dir, "data_check_duplicate_sample_days.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  stop("Duplicate sample-day rows were detected. See data_check_duplicate_sample_days.csv")
}

# -------------------------------
# 3. Derive initial counts, deaths and developmental endpoints
# -------------------------------
day0 <- combined %>%
  filter(day == 0) %>%
  transmute(
    sample_id,
    group,
    concentration,
    replicate,
    init_eggs = eggs
  ) %>%
  distinct(sample_id, .keep_all = TRUE)

if (any(is.na(day0$init_eggs) | day0$init_eggs <= 0)) {
  stop("At least one sample has a missing or non-positive initial egg count.")
}

combined <- combined %>%
  left_join(day0 %>% select(sample_id, init_eggs), by = "sample_id") %>%
  group_by(sample_id) %>%
  arrange(day, .by_group = TRUE) %>%
  mutate(cumulative_dead = cumsum(dead)) %>%
  ungroup()

event_times <- combined %>%
  group_by(sample_id, group, concentration, init_eggs) %>%
  summarise(
    first_tadpole = first_positive_day(day, tadpoles),
    first_limb = first_positive_day(day, limbs),
    first_froglet = first_positive_day(day, froglets),
    all_dead_day = first_true_day(day, cumulative_dead >= init_eggs),
    .groups = "drop"
  ) %>%
  mutate(
    froglet_event = if_else(
      !is.na(first_froglet) &
        (is.na(all_dead_day) | first_froglet <= all_dead_day),
      1L,
      0L
    ),
    followup_time = case_when(
      froglet_event == 1L ~ first_froglet,
      !is.na(all_dead_day) ~ pmin(all_dead_day, END_DAY),
      TRUE ~ END_DAY
    ),
    event_status = case_when(
      froglet_event == 1L ~ "First froglet",
      !is.na(all_dead_day) & all_dead_day <= END_DAY ~ "Complete mortality",
      TRUE ~ "Censored at day 60"
    ),
    tadpole_to_limb = if_else(
      !is.na(first_tadpole) & !is.na(first_limb),
      first_limb - first_tadpole,
      NA_real_
    ),
    limb_to_froglet = if_else(
      !is.na(first_limb) & !is.na(first_froglet),
      first_froglet - first_limb,
      NA_real_
    )
  )

# Observed-event data used only to reproduce the original ANOVA/Dunnett analysis.
timing_observed <- event_times %>%
  filter(froglet_event == 1L) %>%
  select(group, concentration, sample_id, init_eggs, first_froglet)

# Day-60 endpoints. All 75 replicates should be retained.
d60 <- combined %>%
  filter(day == END_DAY) %>%
  transmute(
    sample_id,
    group,
    concentration,
    init_eggs,
    eggs,
    tadpoles,
    limbs,
    froglets,
    adults,
    final_stage_count = eggs + tadpoles + limbs + froglets + adults,
    froglet_rate = froglets / init_eggs,
    survival_rate = final_stage_count / init_eggs,
    froglet_share_survivors = if_else(
      final_stage_count > 0,
      froglets / final_stage_count,
      NA_real_
    ),
    remaining_early_stages = tadpoles + limbs
  )

max_tad <- combined %>%
  group_by(sample_id, group, concentration, init_eggs) %>%
  summarise(max_tadpole = max(tadpoles, na.rm = TRUE), .groups = "drop")

# -------------------------------
# 4. Data-quality balance checks
# -------------------------------
death_summary <- combined %>%
  group_by(sample_id) %>%
  summarise(total_dead = sum(dead, na.rm = TRUE), .groups = "drop")

balance_check <- d60 %>%
  left_join(death_summary, by = "sample_id") %>%
  mutate(
    balance_difference = init_eggs - final_stage_count - total_dead,
    balanced = abs(balance_difference) < 1e-8
  )

write.csv(
  balance_check,
  file.path(output_dir, "data_check_day60_balance.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (any(!balance_check$balanced)) {
  warning(
    "Some replicates do not satisfy initial eggs = Day-60 stage counts + total deaths. ",
    "Review data_check_day60_balance.csv before interpreting final endpoints."
  )
}

sample_counts <- tibble(
  dataset = c(
    "Day 0", "Day 60", "Observed first froglet",
    "Censored/competing first-froglet records"
  ),
  n = c(
    nrow(day0),
    nrow(d60),
    sum(event_times$froglet_event == 1L),
    sum(event_times$froglet_event == 0L)
  )
)
print(sample_counts)

# -------------------------------
# 5. Corrected primary analyses
# -------------------------------
cat("\n========================================================\n")
cat("CORRECTED PRIMARY RESULTS\n")
cat("========================================================\n")

res <- list(
  timing = run_anova_dunnett(
    timing_observed,
    "first_froglet",
    "Metamorphosis timing among observed events (days)",
    alternative = DUNNETT_ALTERNATIVE
  ),
  froglet_rate = run_anova_dunnett(
    d60,
    "froglet_rate",
    "Day-60 froglet rate",
    alternative = "two.sided"
  ),
  froglet_count = run_anova_dunnett(
    d60,
    "froglets",
    "Day-60 froglet count",
    alternative = "two.sided"
  ),
  max_tadpole = run_anova_dunnett(
    max_tad,
    "max_tadpole",
    "Maximum tadpole count",
    alternative = "two.sided"
  )
)

# Initial egg count is tested because it is a potential design imbalance/confounder.
initial_egg_test <- run_anova_dunnett(
  day0,
  "init_eggs",
  "Initial egg count",
  alternative = "two.sided"
)

# -------------------------------
# 6. Time-to-first-froglet analyses
# -------------------------------
# Cause-specific Cox models: complete mortality is censored at the mortality day.
# The continuous concentration model tests an ordered dose trend.
cox_group <- coxph(
  Surv(followup_time, froglet_event) ~ group + init_eggs,
  data = event_times,
  ties = "efron",
  x = TRUE
)

cox_concentration <- coxph(
  Surv(followup_time, froglet_event) ~ concentration + init_eggs,
  data = event_times,
  ties = "efron",
  x = TRUE
)

cox_results <- bind_rows(
  cox_to_table(cox_group, "Categorical treatment model"),
  cox_to_table(cox_concentration, "Continuous concentration model")
)

cox_ph_tests <- bind_rows(
  as.data.frame(cox.zph(cox_group)$table) %>%
    rownames_to_column("term") %>%
    mutate(model = "Categorical treatment model", .before = 1),
  as.data.frame(cox.zph(cox_concentration)$table) %>%
    rownames_to_column("term") %>%
    mutate(model = "Continuous concentration model", .before = 1)
)

cat("\n--- Cause-specific Cox model: categorical groups ---\n")
print(summary(cox_group))
cat("\n--- Cause-specific Cox model: continuous concentration ---\n")
print(summary(cox_concentration))
cat("\n--- Proportional-hazards checks ---\n")
print(cox_ph_tests)

# Kaplan-Meier estimates for the cumulative probability of first froglet.
km_fit <- survfit(
  Surv(followup_time, froglet_event) ~ group,
  data = event_times,
  conf.type = "log-log"
)

km_summary <- summary(km_fit, censored = TRUE)
km_data <- tibble(
  time = km_summary$time,
  survival = km_summary$surv,
  conf_low_survival = km_summary$lower,
  conf_high_survival = km_summary$upper,
  strata = as.character(km_summary$strata)
) %>%
  mutate(
    group = factor(
      stringr::str_remove(strata, "^group="),
      levels = group_order
    ),
    cumulative_probability = 1 - survival,
    conf_low = 1 - conf_high_survival,
    conf_high = 1 - conf_low_survival
  )

# -------------------------------
# 7. Exploratory developmental-stage analyses
# -------------------------------
# These analyses should be labelled exploratory unless they were prespecified.
exploratory <- list(
  first_tadpole = welch_vs_control(
    event_times,
    "first_tadpole",
    "First tadpole appearance (days)"
  ),
  first_limb = welch_vs_control(
    event_times,
    "first_limb",
    "First limb appearance (days)"
  ),
  tadpole_to_limb = welch_vs_control(
    event_times,
    "tadpole_to_limb",
    "Tadpole-to-limb interval (days)"
  ),
  limb_to_froglet = welch_vs_control(
    event_times,
    "limb_to_froglet",
    "Limb-to-froglet interval (days)"
  )
)

# Optional continuous dose-response models for exploratory interpretation.
limb_linear_model <- lm(
  first_limb ~ concentration + init_eggs,
  data = event_times
)

limb_quadratic_model <- lm(
  first_limb ~ concentration + I(concentration^2) + init_eggs,
  data = event_times
)

limb_model_comparison <- tibble(
  model = c("Linear concentration", "Quadratic concentration"),
  AIC = c(AIC(limb_linear_model), AIC(limb_quadratic_model)),
  BIC = c(BIC(limb_linear_model), BIC(limb_quadratic_model)),
  adjusted_R2 = c(
    summary(limb_linear_model)$adj.r.squared,
    summary(limb_quadratic_model)$adj.r.squared
  )
)

cat("\n--- Exploratory first-limb dose-response models ---\n")
print(summary(limb_linear_model))
print(summary(limb_quadratic_model))
print(limb_model_comparison)

# -------------------------------
# 8. Figures
# -------------------------------
group_colors <- c(
  "Control" = "#9DBBE3",
  "Low" = "#D5C3B8",
  "Medium" = "#C39A82",
  "High" = "#A66F50",
  "Ultra-High" = "#75452F"
)

conc_labels <- c(
  "Control\n(0)", "Low\n(0.8)", "Medium\n(1.2)",
  "High\n(2.5)", "Ultra-High\n(3.5)"
)

theme_nature <- theme_classic(base_size = 9, base_family = "Times New Roman") +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    legend.position = "none",
    strip.background = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    panel.border = element_rect(
      fill = NA, color = "black", linewidth = 0.5
    )
  )

# ---- Figure 4a: observed-event boxplot ----
ultra_row <- res$timing$dunnett %>%
  filter(comparison == "Ultra-High-Control")

if (nrow(ultra_row) != 1) {
  stop("Could not identify the Ultra-High vs Control Dunnett comparison.")
}

p_ultra <- ultra_row$p_adjusted
annotation_label <- paste0("Dunnett-adjusted p = ", formatC(p_ultra, digits = 3, format = "f"))

y_max_data <- max(timing_observed$first_froglet, na.rm = TRUE)
y_min_data <- min(timing_observed$first_froglet, na.rm = TRUE)
y_range <- y_max_data - y_min_data

pA <- timing_observed %>%
  ggplot(aes(x = group, y = first_froglet, fill = group)) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.60,
    width = 0.45,
    color = "black",
    linewidth = 0.5
  ) +
  geom_jitter(
    aes(color = group),
    width = 0.12,
    size = 1.8,
    alpha = 0.55
  ) +
  scale_fill_manual(values = group_colors, breaks = group_order) +
  scale_color_manual(values = group_colors, breaks = group_order) +
  scale_x_discrete(labels = conc_labels) +
  annotate(
    "segment",
    x = 1, xend = 5,
    y = y_max_data + y_range * 0.10,
    yend = y_max_data + y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "segment",
    x = 1, xend = 1,
    y = y_max_data + y_range * 0.05,
    yend = y_max_data + y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "segment",
    x = 5, xend = 5,
    y = y_max_data + y_range * 0.05,
    yend = y_max_data + y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = 3,
    y = y_max_data + y_range * 0.17,
    label = annotation_label,
    size = 3.0,
    fontface = "bold"
  ) +
  labs(
    x = "Concentration (mL/L)",
    y = "Time to first froglet (days)",
    title = "a  Metamorphosis timing"
  ) +
  theme_nature +
  theme(axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(y_min_data - y_range * 0.08, y_max_data + y_range * 0.27),
    clip = "off"
  )

# ---- Figure 4b: internally consistent two-sided Dunnett forest plot ----
dunnett_df <- res$timing$dunnett %>%
  mutate(
    treatment_text = stringr::str_remove(comparison, "-Control$"),
    treatment = factor(treatment_text, levels = group_order[-1]),
    pval_fmt = formatC(p_adjusted, digits = 3, format = "f"),
    label_name = factor(
      case_when(
        treatment_text == "Ultra-High" ~ "Ultra-High (3.5)",
        treatment_text == "High" ~ "High (2.5)",
        treatment_text == "Medium" ~ "Medium (1.2)",
        treatment_text == "Low" ~ "Low (0.8)"
      ),
      levels = c(
        "Ultra-High (3.5)", "High (2.5)",
        "Medium (1.2)", "Low (0.8)"
      )
    )
  )

text_x <- max(dunnett_df$conf_high, na.rm = TRUE) + 0.6
plot_x_max <- text_x + 2.8

pB <- ggplot(dunnett_df, aes(x = difference, y = label_name)) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbarh(
    aes(xmin = conf_low, xmax = conf_high),
    height = 0.15,
    linewidth = 0.6
  ) +
  geom_point(
    aes(fill = treatment),
    size = 3.5,
    shape = 21,
    color = "black",
    stroke = 0.5
  ) +
  scale_fill_manual(values = group_colors, breaks = group_order) +
  geom_text(
    aes(label = paste0("p = ", pval_fmt)),
    x = text_x,
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  labs(
    x = "Difference from control (days)",
    y = "",
    title = "b  Dunnett comparison"
  ) +
  theme_nature +
  theme(axis.line = element_blank()) +
  coord_cartesian(
    xlim = c(min(dunnett_df$conf_low, na.rm = TRUE) - 1, plot_x_max),
    clip = "off"
  )

figure4 <- pA + pB +
  patchwork::plot_layout(ncol = 2, widths = c(1.08, 0.92))

save_plot(figure4, "Figure4_corrected", width = 9.0, height = 4.0)

# ---- Corrected daily progression plot ----
# This is the proportion of initial eggs currently present as froglets, not a
# cumulative count. Therefore values cannot exceed 100% by construction.
daily_froglet <- combined %>%
  mutate(froglet_pct = froglets / init_eggs * 100) %>%
  group_by(group, day) %>%
  summarise(
    mean = mean(froglet_pct, na.rm = TRUE),
    sd = sd(froglet_pct, na.rm = TRUE),
    n = sum(!is.na(froglet_pct)),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    ribbon_low = pmax(0, mean - se),
    ribbon_high = pmin(100, mean + se)
  )

pC <- ggplot(
  daily_froglet,
  aes(x = day, y = mean, color = group, fill = group)
) +
  geom_ribbon(
    aes(ymin = ribbon_low, ymax = ribbon_high),
    alpha = 0.10,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = group_colors, breaks = group_order) +
  scale_fill_manual(values = group_colors, breaks = group_order) +
  labs(
    x = "Day of experiment",
    y = "Froglets / initial eggs (%)",
    title = "a  Froglet prevalence over time",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_nature +
  theme(
    legend.position = c(0.22, 0.73),
    legend.background = element_blank(),
    legend.key.size = grid::unit(0.40, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.line = element_blank()
  ) +
  scale_x_continuous(limits = c(0, END_DAY), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))

# ---- Kaplan-Meier cumulative probability of first froglet ----
pKM <- ggplot(
  km_data,
  aes(x = time, y = cumulative_probability, color = group, fill = group)
) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    alpha = 0.08,
    color = NA
  ) +
  geom_step(linewidth = 0.8) +
  scale_color_manual(values = group_colors, breaks = group_order) +
  scale_fill_manual(values = group_colors, breaks = group_order) +
  labs(
    x = "Day of experiment",
    y = "Cumulative probability of first froglet",
    title = "b  Time-to-event progression"
  ) +
  theme_nature +
  theme(axis.line = element_blank()) +
  scale_x_continuous(limits = c(0, END_DAY), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0, 0.02))
  )

figureS_progression <- pC + pKM +
  patchwork::plot_layout(ncol = 2, widths = c(1, 1))

save_plot(
  figureS_progression,
  "FigureS_progression_corrected",
  width = 9.0,
  height = 4.0
)

# ---- Exploratory first-limb timing plot ----
first_limb_ultra <- exploratory$first_limb$pairwise %>%
  filter(comparison == "Ultra-High-Control")

limb_annotation <- if (nrow(first_limb_ultra) == 1) {
  paste0(
    "Holm-adjusted p = ",
    formatC(first_limb_ultra$p_holm, digits = 3, format = "f")
  )
} else {
  ""
}

limb_plot_data <- event_times %>% filter(!is.na(first_limb))
limb_y_max <- max(limb_plot_data$first_limb, na.rm = TRUE)
limb_y_min <- min(limb_plot_data$first_limb, na.rm = TRUE)
limb_y_range <- limb_y_max - limb_y_min

p_limb <- ggplot(
  limb_plot_data,
  aes(x = group, y = first_limb, fill = group)
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.60,
    width = 0.45,
    color = "black",
    linewidth = 0.5
  ) +
  geom_jitter(
    aes(color = group),
    width = 0.12,
    size = 1.8,
    alpha = 0.55
  ) +
  scale_fill_manual(values = group_colors, breaks = group_order) +
  scale_color_manual(values = group_colors, breaks = group_order) +
  scale_x_discrete(labels = conc_labels) +
  annotate(
    "segment",
    x = 1, xend = 5,
    y = limb_y_max + limb_y_range * 0.10,
    yend = limb_y_max + limb_y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "segment",
    x = 1, xend = 1,
    y = limb_y_max + limb_y_range * 0.05,
    yend = limb_y_max + limb_y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "segment",
    x = 5, xend = 5,
    y = limb_y_max + limb_y_range * 0.05,
    yend = limb_y_max + limb_y_range * 0.10,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = 3,
    y = limb_y_max + limb_y_range * 0.17,
    label = limb_annotation,
    size = 3.0,
    fontface = "bold"
  ) +
  labs(
    x = "Concentration (mL/L)",
    y = "Time to first limb appearance (days)",
    title = "Exploratory first-limb timing"
  ) +
  theme_nature +
  theme(axis.line = element_blank()) +
  coord_cartesian(
    ylim = c(limb_y_min - limb_y_range * 0.08, limb_y_max + limb_y_range * 0.27),
    clip = "off"
  )

save_plot(p_limb, "FigureS_first_limb_exploratory", width = 5.2, height = 4.0)

# ---- Day-60 developmental-stage composition ----
stage_composition <- d60 %>%
  select(sample_id, group, init_eggs, eggs, tadpoles, limbs, froglets, adults) %>%
  left_join(death_summary, by = "sample_id") %>%
  pivot_longer(
    cols = c(eggs, tadpoles, limbs, froglets, adults, total_dead),
    names_to = "stage",
    values_to = "count"
  ) %>%
  mutate(
    stage = recode(
      stage,
      eggs = "Eggs",
      tadpoles = "Tadpoles",
      limbs = "Limb-development stage",
      froglets = "Froglets",
      adults = "Adults",
      total_dead = "Cumulative deaths"
    ),
    proportion = count / init_eggs * 100
  ) %>%
  group_by(group, stage) %>%
  summarise(
    mean_percent = mean(proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stage = factor(
      stage,
      levels = c(
        "Cumulative deaths", "Eggs", "Tadpoles",
        "Limb-development stage", "Froglets", "Adults"
      )
    )
  )

p_stage <- ggplot(
  stage_composition,
  aes(x = group, y = mean_percent, fill = stage)
) +
  geom_col(width = 0.70, color = "black", linewidth = 0.25) +
  scale_x_discrete(labels = conc_labels) +
  labs(
    x = "Concentration (mL/L)",
    y = "Mean proportion of initial eggs (%)",
    fill = "Day-60 status",
    title = "Exploratory Day-60 developmental-stage composition"
  ) +
  theme_classic(base_size = 9, base_family = "Times New Roman") +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02)))

save_plot(p_stage, "FigureS_day60_stage_composition", width = 7.0, height = 4.2)

# -------------------------------
# 9. Export tables
# -------------------------------
primary_omnibus <- bind_rows(
  res$timing$omnibus,
  res$froglet_rate$omnibus,
  res$froglet_count$omnibus,
  res$max_tadpole$omnibus,
  initial_egg_test$omnibus
)

primary_descriptive <- bind_rows(
  res$timing$descriptive,
  res$froglet_rate$descriptive,
  res$froglet_count$descriptive,
  res$max_tadpole$descriptive,
  initial_egg_test$descriptive
)

primary_dunnett <- bind_rows(
  res$timing$dunnett,
  res$froglet_rate$dunnett,
  res$froglet_count$dunnett,
  res$max_tadpole$dunnett,
  initial_egg_test$dunnett
)

exploratory_omnibus <- bind_rows(
  exploratory$first_tadpole$omnibus,
  exploratory$first_limb$omnibus,
  exploratory$tadpole_to_limb$omnibus,
  exploratory$limb_to_froglet$omnibus
)

exploratory_descriptive <- bind_rows(
  exploratory$first_tadpole$descriptive,
  exploratory$first_limb$descriptive,
  exploratory$tadpole_to_limb$descriptive,
  exploratory$limb_to_froglet$descriptive
)

exploratory_pairwise <- bind_rows(
  exploratory$first_tadpole$pairwise,
  exploratory$first_limb$pairwise,
  exploratory$tadpole_to_limb$pairwise,
  exploratory$limb_to_froglet$pairwise
)

summary_table <- primary_descriptive %>%
  mutate(
    mean_display = case_when(
      outcome == "Day-60 froglet rate" ~ sprintf("%.1f +/- %.1f%%", mean * 100, sd * 100),
      TRUE ~ sprintf("%.2f +/- %.2f", mean, sd)
    )
  ) %>%
  select(outcome, group, mean, sd, n, mean_display)

write.csv(
  primary_omnibus,
  file.path(output_dir, "Table_primary_omnibus.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  primary_descriptive,
  file.path(output_dir, "Table_primary_descriptive.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  primary_dunnett,
  file.path(output_dir, "Table_primary_Dunnett.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  summary_table,
  file.path(output_dir, "Summary_table_corrected.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  event_times,
  file.path(output_dir, "Derived_event_times.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  cox_results,
  file.path(output_dir, "Table_Cox_models.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  cox_ph_tests,
  file.path(output_dir, "Table_Cox_PH_assumption.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  exploratory_omnibus,
  file.path(output_dir, "Table_exploratory_Welch_omnibus.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  exploratory_descriptive,
  file.path(output_dir, "Table_exploratory_descriptive.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  exploratory_pairwise,
  file.path(output_dir, "Table_exploratory_pairwise_Holm.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  limb_model_comparison,
  file.path(output_dir, "Table_first_limb_dose_model_comparison.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  stage_composition,
  file.path(output_dir, "Table_day60_stage_composition.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n========================================================\n")
cat("ANALYSIS COMPLETED\n")
cat("Output directory: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("========================================================\n")
