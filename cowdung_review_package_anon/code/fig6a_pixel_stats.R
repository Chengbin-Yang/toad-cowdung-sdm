# PURPOSE  Medians, interquartile ranges and class areas behind Figure 6a
# INPUT    ../data/04_pixel_habitat_concentration.csv
# OUTPUT   ../results/Fig6a_concentration_summary.csv, ../results/Fig6a_threshold_summary.csv
# RUN      Rscript fig6a_pixel_stats.R   (working directory: code/)
# PACKAGES dplyr

library(dplyr)


# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------

dfa <- read.csv(
  "../data/04_pixel_habitat_concentration.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------
# 2. Clean data
# ------------------------------------------------------------

dfa <- dfa %>%
  filter(
    !is.na(conc),
    !is.na(habitat),
    conc >= 0
  )


# ------------------------------------------------------------
# 3. Habitat order
# ------------------------------------------------------------

dfa$habitat <- factor(
  dfa$habitat,
  levels = c(
    "低适宜",
    "中适宜",
    "高适宜"
  )
)



# ============================================================
# 浓度分布统计（供正文引用，非正文 Table 1）
# Concentration distribution by habitat
# ============================================================

conc_summary <- dfa %>%

  group_by(habitat) %>%

  summarise(

    n_pixel = n(),

    mean =
      mean(conc),

    median =
      median(conc),

    Q1 =
      quantile(conc,0.25),

    Q3 =
      quantile(conc,0.75),

    IQR =
      IQR(conc),

    maximum =
      max(conc),

    .groups="drop"
  )


print(conc_summary)



# ============================================================
# Table 2
# Percentage of pixels exceeding exposure thresholds
# ============================================================

threshold_summary <- dfa %>%

  group_by(habitat) %>%

  summarise(

    n_pixel = n(),


    ge_08 =
      sum(conc >= 0.8),

    percent_ge_08 =
      sum(conc >= 0.8)/n()*100,


    ge_12 =
      sum(conc >= 1.2),

    percent_ge_12 =
      sum(conc >= 1.2)/n()*100,


    ge_25 =
      sum(conc >= 2.5),

    percent_ge_25 =
      sum(conc >= 2.5)/n()*100,


    ge_35 =
      sum(conc >= 3.5),

    percent_ge_35 =
      sum(conc >= 3.5)/n()*100,


    .groups="drop"
  )


print(threshold_summary)

print(conc_summary)

# ------------------------------------------------------------
# Export
# ------------------------------------------------------------

write.csv(
  conc_summary,
  "../results/Fig6a_concentration_summary.csv",
  row.names=FALSE
)


write.csv(
  threshold_summary,
  "../results/Fig6a_threshold_summary.csv",
  row.names=FALSE
)