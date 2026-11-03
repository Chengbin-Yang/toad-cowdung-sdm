# PURPOSE  Figure 6a: violin plots of cow dung concentration by habitat suitability class, with Dunn post-hoc tests
# INPUT    ../data/04_pixel_habitat_concentration.csv (570,004 pixels)
# OUTPUT   ../results/Figure6a.pptx
# RUN      Rscript fig6a_violin.R   (working directory: code/)
# PACKAGES ggplot2, grid, eoffice, FSA

library(ggplot2)
library(grid)
library(eoffice)
library(FSA)      # dunnTest(): Dunn post-hoc after Kruskal-Wallis
                  # install.packages("FSA") if not available

# ------------------------------------------------------------
# 1. Import data
# ------------------------------------------------------------
dfa <- read.csv(
  "../data/04_pixel_habitat_concentration.csv",
  stringsAsFactors = FALSE
)

# Remove missing and invalid observations
dfa <- dfa[
  !is.na(dfa$conc) &
    !is.na(dfa$habitat) &
    dfa$conc >= 0,
]

# ------------------------------------------------------------
# 2. Habitat order and English labels
# ------------------------------------------------------------
dfa$habitat <- factor(
  dfa$habitat,
  levels = c("低适宜", "中适宜", "高适宜"),
  labels = c("Low", "Moderate", "High")
)

# ------------------------------------------------------------
# 3. Violin colors
# ------------------------------------------------------------
hab_col <- c(
  "Low"      = "#D9E1EC",
  "Moderate" = "#8EB9CE",
  "High"     = "#397FA3"
)

# ------------------------------------------------------------
# 4. Concentration thresholds
# ------------------------------------------------------------
threshold_df <- data.frame(
  threshold = c(0.8, 1.2, 2.5, 3.5),
  level = factor(
    c(
      "0.8–1.2",
      "1.2–2.5",
      "2.5–3.5",
      "\u22653.5 (LOEC)"
    ),
    levels = c(
      "0.8–1.2",
      "1.2–2.5",
      "2.5–3.5",
      "\u22653.5 (LOEC)"
    )
  )
)

threshold_col <- c(
  "0.8–1.2"      = "#D5C3B8",
  "1.2–2.5"      = "#C39A82",
  "2.5–3.5"      = "#A66F50",
  "\u22653.5 (LOEC)" = "#75452F"
)

# ------------------------------------------------------------
# 5. Y-axis range
# ------------------------------------------------------------
y_max <- max(dfa$conc, na.rm = TRUE)

y_upper <- max(
  y_max * 1.22,   # extra head-room for the significance brackets
  4.4
)

# ------------------------------------------------------------
# 6. Plot
# ------------------------------------------------------------
p <- ggplot(
  dfa,
  aes(
    x = habitat,
    y = conc
  )
) +

  # Violin distributions based on all pixels
  geom_violin(
    aes(fill = habitat),
    width = 0.82,
    scale = "width",
    trim = TRUE,
    adjust = 1.1,
    alpha = 0.82,
    colour = "black",
    linewidth = 0.55
  ) +

  # Internal boxplots
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    fill = "white",
    colour = "black",
    linewidth = 0.55,
    alpha = 0.92
  ) +

  # Concentration-category threshold lines
  geom_hline(
    data = threshold_df,
    aes(
      yintercept = threshold,
      colour = level
    ),
    linetype = "dashed",
    linewidth = 0.70,
    inherit.aes = FALSE
  ) +

  # Habitat colors
  scale_fill_manual(
    values = hab_col,
    guide = "none"
  ) +

  # Threshold-line colors
  scale_colour_manual(
    values = threshold_col,
    breaks = c(
      "0.8–1.2",
      "1.2–2.5",
      "2.5–3.5",
      "\u22653.5 (LOEC)"
    ),
    labels = c(
      "0.8 mL/L",
      "1.2 mL/L",
      "2.5 mL/L",
      "3.5 mL/L (LOEC)"
    ),
    name = "Concentration thresholds"
  ) +

  scale_y_continuous(
    limits = c(0, y_upper),
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number(
      accuracy = 0.1,
      trim = TRUE
    ),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +

  labs(
    x = "Habitat suitability",
    y = expression(
      paste("Dung concentration (mL ", L^{-1}, ")")
    )
  ) +

  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      override.aes = list(
        linetype = "dashed",
        linewidth = 1.2
      )
    )
  ) +

  theme_classic(
    base_size = 11,
    base_family = "Arial"
  ) +

  theme(
    # Complete rectangular frame
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.7
    ),

    axis.line = element_blank(),

    # Remove default grid lines because thresholds act as guides
    panel.grid = element_blank(),

    # Remove x-axis tick marks
    axis.ticks.x = element_blank(),
    axis.ticks.length.x = unit(0, "mm"),

    # Retain y-axis tick marks
    axis.ticks.y = element_line(
      colour = "black",
      linewidth = 0.5
    ),

    axis.ticks.length.y = unit(2, "mm"),

    axis.text.x = element_text(
      size = 10.5,
      colour = "black",
      margin = margin(t = 6)
    ),

    axis.text.y = element_text(
      size = 10,
      colour = "black",
      margin = margin(r = 4)
    ),

    axis.title.x = element_text(
      size = 11.5,
      colour = "black",
      margin = margin(t = 8)
    ),

    axis.title.y = element_text(
      size = 11.5,
      colour = "black",
      margin = margin(r = 8)
    ),

    # Legend
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",

    legend.title = element_text(
      size = 10.2,
      colour = "black"
    ),

    legend.text = element_text(
      size = 9.2,
      colour = "black"
    ),

    legend.key.width = unit(11, "mm"),
    legend.key.height = unit(4, "mm"),
    legend.spacing.x = unit(2, "mm"),

    legend.margin = margin(
      t = 0,
      r = 0,
      b = 5,
      l = 0
    ),

    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),

    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),

    plot.margin = margin(
      t = 7,
      r = 9,
      b = 7,
      l = 9
    )
  )

# ------------------------------------------------------------
# 7. Significance testing (Kruskal-Wallis + Dunn post-hoc)
# ------------------------------------------------------------
kw_test <- kruskal.test(conc ~ habitat, data = dfa)

dunn_res <- FSA::dunnTest(
  conc ~ habitat,
  data = dfa,
  method = "holm"
)$res

cat("\n--- Kruskal-Wallis ---\n"); print(kw_test)
cat("\n--- Dunn post-hoc (Holm) ---\n"); print(dunn_res)

# p value -> asterisks
star <- function(p) {
  ifelse(is.na(p), "ns",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*", "ns"))))
}

get_p <- function(g1, g2) {
  hit <- dunn_res$Comparison %in% c(paste(g1, "-", g2), paste(g2, "-", g1))
  if (any(hit)) dunn_res$P.adj[which(hit)[1]] else NA_real_
}

xpos <- c(Low = 1, Moderate = 2, High = 3)

brk <- data.frame(
  g1 = c("Low", "Moderate", "Low"),
  g2 = c("Moderate", "High", "High"),
  stringsAsFactors = FALSE
)
brk$x1  <- xpos[brk$g1]
brk$x2  <- xpos[brk$g2]
step    <- y_max * 0.055
brk$y   <- y_max * 1.055 + step * (seq_len(nrow(brk)) - 1)
brk$tip <- brk$y - step * 0.45
brk$lab <- star(get_p(brk$g1, brk$g2))

# ------------------------------------------------------------
# 8. Add significance brackets to the plot
# ------------------------------------------------------------
p <- p +
  geom_segment(
    data = brk,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE, linewidth = 0.5, colour = "black"
  ) +
  geom_segment(
    data = brk,
    aes(x = x1, xend = x1, y = y, yend = tip),
    inherit.aes = FALSE, linewidth = 0.5, colour = "black"
  ) +
  geom_segment(
    data = brk,
    aes(x = x2, xend = x2, y = y, yend = tip),
    inherit.aes = FALSE, linewidth = 0.5, colour = "black"
  ) +
  geom_text(
    data = brk,
    aes(x = (x1 + x2) / 2, y = y + step * 0.20, label = lab),
    inherit.aes = FALSE, size = 3.4, family = "Arial", colour = "black"
  )


print(p)



# ------------------------------------------------------------
# 9. Export editable PowerPoint
# ------------------------------------------------------------

ppt_file <- paste0(
  "../results/",
  "Figure6a.pptx"
)


topptx(
  figure = p,
  filename = ppt_file,
  title = "",
  width = 7.0,
  height = 4,
  units = "in",
  append = FALSE
)