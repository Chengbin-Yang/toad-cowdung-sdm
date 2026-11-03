# PURPOSE  Figure 6b: area of each concentration class within low, moderate and high suitability habitat
# INPUT    ../data/08_class_area_data.csv  (reconstructed from 04_pixel_habitat_concentration.csv, see README)
# OUTPUT   ../results/Figure6b_SCI_log10_250.tiff, .pdf, Figure6b.pptx
# RUN      Rscript fig6b_class_area.R   (working directory: code/)
# PACKAGES ggplot2, scales, grid, eoffice

library(ggplot2)
library(scales)
library(grid)
library(eoffice)

# ------------------------------------------------------------
# 1. Import data
# ------------------------------------------------------------
df <- read.csv(
  "../data/08_class_area_data.csv",
  stringsAsFactors = FALSE
)

# Remove unsuitable habitat
df <- df[df$habitat != "不适宜", ]

# ------------------------------------------------------------
# 2. Check data
# ------------------------------------------------------------
if (any(is.na(df$area_km2))) {
  stop("Missing values were detected in area_km2.")
}

if (any(df$area_km2 <= 0)) {
  stop("area_km2 must be greater than zero for a log10 axis.")
}

if (any(df$area_km2 < 1)) {
  warning(
    "Some area values are below 1 km². Change bar_baseline to a smaller positive value."
  )
}

# ------------------------------------------------------------
# 3. Factor order and English labels
# ------------------------------------------------------------
df$habitat <- factor(
  df$habitat,
  levels = c("低适宜", "中适宜", "高适宜"),
  labels = c("Low", "Moderate", "High")
)

df$conc_class <- factor(
  df$conc_class,
  levels = c(
    "<0.8",
    "0.8-1.2",
    "1.2-2.5",
    "2.5-3.5",
    ">=3.5 (LOEC)"
  )
)

# ------------------------------------------------------------
# 4. Color palette
# ------------------------------------------------------------
cc_col <- c(
  "<0.8"         = "#9DBBE3",
  "0.8-1.2"      = "#D5C3B8",
  "1.2-2.5"      = "#C39A82",
  "2.5-3.5"      = "#A66F50",
  ">=3.5 (LOEC)" = "#75452F"
)

# ------------------------------------------------------------
# 5. Calculate bar positions
# ------------------------------------------------------------
df$x_group <- as.numeric(df$habitat)
df$conc_id <- as.numeric(df$conc_class)

n_class <- nlevels(df$conc_class)

# The bar width equals the spacing:
# adjacent bars within each group have no gap
bar_width   <- 0.145
bar_spacing <- bar_width

df$x_position <- df$x_group +
  (df$conc_id - (n_class + 1) / 2) * bar_spacing

df$xmin <- df$x_position - bar_width / 2
df$xmax <- df$x_position + bar_width / 2

# Bar baseline and plot lower limit must be identical
bar_baseline <- 1

# ------------------------------------------------------------
# 6. Plot
# ------------------------------------------------------------
p <- ggplot(df) +

  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = bar_baseline,
      ymax = area_km2,
      fill = conc_class
    ),
    colour = NA
  ) +

  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Low", "Moderate", "High"),
    limits = c(0.52, 3.48),
    expand = expansion(mult = c(0, 0))
  ) +

  scale_fill_manual(
    values = cc_col,
    breaks = c(
      "<0.8",
      "0.8-1.2",
      "1.2-2.5",
      "2.5-3.5",
      ">=3.5 (LOEC)"
    ),
    labels = c(
      "<0.8",
      "0.8–1.2",
      "1.2–2.5",
      "2.5–3.5",
      expression(paste("\u2265", "3.5 (LOEC)"))
    ),
    name = expression(
      paste("Dung concentration (mL ", L^{-1}, ")")
    )
  ) +

  scale_y_log10(
    breaks = c(1, 10, 50, 250),
    labels = scales::label_number(
      accuracy = 1,
      big.mark = ","
    ),
    limits = c(bar_baseline, 250),
    expand = expansion(
      mult = c(0, 0),
      add = c(0, 0)
    )
  ) +

  labs(
    x = "Habitat suitability",
    y = expression(paste("Area (km"^2, ")"))
  ) +

  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  ) +

  theme_classic(
    base_size = 11,
    base_family = "Arial"
  ) +

  theme(
    # Complete plot frame
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.7
    ),

    axis.line = element_blank(),

    # Horizontal reference lines
    panel.grid.major.y = element_line(
      colour = "#D9D9D9",
      linewidth = 0.35
    ),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    # Remove x-axis ticks
    axis.ticks.x = element_blank(),
    axis.ticks.length.x = unit(0, "mm"),

    # Keep y-axis ticks
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

    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",

    legend.title = element_text(
      size = 10.5,
      colour = "black"
    ),

    legend.text = element_text(
      size = 9.5,
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
      t = 6,
      r = 8,
      b = 6,
      l = 8
    )
  )

print(p)

# ------------------------------------------------------------
# 7. Export
# ------------------------------------------------------------
ggsave(
  filename = paste0(
    "../results/",
    "Figure6b_SCI_log10_250.tiff"
  ),
  plot = p,
  width = 170,
  height = 115,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = paste0(
    "../results/",
    "Figure6b_SCI_log10_250.pdf"
  ),
  plot = p,
  width = 170,
  height = 115,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

# ------------------------------------------------------------
# 7. Export
# ------------------------------------------------------------
ppt_file <- paste0(
  "../results/",
  "Figure6b_SCI_log10_250.pptx"
)

topptx(
  figure   = p,
  filename = ppt_file,
  title    = "",
  width    = 7,
  height   = 4,
  units    = "in",
  append   = FALSE
)

