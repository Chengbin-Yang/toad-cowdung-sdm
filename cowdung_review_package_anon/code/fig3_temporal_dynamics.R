# PURPOSE  Figure 3 (temporal dynamics of developmental stages) and the 50% transition days and cumulative mortality used in Table 1
# INPUT    ../data/01_experiment_daily_records.xlsx (61 daily sheets, five treatments x 15 containers)
# OUTPUT   ../results/Figure3.pptx, ../results/Figure3_summary.csv
# RUN      Rscript fig3_temporal_dynamics.R   (working directory: code/)
# PACKAGES tidyverse, readxl, eoffice

# ==============================================================================
# 1. 初始化设置与加载必要的包
# ==============================================================================
#  极其关键：强行设置 Windows 系统语言环境为中文，防止系统找不到路径和控制台乱码
try(Sys.setlocale("LC_ALL", "C"), silent = TRUE)

library(tidyverse)  # 核心工具包（包含 dplyr, purrr 等），用于数据清洗和转换
library(readxl)     #  替代 openxlsx，完美处理富文本和中文，绝不乱码

# ==============================================================================
# 2. 设置文件路径并读取工作表名称
# ==============================================================================
# 原始路径
raw_path <- "../data/01_experiment_daily_records.xlsx"
#  强行将其转换为 Windows 本地编码，专治“系统找不到指定的路径”
file_path <- enc2native(raw_path)

# 使用 readxl 读取表名（解决 openxlsx 的解压报错）
excel_sheets <- readxl::excel_sheets(file_path)

# ==============================================================================
# 3. 定义安全读取函数（防报错、自动提取天数）
# ==============================================================================
safe_read <- function(sheet_name) {
  out <- tryCatch({
    #  使用 read_excel 替代 read.xlsx
    data <- readxl::read_excel(
      path      = file_path,
      sheet     = sheet_name,
      col_names = TRUE
    )

    # 限制只读取前 10 列，避免读入右侧的空白干扰列
    data <- data[, 1:min(10, ncol(data))]

    # 如果没读到数据或格式不对，返回 NULL
    if (is.null(data) || nrow(data) == 0) return(NULL)

    # 提取天数 (假设名字是 "Day-1" 这种格式)
    day_str <- gsub("Day-", "", sheet_name)
    data$day <- as.numeric(day_str)

    return(data)
  }, error = function(e) {
    message(paste("跳过工作表", sheet_name, "，原因:", e$message))
    return(NULL)
  })
  return(out)
}

# ==============================================================================
# 4. 批量读取工作表并进行纵向合并
# ==============================================================================
# 读取数据并剔除空值/错误值
data_list <- lapply(excel_sheets, safe_read) %>%
  purrr::discard(is.null)

# 检查是否成功读取到了数据
if(length(data_list) == 0) {
  stop("没有读取到任何数据，请检查文件和工作表内容！")
} else {
  message(sprintf("成功读取了 %d/%d 个工作表", length(data_list), length(excel_sheets)))
}

# 合并所有天数的数据，并智能转换列类型
combined_data <- lapply(data_list, function(df) {
  df %>% mutate(across(everything(), as.character))  # 先统一转为字符型防止合并冲突
}) %>%
  bind_rows() %>%
  type_convert(
    col_types = cols(
      .default = col_guess(),  # 自动猜测类型
      day = col_double()       # 明确指定day为数值型
    ),
    locale = locale(encoding = "UTF-8") #  极其关键：强制按 UTF-8 解码数据，解决表头乱码！
  )

# 创建映射关系的命名向量
group_mapping <- c("D" = "Control", "E" = "Low", "F" = "Medium", "G" = "High", "H" = "Ultra High")

# 进行分组操作
concentration_groups <- combined_data %>%
  mutate(group_prefix = substr(样本组, 1, 1),
         group_name = group_mapping[group_prefix]) %>%
  group_by(group_name) %>%
  group_split()

# 将分组结果存储在一个以分组名称命名的列表中
names(concentration_groups) <- unique(combined_data %>% mutate(group_prefix = substr(样本组, 1, 1),
                                                               group_name = group_mapping[group_prefix]) %>%
                                        pull(group_name))


#########################################################################################
# 1️⃣ 汇总按组、天数、阶段
plot_data <- combined_data %>%
  mutate(group_prefix = substr(样本组, 1, 1),
         group_name = group_mapping[group_prefix]) %>%
  group_by(group_name, day) %>%
  summarise(
    Tadpole_hatching = sum(蝌蚪, na.rm = TRUE),
    Limb_development = sum(四肢生长, na.rm = TRUE),
    Froglet = sum(幼蛙, na.rm = TRUE),
    Deaths = sum(死, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(Tadpole_hatching, Limb_development, Froglet),
    names_to = "Stage",
    values_to = "Total"
  ) %>%
  mutate(group_name = factor(group_name,
                             levels = c("Control", "Low", "Medium", "High", "Ultra High")))

# 2️⃣ 阶段颜色
stage_colors <- c(
  "Tadpole_hatching" = "#59A4CB",
  "Limb_development" = "#FD9F2F",
  "Froglet" = "#b15928"
)

# 3️⃣ 计算每个组每个阶段达到50%总量的天数
vline_data <- plot_data %>%
  group_by(group_name, Stage) %>%
  summarise(
    day_50 = day[which(cumsum(Total) >= 0.5 * sum(Total))[1]],
    y_pos = max(Total, na.rm = TRUE) * 0.95, # 文本纵坐标略高于柱子
    .groups = "drop"
  )

# 4️⃣ 绘图
ggplot() +
  # 阶段柱
  geom_col(data = plot_data,
           aes(x = day, y = Total, fill = Stage),
           position = "identity", width = 0.8, alpha = 0.6) +
  # 死亡实线
  geom_line(data = plot_data %>%
              group_by(group_name, day) %>%
              summarise(Deaths = sum(Deaths), .groups = 'drop'),
            aes(x = day, y = Deaths),
            color = "black", size = 0.5) +
  # 阶段 50% 垂直虚线
  geom_vline(data = vline_data,
             aes(xintercept = day_50, color = Stage),
             linetype = "dashed", size = 0.65) +
  # 左侧标注天数
  geom_text(data = vline_data,
            aes(x = day_50 - 0.5, y = y_pos, label = day_50, color = Stage),
            hjust = 1, size = 4) +
  facet_wrap(~ group_name, ncol = 2, scales = "fixed") +
  scale_fill_manual(values = stage_colors,
                    labels = c("Tadpole hatching", "Development of limbs", "Froglet")) +
  scale_color_manual(values = stage_colors) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, 100),
                     expand = c(0,0)) +
  labs(x = "Day", y = "Total numbers", fill = "Stage", color = "Stage") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # 刻度线向内，刻度标签仍在外
    axis.ticks.length = unit(-0.1, "cm")
  )

library(eoffice)
topptx(filename = "../results/Figure3.pptx",width=8,height=7)



############################################################################


################################################################################
# Figure 3 statistics table (same logic as plot)
################################################################################

# 1. 计算各阶段50%时间
stage_50_summary <- plot_data %>%
  group_by(group_name, Stage) %>%
  summarise(

    Total_stage = sum(Total, na.rm = TRUE),

    day_50 = day[
      which(
        cumsum(Total) >= 0.5 * sum(Total, na.rm = TRUE)
      )[1]
    ],

    .groups = "drop"
  )


# 2. 转换为宽表
stage_50_table <- stage_50_summary %>%
  select(group_name, Stage, day_50) %>%
  pivot_wider(
    names_from = Stage,
    values_from = day_50
  )


# 3. 死亡和最终数量统计
mortality_summary <- combined_data %>%
  mutate(
    group_prefix = substr(样本组,1,1),
    group_name = group_mapping[group_prefix]
  ) %>%
  group_by(group_name, day) %>%
  summarise(

    Tadpole = sum(蝌蚪, na.rm=TRUE),
    Limb = sum(四肢生长, na.rm=TRUE),
    Froglet = sum(幼蛙, na.rm=TRUE),
    Death = sum(死, na.rm=TRUE),

    .groups="drop"
  ) %>%
  group_by(group_name) %>%
  summarise(

    Final_froglet =
      max(Froglet, na.rm=TRUE),

    Total_death =
      sum(Death, na.rm=TRUE),

    .groups="drop"
  )


# 4. 合并最终表格

Figure3_summary <- stage_50_table %>%
  left_join(
    mortality_summary,
    by="group_name"
  ) %>%
  mutate(
    group_name=factor(
      group_name,
      levels=c(
        "Control",
        "Low",
        "Medium",
        "High",
        "Ultra High"
      )
    )
  )


print(Figure3_summary)


write.csv(
  Figure3_summary,
  "../results/Figure3_summary.csv",
  row.names=FALSE
)



