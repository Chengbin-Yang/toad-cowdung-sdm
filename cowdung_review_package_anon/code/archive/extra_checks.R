# ARCHIVE  additional checks (source group, assumptions, unbalanced initial egg number, proportional data)

# 追加核查：来源地随机效应(#34) + 假设检验(#35) + 初始卵数不平衡 + 比例数据模型
# 复用原脚本已定义的 all 数据对象
suppressPackageStartupMessages({
  library(tidyverse); library(DescTools); library(survival)
})
setwd(".")
source("experiment_analysis_revised.R")   # 会重跑一遍主分析（含出图）

cat("\n\n########## EXTRA CHECKS ##########\n")

# ---- 0. 卵来源地映射（按主文 2.2.1：每处理 15 桶，1-3 HS1 / 4-6 HS2 / 7-9 SDG / 10-12 CH1 / 13-15 CH2）
et <- event_times %>%
  left_join(combined %>% select(sample_id, replicate) %>% distinct(), by = "sample_id") %>%
  mutate(origin = factor(case_when(
    replicate <= 3  ~ "Heshangou1",
    replicate <= 6  ~ "Heshangou2",
    replicate <= 9  ~ "Sandaogou",
    replicate <= 12 ~ "Chunhua1",
    TRUE            ~ "Chunhua2"
  ), levels = c("Heshangou1","Heshangou2","Sandaogou","Chunhua1","Chunhua2")))
cat("\n-- origin x group counts (每处理应各 3 桶)\n"); print(table(et$group, et$origin))

tim <- et %>% filter(froglet_event == 1L)

# ---- 1. 假设检验：正态性 + 方差齐性
cat("\n-- Shapiro-Wilk by group (first froglet)\n")
print(tim %>% group_by(group) %>% summarise(W = shapiro.test(first_froglet)$statistic,
                                            p = shapiro.test(first_froglet)$p.value, n = n()))
cat("\n-- Levene / Bartlett (first froglet ~ group)\n")
print(LeveneTest(first_froglet ~ group, data = tim))
print(bartlett.test(first_froglet ~ group, data = tim))

cat("\n-- Shapiro by group (Day-60 froglet rate)\n")
print(d60 %>% group_by(group) %>% summarise(W = shapiro.test(froglet_rate)$statistic,
                                            p = shapiro.test(froglet_rate)$p.value, n = n()))
cat("\n-- Levene (froglet rate)\n"); print(LeveneTest(froglet_rate ~ group, data = d60))
cat("\n-- Shapiro residual of main ANOVA (first froglet)\n")
m0 <- aov(first_froglet ~ group, data = tim); print(shapiro.test(resid(m0)))

# ---- 2. 初始卵数不平衡的后果：ANCOVA / 调整对照
cat("\n-- ANCOVA: first_froglet ~ group + init_eggs\n")
m_anc <- aov(first_froglet ~ group + init_eggs, data = tim)
print(summary(m_anc))
cat("\n-- lm contrasts vs Control, Holm-adjusted\n")
m_lm <- lm(first_froglet ~ group + init_eggs, data = tim)
co <- summary(m_lm)$coefficients
pv <- co[grep("^group", rownames(co)), 4]
print(data.frame(term = rownames(co)[grep("^group", rownames(co))],
                 estimate = co[grep("^group", rownames(co)), 1],
                 p_raw = pv, p_holm = p.adjust(pv, "holm")))

# ---- 3. 来源地作为区组/随机效应
cat("\n-- aov with origin block: first_froglet ~ group + init_eggs + origin\n")
print(summary(aov(first_froglet ~ group + init_eggs + origin, data = tim)))
cat("\n-- aov with origin as error stratum (random effect)\n")
print(summary(aov(first_froglet ~ group + init_eggs + Error(origin), data = tim)))

# ---- 4. Cox: 加 frailty(origin) / 加 origin 固定效应
cat("\n-- Cox categorical + frailty(origin)\n")
print(summary(coxph(Surv(followup_time, froglet_event) ~ group + init_eggs + frailty(origin),
                    data = et, ties = "efron")))
cat("\n-- Cox continuous + frailty(origin)\n")
print(summary(coxph(Surv(followup_time, froglet_event) ~ concentration + init_eggs + frailty(origin),
                    data = et, ties = "efron")))
cat("\n-- PH check for continuous + frailty\n")
print(cox.zph(coxph(Surv(followup_time, froglet_event) ~ concentration + init_eggs + frailty(origin), data = et)))

# ---- 5. 比例数据（Day-60 幼蛙率）改拟二项 GLM
cat("\n-- quasi-binomial GLM for Day-60 froglet rate\n")
d60b <- d60 %>% mutate(succ = round(froglet_rate * init_eggs), fail = init_eggs - succ)
g <- glm(cbind(succ, fail) ~ group, data = d60b, family = quasibinomial)
print(anova(g, test = "F"))
print(summary(g)$dispersion)

cat("\n########## EXTRA CHECKS DONE ##########\n")
