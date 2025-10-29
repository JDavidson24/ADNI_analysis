install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("forcats")
library(forcats)
install.packages("knitr")
library(knitr)
install.packages("lme4")
library(lme4)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggeffects")
library(ggeffects)
install.packages("splines2")
library(splines2)
install.packages("pbkrtest")
library(pbkrtest)
install.packages("emmeans")
library(emmeans)
install.packages("tidyr")
library(tidyr)
install.packages("broom")
library(broom)


PACC_all <- read.csv("data/all_risk_factor_df_raw.csv")
colnames(PACC_all)

# 計算各 SUBSTUDY 在每年平均 PACC.raw
pacc_summary <- PACC_all %>%
  group_by(SUBSTUDY, year) %>%
  summarise(mean_PACC = mean(PACC.raw, na.rm = TRUE),
            sd_PACC   = sd(PACC.raw, na.rm = TRUE),
            n         = n()) %>%
  ungroup()

# 畫趨勢圖
p <- ggplot(pacc_summary, aes(x = year, y = mean_PACC, color = SUBSTUDY, group = SUBSTUDY)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_PACC - sd_PACC/sqrt(n),
                    ymax = mean_PACC + sd_PACC/sqrt(n)),
                width = 0.1) +
  labs(x = "Year", y = "Mean PACC.raw", color = "Study") +
  theme_minimal()

ggsave("data/plots/trend_graph.png", plot = p, width = 8, height = 6, dpi = 300)


# PACC_all <- PACC_all %>%
#   mutate(across(
#     c(
#       BID,PTGENDER_base, PTMARRY_base, AAPOEGNPRSNFLG_base, PTETHNIC_base, PTEDUCAT_group,
#       CAFFEINE_group, WALKING_group, BMI_group, SMOKE_group, Alcohol_group,
#       AEROBIC_group, SLEEP_group, SBP_group, SLEEPDAY_group,
#       GDS15_group, Chol200_group,Chol5.1_group, HbA1c6.5_group,PTEDUCAT_group_all,
#       CAFFEINE_group_all, WALKING_group_all, BMI_group_all, SMOKE_group_all, Alcohol_group_all,
#       AEROBIC_group_all, SLEEP_group_all, SBP_group_all, SLEEPDAY_group_all,
#       GDS15_group_all, Chol200_group_all, HbA1c6.5_group_all, PTGENDER_all,
#       PTMARRY_all,AAPOEGNPRSNFLG_all,Chol5.1_group_all,
#       PTETHNIC_all),
#     ~ factor(.)
#   ))


colnames(PACC_all)

### change to only keep the columns that we need that match the code 
PACC_all <- PACC_all %>%
  mutate(across(
    c(
      RID, SBP_group,Chol200_group,HbA1c6.5_group,PTEDUCAT_all,
      SBP_group_all, Chol200_group_all, HbA1c6.5_group_all, PTGENDER_all, PTAGE_all,
      PTMARRY_all,AAPOEGNPRSNFLG_all,PTETHNIC_all),
    ~ factor(.)
  ))


PACC_all %>%
  group_by(SUBSTUDY) %>%
  summarise(n_subjects = n_distinct(BID)) %>%
  ungroup() %>%
  bind_rows(
    tibble(SUBSTUDY = "Total", n_subjects = n_distinct(PACC_all$BID))
  )


# 要計算的連續變數
variables <- c("PTAGE_base", "PACC.raw")

# 儲存結果
result <- data.frame()

for (var in variables) {
  # 平均與標準差
  summary_stats <- PACC_all %>%
    group_by(SUBSTUDY) %>%
    summarise(
      mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      sd = round(sd(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    mutate(stat = paste0(mean, " (", sd, ")")) %>%
    select(SUBSTUDY, stat) %>%
    pivot_wider(names_from = SUBSTUDY, values_from = stat)
  
  # t-test
  test <- t.test(as.formula(paste(var, "~ SUBSTUDY")), data = PACC_all)
  p_val <- ifelse(test$p.value < 0.001, "<0.001", formatC(test$p.value, format = "f", digits = 3))
  
  # 結合變數與結果
  summary_stats <- summary_stats %>%
    mutate(variable = var, `P-value` = p_val) %>%
    select(variable, everything())
  
  # 合併
  result <- bind_rows(result, summary_stats)
}

# 顯示結果
print(result)

## AGE is not significant between groups
#     variable           A4        Learn           NA P-value
# 1 PTAGE_base 72.71 (7.04) 72.33 (7.59) 72.93 (7.39)   0.670
# 2   PACC.raw  -6.6 (9.04) -5.32 (7.74) -5.83 (8.08)  <0.001

#用 dplyr 計算各組age非 NA 樣本數
PACC_all %>%
  filter(!is.na(PTAGE_all)) %>%
  group_by(SUBSTUDY) %>%
  summarise(n = n())


# 分類變數清單（欄位名稱來自你的資料）
categorical_vars <- c(
  "PTGENDER_base", "PTMARRY_base", "PTETHNIC_base", "PTEDUCAT_group",
  "AAPOEGNPRSNFLG_base", "HbA1c6.5_group", "Chol200_group",
  "SBP_group")

# 初始化結果表格
formatted_results <- data.frame()

# for (var in categorical_vars) {
#   if (!(var %in% names(PACC_all))) next
  
#   # 去除 NA 值（不分析 missing）
#   subset_data <- PACC_all %>%
#     filter(!is.na(.data[[var]]))
  
#   # 建立列聯表
#   contingency_table <- table(subset_data$SUBSTUDY, subset_data[[var]])
#   categories <- colnames(contingency_table)
#   proportions <- prop.table(contingency_table, margin = 1) * 100
  
#   temp <- data.frame(
#     Variable = var,
#     Group = categories,
#     `A4 (%)` = paste0(contingency_table["A4", ], " (", round(proportions["A4", ], 1), "%)"),
#     `LEARN (%)` = paste0(contingency_table["LEARN", ], " (", round(proportions["LEARN", ], 1), "%)")
#   )
  
#   # 卡方檢定
#   chi_test <- suppressWarnings(chisq.test(contingency_table))
#   temp$`P-value` <- format.pval(chi_test$p.value, digits = 3, eps = 0.001)
  
#   formatted_results <- rbind(formatted_results, temp)
# }

for (var in categorical_vars) {
  if (!(var %in% names(PACC_all))) next
  
  subset_data <- PACC_all %>%
    filter(!is.na(.data[[var]]))
  
  # 建立列聯表
  contingency_table <- table(subset_data$SUBSTUDY, subset_data[[var]])
  
  # 確保 A4 和 LEARN 都在 rownames 中
  for (grp in c("A4", "LEARN")) {
    if (!(grp %in% rownames(contingency_table))) {
      contingency_table <- rbind(contingency_table, setNames(rep(0, ncol(contingency_table)), colnames(contingency_table)))
      rownames(contingency_table)[nrow(contingency_table)] <- grp
    }
  }
  
  categories <- colnames(contingency_table)
  proportions <- prop.table(contingency_table, margin = 1) * 100
  
  temp <- data.frame(
    Variable = var,
    Group = categories,
    `A4 (%)` = paste0(contingency_table["A4", ], " (", round(proportions["A4", ], 1), "%)"),
    `LEARN (%)` = paste0(contingency_table["LEARN", ], " (", round(proportions["LEARN", ], 1), "%)")
  )
  
  chi_test <- suppressWarnings(chisq.test(contingency_table))
  temp$`P-value` <- format.pval(chi_test$p.value, digits = 3, eps = 0.001)
  
  formatted_results <- rbind(formatted_results, temp)
}


knitr::kable(formatted_results, format = "markdown", row.names = FALSE)

####
knitr::kable(formatted_results, format = "markdown", row.names = FALSE)
# |Variable            |Group           |A4....      |LEARN.... |P-value |
# |:-------------------|:---------------|:-----------|:---------|:-------|
# |PTGENDER_base       |Female          |69 (48.6%)  |0 (NaN%)  |NA      |
# |PTGENDER_base       |Male            |73 (51.4%)  |0 (NaN%)  |NA      |
# |PTMARRY_base        |Divorced        |16 (11.3%)  |0 (NaN%)  |NA      |
# |PTMARRY_base        |Married         |106 (74.6%) |0 (NaN%)  |NA      |
# |PTMARRY_base        |Never married   |4 (2.8%)    |0 (NaN%)  |NA      |
# |PTMARRY_base        |Unknown         |0 (0%)      |0 (NaN%)  |NA      |
# |PTMARRY_base        |Widowed         |16 (11.3%)  |0 (NaN%)  |NA      |
# |PTETHNIC_base       |Hisp/Latino     |5 (3.5%)    |0 (NaN%)  |NA      |
# |PTETHNIC_base       |Not Hisp/Latino |137 (96.5%) |0 (NaN%)  |NA      |
# |PTETHNIC_base       |Unknown         |0 (0%)      |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |4               |0 (0%)      |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |6               |1 (0.7%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |7               |0 (0%)      |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |8               |2 (1.4%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |9               |1 (0.7%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |10              |1 (0.7%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |11              |2 (1.4%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |12              |14 (9.9%)   |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |13              |4 (2.8%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |14              |17 (12%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |15              |8 (5.6%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |16              |38 (26.8%)  |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |17              |4 (2.8%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |18              |28 (19.7%)  |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |19              |5 (3.5%)    |0 (NaN%)  |NA      |
# |PTEDUCAT_group      |20              |17 (12%)    |0 (NaN%)  |NA      |
# |AAPOEGNPRSNFLG_base |E4-             |51 (78.5%)  |0 (NaN%)  |NA      |
# |AAPOEGNPRSNFLG_base |E4+             |14 (21.5%)  |0 (NaN%)  |NA      |
# |HbA1c6.5_group      |No              |142 (100%)  |0 (NaN%)  |<0.001  |
# |Chol200_group       |Normal          |142 (100%)  |0 (NaN%)  |<0.001  |
# |SBP_group           |High            |47 (33.1%)  |0 (NaN%)  |NA      |
# |SBP_group           |Normal          |95 (66.9%)  |0 (NaN%)  |NA      |

#Education
# 建立四組交互變數
# PACC_all <- PACC_all %>%
#   mutate(
#     PTEDUCAT_G4 = interaction(SUBSTUDY, PTEDUCAT_all, drop = TRUE),
#     PTEDUCAT_G4 = factor(PTEDUCAT_G4,
#                          levels = c("A4.Low", "A4.High", "LEARN.Low", "LEARN.High"))
#   )
# See how many rows have complete data for your model
sum(complete.cases(PACC_all[, c("PACC.raw", "year", "PTEDUCAT_G4", "PTAGE_all", "SUBSTUDY")]))
## 1011 complete rows/cases to run this model on for education 

PACC_PTEDUCAT <- lmer(
  PACC.raw ~ year * PTEDUCAT_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_PTEDUCAT)
# REML criterion at convergence: 2045.3

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.6738 -0.1287  0.0107  0.0713  6.6460 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  BID      (Intercept) 59.609   7.721   
#  Residual              3.548   1.884   
# Number of obs: 407, groups:  BID, 117

# Fixed effects:
#                              Estimate Std. Error t value
# (Intercept)                   29.4608     9.1449   3.222
# year                          -3.0885     0.2702 -11.430
# PTEDUCAT_G4A4.Normal          -6.7263     2.5777  -2.609
# PTEDUCAT_G4Learn.High         -1.6573     1.7005  -0.975
# PTEDUCAT_G4Learn.Normal      -10.5520     2.6962  -3.914
# PTAGE_all                     -0.3428     0.1189  -2.882
# PTGENDER_allMale               1.1294     1.5894   0.711
# PTMARRY_allMarried            -6.8549     3.8040  -1.802
# PTMARRY_allNever married      -2.8230     5.8899  -0.479
# PTMARRY_allWidowed            -4.3071     4.3645  -0.987
# AAPOEGNPRSNFLG_allE4+         -2.9361     1.8417  -1.594
# PTETHNIC_allNot Hisp/Latino   -2.2704     4.1342  -0.549
# year:PTEDUCAT_G4A4.Normal     -0.3420     0.7358  -0.465
# year:PTEDUCAT_G4Learn.High     1.0624     0.4374   2.429
# year:PTEDUCAT_G4Learn.Normal   1.9879     0.4472   4.445


# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_PTEDUCAT,
  ~ PTEDUCAT_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
#PTEDUCAT_G4   emmean   SE  df lower.CL upper.CL
#  A4.High       -0.778 2.84 110    -6.41    4.849
#  A4.Normal     -7.504 3.36 125   -14.15   -0.857
#  Learn.High    -2.435 2.77 109    -7.93    3.058
#  Learn.Normal -11.330 3.54 139   -18.33   -4.333

# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
##significance varies for the model
#  contrast                  estimate   SE  df t.ratio p.value
#  A4.High - A4.Normal           6.73 2.58 153   2.605  0.0491
#  A4.High - Learn.High          1.66 1.70 123   0.974  0.7642
#  A4.High - Learn.Normal       10.55 2.70 166   3.906  0.0008
#  A4.Normal - Learn.High       -5.07 2.56 150  -1.979  0.2006
#  A4.Normal - Learn.Normal      3.83 2.88 342   1.328  0.5458
#  Learn.High - Learn.Normal     8.89 2.69 166   3.303  0.0064


# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_PTEDUCAT,
  specs = "PTEDUCAT_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)

# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
### em slope is not significant

#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_PTEDUCAT, at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | PTEDUCAT_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

p <- ggplot(pred_df, aes(x=year, y=emmean, color=PTEDUCAT_G4, fill=PTEDUCAT_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title = "Modeled PACC by Aβ status and Educational Group",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank())

ggsave("data/plots/PTEDUCAT_graph.png", plot = p, width = 8, height = 6, dpi = 300)

# #Smoke
# PACC_all <- PACC_all %>%
#   mutate(
#     SMOKE_G4 = interaction(SUBSTUDY, SMOKE_group_all, drop = TRUE),
#     SMOKE_G4 = factor(SMOKE_G4,
#                          levels = c("A4.Smoker", "A4.No smoke", "LEARN.Smoker", "LEARN.No smoke"))
#   )

# PACC_SMOKE <- lmer(
#   PACC.raw ~ year * SMOKE_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)
# summary(PACC_SMOKE)



# # 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_SMOKE,
#   ~ SMOKE_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)
# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.Smoker vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_SMOKE,
#   specs = "SMOKE_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)
# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))


# #Alcohol
# PACC_all <- PACC_all %>%
#   mutate(
#     Alcohol_G4 = interaction(SUBSTUDY, Alcohol_group_all, drop = TRUE),
#     Alcohol_G4 = factor(Alcohol_G4,
#                          levels = c("A4.Alcohol", "A4.No Alcohol", "LEARN.Alcohol", "LEARN.No Alcohol"))
#   )
# levels(PACC_all$Alcohol_group_all)
# PACC_Alcohol <- lmer(
#   PACC.raw ~ year * Alcohol_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_Alcohol)

# # 取得每組 group4 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_Alcohol,
#   ~ Alcohol_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)
# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.Alcohol vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_Alcohol,
#   specs = "Alcohol_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)
# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

# #Caffeine
# PACC_all <- PACC_all %>%
#   mutate(
#     CAFFEINE_G6 = interaction(SUBSTUDY, CAFFEINE_group_all, drop = TRUE),
#     CAFFEINE_G6 = factor(CAFFEINE_G6,
#                         levels = c("A4.No caffeine", "A4.Moderate", "A4.High", "LEARN.No caffeine", "LEARN.Moderate","LEARN.High"))
#   )
# PACC_CAFFEINE <- lmer(
#   PACC.raw ~ year * CAFFEINE_G6 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_CAFFEINE)

# # 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_CAFFEINE,
#   ~ CAFFEINE_G6,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)
# # 一對一比較 A4.High vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_CAFFEINE,
#   specs = "CAFFEINE_G6",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)

# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
# #Prediction 線性 LMM（隨機截距，固定斜率）
# yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
# rg <- ref_grid(PACC_Alcohol, at=list(year=yr), cov.reduce=mean, weights="proportional",
#                nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
# emm <- emmeans(rg, ~ year | Alcohol_G4, type="response")

# # 產生CI並統一欄名
# pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
#   rename(
#     lcl = any_of(c("lower.CL","asymp.LCL")),
#     ucl = any_of(c("upper.CL","asymp.UCL"))
#   )

# ggplot(pred_df, aes(x=year, y=emmean, color=Alcohol_G4, fill=Alcohol_G4)) +
#   geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
#   geom_line(linewidth=1) +
#   labs(x="Year", y="Modeled mean PACC",
#        title="Modeled PACC by Aβ status and Alcohol group",
#        color = NULL, 
#        fill  = NULL) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),   # 移除主網格
#     panel.grid.minor = element_blank(),   # 移除次網格
#     axis.line = element_line(color = "black"),  # 繪製X/Y軸線
#     panel.border = element_blank()        # 不要外框)
#   )

#HbA1c>=6.5
# PACC_all <- PACC_all %>%
#   mutate(
#     HbA1c_G4 = interaction(SUBSTUDY, HbA1c6.5_group_all, drop = TRUE),
#     HbA1c_G4 = factor(HbA1c_G4,
#                          levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
#   )
# See how many rows have complete data for your model
sum(complete.cases(PACC_all[, c("PACC.raw", "year", "HbA1c_G4", "PTAGE_all", "SUBSTUDY")]))
##1011 complete cases

PACC_HbA1c <- lmer(
  PACC.raw ~ year * HbA1c_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_HbA1c)

# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_HbA1c,
  ~ HbA1c_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
#預設限制在 3000 observation 以下才計算自由度 才用以下
#em_intercept <- emmeans(PACC_G4, ~ group4, at = list(VISIT_WEEK = -1))


# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
## not significant

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_HbA1c,
  specs = "HbA1c_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)


# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_HbA1c, at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | HbA1c_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

p <- ggplot(pred_df, aes(x=year, y=emmean, color=HbA1c_G4, fill=HbA1c_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title="Modeled PACC by Aβ status and Diabetes",
       color = NULL, 
       fill  = NULL) +
  scale_color_discrete(
    labels = c("A4.High"   = "A4 Diabetes",
               "A4.Normal" = "A4 Normal",
               "LEARN.High"   = "LEARN Diabetes",
               "LEARN.Normal" = "LEARN Normal")
  ) +
  scale_fill_discrete(
    labels = c("A4.High"   = "A4 Diabetes",
               "A4.Normal" = "A4 Normal",
               "LEARN.High"   = "LEARN Diabetes",
               "LEARN.Normal" = "LEARN Normal")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank()        # 不要外框)
  )

ggsave("data/plots/HbA1c_graph.png", plot = p, width = 8, height = 6, dpi = 300)


#Cholesterol >=200
# PACC_all <- PACC_all %>%
#   mutate(
#     chol200_G4 = interaction(SUBSTUDY, Chol200_group_all, drop = TRUE),
#     chol200_G4 = factor(chol200_G4,
#                       levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
#   )
# See how many rows have complete data for your model
sum(complete.cases(PACC_all[, c("PACC.raw", "year", "chol200_G4", "PTAGE_all", "SUBSTUDY")]))
##1011 complete cases

PACC_chol200 <- lmer(
  PACC.raw ~ year * chol200_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_chol200)


# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_chol200,
  ~ chol200_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
## error in contrast because not enough data in one group

# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
# contrast                 estimate   SE  df t.ratio p.value
#  A4.Normal - Learn.Normal     1.86 1.38 253   1.353  0.1771

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_chol200,
  specs = "chol200_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)
# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#  contrast                 estimate    SE  df lower.CL upper.CL t.ratio p.value
#  A4.Normal - Learn.Normal     -1.5 0.352 353    -2.19   -0.804  -4.249  <.0001

## we only need the chol200 group 
# #Cholesterol >=5.1 mmol/L (SI)
# PACC_all <- PACC_all %>%
#   mutate(
#     chol5.1_G4 = interaction(SUBSTUDY, Chol5.1_group_all, drop = TRUE),
#     chol5.1_G4 = factor(chol5.1_G4,
#                         levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
#   )
# PACC_chol5.1 <- lmer(
#   PACC.raw ~ year * chol5.1_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_chol5.1)
# levels(PACC_all$Chol5.1_group_all)


# # 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_chol5.1,
#   ~ chol5.1_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)
# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.High vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_chol5.1,
#   specs = "chol5.1_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)
# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_chol200 , at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | chol200_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

P <- ggplot(pred_df, aes(x=year, y=emmean, color=chol200_G4, fill=chol200_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title="Modeled PACC by Aβ status and Cholesterol group",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank()        # 不要外框)
  )

ggsave("data/plots/chol200_graph.png", plot = p, width = 8, height = 6, dpi = 300)

#BP_group

# PACC_all <- PACC_all %>%
#   mutate(
#     SBP_G4 = interaction(SUBSTUDY, SBP_group_all, drop = TRUE),
#     SBP_G4 = factor(SBP_G4,
#                       levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
#   )

PACC_SBP <- lmer(
  PACC.raw ~ year * SBP_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_SBP)

# 取得每組 group4 在 year = 0 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_SBP,
  ~ SBP_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)


# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
#  contrast                  estimate   SE  df t.ratio p.value
#  A4.High - A4.Normal          -2.35 1.15 359  -2.046  0.1731
#  A4.High - Learn.High          4.90 1.94 361   2.522  0.0582
#  A4.High - Learn.Normal        1.31 1.54 330   0.852  0.8295
#  A4.Normal - Learn.High        7.25 1.92 337   3.766  0.0011
#  A4.Normal - Learn.Normal      3.66 1.53 288   2.397  0.0800
#  Learn.High - Learn.Normal    -3.58 1.43 376  -2.504  0.0608

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_SBP,
  specs = "SBP_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)


# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
# contrast                  estimate    SE  df lower.CL upper.CL t.ratio p.value
#  A4.High - A4.Normal          1.855 0.542 362    0.456    3.253   3.423  0.0038
#  A4.High - Learn.High        -2.783 0.850 361   -4.978   -0.588  -3.272  0.0064
#  A4.High - Learn.Normal      -0.287 0.478 341   -1.522    0.947  -0.601  0.9317
#  A4.Normal - Learn.High      -4.637 0.810 367   -6.728   -2.546  -5.724  <.0001
#  A4.Normal - Learn.Normal    -2.142 0.413 358   -3.207   -1.077  -5.192  <.0001
#  Learn.High - Learn.Normal    2.495 0.760 364    0.534    4.457   3.283  0.0062
#BP_group
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_SBP , at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | SBP_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

p <- ggplot(pred_df, aes(x=year, y=emmean, color=SBP_G4, fill=SBP_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title="Modeled PACC by Aβ status and Blood Pressure group",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank()        # 不要外框)
  )
ggsave("data/plots/SBP_graph.png", plot = p, width = 8, height = 6, dpi = 300)

# #BMI
# PACC_all <- PACC_all %>%
#   mutate(
#     BMI_G4 = interaction(SUBSTUDY, BMI_group_all, drop = TRUE),
#     BMI_G4 = factor(BMI_G4,
#                       levels = c("A4.Obesity", "A4.No obesity",  "LEARN.Obesity", "LEARN.No obesity"))
#   )
# PACC_BMI <- lmer(
#   PACC.raw ~ year * BMI_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_BMI)

# # 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_BMI,
#   ~ BMI_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)

# # 一對一比較 A4.Obesity vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_BMI,
#   specs = "BMI_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)
# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))

# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
# #BMI
# #Prediction 線性 LMM（隨機截距，固定斜率）
# yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
# rg <- ref_grid(PACC_BMI , at=list(year=yr), cov.reduce=mean, weights="proportional",
#                nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
# emm <- emmeans(rg, ~ year | BMI_G4, type="response")

# # 產生CI並統一欄名
# pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
#   rename(
#     lcl = any_of(c("lower.CL","asymp.LCL")),
#     ucl = any_of(c("upper.CL","asymp.UCL"))
#   )

# ggplot(pred_df, aes(x=year, y=emmean, color=BMI_G4, fill=BMI_G4)) +
#   geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
#   geom_line(linewidth=1) +
#   labs(x="Year", y="Modeled mean PACC",
#        title="Modeled PACC by Aβ status and Obesity",
#        color = NULL, 
#        fill  = NULL) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),   # 移除主網格
#     panel.grid.minor = element_blank(),   # 移除次網格
#     axis.line = element_line(color = "black"),  # 繪製X/Y軸線
#     panel.border = element_blank()        # 不要外框)
#   )

# #GDS15
# PACC_all <- PACC_all %>%
#   mutate(
#     GDS15_G4 = interaction(SUBSTUDY, GDS15_group_all, drop = TRUE),
#     GDS15_G4 = factor(GDS15_G4,
#                     levels = c("A4.Positive", "A4.Negative", "LEARN.Positive", "LEARN.Negative"))
#   )
# PACC_GDS <- lmer(
#   PACC.raw ~ year * GDS15_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_GDS)

# # 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_GDS,
#   ~ GDS15_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)
# #預設限制在 3000 observation 以下才計算自由度 才用以下
# #em_intercept <- emmeans(PACC_G4, ~ group4, at = list(VISIT_WEEK = -1))


# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.High vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_GDS,
#   specs = "GDS15_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)


# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
# #GDS
# #Prediction 線性 LMM（隨機截距，固定斜率）
# yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
# rg <- ref_grid(PACC_GDS , at=list(year=yr), cov.reduce=mean, weights="proportional",
#                nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
# emm <- emmeans(rg, ~ year | GDS15_G4, type="response")

# # 產生CI並統一欄名
# pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
#   rename(
#     lcl = any_of(c("lower.CL","asymp.LCL")),
#     ucl = any_of(c("upper.CL","asymp.UCL"))
#   )
# ggplot(pred_df, aes(x=year, y=emmean, color=GDS15_G4, fill=GDS15_G4)) +
#   geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
#   geom_line(linewidth=1) +
#   labs(
#     x = "Year", y = "Modeled mean PACC",
#     title = "Modeled PACC by Aβ status and Depressive group",
#     color = NULL,   # 這裡 legend 標題你可以改成 NULL 或 "Depressive status"
#     fill  = NULL
#   ) +
#   scale_color_discrete(
#     labels = c("A4.Positive"="A4 Depressive mood",
#                "A4.Negative"="A4 Normal",
#                "LEARN.Positive"="LEARN Depressive mood",
#                "LEARN.Negative"="LEARN Normal")
#   ) +
#   scale_fill_discrete(
#     labels = c("A4.Positive"="A4 Depressive mood",
#                "A4.Negative"="A4 Normal",
#                "LEARN.Positive"="LEARN Depressive mood",
#                "LEARN.Negative"="LEARN Normal")
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(color = "black"),
#     panel.border = element_blank()
#   )

# #Walking time
# PACC_all <- PACC_all %>%
#   mutate(
#     WALKING_G4 = interaction(SUBSTUDY, WALKING_group_all, drop = TRUE),
#     WALKING_G4 = factor(WALKING_G4,
#                       levels = c("A4.Inactive", "A4.Active", "LEARN.Inactive", "LEARN.Active"))
#   )
# PACC_WALKING <- lmer(
#   PACC.raw ~ year * WALKING_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_WALKING)
# # 取得每組 group4 在 year = 0 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_WALKING,
#   ~ WALKING_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)


# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.Inactive vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_WALKING,
#   specs = "WALKING_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)
# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
# #WALKING
# #Prediction 線性 LMM（隨機截距，固定斜率）
# yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
# rg <- ref_grid(PACC_WALKING, at=list(year=yr), cov.reduce=mean, weights="proportional",
#                nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
# emm <- emmeans(rg, ~ year | WALKING_G4, type="response")

# # 產生CI並統一欄名
# pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
#   rename(
#     lcl = any_of(c("lower.CL","asymp.LCL")),
#     ucl = any_of(c("upper.CL","asymp.UCL"))
#   )

# ggplot(pred_df, aes(x=year, y=emmean, color=WALKING_G4, fill=WALKING_G4)) +
#   geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
#   geom_line(linewidth=1) +
#   labs(x="Year", y="Modeled mean PACC",
#        title = "Modeled PACC by Aβ status and Walking Group",
#        color = NULL, 
#        fill  = NULL) +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),   # 移除主網格
#     panel.grid.minor = element_blank(),   # 移除次網格
#     axis.line = element_line(color = "black"),  # 繪製X/Y軸線
#     panel.border = element_blank())
# #AEROBIC time
# PACC_all <- PACC_all %>%
#   mutate(
#     AEROBIC_G4 = interaction(SUBSTUDY, AEROBIC_group_all, drop = TRUE),
#     AEROBIC_G4 = factor(AEROBIC_G4,
#                         levels = c("A4.No AEROBIC", "A4.AEROBIC", "LEARN.No AEROBIC", "LEARN.AEROBIC"))
#   )
# PACC_AEROBIC <- lmer(
#   PACC.raw ~ year * AEROBIC_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_AEROBIC)

# # 取得每組 group4 在 year = 0 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_AEROBIC,
#   ~ AEROBIC_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)

# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.InAEROBIC vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_AEROBIC,
#   specs = "AEROBIC_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)


# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

# #SLEEP time
# PACC_all <- PACC_all %>%
#   mutate(
#     SLEEP_G4 = interaction(SUBSTUDY, SLEEP_group_all, drop = TRUE),
#     SLEEP_G4 = factor(SLEEP_G4,
#                       levels = c("A4.Inadequate","A4.Adequate", "LEARN.Inadequate", "LEARN.Adequate"))
#   )
# PACC_SLEEP <- lmer(
#   PACC.raw ~ year * SLEEP_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_SLEEP)

# # 取得每組 group4 在 year = 0 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_SLEEP,
#   ~ SLEEP_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)


# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.InSLEEP vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_SLEEP,
#   specs = "SLEEP_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)

# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

# #NAP TIME
# PACC_all <- PACC_all %>%
#   mutate(
#     SLEEPDAY_G4 = interaction(SUBSTUDY, SLEEPDAY_group_all, drop = TRUE),
#     SLEEPDAY_G4 = factor(SLEEPDAY_G4,
#                          levels = c("A4.Extended", "A4.Short", "LEARN.Extended", "LEARN.Short"))
#   )
# PACC_SLEEPDAY <- lmer(
#   PACC.raw ~ year * SLEEPDAY_G4 +
#     PTAGE_all + PTGENDER_all + PTMARRY_all + 
#     AAPOEGNPRSNFLG_all + PTETHNIC_all +
#     (1 | BID), data = PACC_all)

# summary(PACC_SLEEPDAY)

# # 取得每組 group4 在 year = 0 的估計值（即 intercept）
# em_intercept <- emmeans(
#   PACC_SLEEPDAY,
#   ~ SLEEPDAY_G4,
#   at = list(year = 0),
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_intercept)


# # 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
# contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一比較 A4.InSLEEP vs 每一組，
# pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# # 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
# em_slope <- emtrends(
#   PACC_SLEEPDAY,
#   specs = "SLEEPDAY_G4",
#   var = "year",
#   lmerTest.limit = 1e5,
#   pbkrtest.limit = 1e5
# )
# summary(em_slope)

# # 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
# contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# # 一對一配對檢定組間 slope 差異
# pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))


#interaction
#P value_9 exclude sleep
pvals9 <- c(0.0431, 0.8528, 0.8721,  0.0039, 0.0001, 0.1515, 0.0914, 0.225, 0.0061)
# Bonferroni 校正
pvals9_bonf <- p.adjust(pvals9, method = "bonferroni")
# 看結果
pvals9_bonf

#P value_8 exclude smoke
pvals8 <- c(0.0431, 0.8721,  0.0039, 0.0001, 0.1515, 0.0914, 0.225, 0.0061)
# Bonferroni 校正
pvals8_bonf <- p.adjust(pvals8, method = "bonferroni")
# 看結果
pvals8_bonf


#P value_7 exclude smoke and depression
pvals7 <- c(0.0431, 0.8721,  0.0039, 0.0001, 0.1515, 0.0914,  0.0061)
# Bonferroni 校正
pvals7_bonf <- p.adjust(pvals7, method = "bonferroni")
# 看結果
pvals7_bonf

#A4 study adjusted P
#P value_9 exclude sleep
A4pvals9 <- c(0.0001, 0.1954, 0.9936, 0.0001, 0.0009, 0.5489, 0.0001, 0.8847, 0.0002)
# Bonferroni 校正
A4pvals9_bonf <- p.adjust(A4pvals9, method = "bonferroni")
# 看結果
A4pvals9_bonf

#P value_8 exclude smoke
A4pvals8 <- c(0.0001, 0.9936, 0.0001, 0.0009, 0.5489, 0.0001, 0.8847, 0.0002)
# Bonferroni 校正
A4pvals8_bonf <- p.adjust(A4pvals8, method = "bonferroni")
# 看結果
A4pvals8_bonf

#P value_7 exclude smoke and depression
A4pvals7 <- c(0.0001, 0.9936, 0.0001, 0.0009, 0.5489, 0.0001, 0.0002)
# Bonferroni 校正
A4pvals7_bonf <- p.adjust(A4pvals7, method = "bonferroni")
# 看結果
A4pvals7_bonf

#LEARN STUDY adjusted P
#P value_9 exclude sleep
LNpvals9 <- c(0.9933,0.9954, 0.9876,  0.0751, 0.0364, 0.8021, 0.9150, 0.2372, 0.8665)
# Bonferroni 校正
LNpvals9_bonf <- p.adjust(LNpvals9, method = "bonferroni")
# 看結果
LNpvals9_bonf

#P value_8 exclude smoke
LNpvals8 <- c(0.9933, 0.9876,  0.0751, 0.0364, 0.8021, 0.9150, 0.2372, 0.8665)
# Bonferroni 校正
LNpvals8_bonf <- p.adjust(LNpvals8, method = "bonferroni")
# 看結果
LNpvals8_bonf


#P value_7 exclude smoke and depression
LNpvals7 <- c(0.9933, 0.9876,  0.0751, 0.0364, 0.8021, 0.9150,  0.8665)
# Bonferroni 校正
LNpvals7_bonf <- p.adjust(LNpvals7, method = "bonferroni")
# 看結果
LNpvals7_bonf

