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
ggplot(pacc_summary, aes(x = year, y = mean_PACC, color = SUBSTUDY, group = SUBSTUDY)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_PACC - sd_PACC/sqrt(n),
                    ymax = mean_PACC + sd_PACC/sqrt(n)),
                width = 0.1) +
  labs(x = "Year", y = "Mean PACC.raw", color = "Study") +
  theme_minimal()


PACC_all <- PACC_all %>%
  mutate(across(
    c(
      BID,PTGENDER_base, PTMARRY_base, AAPOEGNPRSNFLG_base, PTETHNIC_base, PTEDUCAT_group,
      CAFFEINE_group, WALKING_group, BMI_group, SMOKE_group, Alcohol_group,
      AEROBIC_group, SLEEP_group, SBP_group, SLEEPDAY_group,
      GDS15_group, Chol200_group,Chol5.1_group, HbA1c6.5_group,PTEDUCAT_group_all,
      CAFFEINE_group_all, WALKING_group_all, BMI_group_all, SMOKE_group_all, Alcohol_group_all,
      AEROBIC_group_all, SLEEP_group_all, SBP_group_all, SLEEPDAY_group_all,
      GDS15_group_all, Chol200_group_all, HbA1c6.5_group_all, PTGENDER_all,
      PTMARRY_all,AAPOEGNPRSNFLG_all,Chol5.1_group_all,
      PTETHNIC_all),
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

#用 dplyr 計算各組age非 NA 樣本數
PACC_all %>%
  filter(!is.na(PTAGE_base)) %>%
  group_by(SUBSTUDY) %>%
  summarise(n = n())


# 分類變數清單（欄位名稱來自你的資料）
categorical_vars <- c(
  "PTGENDER_base", "PTMARRY_base", "PTETHNIC_base", "PTEDUCAT_group",
  "AAPOEGNPRSNFLG_base","SMOKE_group", "Alcohol_group","CAFFEINE_group", "HbA1c6.5_group", "Chol200_group",
  "SBP_group", "BMI_group", "GDS15_group", "WALKING_group", "AEROBIC_group",
   "SLEEP_group", "SLEEPDAY_group","Chol5.1_group")

# 初始化結果表格
formatted_results <- data.frame()

for (var in categorical_vars) {
  if (!(var %in% names(PACC_all))) next
  
  # 去除 NA 值（不分析 missing）
  subset_data <- PACC_all %>%
    filter(!is.na(.data[[var]]))
  
  # 建立列聯表
  contingency_table <- table(subset_data$SUBSTUDY, subset_data[[var]])
  categories <- colnames(contingency_table)
  proportions <- prop.table(contingency_table, margin = 1) * 100
  
  temp <- data.frame(
    Variable = var,
    Group = categories,
    `A4 (%)` = paste0(contingency_table["A4", ], " (", round(proportions["A4", ], 1), "%)"),
    `LEARN (%)` = paste0(contingency_table["LEARN", ], " (", round(proportions["LEARN", ], 1), "%)")
  )
  
  # 卡方檢定
  chi_test <- suppressWarnings(chisq.test(contingency_table))
  temp$`P-value` <- format.pval(chi_test$p.value, digits = 3, eps = 0.001)
  
  formatted_results <- rbind(formatted_results, temp)
}

knitr::kable(formatted_results, format = "markdown", row.names = FALSE)



#Education
# 建立四組交互變數
PACC_all <- PACC_all %>%
  mutate(
    PTEDUCAT_G4 = interaction(SUBSTUDY, PTEDUCAT_group_all, drop = TRUE),
    PTEDUCAT_G4 = factor(PTEDUCAT_G4,
                         levels = c("A4.Low", "A4.High", "LEARN.Low", "LEARN.High"))
  )
PACC_PTEDUCAT <- lmer(
  PACC.raw ~ year * PTEDUCAT_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_PTEDUCAT)


# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_PTEDUCAT,
  ~ PTEDUCAT_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)

# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

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

ggplot(pred_df, aes(x=year, y=emmean, color=PTEDUCAT_G4, fill=PTEDUCAT_G4)) +
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


#Smoke
PACC_all <- PACC_all %>%
  mutate(
    SMOKE_G4 = interaction(SUBSTUDY, SMOKE_group_all, drop = TRUE),
    SMOKE_G4 = factor(SMOKE_G4,
                         levels = c("A4.Smoker", "A4.No smoke", "LEARN.Smoker", "LEARN.No smoke"))
  )

PACC_SMOKE <- lmer(
  PACC.raw ~ year * SMOKE_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)
summary(PACC_SMOKE)



# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_SMOKE,
  ~ SMOKE_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.Smoker vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_SMOKE,
  specs = "SMOKE_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)
# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))


#Alcohol
PACC_all <- PACC_all %>%
  mutate(
    Alcohol_G4 = interaction(SUBSTUDY, Alcohol_group_all, drop = TRUE),
    Alcohol_G4 = factor(Alcohol_G4,
                         levels = c("A4.Alcohol", "A4.No Alcohol", "LEARN.Alcohol", "LEARN.No Alcohol"))
  )
levels(PACC_all$Alcohol_group_all)
PACC_Alcohol <- lmer(
  PACC.raw ~ year * Alcohol_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_Alcohol)

# 取得每組 group4 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_Alcohol,
  ~ Alcohol_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.Alcohol vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_Alcohol,
  specs = "Alcohol_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)
# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

#Caffeine
PACC_all <- PACC_all %>%
  mutate(
    CAFFEINE_G6 = interaction(SUBSTUDY, CAFFEINE_group_all, drop = TRUE),
    CAFFEINE_G6 = factor(CAFFEINE_G6,
                        levels = c("A4.No caffeine", "A4.Moderate", "A4.High", "LEARN.No caffeine", "LEARN.Moderate","LEARN.High"))
  )
PACC_CAFFEINE <- lmer(
  PACC.raw ~ year * CAFFEINE_G6 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_CAFFEINE)

# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_CAFFEINE,
  ~ CAFFEINE_G6,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_CAFFEINE,
  specs = "CAFFEINE_G6",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)

# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_Alcohol, at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | Alcohol_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

ggplot(pred_df, aes(x=year, y=emmean, color=Alcohol_G4, fill=Alcohol_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title="Modeled PACC by Aβ status and Alcohol group",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank()        # 不要外框)
  )

#HbA1c>=6.5
PACC_all <- PACC_all %>%
  mutate(
    HbA1c_G4 = interaction(SUBSTUDY, HbA1c6.5_group_all, drop = TRUE),
    HbA1c_G4 = factor(HbA1c_G4,
                         levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )
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

ggplot(pred_df, aes(x=year, y=emmean, color=HbA1c_G4, fill=HbA1c_G4)) +
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


#Cholesterol >=200
PACC_all <- PACC_all %>%
  mutate(
    chol200_G4 = interaction(SUBSTUDY, Chol200_group_all, drop = TRUE),
    chol200_G4 = factor(chol200_G4,
                      levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )
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
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

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

#Cholesterol >=5.1 mmol/L (SI)
PACC_all <- PACC_all %>%
  mutate(
    chol5.1_G4 = interaction(SUBSTUDY, Chol5.1_group_all, drop = TRUE),
    chol5.1_G4 = factor(chol5.1_G4,
                        levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )
PACC_chol5.1 <- lmer(
  PACC.raw ~ year * chol5.1_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_chol5.1)
levels(PACC_all$Chol5.1_group_all)


# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_chol5.1,
  ~ chol5.1_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.High vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_chol5.1,
  specs = "chol5.1_G4",
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
rg <- ref_grid(PACC_chol200 , at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | chol200_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

ggplot(pred_df, aes(x=year, y=emmean, color=chol200_G4, fill=chol200_G4)) +
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

#BP_group

PACC_all <- PACC_all %>%
  mutate(
    SBP_G4 = interaction(SUBSTUDY, SBP_group_all, drop = TRUE),
    SBP_G4 = factor(SBP_G4,
                      levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )
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

ggplot(pred_df, aes(x=year, y=emmean, color=SBP_G4, fill=SBP_G4)) +
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

#BMI
PACC_all <- PACC_all %>%
  mutate(
    BMI_G4 = interaction(SUBSTUDY, BMI_group_all, drop = TRUE),
    BMI_G4 = factor(BMI_G4,
                      levels = c("A4.Obesity", "A4.No obesity",  "LEARN.Obesity", "LEARN.No obesity"))
  )
PACC_BMI <- lmer(
  PACC.raw ~ year * BMI_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_BMI)

# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_BMI,
  ~ BMI_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)

# 一對一比較 A4.Obesity vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_BMI,
  specs = "BMI_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)
# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))

# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#BMI
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_BMI , at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | BMI_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

ggplot(pred_df, aes(x=year, y=emmean, color=BMI_G4, fill=BMI_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title="Modeled PACC by Aβ status and Obesity",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank()        # 不要外框)
  )

#GDS15
PACC_all <- PACC_all %>%
  mutate(
    GDS15_G4 = interaction(SUBSTUDY, GDS15_group_all, drop = TRUE),
    GDS15_G4 = factor(GDS15_G4,
                    levels = c("A4.Positive", "A4.Negative", "LEARN.Positive", "LEARN.Negative"))
  )
PACC_GDS <- lmer(
  PACC.raw ~ year * GDS15_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_GDS)

# 取得每組 group4 在 VISIT_WEEK = -1 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_GDS,
  ~ GDS15_G4,
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

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_GDS,
  specs = "GDS15_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)


# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#GDS
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_GDS , at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | GDS15_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )
ggplot(pred_df, aes(x=year, y=emmean, color=GDS15_G4, fill=GDS15_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(
    x = "Year", y = "Modeled mean PACC",
    title = "Modeled PACC by Aβ status and Depressive group",
    color = NULL,   # 這裡 legend 標題你可以改成 NULL 或 "Depressive status"
    fill  = NULL
  ) +
  scale_color_discrete(
    labels = c("A4.Positive"="A4 Depressive mood",
               "A4.Negative"="A4 Normal",
               "LEARN.Positive"="LEARN Depressive mood",
               "LEARN.Negative"="LEARN Normal")
  ) +
  scale_fill_discrete(
    labels = c("A4.Positive"="A4 Depressive mood",
               "A4.Negative"="A4 Normal",
               "LEARN.Positive"="LEARN Depressive mood",
               "LEARN.Negative"="LEARN Normal")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

#Walking time
PACC_all <- PACC_all %>%
  mutate(
    WALKING_G4 = interaction(SUBSTUDY, WALKING_group_all, drop = TRUE),
    WALKING_G4 = factor(WALKING_G4,
                      levels = c("A4.Inactive", "A4.Active", "LEARN.Inactive", "LEARN.Active"))
  )
PACC_WALKING <- lmer(
  PACC.raw ~ year * WALKING_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_WALKING)
# 取得每組 group4 在 year = 0 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_WALKING,
  ~ WALKING_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)


# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.Inactive vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正
# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_WALKING,
  specs = "WALKING_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)
# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))
#WALKING
#Prediction 線性 LMM（隨機截距，固定斜率）
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)
rg <- ref_grid(PACC_WALKING, at=list(year=yr), cov.reduce=mean, weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))
emm <- emmeans(rg, ~ year | WALKING_G4, type="response")

# 產生CI並統一欄名
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  )

ggplot(pred_df, aes(x=year, y=emmean, color=WALKING_G4, fill=WALKING_G4)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.15, color=NA) +
  geom_line(linewidth=1) +
  labs(x="Year", y="Modeled mean PACC",
       title = "Modeled PACC by Aβ status and Walking Group",
       color = NULL, 
       fill  = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # 移除主網格
    panel.grid.minor = element_blank(),   # 移除次網格
    axis.line = element_line(color = "black"),  # 繪製X/Y軸線
    panel.border = element_blank())
#AEROBIC time
PACC_all <- PACC_all %>%
  mutate(
    AEROBIC_G4 = interaction(SUBSTUDY, AEROBIC_group_all, drop = TRUE),
    AEROBIC_G4 = factor(AEROBIC_G4,
                        levels = c("A4.No AEROBIC", "A4.AEROBIC", "LEARN.No AEROBIC", "LEARN.AEROBIC"))
  )
PACC_AEROBIC <- lmer(
  PACC.raw ~ year * AEROBIC_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_AEROBIC)

# 取得每組 group4 在 year = 0 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_AEROBIC,
  ~ AEROBIC_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)

# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.InAEROBIC vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_AEROBIC,
  specs = "AEROBIC_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)


# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

#SLEEP time
PACC_all <- PACC_all %>%
  mutate(
    SLEEP_G4 = interaction(SUBSTUDY, SLEEP_group_all, drop = TRUE),
    SLEEP_G4 = factor(SLEEP_G4,
                      levels = c("A4.Inadequate","A4.Adequate", "LEARN.Inadequate", "LEARN.Adequate"))
  )
PACC_SLEEP <- lmer(
  PACC.raw ~ year * SLEEP_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_SLEEP)

# 取得每組 group4 在 year = 0 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_SLEEP,
  ~ SLEEP_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)


# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.InSLEEP vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_SLEEP,
  specs = "SLEEP_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)

# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))

#NAP TIME
PACC_all <- PACC_all %>%
  mutate(
    SLEEPDAY_G4 = interaction(SUBSTUDY, SLEEPDAY_group_all, drop = TRUE),
    SLEEPDAY_G4 = factor(SLEEPDAY_G4,
                         levels = c("A4.Extended", "A4.Short", "LEARN.Extended", "LEARN.Short"))
  )
PACC_SLEEPDAY <- lmer(
  PACC.raw ~ year * SLEEPDAY_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all + 
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 | BID), data = PACC_all)

summary(PACC_SLEEPDAY)

# 取得每組 group4 在 year = 0 的估計值（即 intercept）
em_intercept <- emmeans(
  PACC_SLEEPDAY,
  ~ SLEEPDAY_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)


# 建立 custom contrast：檢驗是否 G4 的效應大於加總效應
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一比較 A4.InSLEEP vs 每一組，
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# 估計各 group4 在 VISIT_WEEK 上的 slope（斜率）
em_slope <- emtrends(
  PACC_SLEEPDAY,
  specs = "SLEEPDAY_G4",
  var = "year",
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_slope)

# 檢驗：slope_G4 − (slope_G2 + slope_G3 − slope_G1)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 一對一配對檢定組間 slope 差異
pairs(em_slope, adjust = "tukey",infer = c(TRUE, TRUE))


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

