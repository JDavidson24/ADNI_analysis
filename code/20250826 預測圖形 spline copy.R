# 套件
library(lme4)
library(emmeans)
library(ggplot2)
library(dplyr)
library(splines)
library(grid)
## 1) 樣條 LMM：時間改用 natural cubic spline（兩個基底項）
#   固定效應：ns(year, df=3) * PTEDUCAT_G4（可改 df=2~4 調整彎曲度）
PACC_PTEDUCAT_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * PTEDUCAT_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)
## 2) 建參考格並取得「邊際預測」與 95% CI
yr <- seq(min(PACC_all$year, na.rm = TRUE),
          max(PACC_all$year, na.rm = TRUE),
          length.out = 200)
rg <- ref_grid(
  PACC_PTEDUCAT_spline,
  at = list(year = yr),
  cov.reduce = mean,                 # 連續共變項取平均
  weights   = "proportional",        # 類別依樣本比例加權
  nuisance  = c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all")
)
emm <- emmeans(rg, ~ year | PTEDUCAT_G4, type = "response")
pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL", "asymp.LCL")),
    ucl = any_of(c("upper.CL", "asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(PTEDUCAT_G4,
                         levels = c("A4.High","A4.Low","LEARN.High","LEARN.Low"))
  )

# 3) 繪圖（natural cubic spline帶有信賴帶、樣條彎曲
# 自訂顏色：A4 (紅色系), LEARN (藍色系)
custom_colors <- c(
  "A4.High"    = "#d73027",  # 深紅
  "A4.Low"     = "#fcae91",  # 更淺的紅
  "LEARN.High" = "#4575b4",  # 深藍
  "LEARN.Low"  = "#91bfdb"   # 淺藍
)

# 自訂線型：High = 實線, Low = 虛線
custom_linetypes <- c(
  "A4.High"    = "solid",
  "A4.Low"     = "dashed",
  "LEARN.High" = "solid",
  "LEARN.Low"  = "dashed"
)


# 先算出範圍
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

#繪圖
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2A. Modeled PACC by Aβ status and Educational group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),   # 移除預設軸線
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),  # 圖例線段長度
    legend.key.height = grid::unit(0.5, "cm"),  # 圖例高度
    legend.text = element_text(size = 10)       # 註解文字大小
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,              # 圖例線條細一點
        linetype  = custom_linetypes, # 保持 High/Low 區分
        color     = custom_colors     # 保持正確顏色
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -10)
  geom_segment(aes(x = 0, xend = x_max,
                   y = -10, yend = -10), color = "black") +   # X 軸
  geom_segment(aes(x = 0, xend = 0,
                   y = -10, yend = y_top), color = "black") + # Y 軸
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-10, NA)) +
  coord_cartesian(clip = "off")


#Alcohol
# --- 1) Alcohol Spline LMM ---
PACC_Alcohol_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * Alcohol_G4 +      # 🔑 使用 spline
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) Prediction (reference grid) ---
yr <- seq(min(PACC_all$year, na.rm=TRUE),
          max(PACC_all$year, na.rm=TRUE),
          length.out=50)   # 可以調整點數 (30~100)，越多曲線越平滑

rg <- ref_grid(PACC_Alcohol_spline,
               at=list(year=yr),
               cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | Alcohol_G4, type="response")

# --- 3) 建立預測資料集 (含信賴區間與分組) ---
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(Alcohol_G4,
                         levels = c("A4.Alcohol","A4.No Alcohol",
                                    "LEARN.Alcohol","LEARN.No Alcohol"))
  )

# --- 4) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.No Alcohol"     = "#d73027",  # 深紅
  "A4.Alcohol"  = "#fcae91",  # 淺紅
  "LEARN.No Alcohol"  = "#4575b4",  # 深藍
  "LEARN.Alcohol" = "#91bfdb" # 淺藍
)

custom_linetypes <- c(
  "A4.No Alcohol"     = "solid",
  "A4.Alcohol"  = "dashed",
  "LEARN.No Alcohol"  = "solid",
  "LEARN.Alcohol" = "dashed"
)

# --- 5) 軸線範圍 (人工軸線用) ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl,  na.rm = TRUE))

# --- 6) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2B. Modeled PACC by Aβ status and Alcohol group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -10)
  geom_segment(x = 0, xend = x_max,
               y = -8, yend = -8, color = "black") +   # X 軸
  geom_segment(x = 0, xend = 0,
               y = -8, yend = y_top, color = "black") + # Y 軸
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-8, NA)) +
  coord_cartesian(clip = "off")


# HbA1c 分組變數 ---
PACC_all <- PACC_all %>%
  mutate(
    HbA1c_G4 = interaction(SUBSTUDY, HbA1c6.5_group_all, drop = TRUE),
    HbA1c_G4 = factor(HbA1c_G4,
                      levels = c("A4.Normal","A4.High",
                                 "LEARN.Normal","LEARN.High"))
  )

# --- 1) HbA1c Spline LMM ---
PACC_HbA1c_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * HbA1c_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) Prediction (reference grid) ---
common_max <- PACC_all %>%
  group_by(HbA1c_G4) %>%
  summarise(max_year = max(year, na.rm = TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE),
          common_max,
          length.out = 50)

rg <- ref_grid(PACC_HbA1c_spline,
               at=list(year=yr),
               cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all",
                          "AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | HbA1c_G4, type="response")

# --- 3) 建立預測資料集 (含信賴區間與分組) ---
pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(HbA1c_G4,
                         levels = c("A4.Normal","A4.High",
                                    "LEARN.Normal","LEARN.High"))
  )

# --- 4) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.Normal"    = "#d73027",  # 深紅 (A4 高 HbA1c)
  "A4.High"  = "#fcae91",  # 淺紅 (A4 正常 HbA1c)
  "LEARN.Normal" = "#4575b4",  # 深藍 (LEARN 高 HbA1c)
  "LEARN.High" = "#91bfdb" # 淺藍 (LEARN 正常 HbA1c)
)

custom_linetypes <- c(
  "A4.Normal"    = "solid",
  "A4.High"  = "dashed",
  "LEARN.Normal" = "solid",
  "LEARN.High" = "dashed"
)
# --- 5) 軸線範圍 (人工軸線用) ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl,  na.rm = TRUE))

# --- 6) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2C. Modeled PACC by Aβ status and Diabetes group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -10)
  geom_segment(x = 0, xend = x_max,
               y = -10, yend = -10, color = "black") +
  geom_segment(x = 0, xend = 0,
               y = -10, yend = y_top, color = "black") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-10, NA)) +
  coord_cartesian(clip = "off")


#膽固醇
# --- 0) 在資料中建立 Cholesterol 分組變數 ---
PACC_all <- PACC_all %>%
  mutate(
    chol200_G4 = interaction(SUBSTUDY, Chol200_group_all, drop = TRUE),
    chol200_G4 = factor(chol200_G4,
                        levels = c("A4.High","A4.Normal",
                                   "LEARN.High","LEARN.Normal"))
  )

# --- 1) Cholesterol Spline LMM ---
PACC_Chol_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * chol200_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) 建參考格並取得「邊際預測」與 95% CI ---
# 找出所有組別共同的最大 year，避免有組別提早結束
common_max <- PACC_all %>%
  group_by(chol200_G4) %>%
  summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm = TRUE),
          common_max,
          length.out = 200)

rg <- ref_grid(
  PACC_Chol_spline,
  at = list(year = yr),
  cov.reduce = mean,
  weights   = "proportional",
  nuisance  = c("PTGENDER_all","PTMARRY_all",
                "AAPOEGNPRSNFLG_all","PTETHNIC_all")
)

emm <- emmeans(rg, ~ year | chol200_G4, type = "response")

pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(chol200_G4,
                         levels = c("A4.Normal","A4.High",
                                    "LEARN.Normal","LEARN.High"))
  )

# --- 3) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.Normal"    = "#d73027",  
  "A4.High"  = "#fcae91",  
  "LEARN.Normal" = "#4575b4",  
  "LEARN.High" = "#91bfdb" 
)

custom_linetypes <- c(
  "A4.Normal"    = "solid",
  "A4.High"  = "dashed",
  "LEARN.Normal" = "solid",
  "LEARN.High" = "dashed"
)

# --- 4) 軸線範圍 ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

# --- 5) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2D. Modeled PACC by Aβ status and cholesterol group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -10)
  geom_segment(x = 0, xend = x_max,
               y = -6, yend = -6, color = "black") +
  geom_segment(x = 0, xend = 0,
               y = -6, yend = 2, color = "black") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-6, 2)) +
  coord_cartesian(clip = "off")

#BP_group

PACC_all <- PACC_all %>%
  mutate(
    SBP_G4 = interaction(SUBSTUDY, SBP_group_all, drop = TRUE),
    SBP_G4 = factor(SBP_G4,
                    levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )
# --- 1) Blood Pressure Spline LMM ---
PACC_SBP_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * SBP_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) 建參考格並取得「邊際預測」與 95% CI ---
# 找出所有組別共同的最大 year，避免有組別提早結束
common_max <- PACC_all %>%
  group_by(SBP_G4) %>%
  summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm = TRUE),
          common_max,
          length.out = 200)

rg <- ref_grid(
  PACC_SBP_spline,
  at = list(year = yr),
  cov.reduce = mean,
  weights   = "proportional",
  nuisance  = c("PTGENDER_all","PTMARRY_all",
                "AAPOEGNPRSNFLG_all","PTETHNIC_all")
)

emm <- emmeans(rg, ~ year | SBP_G4, type = "response")

pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(SBP_G4,
                         levels = c("A4.Normal","A4.High",
                                    "LEARN.Normal","LEARN.High"))
  )

# --- 3) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.Normal"    = "#d73027",  # 淺紅 (A4 正常血壓)
  "A4.High"      = "#fcae91",  # 深紅 (A4 高血壓)
  "LEARN.Normal" = "#4575b4",  # 淺藍 (LEARN 正常血壓)
  "LEARN.High"   = "#91bfdb"   # 深藍 (LEARN 高血壓)
)

custom_linetypes <- c(
  "A4.Normal"    = "solid",
  "A4.High"      = "dashed",
  "LEARN.Normal" = "solid",
  "LEARN.High"   = "dashed"
)

# --- 4) 軸線範圍 ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

# --- 5) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2E. Modeled PACC by Aβ status and Blood Pressure group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -10)
  geom_segment(x = 0, xend = x_max,
               y = -6, yend = -6, color = "black") +
  geom_segment(x = 0, xend = 0,
               y = -6, yend = 2, color = "black") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-6, 2)) +
  coord_cartesian(clip = "off")



#BMI
PACC_all <- PACC_all %>%
  mutate(
    BMI_G4 = interaction(SUBSTUDY, BMI_group_all, drop = TRUE),
    BMI_G4 = factor(BMI_G4,
                    levels = c("A4.Obesity", "A4.No obesity",  "LEARN.Obesity", "LEARN.No obesity"))
  )
# --- 1) BMI Spline LMM ---
PACC_BMI_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * BMI_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) 建參考格並取得「邊際預測」與 95% CI ---
# 找出所有組別共同的最大 year，避免有組別提早結束
common_max <- PACC_all %>%
  group_by(BMI_G4) %>%
  summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm = TRUE),
          common_max,
          length.out = 200)

rg <- ref_grid(
  PACC_BMI_spline,
  at = list(year = yr),
  cov.reduce = mean,
  weights   = "proportional",
  nuisance  = c("PTGENDER_all","PTMARRY_all",
                "AAPOEGNPRSNFLG_all","PTETHNIC_all")
)

emm <- emmeans(rg, ~ year | BMI_G4, type = "response")

pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(BMI_G4,
                         levels = c("A4.Obesity","A4.No obesity",
                                    "LEARN.Obesity","LEARN.No obesity"))
  )

# --- 3) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.Obesity"     = "#d73027",  # 深紅 (A4 肥胖)
  "A4.No obesity"  = "#fcae91",  # 淺紅 (A4 非肥胖)
  "LEARN.Obesity"  = "#4575b4",  # 深藍 (LEARN 肥胖)
  "LEARN.No obesity" = "#91bfdb" # 淺藍 (LEARN 非肥胖)
)

custom_linetypes <- c(
  "A4.Obesity"     = "solid",
  "A4.No obesity"  = "dashed",
  "LEARN.Obesity"  = "solid",
  "LEARN.No obesity" = "dashed"
)

# --- 4) 軸線範圍 ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

# --- 5) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2F. Modeled PACC by Aβ status and BMI group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -5)
  geom_segment(x = 0, xend = x_max,
               y = -6, yend = -6, color = "black") +
  geom_segment(x = 0, xend = 0,
               y = -6, yend = 2, color = "black") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-6, 2)) +
  coord_cartesian(clip = "off")

#GDS-15
PACC_all <- PACC_all %>%
mutate(
  GDS15_G4 = interaction(SUBSTUDY, GDS15_group_all, drop = TRUE),
  GDS15_G4 = factor(GDS15_G4,
                    levels = c("A4.Positive", "A4.Negative", "LEARN.Positive", "LEARN.Negative"))
)
# --- 1) GDS15 Spline LMM ---
PACC_GDS15_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * GDS15_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- 2) 建參考格並取得「邊際預測」與 95% CI ---
common_max <- PACC_all %>%
  group_by(GDS15_G4) %>%
  summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm = TRUE),
          common_max,
          length.out = 200)

rg <- ref_grid(
  PACC_GDS15_spline,
  at = list(year = yr),
  cov.reduce = mean,
  weights   = "proportional",
  nuisance  = c("PTGENDER_all","PTMARRY_all",
                "AAPOEGNPRSNFLG_all","PTETHNIC_all")
)

emm <- emmeans(rg, ~ year | GDS15_G4, type = "response")

pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%
  rename(
    lcl = any_of(c("lower.CL","asymp.LCL")),
    ucl = any_of(c("upper.CL","asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(GDS15_G4,
                         levels = c("A4.Negative","A4.Positive",
                                    "LEARN.Negative","LEARN.Positive"))
  )

# --- 3) 自訂顏色與線型 ---
custom_colors <- c(
  "A4.Negative"    = "#d73027",  # 深紅 (A4 憂鬱 Positive)
  "A4.Positive"    = "#fcae91",  # 淺紅 (A4 憂鬱 Negative)
  "LEARN.Negative" = "#4575b4",  # 深藍 (LEARN 憂鬱 Positive)
  "LEARN.Positive" = "#91bfdb"   # 淺藍 (LEARN 憂鬱 Negative)
)

custom_linetypes <- c(
  "A4.Negative"    = "solid",
  "A4.Positive"    = "dashed",
  "LEARN.Negative" = "solid",
  "LEARN.Positive" = "dashed"
)

# --- 4) 軸線範圍 ---
x_max <- max(pred_df$year, na.rm = TRUE)

# --- 5) 繪圖 ---
ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "2G. Modeled PACC by Aβ status and GDS-15 group",
    x = "Year", y = "Modeled mean PACC"
  ) +
  theme_classic() +
  theme(
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.key.height = grid::unit(0.5, "cm"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values  = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  guides(
    fill = "none",
    color = guide_legend(
      override.aes = list(
        linewidth = 1.0,
        linetype  = custom_linetypes,
        color     = custom_colors
      )
    )
  ) +
  # 人工畫軸線 (交會點 0, -6) + 限制 y 軸範圍 -6~2
  geom_segment(x = 0, xend = x_max,
               y = -10, yend = -10, color = "black") +
  geom_segment(x = 0, xend = 0,
               y = -10, yend = 2, color = "black") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-10, 2)) +
  coord_cartesian(clip = "off")

