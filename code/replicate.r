# --- 套件 / Packages ---
library(lme4)       # Linear Mixed Models / 線性混合模型
library(emmeans)    # Estimated Marginal Means / 邊際均值
library(ggplot2)    # Visualization / 繪圖
library(dplyr)      # Data manipulation / 資料操作
library(splines)    # Natural cubic splines / 樣條函數
library(grid)       # Unit for legend size / 圖例單位設定

# -------------------------------------------------
# 1) Education (PTEDUCAT_G4) Spline LMM / 教育程度樣條 LMM
# -------------------------------------------------
PACC_PTEDUCAT_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * PTEDUCAT_G4 +   # natural cubic spline * Education
    PTAGE_all + PTGENDER_all + PTMARRY_all +   # Covariates / 協變數
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),                           # Random intercept + slope per subject / 個體隨機截距與斜率
  data = PACC_all
)

# --- Reference grid & marginal predictions / 建參考格與邊際預測 ---
yr <- seq(min(PACC_all$year, na.rm = TRUE),
          max(PACC_all$year, na.rm = TRUE),
          length.out = 200)

rg <- ref_grid(
  PACC_PTEDUCAT_spline,
  at = list(year = yr),
  cov.reduce = mean,            # Continuous covariates averaged / 連續協變數取平均
  weights   = "proportional",   # Factor levels weighted by sample proportion / 類別依樣本比例加權
  nuisance  = c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all") # Nuisance vars / 擾動變數
)

emm <- emmeans(rg, ~ year | PTEDUCAT_G4, type = "response") # Marginal means / 邊際均值

pred_df <- as.data.frame(confint(emm, level = 0.95)) %>%    # Confidence intervals / 信賴區間
  rename(
    lcl = any_of(c("lower.CL", "asymp.LCL")),
    ucl = any_of(c("upper.CL", "asymp.UCL"))
  ) %>%
  mutate(
    Group_Level = factor(PTEDUCAT_G4,
                         levels = c("A4.High","A4.Low","LEARN.High","LEARN.Low"))
  )

# --- Custom colors & linetypes / 自訂顏色與線型 ---
custom_colors <- c(
  "A4.High"    = "#d73027",  # Deep red / 深紅
  "A4.Low"     = "#fcae91",  # Light red / 淺紅
  "LEARN.High" = "#4575b4",  # Deep blue / 深藍
  "LEARN.Low"  = "#91bfdb"   # Light blue / 淺藍
)

custom_linetypes <- c(
  "A4.High"    = "solid",    # Solid line / 實線
  "A4.Low"     = "dashed",   # Dashed line / 虛線
  "LEARN.High" = "solid",
  "LEARN.Low"  = "dashed"
)

# --- Plot / 繪圖 ---
x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

ggplot(pred_df, aes(x = year, y = emmean,
                    color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group_Level),
              alpha = 0.18, color = NA, show.legend = FALSE) + # CI ribbon / 信賴帶
  geom_line(linewidth = 1.2) +                             # Mean line / 均值線
  labs(
    title = "2A. Modeled PACC by Aβ status and Educational group",
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
  geom_segment(aes(x = 0, xend = x_max, y = -10, yend = -10), color = "black") +  # X-axis / X 軸
  geom_segment(aes(x = 0, xend = 0, y = -10, yend = y_top), color = "black") +   # Y-axis / Y 軸
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(-10, NA)) +
  coord_cartesian(clip = "off")

# -------------------------------------------------
# 2) Alcohol (Alcohol_G4) Spline LMM / 酒精樣條 LMM
# -------------------------------------------------
PACC_Alcohol_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * Alcohol_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# Reference grid
yr <- seq(min(PACC_all$year, na.rm=TRUE), max(PACC_all$year, na.rm=TRUE), length.out=50)

rg <- ref_grid(PACC_Alcohol_spline,
               at=list(year=yr),
               cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | Alcohol_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl = any_of(c("lower.CL","asymp.LCL")),
         ucl = any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level = factor(Alcohol_G4,
                         levels = c("A4.Alcohol","A4.No Alcohol","LEARN.Alcohol","LEARN.No Alcohol")))

# Colors & linetypes
custom_colors <- c("A4.No Alcohol"="#d73027","A4.Alcohol"="#fcae91","LEARN.No Alcohol"="#4575b4","LEARN.Alcohol"="#91bfdb")
custom_linetypes <- c("A4.No Alcohol"="solid","A4.Alcohol"="dashed","LEARN.No Alcohol"="solid","LEARN.Alcohol"="dashed")

x_max <- max(pred_df$year, na.rm = TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm = TRUE))

# Plot
ggplot(pred_df, aes(x = year, y = emmean, color = Group_Level, linetype = Group_Level)) +
  geom_ribbon(aes(ymin=lcl,ymax=ucl,fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2B. Modeled PACC by Aβ status and Alcohol group", x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) + scale_fill_manual(values=custom_colors) + scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0,xend=x_max,y=-8,yend=-8,color="black") +
  geom_segment(x=0,xend=0,y=-8,yend=y_top,color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-8,NA)) + coord_cartesian(clip="off")


# -------------------------------------------------
# 3) HbA1c (HbA1c_G4) Spline LMM / 糖化血色素樣條 LMM
# -------------------------------------------------
PACC_all <- PACC_all %>%
  mutate(
    HbA1c_G4 = interaction(SUBSTUDY, HbA1c6.5_group_all, drop = TRUE),  # Combine study + HbA1c / 組合子研究+HbA1c
    HbA1c_G4 = factor(HbA1c_G4,
                      levels = c("A4.Normal","A4.High","LEARN.Normal","LEARN.High"))
  )

# --- Model / 模型 ---
PACC_HbA1c_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * HbA1c_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- Reference grid / 參考格 ---
common_max <- PACC_all %>%
  group_by(HbA1c_G4) %>%
  summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>%
  pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE), common_max, length.out=50)

rg <- ref_grid(PACC_HbA1c_spline,
               at=list(year=yr),
               cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | HbA1c_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl = any_of(c("lower.CL","asymp.LCL")),
         ucl = any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level = factor(HbA1c_G4,
                              levels = c("A4.Normal","A4.High","LEARN.Normal","LEARN.High")))

# --- Colors & linetypes / 顏色與線型 ---
custom_colors <- c(
  "A4.Normal" = "#d73027",  "A4.High" = "#fcae91",
  "LEARN.Normal" = "#4575b4", "LEARN.High" = "#91bfdb"
)

custom_linetypes <- c(
  "A4.Normal" = "solid", "A4.High" = "dashed",
  "LEARN.Normal" = "solid", "LEARN.High" = "dashed"
)

x_max <- max(pred_df$year, na.rm=TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm=TRUE))

# --- Plot / 繪圖 ---
ggplot(pred_df, aes(x=year, y=emmean, color=Group_Level, linetype=Group_Level)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2C. Modeled PACC by Aβ status and Diabetes group",
       x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) +
  scale_fill_manual(values=custom_colors) +
  scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0, xend=x_max, y=-10, yend=-10, color="black") +
  geom_segment(x=0, xend=0, y=-10, yend=y_top, color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-10,NA)) + coord_cartesian(clip="off")


# -------------------------------------------------
# 4) Cholesterol (chol200_G4) Spline LMM / 膽固醇樣條 LMM
# -------------------------------------------------
PACC_all <- PACC_all %>%
  mutate(
    chol200_G4 = interaction(SUBSTUDY, Chol200_group_all, drop=TRUE),
    chol200_G4 = factor(chol200_G4,
                        levels = c("A4.High","A4.Normal","LEARN.High","LEARN.Normal"))
  )

PACC_Chol_spline <- lmer(
  PACC.raw ~ ns(year, df=3) * chol200_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

common_max <- PACC_all %>%
  group_by(chol200_G4) %>% summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>% pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE), common_max, length.out=200)

rg <- ref_grid(PACC_Chol_spline, at=list(year=yr), cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | chol200_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl=any_of(c("lower.CL","asymp.LCL")), ucl=any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level=factor(chol200_G4, levels=c("A4.Normal","A4.High","LEARN.Normal","LEARN.High")))

custom_colors <- c("A4.Normal"="#d73027","A4.High"="#fcae91","LEARN.Normal"="#4575b4","LEARN.High"="#91bfdb")
custom_linetypes <- c("A4.Normal"="solid","A4.High"="dashed","LEARN.Normal"="solid","LEARN.High"="dashed")

x_max <- max(pred_df$year, na.rm=TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm=TRUE))

ggplot(pred_df, aes(x=year, y=emmean, color=Group_Level, linetype=Group_Level)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2D. Modeled PACC by Aβ status and cholesterol group", x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) +
  scale_fill_manual(values=custom_colors) +
  scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0,xend=x_max,y=-6,yend=-6,color="black") +
  geom_segment(x=0,xend=0,y=-6,yend=2,color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-6,2)) +
  coord_cartesian(clip="off")


# -------------------------------------------------
# 5) Blood Pressure (SBP_G4) Spline LMM / 血壓樣條 LMM
# -------------------------------------------------
PACC_all <- PACC_all %>%
  mutate(
    SBP_G4 = interaction(SUBSTUDY, SBP_group_all, drop = TRUE),
    SBP_G4 = factor(SBP_G4, levels = c("A4.High", "A4.Normal", "LEARN.High", "LEARN.Normal"))
  )

# --- Model / 模型 ---
PACC_SBP_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * SBP_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

# --- Reference grid / 參考格 ---
common_max <- PACC_all %>%
  group_by(SBP_G4) %>% summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>% pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE), common_max, length.out=200)

rg <- ref_grid(PACC_SBP_spline, at=list(year=yr), cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | SBP_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl=any_of(c("lower.CL","asymp.LCL")), ucl=any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level=factor(SBP_G4, levels=c("A4.Normal","A4.High","LEARN.Normal","LEARN.High")))

# --- Colors & Linetypes / 顏色與線型 ---
custom_colors <- c("A4.Normal"="#d73027","A4.High"="#fcae91","LEARN.Normal"="#4575b4","LEARN.High"="#91bfdb")
custom_linetypes <- c("A4.Normal"="solid","A4.High"="dashed","LEARN.Normal"="solid","LEARN.High"="dashed")

x_max <- max(pred_df$year, na.rm=TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm=TRUE))

# --- Plot / 繪圖 ---
ggplot(pred_df, aes(x=year, y=emmean, color=Group_Level, linetype=Group_Level)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2E. Modeled PACC by Aβ status and Blood Pressure group", x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) +
  scale_fill_manual(values=custom_colors) +
  scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0,xend=x_max,y=-6,yend=-6,color="black") +
  geom_segment(x=0,xend=0,y=-6,yend=2,color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-6,2)) +
  coord_cartesian(clip="off")


# -------------------------------------------------
# 6) BMI (BMI_G4) Spline LMM / 身體質量指數樣條 LMM
# -------------------------------------------------
PACC_all <- PACC_all %>%
  mutate(
    BMI_G4 = interaction(SUBSTUDY, BMI_group_all, drop = TRUE),
    BMI_G4 = factor(BMI_G4, levels = c("A4.Obesity","A4.No obesity","LEARN.Obesity","LEARN.No obesity"))
  )

PACC_BMI_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * BMI_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

common_max <- PACC_all %>%
  group_by(BMI_G4) %>% summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>% pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE), common_max, length.out=200)

rg <- ref_grid(PACC_BMI_spline, at=list(year=yr), cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | BMI_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl=any_of(c("lower.CL","asymp.LCL")), ucl=any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level=factor(BMI_G4, levels=c("A4.Obesity","A4.No obesity","LEARN.Obesity","LEARN.No obesity")))

custom_colors <- c("A4.Obesity"="#d73027","A4.No obesity"="#fcae91","LEARN.Obesity"="#4575b4","LEARN.No obesity"="#91bfdb")
custom_linetypes <- c("A4.Obesity"="solid","A4.No obesity"="dashed","LEARN.Obesity"="solid","LEARN.No obesity"="dashed")

x_max <- max(pred_df$year, na.rm=TRUE)
y_top <- ceiling(max(pred_df$ucl, na.rm=TRUE))

ggplot(pred_df, aes(x=year, y=emmean, color=Group_Level, linetype=Group_Level)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2F. Modeled PACC by Aβ status and BMI group", x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) +
  scale_fill_manual(values=custom_colors) +
  scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0,xend=x_max,y=-6,yend=-6,color="black") +
  geom_segment(x=0,xend=0,y=-6,yend=2,color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-6,2)) +
  coord_cartesian(clip="off")


# -------------------------------------------------
# 7) GDS-15 (GDS15_G4) Spline LMM / 憂鬱量表樣條 LMM
# -------------------------------------------------
PACC_all <- PACC_all %>%
  mutate(
    GDS15_G4 = interaction(SUBSTUDY, GDS15_group_all, drop = TRUE),
    GDS15_G4 = factor(GDS15_G4, levels = c("A4.Positive","A4.Negative","LEARN.Positive","LEARN.Negative"))
  )

PACC_GDS15_spline <- lmer(
  PACC.raw ~ ns(year, df = 3) * GDS15_G4 +
    PTAGE_all + PTGENDER_all + PTMARRY_all +
    AAPOEGNPRSNFLG_all + PTETHNIC_all +
    (1 + year | BID),
  data = PACC_all
)

common_max <- PACC_all %>%
  group_by(GDS15_G4) %>% summarise(max_year = max(year, na.rm=TRUE)) %>%
  summarise(min_max = min(max_year)) %>% pull(min_max)

yr <- seq(min(PACC_all$year, na.rm=TRUE), common_max, length.out=200)

rg <- ref_grid(PACC_GDS15_spline, at=list(year=yr), cov.reduce=mean,
               weights="proportional",
               nuisance=c("PTGENDER_all","PTMARRY_all","AAPOEGNPRSNFLG_all","PTETHNIC_all"))

emm <- emmeans(rg, ~ year | GDS15_G4, type="response")

pred_df <- as.data.frame(confint(emm, level=0.95)) %>%
  rename(lcl=any_of(c("lower.CL","asymp.LCL")), ucl=any_of(c("upper.CL","asymp.UCL"))) %>%
  mutate(Group_Level=factor(GDS15_G4, levels=c("A4.Negative","A4.Positive","LEARN.Negative","LEARN.Positive")))

custom_colors <- c("A4.Negative"="#d73027","A4.Positive"="#fcae91",
                   "LEARN.Negative"="#4575b4","LEARN.Positive"="#91bfdb")
custom_linetypes <- c("A4.Negative"="solid","A4.Positive"="dashed",
                      "LEARN.Negative"="solid","LEARN.Positive"="dashed")

x_max <- max(pred_df$year, na.rm=TRUE)

ggplot(pred_df, aes(x=year, y=emmean, color=Group_Level, linetype=Group_Level)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Group_Level), alpha=0.18, color=NA, show.legend=FALSE) +
  geom_line(linewidth=1.2) +
  labs(title="2G. Modeled PACC by Aβ status and GDS-15 group", x="Year", y="Modeled mean PACC") +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),
        legend.background=element_blank(), legend.key=element_blank(), legend.title=element_blank(),
        legend.key.width=grid::unit(1.5,"cm"), legend.key.height=grid::unit(0.5,"cm"), legend.text=element_text(size=10)) +
  scale_color_manual(values=custom_colors) +
  scale_fill_manual(values=custom_colors) +
  scale_linetype_manual(values=custom_linetypes) +
  guides(fill="none", color=guide_legend(override.aes=list(linewidth=1.0, linetype=custom_linetypes, color=custom_colors))) +
  geom_segment(x=0,xend=x_max,y=-10,yend=-10,color="black") +
  geom_segment(x=0,xend=0,y=-10,yend=2,color="black") +
  scale_x_continuous(limits=c(0,NA)) + scale_y_continuous(limits=c(-10,2)) +
  coord_cartesian(clip="off")


