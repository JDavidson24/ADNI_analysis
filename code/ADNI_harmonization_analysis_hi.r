# load library
install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("haven")
library(haven)
install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest)
install.packages("emmeans")
library(emmeans)
library(dplyr)
install.packages("yardstick")
library(yardstick)
install.packages("data/ADNIMERGE", repos = NULL, type = "source")
library(ADNIMERGE)
library(lme4)
library(emmeans)
library(ggplot2)
library(dplyr)
library(splines)
library(grid)

# load adni
data(adnimerge)
## Load the datasets as csv files
adnimerge <- read.csv("data/ADNIMERGE.csv")
gdscale <- read.csv("data/GDSCALE.csv")
npi <- read.csv("data/NPI.csv")
medhist <- read.csv("data/MEDHIST.csv")
admcgctof <- read.csv("data/ADMCGCTOF.csv")
zhang <- read.csv("data/ZHANG.csv")
cvrf <- read.csv("data/Assessment_Analyses/ADSP_PHC_CVRF_18Sep2025.csv")
adni1lipidomicsrader <- read.csv("data/adni1lipidomicsrader.csv")
pet <- read.csv("data/Image_Analyses/ADSP_PHC_PET_Amyloid_Simple_18Sep2025.csv")
apo <- read.csv("data/APOEGON.csv")
vitals <- read.csv("data/Vitals/VITALS_21Oct2025.csv")
medications <- read.csv("data/Medication/RECCMEDS_21Oct2025.csv")
medical_hist <- read.csv("data/Medical_History/INITHEALTH_21Oct2025.csv")

data(adnimerge)
colnames(adnimerge)

dbl <- adnimerge %>% 
  filter(VISCODE == "bl") %>% 
  select(RID, AGE, PTGENDER, PTEDUCAT, PTMARRY, PTETHCAT, PTRACCAT, DX.bl, APOE4)

# check if dbl$RID is unique
length(unique(dbl$RID)) == nrow(dbl)

colnames(dbl)

# check variable types
sapply(dbl,class)
attributes(dbl$DX.bl)
attributes(dbl$AGE)
attributes(dbl$PTMARRY)
attributes(dbl$PTGENDER)
attributes(dbl$PTEDUCAT)
attributes(dbl$PTETHCAT)
attributes(dbl$PTRACCAT)
attributes(dbl$APOE4)

# remove the label and make them flat
dbl <- dbl %>%
  mutate(
    RID = as.numeric(RID),
    AGE = as.numeric(AGE),
    PTGENDER = as.factor(as.character(PTGENDER)),
    PTEDUCAT = as.numeric(PTEDUCAT),
    PTMARRY = as.factor(as.character(PTMARRY)),
    PTETHCAT = as.factor(as.character(PTETHCAT)),
    PTRACCAT = as.factor(as.character(PTRACCAT)),
    DX.bl = as.factor(as.character(DX.bl)),
    APOE4 = as.factor(as.numeric(APOE4))
  )

skim(dbl) # some missing in APOE4 ##2436 rows

table(dbl$APOE4)
# make a complete dataset for baseline variables
dbl_set <- dbl[complete.cases(dbl), ]
# check the ID uniquness
length(unique(dbl_set$RID)) == nrow(dbl_set)
skim(dbl_set) # n rows = 2393

# PET status (should be defined at baseline)
pet <- read.csv("data/Image_Analyses/ADSP_PHC_PET_Amyloid_Simple_18Sep2025.csv") %>%
  select(RID, VISCODE, VISCODE2, PTID, PHC_CENTILOIDS, PHC_AMYLOID_STATUS, PHC_CL_FAIL)
pet %>% with(table(VISCODE, VISCODE2)) # VISCODE2 looks better

pet_bl = pet %>% filter(VISCODE2 == "bl") %>% 
  filter(!is.na(PHC_AMYLOID_STATUS)) %>% 
  select(RID, PHC_AMYLOID_STATUS)
table(pet_bl$PHC_AMYLOID_STATUS)

# combine bl pet with dbl_set
dbl_pet <- inner_join(dbl_set, pet_bl, by = "RID")
skim(dbl_pet) # 1536 rows
dbl_pet %>% with(table(DX.bl, PHC_AMYLOID_STATUS))

# picc outcome - longitudinal
dd <- adnimerge %>%
  select(ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, DX.bl, VISCODE, RID, PTID)

adni_pacc <- pacc(dd, keepComponents = FALSE) %>%
  select(RID, VISCODE, mPACCdigit, mPACCtrailsB) %>%
  mutate(
    RID = as.numeric(RID),
    month = if_else(VISCODE == "bl", 0, as.numeric(str_extract(VISCODE, "\\d+")))
  )

# check month and viscode
adni_pacc %>% with(table(VISCODE, month))
# check the uniqueness of RID and month combination
{adni_pacc %>% group_by(RID, month) %>% filter(n() > 1) %>% nrow()} == 0

skim(adni_pacc) #nrows 16467 

adni_pacc_complete = adni_pacc[complete.cases(adni_pacc), ]

skim(adni_pacc_complete) # n rows = 11508

# combine with baseline variables (dbl_pet)
df = inner_join(adni_pacc_complete, dbl_pet, by = "RID")
df %>% with(table(month, PHC_AMYLOID_STATUS))

skim(df) # nrows 6317


# Baseline DX.bl vs PHC_AMYLOID_STATUS
df %>% filter(month==0) %>%
  with(table(DX.bl, PHC_AMYLOID_STATUS))

### EXPOSURES (BASELINE STATUS) ####
medications <- read.csv("data/Medication/RECCMEDS_21Oct2025.csv")
medical_hist <- read.csv("data/Medical_History/INITHEALTH_21Oct2025.csv")

# diabetes exposure dataset
med_bl <- medications %>% filter(VISCODE2 %in% c('bl', 'sc'))
id_med_bl_all = unique(med_bl$RID)
length(id_med_bl_all) #3948
id_med_bl_dm <- med_bl %>%
  mutate(DMMED = case_when(
      str_detect(tolower(CMMED), "humulin|novolin|fiasp|novoLog|afrezza|admelog| 
      humalog|lyumjev|tresiba|insulin|basaglar|kwikpen|lantus|toujeo solostar|semglee| 
      amylinomimetic|acarbose|miglitol|glyset|kazano|invokamet|xigduo|synjardy|segluromet|
      glipizide|glucovance|jentadueto|actoplus|met|metformin|repaglinide|saxagliptin|janumet|
      cycloset|alogliptin|nesina|kazano|tradjenta|glyxambi|onglyza|januvia|sitagliptin|simvastatin|
      trulicity|byetta|bydureon|bCise|saxenda|victoza|lixisenatide|ozempic|mounjaro|starlix|prandin|
      invokana|invokamet|farxiga|qtern|jardiance|trijardy|synjardy|steglatro|amaryl|duetact|gliclazide|
      glipizide|glucotrol|glynase|oseni|actoplus|rosiglitazone|lyumjev|lyumjev|
      tresiba|Semglee-yfgn|humulin|novoLog|glyset|kazano|invokamet|xigduo|synjardy|segluromet|glucovance|
      jentadueto|actoplus|janumet|cycloset|nesina|tradjenta|glyxambi|onglyza|januvia|trulicity|byetta|
      bydureon|bcise|saxenda|victoza") ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% filter(DMMED == "Yes") %>% pull(RID) %>% unique()
length(id_med_bl_dm) ##515 rows


# Medical history diabetes exposure
table(medical_hist$VISCODE2) # no bl only sc
hist_sc = medical_hist %>% filter(VISCODE2 =='sc')
id_hist_sc_all = unique(hist_sc$RID)
length(id_hist_sc_all) #1802
id_hist_sc_dm <- medical_hist %>% 
  filter(VISCODE2 =='sc') %>%
  mutate(
    DMHIST = case_when(
      str_detect(
        str_to_lower(IHDESC),
        "diabetes|t2dm|type 2 dm|type ii dm|type 2 diabet|dm 2|dm ii|^dm$"
      ) ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% filter(DMHIST == "Yes") %>% pull(RID) %>% unique()

length(id_hist_sc_dm) ##327 rows 

# combined diabetes exposure at baseline
id_dm = union(id_med_bl_dm, id_hist_sc_dm)
id_all = union(id_med_bl_all, id_hist_sc_all)

df_dm = df %>% filter(RID %in% id_all) %>%
  mutate(DM = if_else(RID %in% id_dm, 1, 0)) %>%
  mutate(DM_G4 = case_when(
    (PHC_AMYLOID_STATUS==1)&(DM == 1) ~ "A4.High",
    (PHC_AMYLOID_STATUS==1)&(DM == 0) ~ "A4.Normal",
    (PHC_AMYLOID_STATUS==0)&(DM == 1) ~ "Learn.High",
    (PHC_AMYLOID_STATUS==0)&(DM == 0) ~ "Learn.Normal",
  )) %>% 
  filter(!is.na(DM_G4)) %>%
  mutate(
    DM_G4 = factor(DM_G4, levels = c("A4.High","A4.Normal","Learn.High","Learn.Normal")),
    APOEe4_carrier = if_else(APOE4 %in% c("1", "2"), 1, 0),
    year = month / 12
  )



skim(df_dm) # nrows 6314 will be in the analysis. 

df_dm_ad = df_dm %>% filter(DX.bl == "AD")
df_dm_mci = df_dm %>% filter(DX.bl %in% c("EMCI", "LMCI", 'SMC'))
df_dm_cn = df_dm %>% filter(DX.bl == "CN")

write.csv(df_dm_cn, "data/dm/PACC_DM_cn_dataset.csv")
write.csv(df_dm_mci, "data/dm/PACC_DM_mci_dataset.csv")
write.csv(df_dm_ad, "data/dm/PACC_DM_ad_dataset.csv")
write.csv(df_dm, "data/dm/PACC_DM_full_dataset.csv")

##bootstrap the Data
library(lme4)
install.packages("lmeresampler")
library(lmeresampler)
library(emmeans)
library(dplyr)

# Fit the LMER model (Your original code)
model <- lmer(
  mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + PTMARRY + APOEe4_carrier + PTETHCAT + (1 | RID),
  data = df_dm_cn
)

# Parametric bootstrap
set.seed(123)
boot_model <- bootstrap(
  model = model,
  .f = function(fit) fixef(fit), # extract fixed effects
  type = "parametric",           # parametric bootstrap
  B = 1000                       # number of bootstrap samples
)

#Bootstrap estimates matrix
boot_est <- as.matrix(boot_model$replicates)
colnames(boot_est) <- names(fixef(model))  # ensure coefficient names are correct

ci_direct <- apply(boot_est, 2, quantile, probs = c(0.025, 0.975))
# Extract the lower and upper bounds
ci_lower_direct <- ci_direct[1, ] # 0.025 quantile
ci_upper_direct <- ci_direct[2, ] # 0.975 quantile

# Two-sided p-value = proportion of bootstrap samples crossing zero
pvals_robust <- apply(boot_est, 2, function(x) {
  # 2 * minimum proportion of replicates greater/less than zero
  p_val <- 2 * min(mean(x <= 0), mean(x >= 0))
  min(p_val, 1)
})

results <- data.frame(
  Estimate = fixef(model),
  CI_lower = ci_lower_direct,
  CI_upper = ci_upper_direct,
  p_value = pvals_robust
)
results
t <- as.data.frame(results)
t
##write csv for bootstrap results
write.csv(t, "data/plots/PACC_DM_cn_bootstrap.csv")

#### ANALYSIS ####
PACC_DM <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + PTMARRY + APOEe4_carrier + PTETHCAT + (1 | RID), data = df_dm_cn)

summary(PACC_DM)


###Perform a model comparison to see the effect of adding each covariate step by step
m1 <- lmer(mPACCdigit ~ year + (1 | RID), data = df_dm_ad, REML = FALSE)
m2 <- lmer(mPACCdigit ~ year * DM_G4 + (1 | RID), data = df_dm_ad, REML = FALSE)
m3 <- lmer(mPACCdigit ~ year * DM_G4 + AGE + (1 | RID), data = df_dm_ad, REML = FALSE)
m4 <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + (1 | RID), data = df_dm_ad, REML = FALSE)
m5 <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + APOEe4_carrier + (1 | RID), data = df_dm_ad, REML = FALSE)
m6 <- lmer(mPACCdigit~ year * DM_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + (1 | RID), data = df_dm_ad, REML = FALSE)
m7 <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + PTETHCAT + (1 | RID), data = df_dm_ad, REML = FALSE)

anova(m1, m2, m3, m4, m5, m6, m7)


table(df_dm_cn$APOEe4_carrier)
colnames(df_dm_cn)
unique(df_dm_cn$PTRACCAT) #Race categories 
length(unique(adnimerge$RID))

library(ggplot2)
library(gridExtra)
anova_tbl <- as.data.frame(anova(m1, m2, m3, m4, m5, m6, m7))
png("data/plots/dm_ad_anova_results.png", width = 1200, height = 600, res = 150)
gridExtra::grid.table(round(anova_tbl,5))
dev.off()

# Group 4 level comparisons (intecept)
em_intercept <- emmeans(
  PACC_DM,
  ~ DM_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# test for interaction worse than additive
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 1-to-1 comparisons
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正


# Group 4 level comparisons (slope)
em_slope <- emtrends(
  PACC_DM,
  ~ DM_G4,
  var = "year"
)
summary(em_slope)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
pairs(em_slope, adjust = "tukey")

install.packages("broom.mixed")
library(broom.mixed)

# Tidy fixed effects table
tbl_fixed <- tidy(PACC_DM, effects = "fixed")
# View table
print(tbl_fixed)

# Save as CSV
write.csv(tbl_fixed, "data/plots/PACC_DM_cn_fixed_effects.csv", row.names = FALSE)


# Intercept emmeans
# em_int <- emmeans(PACC_CHOL, ~ DM_G4, at = list(year = 0))
tbl_em_int <- summary(em_int)
write.csv(tbl_em_int, "data/plots/PACC_DM_cn_emmeans_intercept.csv", row.names = FALSE)

# Slope emmeans
# em_slope <- emtrends(PACC_CHOL, ~ DM_G4, var = "year")
tbl_em_slope <- summary(em_slope)
write.csv(tbl_em_slope, "data/plots/PACC_DM_cn_emmeans_slope.csv", row.names = FALSE)
tbl_em_slope_tukey <- pairs(em_slope, adjust = "tukey")
write.csv(tbl_em_slope_tukey, "data/plots/PACC_DM_cn_emmeans_slope_tukey.csv", row.names = FALSE)




##### Cholesterol exposure dataset ###
med_bl <- medications %>% filter(VISCODE2 %in% c('bl', 'sc'))
id_med_bl_all = unique(med_bl$RID)
length(id_med_bl_all) ##3948
id_med_bl_chol <- med_bl %>%
  mutate(CHOLMED = case_when(
      str_detect(tolower(CMMED), "lipitor|lescol|altoprev|livalo|pravachol|
crestor|zocor|zetia|praluent|repatha|nexletol|nexlizet|prevalite|welchol|colestid|
vytorin|caduet|antara|lipofen|lopid|niacor|niaspan|lovaza|omacor|vascepa|atorvastatin|
fluvastatin|lovastatin|pitavastatin|pravastatin|rosuvastatin|simvastatin|ezetimibe|alirocumab|
evolocumab|bempedoic|bempedoic acid-ezetimibe|cholestyramine|colesevelam|colestipol| 
ezetimibe|simvastatin|amlodipine|atorvastatin|fenofibrate|gemfibrozil|niacint|cycloset|nesina|tradjenta|glyxambi|onglyza|januvia|trulicity|byetta|
      bydureon|saxenda|victoza") ~ "High",
      TRUE ~ "Normal"
    )
  ) %>% filter(CHOLMED == "High") %>% pull(RID) %>% unique()
length(id_med_bl_chol) ##0 rows with high cholesterol

##Lipidomics cholesterol exposure
table(adni1lipidomicsrader$VISCODE)
lipid_bl <- adni1lipidomicsrader %>% filter(VISCODE == "bl")
id_lipid_bl_all = unique(lipid_bl$RID)
length(id_lipid_bl_all) ##772
 
id_lipid_bl_chol <- lipid_bl %>%
  mutate(
    CHOLLAB = case_when(
      CHOL > 200 ~ "High",
      TRUE ~ "Normal"
    )
  ) %>%
  filter(CHOLLAB == "High") %>%
  pull(RID) %>%
  unique()
length(id_lipid_bl_chol)  # 248 rows with high cholesterol


# Medical history cholesterol exposure
table(medical_hist$VISCODE2) # no bl only sc
hist_sc = medical_hist %>% filter(VISCODE2 =='sc')
id_hist_sc_all = unique(hist_sc$RID)
length(id_hist_sc_all) ##1802
id_hist_sc_chol <- medical_hist %>% 
  filter(VISCODE2 =='sc') %>%
  mutate(
    CHOLHIST = case_when(
      str_detect(
        str_to_lower(IHDESC),
        "cholesterol|hyperlipidemia"
      ) ~ "High",
      TRUE ~ "Normal"
    )
  ) %>% filter(CHOLHIST == "High") %>% pull(RID) %>% unique()

length(id_hist_sc_chol) ##872 rows with high cholesterol


# combined cholesterol exposure at baseline
# id_chol = union(id_med_bl_chol, id_hist_sc_chol, id_lipid_bl_chol)
id_chol <- union(union(id_med_bl_chol, id_hist_sc_chol), id_lipid_bl_chol)
id_all = union(union(id_med_bl_all, id_hist_sc_all), id_lipid_bl_all)

df_chol = df %>% filter(RID %in% id_all) %>%
  mutate(Chol = if_else(RID %in% id_chol, 1, 0)) %>%
  mutate(Chol_G4 = case_when(
    (PHC_AMYLOID_STATUS==1)&(Chol == 1) ~ "A4.High",
    (PHC_AMYLOID_STATUS==1)&(Chol == 0) ~ "A4.Normal",
    (PHC_AMYLOID_STATUS==0)&(Chol == 1) ~ "Learn.High",
    (PHC_AMYLOID_STATUS==0)&(Chol == 0) ~ "Learn.Normal",
  )) %>% 
  filter(!is.na(Chol_G4)) %>%
  mutate(
    Chol_G4 = factor(Chol_G4, levels = c("A4.High","A4.Normal","Learn.High","Learn.Normal")),
    APOEe4_carrier = if_else(APOE4 %in% c("1", "2"), 1, 0),
    year = month / 12
  )


skim(df_chol) ###6314 rows will be in the analysis

df_chol_ad = df_chol %>% filter(DX.bl == "AD")
df_chol_mci = df_chol %>% filter(DX.bl %in% c("EMCI", "LMCI", 'SMC'))
df_chol_cn = df_chol %>% filter(DX.bl == "CN")

write.csv(df_chol_cn, "data/chol/PACC_chol_cn_dataset.csv")
write.csv(df_chol_mci, "data/chol/PACC_chol_mci_dataset.csv")
write.csv(df_chol_ad, "data/chol/PACC_chol_ad_dataset.csv")
write.csv(df_chol, "data/chol/PACC_chol_full_dataset.csv")


#### Cholesterol ANALYSIS ####
# Fit the LMER model
model <- lmer(
  mPACCdigit ~ year * Chol_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + PTETHCAT + (1 | RID),
  data = df_chol_cn
)

# Parametric bootstrap
set.seed(123)
boot_model <- bootstrap(
  model = model,
  .f = function(fit) fixef(fit), # extract fixed effects
  type = "parametric",           # parametric bootstrap
  B = 1000                       # number of bootstrap samples
)

#Bootstrap estimates matrix
boot_est <- as.matrix(boot_model$replicates)
colnames(boot_est) <- names(fixef(model))  # ensure coefficient names are correct

ci_direct <- apply(boot_est, 2, quantile, probs = c(0.025, 0.975))
# Extract the lower and upper bounds
ci_lower_direct <- ci_direct[1, ] # 0.025 quantile
ci_upper_direct <- ci_direct[2, ] # 0.975 quantile

# Two-sided p-value = proportion of bootstrap samples crossing zero
pvals_robust <- apply(boot_est, 2, function(x) {
  # 2 * minimum proportion of replicates greater/less than zero
  p_val <- 2 * min(mean(x <= 0), mean(x >= 0))
  min(p_val, 1)
})

results <- data.frame(
  Estimate = fixef(model),
  CI_lower = ci_lower_direct,
  CI_upper = ci_upper_direct,
  p_value = pvals_robust
)
t <- as.data.frame(results)
t
##write csv for bootstrap results
write.csv(t, "data/plots/chol/PACC_CHOL_cn_bootstrap.csv")

PACC_CHOL <- lmer(mPACCdigit ~ year * Chol_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + PTETHCAT + (1 | RID), data = df_chol_cn)

summary(PACC_CHOL)

###Perform a model comparison to see the effect of adding each covariate step by step
m1 <- lmer(mPACCdigit ~ ns(year, df = 3) + (1 | RID), data = df_chol_ad, REML = FALSE)
m2 <- lmer(mPACCdigit ~ ns(year, df = 3) * Chol_G4 + (1 | RID), data = df_chol_ad, REML = FALSE)
m3 <- lmer(mPACCdigit ~ ns(year, df = 3) * Chol_G4 + AGE + (1 | RID), data = df_chol_ad, REML = FALSE)
m4 <- lmer(mPACCdigit ~ ns(year, df = 3) * Chol_G4 + AGE + PTGENDER + (1 | RID), data = df_chol_ad, REML = FALSE)
m5 <- lmer(mPACCdigit ~ ns(year, df = 3) * Chol_G4 + AGE + PTGENDER + APOEe4_carrier + (1 | RID), data = df_chol_ad, REML = FALSE)
m6 <- lmer(mPACCdigit~ ns(year, df = 3) * Chol_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + (1 | RID), data = df_chol_ad, REML = FALSE)
m7 <- lmer(mPACCdigit ~ ns(year, df = 3) * Chol_G4 + AGE + PTGENDER + APOEe4_carrier + PTMARRY + PTETHCAT + (1 | RID), data = df_chol_ad, REML = FALSE)

anova(m1, m2, m3, m4, m5, m6, m7)

##share anova as a png file for powerpoint
library(ggplot2)
library(gridExtra)
anova_tbl <- as.data.frame(anova(m1, m2, m3, m4, m5, m6, m7))
png("data/plots/chol/chol_ad_anova_results.png", width = 1200, height = 600, res = 150)
gridExtra::grid.table(round(anova_tbl,5))
dev.off()

table(df_chol_cn$APOEe4_carrier)

# Group 4 level comparisons (intecept)
em_intercept <- emmeans(
  PACC_CHOL,
  ~ Chol_G4,
  at = list(year = 0),
  lmerTest.limit = 1e5,
  pbkrtest.limit = 1e5
)
summary(em_intercept)
# test for interaction worse than additive
contrast(em_intercept, list("Interaction worse than additive" = c(1, -1, -1, 1)))
# 1-to-1 comparisons
pairs(em_intercept, adjust = "tukey")  # 或 adjust = "tukey" 做多重比較校正

# Group 4 level comparisons (slope)
em_slope <- emtrends(
  PACC_CHOL,
  ~ Chol_G4,
  var = "year"
)
summary(em_slope)
contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
pairs(em_slope, adjust = "tukey")

# install.packages("broom.mixed")
library(broom.mixed)

# Tidy fixed effects table
tbl_fixed <- tidy(PACC_CHOL, effects = "fixed")
# View table
# Save as CSV
write.csv(tbl_fixed, "data/plots/chol/PACC_CHOL_cn_fixed_effects.csv", row.names = FALSE)


# Intercept emmeans
em_int <- emmeans(PACC_CHOL, ~ Chol_G4, at = list(year = 0))
tbl_em_int <- summary(em_int)
write.csv(tbl_em_int, "data/plots/chol/PACC_CHOL_cn_emmeans_intercept.csv", row.names = FALSE)
# Slope emmeans
tbl_em_slope <- summary(em_slope)
write.csv(tbl_em_slope, "data/plots/chol/PACC_CHOL_cn_emmeans_slope.csv", row.names = FALSE)
tbl_em_slope_tukey <- pairs(em_slope, adjust = "tukey")
write.csv(tbl_em_slope_tukey, "data/plots/chol/PACC_CHOL_cn_emmeans_slope_tukey.csv", row.names = FALSE)
tbl_em_contrast <- contrast(em_slope, list("Interaction worse than additive" = c(1, -1, -1, 1)))
write.csv(tbl_em_contrast, "data/plots/chol/PACC_CHOL_cn_emmeans_slope_contrast.csv", row.names = FALSE)








# Generate descriptive statistics for all_risk_factor_df
library(readr)
library(skimr)
library(writexl)

summary_stats <- skim(df_dm_ad)
summary_export <- as.data.frame(summary_stats)

# Export to Excel for all_risk_factor_df
write_xlsx(summary_export, "data/descriptive_stats/dm_ad_Descriptive_Statistics.xlsx")

# Generate descriptive statistics for cn_risk_factor_df
summary_stats <- skim(df_chol_ad)
summary_export <- as.data.frame(summary_stats)

# Export to Excel for cn_risk_factor_df
write_xlsx(summary_export, "data/descriptive_stats/chol_ad_Descriptive_Statistics.xlsx")





####### Create a table 1 of the controls and the MCI groups for both cholesterol/diabetes and amyloid status######
install.packages('car')
library(car)
install.packages('table1')
library(table1)


# # ##function to add p value to the table
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform an ANOVA
    p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# # #simple study characteristics table for each ADI rank w/ treatments, age, sex, race, HbA1c, and insrance status df
caption <- 'Study Characteristics Table'               

#use table1 function to create the table
tbl <- table1::table1(~ PTGENDER + PTETHCAT + PTEDUCAT + PTRACCAT + PTMARRY + AGE + DM_G4 | APOEe4_carrier, data = df_dm, render.missing = NULL, overall = c(Left = 'Total'), extra.col=list(`P_Value`=pvalue), groupspan=c(1, 1, 2), caption = caption, topclass="Rtable1-zebra")

# # ### turn table 1 into a data frame to save as a csv
tbl_df <- as.data.frame(tbl)  
print(tbl_df)

install.packages("webshot2")
library(webshot2)
library(htmltools)

html_file <- "data/plots/dm_table1_output.html"
save_html(tbl, file = html_file)

# Convert the HTML file to PNG
webshot2::webshot(html_file, file = "data/plots/dm_table1_output.png", vwidth = 1200, vheight = 1000, zoom = 2)
