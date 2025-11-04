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

# load adni
# data(adnimerge)
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

dbl <- adnimerge %>% 
  filter(VISCODE == "bl") %>% 
  select(RID, AGE, PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, DX.bl, APOE4)

# check if dbl$RID is unique
length(unique(dbl$RID)) == nrow(dbl)

colnames(dbl)

# check variable types
sapply(dbl,class)
attributes(dbl$DX.bl)
attributes(dbl$AGE)
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
    PTETHCAT = as.factor(as.character(PTETHCAT)),
    PTRACCAT = as.factor(as.character(PTRACCAT)),
    DX.bl = as.factor(as.character(DX.bl)),
    APOE4 = as.factor(as.numeric(APOE4))
  )

skim(dbl) # some missing in APOE4 ##2436 rows

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
skim(dbl_pet) # 1536    #145 rows
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

skim(df) # nrows 6318 (#353 rows actually)


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
      str_detect(tolower(CMMED), "Humulin|Novolin|Fiasp|NovoLog|Afrezza|Admelog| 
      Humalog|Lyumjev|Tresiba|insulin|Basaglar KwikPen|Lantus|Toujeo SoloStar|Semglee-yfgn| 
      Amylinomimetic|acarbose|miglitol|Glyset|Kazano|Invokamet|Xigduo XR|Synjardy|Segluromet|
      glipizide|Glucovance|Jentadueto|Actoplus Met|metformin|repaglinide|saxagliptin|Janumet|
      Cycloset|alogliptin|Nesina|Kazano|Tradjenta|Glyxambi|Onglyza|Januvia|sitagliptin|simvastatin|
      Trulicity|Byetta|Bydureon BCise|Saxenda|Victoza|lixisenatide|Ozempic|Mounjaro|Starlix|Prandin|
      Invokana|Invokamet XR|Farxiga|Qtern|Jardiance|Trijardy XR|Synjardy XR|Steglatro|Amaryl|Duetact|gliclazide|
      glipizide|Glipizide XL|Glucotrol XL|Glynase|Oseni|Actoplus Met XR|rosiglitazone|Lyumjev|Lyumjev KwikPen|
      Tresiba|Semglee-yfgn|Humulin|NovoLog|Glyset|Kazano|Invokamet|Xigduo XR|Synjardy|Segluromet|Glucovance|
      Jentadueto|Actoplus Met|Janumet|Cycloset|Nesina|Tradjenta|Glyxambi|Onglyza|Januvia|Trulicity|Byetta|
      Bydureon BCise|Saxenda|Victoza") ~ "Yes",
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
    (PHC_AMYLOID_STATUS==1)&(DM == 1) ~ "Abeta yes DM yes",
    (PHC_AMYLOID_STATUS==1)&(DM == 0) ~ "Abeta yes DM no",
    (PHC_AMYLOID_STATUS==0)&(DM == 1) ~ "Abeta no DM yes",
    (PHC_AMYLOID_STATUS==0)&(DM == 0) ~ "Abeta no DM no",
  )) %>% 
  filter(!is.na(DM_G4)) %>%
  mutate(
    DM_G4 = factor(DM_G4, levels = c("Abeta no DM no", "Abeta no DM yes", "Abeta yes DM no", "Abeta yes DM yes")),
    APOEe4_carrier = if_else(APOE4 %in% c("1", "2"), 1, 0),
    year = month / 12
  )

skim(df_dm) # nrows 6315 will be in the analysis. ##353 rows will be in the analysis

df_dm_ad = df_dm %>% filter(DX.bl == "AD")
df_dm_mci = df_dm %>% filter(DX.bl %in% c("EMCI", "LMCI", 'SMC'))
df_dm_cn = df_dm %>% filter(DX.bl == "CN")

#### ANALYSIS ####
PACC_DM <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + APOEe4_carrier + PTRACCAT + PTEDUCAT + (1 | RID), data = df_dm_cn)

summary(PACC_DM)

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




# Cholesterol exposure dataset
med_bl <- medications %>% filter(VISCODE2 %in% c('bl', 'sc'))
id_med_bl_all = unique(med_bl$RID)
length(id_med_bl_all) ##3948
id_med_bl_chol <- med_bl %>%
  mutate(CHOLMED = case_when(
      str_detect(tolower(CMMED), "Lipitor|Lescol XL|Altoprev|Livalo|Pravachol|
Crestor|Zocor|Zetia|Praluent|Repatha|Nexletol|Nexlizet|Prevalite|Welchol|Colestid|
Vytorin|Caduet|Antara|Lipofen|Lopid|Niacor|Niaspan|Lovaza|Omacor|Vascepa|Atorvastatin|
Fluvastatin|Lovastatin|Pitavastatin|Pravastatin|Rosuvastatin|Simvastatin|Ezetimibe|Alirocumab|
Evolocumab|Bempedoic acid|Bempedoic acid-ezetimibe|Cholestyramine|Colesevelam|Colestipol| 
Ezetimibe-simvastatin|Amlodipine-atorvastatin|Fenofibrate|Gemfibrozil|Niacint|Cycloset|Nesina|Tradjenta|Glyxambi|Onglyza|Januvia|Trulicity|Byetta|
      Bydureon BCise|Saxenda|Victoza") ~ "High",
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
    (PHC_AMYLOID_STATUS==1)&(Chol == 1) ~ "Abeta yes Chol yes",
    (PHC_AMYLOID_STATUS==1)&(Chol == 0) ~ "Abeta yes Chol no",
    (PHC_AMYLOID_STATUS==0)&(Chol == 1) ~ "Abeta no Chol yes",
    (PHC_AMYLOID_STATUS==0)&(Chol == 0) ~ "Abeta no Chol no",
  )) %>% 
  filter(!is.na(Chol_G4)) %>%
  mutate(
    Chol_G4 = factor(Chol_G4, levels = c("Abeta no Chol no", "Abeta no Chol yes", "Abeta yes Chol no", "Abeta yes Chol yes")),
    APOEe4_carrier = if_else(APOE4 %in% c("1", "2"), 1, 0),
    year = month / 12
  )

skim(df_dm) ###353 rows will be in the analysis

df_chol_ad = df_chol %>% filter(DX.bl == "AD")
df_chol_mci = df_chol %>% filter(DX.bl %in% c("EMCI", "LMCI", 'SMC'))
df_chol_cn = df_chol %>% filter(DX.bl == "CN")

#### Cholesterol ANALYSIS ####

PACC_CHOL <- lmer(mPACCdigit ~ year * DM_G4 + AGE + PTGENDER + APOEe4_carrier + PTRACCAT + PTEDUCAT + (1 | RID), data = df_dm_cn)

summary(PACC_CHOL)

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
