##Pakcages and information needed for the analysis
install.packages("Hmisc")
library(Hmisc)
install.packages("data/ADNIMERGE", repos = NULL, type = "source")
# help(package = "ADNIMERGE")
install.packages("yardstick")
# pacc(dbl, keepComponents = FALSE)
library(dplyr)
library(yardstick)
library(ADNIMERGE)
library(tidyr)

# data(adnimerge)
# data(adas)
# data(apoego2)
data(vitals)



# dim(adnimerge)
# head(adnimerge, 10)
# # Basic variables
# ## ID = RID
# ## number of uniuque subjects
# length(unique(adnimerge$RID))

# ## Visit = VISCODE
# ## Age = AGE
# dbl = adnimerge[adnimerge$VISCODE == "bl", ]
# dim(dbl)
# # Sex is stored in PTGENDER
# table(dbl$PTGENDER)
jd <- pacc(adnimerge, keepComponents = FALSE)


## List of columns of interest
# cols_to_keep <- c("BID", "PTGENDER_base", "PTMARRY_base", "AAPOEGNPRSNFLG_base", "PTETHNIC_base", 
# "PTEDUCAT_group", "CAFFEINE_group", "WALKING_group", "BMI_group", "SMOKE_group", "Alcohol_group",
#       "AEROBIC_group", "SLEEP_group", "SBP_group", "SLEEPDAY_group",
#       "GDS15_group", "Chol200_group","Chol5.1_group", "HbA1c6.5_group","PTEDUCAT_group_all",
#       "CAFFEINE_group_all", "WALKING_group_all", "BMI_group_all", "SMOKE_group_all", "Alcohol_group_all",
#       "AEROBIC_group_all", "SLEEP_group_all", "SBP_group_all", "SLEEPDAY_group_all",
#       "GDS15_group_all", "Chol200_group_all", "HbA1c6.5_group_all", "PTGENDER_all",
#       "PTMARRY_all","AAPOEGNPRSNFLG_all","Chol5.1_group_all",
#       "PTETHNIC_all")


##We want to Merge the unique PTID Of ADNIMERGE with the image, biospecimenm, and image data without having duplicates. 
## The matching columns for ADNI Merge are
## (PTID, VISCODE, PTMARRY, PTETHNIC, PTEDUCAT, PTGENDER, APOE4, Age)
## there is no dataset with GDS15 data
## the matching columns for the other datasets are
## (CVRF Data matching on PTID: BMI, DIabetes, SMoking, ALcohol, SBP, )
## gdscale will be merged to ADNImerge using the VISCODE for GDS15
## medhist will be merged to ADNImerge using the VISCODE for smoking (MH16),  alchol MH14AALCH, 
## data(npi) will be merged to ADNImerge using the VISCODE for NPIK9A and NPIK9B for sleep data
##data(admcgctof) can be used for "Caffiene" and "CHOLESTEROL" on the VISCODE but the different type of values do not match
## data(zhang) can be used for hemoglobin data (FINAL_HGB, and RUNDATE_HGB)
## data(adni1lipidomicsrader) will be used for chol and HDL data

## None of the datasets have HbA1c (they have regular hemoglobin values)
## None of the datasets have Aerobic/exercise data


##write the datasets as csv files
write.csv(adnimerge, "data/ADNIMERGE.csv", row.names = FALSE)
write.csv(gdscale, "data/GDSCALE.csv", row.names = FALSE)
write.csv(npi, "data/NPI.csv", row.names = FALSE)
write.csv(medhist, "data/MEDHIST.csv", row.names = FALSE)
write.csv(admcgctof, "data/ADMCGCTOF.csv", row.names = FALSE)
write.csv(zhang, "data/ZHANG.csv", row.names = FALSE)
write.csv(adni1lipidomicsrader, "data/adni1lipidomicsrader.csv", row.names = FALSE)
write.csv(apoego2, "data/APOEGON.csv", row.names = FALSE)

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


##Make sure the columns to merge on are of the same datatype
adnimerge$RID <- as.numeric(adnimerge$RID)
gdscale$RID <- as.numeric(gdscale$RID)
npi$RID <- as.numeric(npi$RID)
medhist$RID <- as.numeric(medhist$RID)
admcgctof$RID <- as.numeric(admcgctof$RID)
zhang$RID <- as.numeric(zhang$RID)
zhang$VISCODE <- as.character(zhang$VISCODE)
cvrf$PTID <- as.character(cvrf$PTID)
adnimerge$PTID <- as.character(adnimerge$PTID)
admcgctof$RID <- as.numeric(admcgctof$RID)
adni1lipidomicsrader$RID <- as.numeric(adni1lipidomicsrader$RID)
adni1lipidomicsrader$VISCODE <- as.character(adni1lipidomicsrader$VISCODE)
cvrf$VISCODE <- as.character(cvrf$VISCODE)
adnimerge$VISCODE <- as.character(adnimerge$VISCODE)
npi$VISCODE <- as.character(npi$VISCODE)
medhist$VISCODE <- as.character(medhist$VISCODE)
gdscale$VISCODE <- as.character(gdscale$VISCODE)
pet$RID <- as.numeric(pet$RID)
pet$VISCODE <- as.character(pet$VISCODE)
vitals$RID <- as.numeric(vitals$RID)
vitals$VISCODE <- as.character(vitals$VISCODE)
medical_hist$RID <- as.numeric(medical_hist$RID)
medical_hist$VISCODE <- as.character(medical_hist$VISCODE)  
medications$RID <- as.numeric(medications$RID)
medications$VISCODE <- as.character(medications$VISCODE)

## do the Pacc calculations
dd <- adnimerge %>%
  select(ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, DX.bl, VISCODE, RID, PTID)

adni_pacc <- pacc(dd, keepComponents = FALSE)
dim(adni_pacc)
head(adni_pacc, 5)
colnames(adni_pacc)

##Subset the medications dataset to only include cholesterol and diabetes diagnosis
library(stringr)
# medical_hist <- filter(medical_hist, str_detect(tolower(IHDESC), "cholesterol|diabetes"))
head(medical_hist, 50)

# medications <- filter(medications, str_detect(tolower(CMMED), "cholesterol|diabetes"))

##Subset the columns of interest from each dataset so that it's easier to merge for me
sub_pet <- pet %>%
  select(RID, VISCODE, PTID, PHC_CENTILOIDS, PHC_AMYLOID_STATUS, PHC_CL_FAIL)

sub_vitals <- vitals %>%
  select(RID, VISCODE, PTID, VSBPSYS, )

sub_medical_hist <- medical_hist %>%
  select(RID, VISCODE, PTID, IHDESC)

sub_medications <- medications %>%
  select(RID, VISCODE, PTID, CMMED)

sub_adni_pacc <- adni_pacc %>%
  select(RID, VISCODE, ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, mPACCdigit, mPACCtrailsB)

sub_adnimerge <- adnimerge %>%
  select(RID, PTID, VISCODE, DX, DX.bl,
  PTMARRY, PTETHCAT, PTEDUCAT, PTGENDER, AGE, APOE4, Years.bl, Month.bl)

##Baseline data from CVRF for diabetes 
sub_cvrf <- cvrf %>%
  select(RID,  VISCODE, PTID, 
  PHC_BMI, PHC_Diabetes, PHC_Smoker, PHC_SBP)

# sub_npi <- npi %>%
#   select(RID, VISCODE, 
#   NPIK, NPIK1, NPIK2, NPIK3, NPIK4, NPIK5, NPIK6, NPIK7, NPIK8, NPIK9A, NPIK9B, NPIK9C, NPIKTOT)

sub_zhang <- zhang %>%  
  select(RID, VISCODE, 
  FINAL_HGB, RUNDATE_HGB)

## baseline values for GDS15
# sub_gdscale <- gdscale %>%
#   select(RID, 
#   #VISCODE, 
#   GDAFRAID, GDALIVE, GDBETTER, GDBORED, GDDATE, GDEMPTY, GDENERGY, GDHAPPY,
#          GDHELP, GDHOME, GDHOPE, GDMEMORY, GDSATIS, GDSPIRIT, GDTOTAL, GDUNABL, GDWORTH, GDSOURCE)

# sub_medhist <- medhist %>%
#   select(RID, 
#   #VISCODE, 
#   MH16SMOK, MH16ASMOK, MH16BSMOK, MH16CSMOK, MH14ALCH, MH14AALCH)

sub_admcgctof <- admcgctof %>%
  select(RID, CAFFEINE, CHOLESTEROL)

sub_adni1lipidomicsrader <- adni1lipidomicsrader %>%
  select(RID, VISCODE, CHOL, HDL)


## join them all together
master_df <- sub_adnimerge %>%
  # left_join(sub_gdscale, by = c("RID")) %>%
  # left_join(sub_npi, by = c("RID")) %>%
  # left_join(sub_medhist, by = c("RID")) %>%
  # left_join(sub_zhang, by = c("RID", "VISCODE")) %>%
  # left_join(sub_cvrf, by = c("RID", "VISCODE")) %>%
  left_join(adni1lipidomicsrader, by = c("RID", "VISCODE")) %>%
  # left_join(sub_admcgctof, by = c("RID")) %>%
  left_join(adni_pacc, by = c("RID", "VISCODE")) %>%
  left_join(sub_pet, by = c("RID", "VISCODE")) %>%
  left_join(sub_vitals, by = c("RID", "VISCODE")) %>%
  left_join(sub_medical_hist, by = c("RID", "VISCODE")) %>%
  left_join(sub_medications, by = c("RID", "VISCODE"))

dim(master_df) ## 19845 rows
colnames(master_df)

write.csv(master_df, "data/master_df_raw.csv", row.names = FALSE)

## Use Distint to get unique rows
master_df <- master_df %>%
  distinct()  # keeps only unique rows

dim(master_df) ## 16633 rows 
length(unique(master_df$RID)) ## 2436 patients

### We want to allow for missigness for the individual analyiss in the final dataset (lmer)
### three different data groups AD is alzheirmer, CN for cognitive normal, and MCI for mild cognitive impairment
## DX.bl for the diagnosis codes at baseline 
## A4.LEARN 
## If patient has a risk factors (diabetes, etc) and AD,CN or MCI then it is A4.normal
## If not, then the patient is A4.Learn based on the cross table from ALice's work
## we want the cholesterol values based on alice's work
## focus on diabetes, cholesterol, walking/activites)
## we want to use PACC data for adnimerge total dataset
## we want to group them into G1-G4 based on the risk factors
## some datasets dont have bl for baseline and may have screening visits
## check the missingss in each dataset
## use the vitals dataset to sys data where VSBPSYS column
## use the medhistory dataset to check for diabetes and high cholesterol using like terms in IHDESC

## High chol is measured as > 200 mg/dL everything else is considered normal
## Diabetes is measured as 1 for yes and 0 for no if a patient has diabetes then it will count as a risk factor
## BMI is classified as obesity (≥30 kg/m²) or no obesity (<30 kg/m²) as a risk factor
## High blood pressure is defined at baseline as a single measurement with systolic blood pressure >140 mmHg or diastolic blood pressure >90 mmHg.
## PTEducat >12 years is high, else is Low


### Develop a function to classify the risk factors based on measurements for chol, BMI, diabetes, and SBP
## turn risk factotrs into high or normal for patients

# master_df$Chol_group <- ifelse(master_df$CHOL > 200, "High", "Normal")
master_df$BMI_group <- ifelse(master_df$PHC_BMI >= 30, "Obese", "No Obesity")
master_df$SBP_group <- ifelse(master_df$VSBPSYS > 140, "High", "Normal")
master_df$PTEDUCAT_group <- ifelse(master_df$PTEDUCAT > 12, "High", "Low")

master_df <- master_df %>%
mutate(AAPOEGNPRSNFLG_all = case_when(
    APOE4 == 1 ~ "E4-",
    APOE4 == 2 ~ "E4+",
    TRUE ~ "NA"
  ))

master_df <- master_df %>%
  mutate(
    Diabetes_group = case_when(
      str_detect(tolower(IHDESC), "diabetes|mellitus") ~ "Yes",
      TRUE ~ "No"
    )
  )

master_df <- master_df %>%
  mutate(
    Chol_group = case_when(
      str_detect(tolower(IHDESC), "cholesterol|hyperlipidemia") ~ "High",
      TRUE ~ "Normal"
    )
  )

# master_df <- master_df %>%
#   mutate(
#     Diabetes_group_2 = case_when(
#       str_detect(tolower(CMMED), "Humulin" | "Novolin" | "Fiasp" | "NovoLog" | "Afrezza" | "Admelog" | "Humalog" | "Lyumjev" | "Tresiba" | "insulin" | "Basaglar KwikPen" | "Lantus" | "Toujeo SoloStar" | "Semglee-yfgn” | "Amylinomimetic" | "acarbose" | "miglitol" | "Glyset" | "Kazano" | "Invokamet" | "Xigduo XR" | "Synjardy" | "Segluromet" | "metformin/glipizide" | "Glucovance" | "Jentadueto" | "Actoplus Met" | "metformin” | "repaglinide" | "saxagliptin" | "Janumet" | "Cycloset" | "alogliptin" | "Nesina" | "Kazano" | "Tradjenta" | "Glyxambi" | "Onglyza" | "Januvia" | "sitagliptin" | "simvastatin" | "Trulicity" | "Byetta" | "Bydureon BCise" | "Saxenda" | "Victoza" | "lixisenatide" | "Ozempic" | "Mounjaro" | "Starlix" | "Prandin" | "Invokana" | "Invokamet XR" | "Farxiga" | "Qtern" | "Jardiance" | "Trijardy XR" | "Synjardy XR" | "Steglatro" | "Amaryl" | "Duetact" | "gliclazide" | "glipizide" | "Glipizide XL" | "Glucotrol XL" | "Glynase" | "Oseni" | "Actoplus Met XR" | "rosiglitazone" | "Lyumjev" | "Lyumjev KwikPen" | "Tresiba" | "Semglee-yfgn" | "Humulin" | "NovoLog" | "Glyset" | "Kazano" | "Invokamet" | "Xigduo XR" | "Synjardy" | "Segluromet" | "Glucovance" | "Jentadueto" | "Actoplus Met" | "Janumet" | "Cycloset" | "Nesina" | "Tradjenta" | "Glyxambi" | "Onglyza" | "Januvia" | "Trulicity" | "Byetta" | "Bydureon BCise" | "Saxenda" | "Victoza" | "Ozempic" | "Mounjaro" | "Starlix" | "Prandin" | "Invokana" | "Invokamet XR" | "Farxiga" | "Qtern" | "Jardiance" | "Trijardy XR" | "Synjardy XR" | "Steglatro" | "Amaryl" | "Duetact" | "Glipizide XL" | "Glucotrol XL" | "Glynase" | "Oseni") ~ "Yes",
#       TRUE ~ "No"
#     )
#   )

# master_df <- master_df %>%
#   mutate(
#     Chol_group_2 = case_when(
#       str_detect(tolower(CMMED), "Lipitor" | "Lescol XL" | "Altoprev" | "Livalo" | "Pravachol" |
# "Crestor" | "Zocor" | "Zetia" | "Praluent" | "Repatha" | "Nexletol" | "Nexlizet" |
# "Prevalite" | "Welchol" | "Colestid" | "Vytorin" | "Caduet" |
# "Antara" | "Lipofen" | "Lopid" | "Niacor" | "Niaspan" |
# "Lovaza" | "Omacor" | "Vascepa" | "Atorvastatin" | "Fluvastatin" | "Lovastatin" | 
# "Pitavastatin" | "Pravastatin" | "Rosuvastatin" | "Simvastatin" | "Ezetimibe" | 
# "Alirocumab" | "Evolocumab" | "Bempedoic acid" | "Bempedoic acid-ezetimibe" | 
# "Cholestyramine" | "Colesevelam" | "Colestipol" | "Ezetimibe-simvastatin" | 
# "Amlodipine-atorvastatin" | "Fenofibrate" | "Gemfibrozil" | "Niacin")
#        ~ "High",
#       TRUE ~ "Normal"
#     )
#   )

## change names of columns for varaibles to use to replicate the code seamlessly
master_df$year <- as.numeric(master_df$Years.bl)
master_df$PTGENDER_all <- master_df$PTGENDER  
master_df$PTMARRY_all <- master_df$PTMARRY
master_df$PTETHNIC_all <- master_df$PTETHCAT
master_df$PTAGE_all <- master_df$AGE
master_df$PACC.raw <- master_df$mPACCdigit
master_df$BID <- master_df$PTID.x
master_df$PTEDUCAT_all <- master_df$PTEDUCAT




head(master_df, 5)
dim(master_df)


##Create new variables for A4.Learn and A4.Normal based on the risk factors for G1-G4
##Learn.Normal means that there is no risk factor for AD
##A4.Normal means that there is a cognitive impairment with no risk factors for AD
##A4.High means that there is a cognitive impairment with at least one risk factor for AD
##Learn.High means that there is no cognitive impairment with at least one risk factor for AD

master_df <- read.csv("data/master_df_raw.csv")
head(master_df, 5)

master_df %>%
  arrange(desc(PTID.x)) %>%  # sort descending by emmean
  head(10) 



unique(master_df$PHC_AMYLOID_STATUS)
unique(master_df$Diabetes_group)
colnames(master_df)

# library(dplyr)

all_risk_factor_df <- master_df %>%
  mutate(
    Chol_G4 = case_when(
      PHC_AMYLOID_STATUS == 1 & Chol_group == "High" ~ "A4.High",
      PHC_AMYLOID_STATUS == 1 & Chol_group == "Normal" ~ "A4.Normal",
      PHC_AMYLOID_STATUS == 0 & Chol_group == "High" ~ "Learn.High",
      PHC_AMYLOID_STATUS == 0 & Chol_group == "Normal" ~ "Learn.Normal",
      TRUE ~ NA_character_
    ),
    Diabetes_G4 = case_when(
      PHC_AMYLOID_STATUS == 1 & Diabetes_group == "Yes" ~ "A4.High",
      PHC_AMYLOID_STATUS == 1 & Diabetes_group == "No" ~ "A4.Normal",
      PHC_AMYLOID_STATUS == 0 & Diabetes_group == "Yes" ~ "Learn.High",
      PHC_AMYLOID_STATUS == 0 & Diabetes_group == "No" ~ "Learn.Normal",
      TRUE ~ NA_character_
    ),
    # BMI_G4 = case_when(
    #   PHC_AMYLOID_STATUS == 1 & BMI_group == "Obese" ~ "A4.High",
    #   PHC_AMYLOID_STATUS == 1 & BMI_group == "No Obesity" ~ "A4.Normal",
    #   PHC_AMYLOID_STATUS == 0 & BMI_group == "Obese" ~ "Learn.High",
    #   PHC_AMYLOID_STATUS == 0 & BMI_group == "No Obesity" ~ "Learn.Normal",
    #   TRUE ~ NA_character_
    # ),
    SBP_G4 = case_when(
      PHC_AMYLOID_STATUS == 1 & SBP_group == "High" ~ "A4.High",
      PHC_AMYLOID_STATUS == 1 & SBP_group == "Normal" ~ "A4.Normal",
      PHC_AMYLOID_STATUS == 0 & SBP_group == "High" ~ "Learn.High",
      PHC_AMYLOID_STATUS == 0 & SBP_group == "Normal" ~ "Learn.Normal",
      TRUE ~ NA_character_
    ),
    PTEDUCAT_G4 = case_when(
      PHC_AMYLOID_STATUS == 1 & PTEDUCAT_group == "High" ~ "A4.High",
      PHC_AMYLOID_STATUS == 1 & PTEDUCAT_group == "Low" ~ "A4.Normal",
      PHC_AMYLOID_STATUS == 0 & PTEDUCAT_group == "High" ~ "Learn.High",
      PHC_AMYLOID_STATUS == 0 & PTEDUCAT_group == "Low" ~ "Learn.Normal",
      TRUE ~ NA_character_
    )
  )

all_risk_factor_df$chol200_G4 <- all_risk_factor_df$Chol_G4
all_risk_factor_df$HbA1c_G4 <- all_risk_factor_df$Diabetes_G4
all_risk_factor_df$PTEDUCAT_all <- all_risk_factor_df$PTEDUCAT

## We have the AB+ from the PET imaging
## We want patients who have PET imaging data and are congnitively impaired (CN or CU diagnosisis at baseline)
## 1 dataset only with CN and another dataset with all baseline diagnosis
## Basic descrptive analysis of dataset and check for missingsness

all_risk_factor_df <- all_risk_factor_df %>%
  mutate(
    SUBSTUDY = case_when(
      PHC_AMYLOID_STATUS == 1 ~ "A4",
      PHC_AMYLOID_STATUS == 0 ~ "Learn",
      TRUE ~ NA_character_
    )
  )

## we want the substudy column to be if it has one entry of amyloid status ==1 then it will be A4 else learn
# all_risk_factor_df <- all_risk_factor_df %>%
#   group_by(RID) %>%
#   mutate(SUBSTUDY = first(SUBSTUDY)) %>%
#   ungroup()

all_risk_factor_df <- all_risk_factor_df %>%
  group_by(RID) %>%
  mutate(SUBSTUDY = case_when(
    any(PHC_AMYLOID_STATUS == 1, na.rm = TRUE) ~ "A4",
    all(is.na(PHC_AMYLOID_STATUS)) ~ NA_character_,
    TRUE ~ "Learn"
  )) %>%
  ungroup()

all_risk_factor_df$HbA1c6.5_group <- all_risk_factor_df$Diabetes_group
all_risk_factor_df$HbA1c6.5_group_all <- all_risk_factor_df$Diabetes_group

all_risk_factor_df$Chol200_group <- all_risk_factor_df$Chol_group
all_risk_factor_df$Chol200_group_all <- all_risk_factor_df$Chol_group

all_risk_factor_df$SBP_group_all <- all_risk_factor_df$SBP_group
all_risk_factor_df$Chol200_group_all <- all_risk_factor_df$Chol200_group
all_risk_factor_df$HbA1c6.5_group_all <- all_risk_factor_df$HbA1c6.5_group

colnames(PACC_all)
###create base columns that match the code by taking the coresponding entry for age, marry, gender columns, ethnic

all_risk_factor_df <- all_risk_factor_df %>%
  group_by(RID) %>%
  mutate(PTAGE_base = ifelse(row_number() == 1, first(PTAGE_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df %>%
  group_by(RID) %>%
  mutate(PTMARRY_base = ifelse(row_number() == 1, first(PTMARRY_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(PTGENDER_base = ifelse(row_number() == 1, first(PTGENDER_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(PTETHNIC_base = ifelse(row_number() == 1, first(PTETHNIC_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(AAPOEGNPRSNFLG_base = ifelse(row_number() == 1, first(AAPOEGNPRSNFLG_all), NA)) %>%
  ungroup()

## create new group columns that match the code by taking the coresponding entry for all of the group columns
all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(HbA1c6.5_group = ifelse(row_number() == 1, first(HbA1c6.5_group_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(Chol200_group = ifelse(row_number() == 1, first(Chol200_group_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df  %>%
  group_by(RID) %>%
  mutate(SBP_group = ifelse(row_number() == 1, first(SBP_group_all), NA)) %>%
  ungroup()

all_risk_factor_df <- all_risk_factor_df %>%
  group_by(RID) %>%
  mutate(PTEDUCAT_group = ifelse(row_number() == 1, first(PTEDUCAT_all), NA)) %>%
  ungroup()

head(all_risk_factor_df$PTEDUCAT_group, 5)
colnames(all_risk_factor_df)
length(unique(all_risk_factor_df$RID)) ## 2436 unique patients
dim(all_risk_factor_df) ## 19845 rows
write.csv(all_risk_factor_df, "data/all_risk_factor_df_raw.csv", row.names = FALSE)



## Subset the data to only include patients with PET imaging data and are cognitively normal (CN) at baseline
cn_risk_factor_df <- all_risk_factor_df %>%
  filter(DX.bl.x == "CN")

dim(cn_risk_factor_df) ## 4975 rows
length(unique(cn_risk_factor_df$RID)) ## 543 unique patients
write.csv(cn_risk_factor_df, "data/cn_risk_factor_df_raw.csv", row.names = FALSE)


### Basic descriptive analysis of dataset and check for missingsness
# install.packages(c("readr", "skimr", "writexl"))

library(readr)
library(skimr)
library(writexl)

# Generate descriptive statistics for all_risk_factor_df
summary_stats <- skim(all_risk_factor_df)
summary_export <- as.data.frame(summary_stats)

# Export to Excel for all_risk_factor_df
write_xlsx(summary_export, "data/descriptive_stats/all_Descriptive_Statistics.xlsx")

# Generate descriptive statistics for cn_risk_factor_df
summary_stats <- skim(cn_risk_factor_df)
summary_export <- as.data.frame(summary_stats)

# Export to Excel for cn_risk_factor_df
write_xlsx(summary_export, "data/descriptive_stats/cn_Descriptive_Statistics.xlsx")

unique(all_risk_factor_df$HbA1c_G4)
