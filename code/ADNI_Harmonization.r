##Pakcages and information needed for the analysis
install.packages("Hmisc")
library(Hmisc)
install.packages("ADNIMERGE", repos = NULL, type = "source")
# help(package = "ADNIMERGE")
install.packages("yardstick")
# pacc(dbl, keepComponents = FALSE)
library(dplyr)
library(yardstick)
library(ADNIMERGE)
library(tidyr)

# data(adnimerge)
# data(adas)

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

## Load the datasets as csv files
adnimerge <- read.csv("data/ADNIMERGE.csv")
gdscale <- read.csv("data/GDSCALE.csv")
npi <- read.csv("data/NPI.csv")
medhist <- read.csv("data/MEDHIST.csv")
admcgctof <- read.csv("data/ADMCGCTOF.csv")
zhang <- read.csv("data/ZHANG.csv")
cvrf <- read.csv("data/Assessment_Analyses/ADSP_PHC_CVRF_18Sep2025.csv")
adni1lipidomicsrader <- read.csv("data/adni1lipidomicsrader.csv")

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

## do the Pacc calculations
dd <- adnimerge %>%
  select(ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, DX.bl, VISCODE, RID, PTID)

adni_pacc <- pacc(dd, keepComponents = FALSE)
dim(adni_pacc)
head(adni_pacc, 5)
colnames(adni_pacc)


##Subset the columns of interest from each dataset so that it's easier to merge for me
sub_adni_pacc <- adni_pacc %>%
  select(RID, VISCODE, ADASQ4, LDELTOTAL, DIGITSCOR, MMSE, TRABSCOR, mPACCdigit, mPACCtrailsB)

sub_adnimerge <- adnimerge %>%
  select(RID, PTID, VISCODE, DX, DX.bl,
  PTMARRY, PTETHCAT, PTEDUCAT, PTGENDER, APOE4)

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
  left_join(sub_zhang, by = c("RID", "VISCODE")) %>%
  left_join(sub_cvrf, by = c("RID", "VISCODE")) %>%
  left_join(adni1lipidomicsrader, by = c("RID", "VISCODE")) %>%
  left_join(sub_admcgctof, by = c("RID")) %>%
  left_join(adni_pacc, by = c("RID", "VISCODE"))

dim(master_df) ## 16633 rows
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

## High chol is measured as > 200 mg/dL everything else is considered normal
## Diabetes is measured as 1 for yes and 0 for no if a patient has diabetes then it will count as a risk factor
## BMI is classified as obesity (≥30 kg/m²) or no obesity (<30 kg/m²) as a risk factor
## High blood pressure is defined at baseline as a single measurement with systolic blood pressure >140 mmHg or diastolic blood pressure >90 mmHg.
## PTEducat >12 years is high, else is Low


### Develop a function to classify the risk factors based on measurements for chol, BMI, diabetes, and SBP
## turn risk factotrs into high or normal for patients

master_df$Chol_group <- ifelse(master_df$CHOL > 200, "High", "Normal")
master_df$Diabetes_group <- ifelse(master_df$PHC_Diabetes == 1, "Yes", "No")
master_df$BMI_group <- ifelse(master_df$PHC_BMI >= 30, "Obese", "No Obesity")
master_df$SBP_group <- ifelse(master_df$PHC_SBP > 140, "High", "Normal")
master_df$PTEDUCAT_group <- ifelse(master_df$PTEDUCAT > 12, "High", "Low")

head(master_df, 5)
dim(master_df)


##Create new variables for A4.Learn and A4.Normal based on the risk factors for G1-G4
##Learn.Normal means that there is no risk factor for AD
##A4.Normal means that there is a cognitive impairment with no risk factors for AD
##A4.High means that there is a cognitive impairment with at least one risk factor for AD
##Learn.High means that there is no cognitive impairment with at least one risk factor for AD

master_df <- read.csv("data/master_df_raw.csv")
head(master_df, 5)

unique(master_df$DX)

# ### Categorize the DX.bl codes to show when a patient has a cognitive impairment and a risk factor for it
# risk_factor_df <- master_df %>%
#   mutate(Risk_Factor = ifelse(Chol_group == "High" | Diabetes_group == "Yes" | BMI_group == "Obese" | SBP_group == "High", "Yes", "No")) %>%
#   mutate(A4_Category = case_when(
#     DX.bl == "AD" & Risk_Factor == "Yes" ~ "A4.High",
#     DX.bl == "AD" & Risk_Factor == "No" ~ "A4.Normal",
#     DX.bl == "MCI" & Risk_Factor == "Yes" ~ "A4.High",
#     DX.bl == "MCI" & Risk_Factor == "No" ~ "A4.Normal",
#     DX.bl == "CN" & Risk_Factor == "Yes" ~ "Learn.High",
#     DX.bl == "CN" & Risk_Factor == "No" ~ "Learn.Normal",
#     TRUE ~ NA_character_
#   ))

# library(dplyr)

risk_factor_df <- master_df %>%
  mutate(
    Chol_G4 = ifelse(Chol_group == "High", "Normal") & ,
    Diabetes_G4   = ifelse(Diabetes_group == "Yes", "No"),
    BMI_G4       = ifelse(BMI_group == "Obese", "No Obesity",
    SBP_G4        = ifelse(SBP_group == "High", "Normal")
  )
head(risk_factor_df, 5)



# library(dplyr)

# risk_factor_df <- master_df %>%
#   # Create separate G4 columns for each risk factor
#   mutate(
#     Chol_G4    = ifelse(Chol_group == "High", "Yes", "No"),
#     Diabetes_G4 = ifelse(Diabetes_group == "Yes", "Yes", "No"),
#     BMI_G4      = ifelse(BMI_group == "Obese", "Yes", "No"),
#     SBP_G4      = ifelse(SBP_group == "High", "Yes", "No")
#   ) %>%
#   # Create a column for any risk factor in G4
#   mutate(
#     Any_Risk_G4 = ifelse(
#       Chol_G4 == "Yes" | Diabetes_G4 == "Yes" | BMI_G4 == "Yes" | SBP_G4 == "Yes",
#       "Yes",
#       "No"
#     )
#   ) %>%
#   # Categorize based on DX.bl and risk factors
#   mutate(
#     G4_Category = case_when(
#       !is.na(DX.bl) & DX.bl %in% c("AD", "MCI", "Dementia") & Any_Risk_G4 == "Yes" ~ "A4.High",
#       !is.na(DX.bl) & DX.bl %in% c("AD", "MCI", "Dementia") & Any_Risk_G4 == "No"  ~ "A4.Normal",
#       !is.na(DX.bl) & DX.bl %in% c("AD", "MCI", "Dementia") & Any_Risk_G4 == "Yes" ~ "Learn.High",
#       !is.na(DX.bl) & DX.bl %in% c("AD", "MCI", "Dementia") & Any_Risk_G4 == "No"  ~ "Learn.Normal",
#       TRUE ~ NA_character_
#     )
#   )

# # View the first 5 rows
head(risk_factor_df, 5)
