##Pakcages and information needed for the analysis
# install.packages("Hmisc")
# library(Hmisc)
# install.packages("ADNI/ADNIMERGE", repos = NULL, type = "source")
# help(package = "ADNIMERGE")
# install.packages("yardstick")
# pacc(dbl, keepComponents = FALSE)
library(dplyr)

# library(ADNIMERGE)
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
# jd <- pacc(dbl, keepComponents = FALSE)


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

##Subset the columns of interest from each dataset so that it's easier to merge for me
sub_adnimerge <- adnimerge %>%
  select(RID, 
#PTID, #VISCODE, 
  PTMARRY, PTETHCAT, PTEDUCAT, PTGENDER, APOE4)

##Baseline data from CVRF for diabetes 
sub_cvrf <- cvrf %>%
  select(RID, 
  #VISCODE, PTID, 
  PHC_BMI, PHC_Diabetes, PHC_Smoker, PHC_SBP)

# sub_npi <- npi %>%
#   select(RID, 
#   #VISCODE, 
#   NPIK, NPIK1, NPIK2, NPIK3, NPIK4, NPIK5, NPIK6, NPIK7, NPIK8, NPIK9A, NPIK9B, NPIK9C, NPIKTOT)

sub_zhang <- zhang %>%  
  select(RID, 
  #VISCODE, 
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
  select(RID, 
  #VISCODE, 
  CHOL, HDL)


## join them all together
master_df <- sub_adnimerge %>%
  inner_join(sub_gdscale, by = c("RID")) %>%
  inner_join(sub_npi, by = c("RID")) %>%
  inner_join(sub_medhist, by = c("RID")) %>%
  inner_join(sub_zhang, by = c("RID")) %>%
  inner_join(sub_cvrf, by = c("RID")) %>%
  inner_join(adni1lipidomicsrader, by = c("RID")) %>%
  inner_join(sub_admcgctof, by = c("RID"))

dim(master_df) ## 436027 rows

write.csv(master_df, "data/master_df_raw.csv", row.names = FALSE)

## Use Distint to get unique rows
master_df <- master_df %>%
  distinct()  # keeps only unique rows

dim(master_df) ## 8946 rows 
length(unique(master_df$RID)) ## 351 patients

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

head(adnimerge, 2)
