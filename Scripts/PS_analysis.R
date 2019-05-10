# PS_analysis.R
# Run analysis for Processing Speed Data
# See clean_data.R for cleaning data
# Author: Bonhwang Koo
# Last updated 5/9/2019

# Load required packages
require(ggplot2)
require(dplyr)
require(reshape)
require(psych) # for factor analysis
require(viridis)
require(ICC)
require(factoextra)
require(corrplot) 
require(psych) # for corr.test
require(ppcor)
require(venn)
require(UpSetR)
#require(plyr)
#require(grid)
#require(gridExtra)

# Import behavioral data for Windows
setwd("R:/Data Feb 2019/Data Cleaned")

# Import behavioral data for Mac
#setwd("/Volumes/data/Research/Healthy Brain Network/AACAP/2017/Proc Speed/Data Feb 2019/Data Cleaned")

# Read in all .csv files
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv, stringsAsFactors = FALSE), envir = .GlobalEnv)

# sig_code
# return the significance code of a given p-value p
sig_code <- function(p) {
  if (p < 0.001) {
    sig <- "***"
  } else if (p < 0.01) {
    sig <- "**"
  } else if (p < 0.05) {
    sig <- "*"
  } else {
    sig <- ""
  }
  sig
}

# Vectorize sig_code function to apply to vectors
sig_code_v <- Vectorize(sig_code)

# Merge
# merge 2 data frames x and y by EID
Merge <- function(x, y){
  df <- merge(x, y, by= "EID", all.x= TRUE, all.y= TRUE)
  return(df)
}

### Subset dataframes and regress age and sex for some scores
Basic_Demos <- Basic_Demos[, c("EID", "Sex", "Age", "Study_Site")]
Barratt <- Barratt[, c("EID", "Barratt_P1_Edu", "Barratt_P2_Edu", 
                       "Barratt_Total_Edu", "Barratt_P1_Occ", "Barratt_P2_Occ", 
                       "Barratt_Total_Occ", "Barratt_Total")]

ARI_P <- ARI_P[c("EID","ARI_P_Total_Score")]
ARI_S <- ARI_S[c("EID","ARI_S_Total_Score")]

# Regress age and sex from ARI
ARI_P <- Merge(ARI_P, Basic_Demos)
ARI_P <- ARI_P[which(!is.na(ARI_P$ARI_P_Total_Score)), ]
model <- lm(ARI_P_Total_Score ~ Age + Sex, data = ARI_P)
ARI_P$ARI_P_Total_Score_AgeSex_Ctrl <- resid(model)
ARI_P <- ARI_P[, c("EID", "ARI_P_Total_Score", "ARI_P_Total_Score_AgeSex_Ctrl")]

ARI_S <- Merge(ARI_S, Basic_Demos)
ARI_S <- ARI_S[which(!is.na(ARI_S$ARI_S_Total_Score)), ]
model <- lm(ARI_S_Total_Score ~ Age + Sex, data = ARI_S)
ARI_S$ARI_S_Total_Score_AgeSex_Ctrl <- resid(model)
ARI_S <- ARI_S[c("EID", "ARI_S_Total_Score", "ARI_S_Total_Score_AgeSex_Ctrl")]

ASSQ <- ASSQ[c("EID", "ASSQ_Total")]
CBCL <- CBCL[c("EID", "CBCL_Int_T", "CBCL_Ext_T", "CBCL_Total_T")]
CELF <- CELF[c("EID", "CELF_Total", "CELF_ExceedCutoff")]
Conners_SR <- Conners_SR[c("EID","C3SR_AG_T", "C3SR_HY_T", "C3SR_IN_T", "C3SR_LP_T")]
Pegboard <- Pegboard[c("EID", "peg_z_d", "peg_z_nd")]
MFQ_P <- MFQ_P[c("EID", "MFQ_P_Total")]
MFQ_SR <- MFQ_SR[c("EID", "MFQ_SR_Total")]

# Regress age from MFQ
MFQ_P <- Merge(MFQ_P, Basic_Demos)
MFQ_P <- MFQ_P[which(!is.na(MFQ_P$MFQ_P_Total)), ]
model <- lm(MFQ_P_Total ~ Age + Sex, data = MFQ_P)
MFQ_P$MFQ_P_Total_AgeSex_Ctrl <- resid(model)
MFQ_P <- MFQ_P[c("EID", "MFQ_P_Total", "MFQ_P_Total_AgeSex_Ctrl")]

MFQ_SR <- Merge(MFQ_SR, Basic_Demos)
MFQ_SR <- MFQ_SR[which(!is.na(MFQ_SR$MFQ_SR_Total)), ]
model <- lm(MFQ_SR_Total ~ Age + Sex, data = MFQ_SR)
MFQ_SR$MFQ_SR_Total_AgeSex_Ctrl <- resid(model)
MFQ_SR <- MFQ_SR[c("EID", "MFQ_SR_Total", "MFQ_SR_Total_AgeSex_Ctrl")]

NIH_Scores <- NIH_Scores[c("EID", "NIH7_Pattern", "NIH7_Card", "NIH7_Flanker")]

SCARED_P <- SCARED_P[c("EID", "SCARED_P_Total")]
SCARED_SR <- SCARED_SR[c("EID", "SCARED_SR_Total")]

# Regress age and sex from SCARED
SCARED_P <- Merge(SCARED_P, Basic_Demos)
SCARED_P <- SCARED_P[which(!is.na(SCARED_P$SCARED_P_Total)), ]
model <- lm(SCARED_P_Total ~ Age + Sex, data = SCARED_P)
SCARED_P$SCARED_P_Total_AgeSex_Ctrl <- resid(model)
SCARED_P <- SCARED_P[c("EID", "SCARED_P_Total", "SCARED_P_Total_AgeSex_Ctrl")]

SCARED_SR <- Merge(SCARED_SR, Basic_Demos)
SCARED_SR <- SCARED_SR[which(!is.na(SCARED_SR$SCARED_SR_Total)), ]
model <- lm(SCARED_SR_Total ~ Age + Sex, data = SCARED_SR)
SCARED_SR$SCARED_SR_Total_AgeSex_Ctrl <- resid(model)
SCARED_SR <- SCARED_SR[c("EID", "SCARED_SR_Total", "SCARED_SR_Total_AgeSex_Ctrl")]

SCQ <- SCQ[c("EID", "SCQ_Total")]
SRS <- SRS[c("EID", "SRS_Total")]
SWAN <- SWAN[c("EID", 
               "SWAN_01", "SWAN_02", "SWAN_03", "SWAN_04", "SWAN_05",
               "SWAN_06", "SWAN_07", "SWAN_08", "SWAN_09",
               "SWAN_10", "SWAN_11", "SWAN_12", "SWAN_13", "SWAN_14",
               "SWAN_15", "SWAN_16", "SWAN_17", "SWAN_18",
               "SWAN_IN", "SWAN_HY", "SWAN_Total")]

# Regress age and sex from SWAN
SWAN <- Merge(SWAN, Basic_Demos)
SWAN <- SWAN[!(is.na(SWAN$SWAN_IN) | is.na(SWAN$SWAN_Total) |
                 is.na(SWAN$Age) | is.na(SWAN$Sex)), ]
model <- lm(SWAN_IN ~ Age + Sex, data = SWAN)
SWAN$SWAN_IN_AgeSex_Ctrl <- resid(model)
model <- lm(SWAN_HY ~ Age + Sex, data = SWAN)
SWAN$SWAN_HY_AgeSex_Ctrl <- resid(model)
model <- lm(SWAN_Total ~ Age + Sex, data = SWAN)
SWAN$SWAN_Total_AgeSex_Ctrl <- resid(model)
SWAN <- SWAN[c("EID", "SWAN_01", "SWAN_02", "SWAN_03", "SWAN_04", "SWAN_05",
               "SWAN_06", "SWAN_07", "SWAN_08", "SWAN_09",
               "SWAN_10", "SWAN_11", "SWAN_12", "SWAN_13", "SWAN_14",
               "SWAN_15", "SWAN_16", "SWAN_17", "SWAN_18",
               "SWAN_IN", "SWAN_HY", "SWAN_Total", 
               "SWAN_IN_AgeSex_Ctrl", "SWAN_HY_AgeSex_Ctrl", "SWAN_Total_AgeSex_Ctrl")]

#WAIS <- WAIS[c("EID", "WAIS_CODE_S", "WAIS_SYM_S")]
#WASI <- WASI[c("EID", "WASI_FSIQ")]

WISC_PCA <- WISC
WISC <- WISC[c("EID", "WISC_SS_Standard", "WISC_Coding_Standard", "WISC_PSI", "WISC_VCI", "WISC_FSIQ", "WISC_GAI")] 
WIAT <- WIAT[c("EID", "WIAT_Num_Stnd", "WIAT_Pseudo_Stnd", "WIAT_Spell_Stnd",
               "WIAT_Word_Stnd", "WIAT_LCRV_Std", "WIAT_LCODC_Stnd",
               "WIAT_LC_Stnd", "WIAT_RC_Stnd", "WIAT_MP_Stnd")]
YSR <- YSR[c("EID", "YSR_Ext_T", "YSR_Int_T")]

# Removing MRV kids (because there are 3 in this sample)
# Changing study site code to 1 = SI, 2 = Midtown
Basic_Demos <- Basic_Demos[Basic_Demos$Study_Site != 2, ]
Basic_Demos$Study_Site[Basic_Demos$Study_Site == 3] <- 2



### Run factor analysis on ASD measures
asd_data <- Reduce(Merge, list(SCQ, ASSQ, SRS))
row.names(asd_data) <- asd_data$EID
asd_data$EID <- NULL
asd_data <- na.omit(asd_data)
fit <- factanal(asd_data, 1, rotation = "varimax")
print(fit)

load <- fit$loadings[,1] 
plot(load,type="n") # set up plot 
text(load,labels=names(asd_data),cex=.7) # add variable names

fit <- fa(asd_data, nfactors=1, rotation="varimax")
fit # print results
asd_fa <- fit$scores
asd_fa <- data.frame(EID = rownames(asd_fa), ASD_latent_factor = asd_fa, stringsAsFactors = FALSE)
names(asd_fa) = c("EID", "ASD_latent_factor")
rownames(asd_fa) <- NULL

### Grouping participants by diagnosis
Consensus_Dx$ADHD_I <- as.numeric(Consensus_Dx$DX_01_Code == "F90.0" | Consensus_Dx$DX_01 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_02_Code == "F90.0" | Consensus_Dx$DX_02 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_03_Code == "F90.0" | Consensus_Dx$DX_03 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_04_Code == "F90.0" | Consensus_Dx$DX_04 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_05_Code == "F90.0" | Consensus_Dx$DX_05 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_06_Code == "F90.0" | Consensus_Dx$DX_06 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_07_Code == "F90.0" | Consensus_Dx$DX_07 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_08_Code == "F90.0" | Consensus_Dx$DX_08 == "ADHD-Inattentive Type" |
                                                    Consensus_Dx$DX_09_Code == "F90.0" | Consensus_Dx$DX_09 == "ADHD-Inattentive Type")
Consensus_Dx$ADHD_H <- as.numeric(Consensus_Dx$DX_01_Code == "F90.1" | Consensus_Dx$DX_01 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_02_Code == "F90.1" | Consensus_Dx$DX_02 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_03_Code == "F90.1" | Consensus_Dx$DX_03 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_04_Code == "F90.1" | Consensus_Dx$DX_04 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_05_Code == "F90.1" | Consensus_Dx$DX_05 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_06_Code == "F90.1" | Consensus_Dx$DX_06 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_07_Code == "F90.1" | Consensus_Dx$DX_07 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_08_Code == "F90.1" | Consensus_Dx$DX_08 == "ADHD-Hyperactive/Impulsive Type" |
                                                    Consensus_Dx$DX_09_Code == "F90.1" | Consensus_Dx$DX_09 == "ADHD-Hyperactive/Impulsive Type")
Consensus_Dx$ADHD_C <- as.numeric(Consensus_Dx$DX_01_Code == "F90.2" | Consensus_Dx$DX_01 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_02_Code == "F90.2" | Consensus_Dx$DX_02 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_03_Code == "F90.2" | Consensus_Dx$DX_03 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_04_Code == "F90.2" | Consensus_Dx$DX_04 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_05_Code == "F90.2" | Consensus_Dx$DX_05 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_06_Code == "F90.2" | Consensus_Dx$DX_06 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_07_Code == "F90.2" | Consensus_Dx$DX_07 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_08_Code == "F90.2" | Consensus_Dx$DX_08 == "ADHD-Combined Type" |
                                                    Consensus_Dx$DX_09_Code == "F90.2" | Consensus_Dx$DX_09 == "ADHD-Combined Type")
Consensus_Dx$ASD <- as.numeric(Consensus_Dx$DX_01_Code == "F84.0" | Consensus_Dx$DX_01 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_02_Code == "F84.0" | Consensus_Dx$DX_02 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_03_Code == "F84.0" | Consensus_Dx$DX_03 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_04_Code == "F84.0" | Consensus_Dx$DX_04 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_05_Code == "F84.0" | Consensus_Dx$DX_05 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_06_Code == "F84.0" | Consensus_Dx$DX_06 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_07_Code == "F84.0" | Consensus_Dx$DX_07 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_08_Code == "F84.0" | Consensus_Dx$DX_08 == "Autism Spectrum Disorder" |
                                                 Consensus_Dx$DX_09_Code == "F84.0" | Consensus_Dx$DX_09 == "Autism Spectrum Disorder")
Consensus_Dx$DMDD <- as.numeric(Consensus_Dx$DX_01_Code == "F34.8" | Consensus_Dx$DX_01 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_02_Code == "F34.8" | Consensus_Dx$DX_02 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_03_Code == "F34.8" | Consensus_Dx$DX_03 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_04_Code == "F34.8" | Consensus_Dx$DX_04 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_05_Code == "F34.8" | Consensus_Dx$DX_05 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_06_Code == "F34.8" | Consensus_Dx$DX_06 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_07_Code == "F34.8" | Consensus_Dx$DX_07 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_08_Code == "F34.8" | Consensus_Dx$DX_08 == "Disruptive Mood Dysregulation Disorder" |
                                                  Consensus_Dx$DX_09_Code == "F34.8" | Consensus_Dx$DX_09 == "Disruptive Mood Dysregulation Disorder")
Consensus_Dx$GAD <- as.numeric(Consensus_Dx$DX_01_Code == "F41.1" | Consensus_Dx$DX_01 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_02_Code == "F41.1" | Consensus_Dx$DX_02 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_03_Code == "F41.1" | Consensus_Dx$DX_03 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_04_Code == "F41.1" | Consensus_Dx$DX_04 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_05_Code == "F41.1" | Consensus_Dx$DX_05 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_06_Code == "F41.1" | Consensus_Dx$DX_06 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_07_Code == "F41.1" | Consensus_Dx$DX_07 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_08_Code == "F41.1" | Consensus_Dx$DX_08 == "Generalized Anxiety Disorder" |
                                                 Consensus_Dx$DX_09_Code == "F41.1" | Consensus_Dx$DX_09 == "Generalized Anxiety Disorder")
Consensus_Dx$Learning <- as.numeric(Consensus_Dx$DX_01_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_02_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_03_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_04_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_05_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_06_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_07_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_08_Sub == "Specific Learning Disorder" |
                                                      Consensus_Dx$DX_09_Sub == "Specific Learning Disorder")
Consensus_Dx$Learning_Reading <- as.numeric(Consensus_Dx$DX_01 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_02 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_03 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_04 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_05 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_06 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_07 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_08 == "Specific Learning Disorder with Impairment in Reading" |
                                      Consensus_Dx$DX_09 == "Specific Learning Disorder with Impairment in Reading")
Consensus_Dx$Learning_Math <- as.numeric(Consensus_Dx$DX_01 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_02 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_03 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_04 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_05 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_06 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_07 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_08 == "Specific Learning Disorder with Impairment in Mathematics" |
                                              Consensus_Dx$DX_09 == "Specific Learning Disorder with Impairment in Mathematics")
Consensus_Dx$Learning_Writing <- as.numeric(Consensus_Dx$DX_01 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_02 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_03 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_04 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_05 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_06 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_07 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_08 == "Specific Learning Disorder with Impairment in Written Expression" |
                                              Consensus_Dx$DX_09 == "Specific Learning Disorder with Impairment in Written Expression")
Consensus_Dx$Anx_Soc <- as.numeric(grepl("Social Anxiety", Consensus_Dx$DX_01) |
                                                     grepl("Anxiety", Consensus_Dx$DX_02) |
                                                     grepl("Anxiety", Consensus_Dx$DX_03) |
                                                     grepl("Anxiety", Consensus_Dx$DX_04) |
                                                     grepl("Anxiety", Consensus_Dx$DX_05) |
                                                     grepl("Anxiety", Consensus_Dx$DX_06) |
                                                     grepl("Anxiety", Consensus_Dx$DX_07) |
                                                     grepl("Anxiety", Consensus_Dx$DX_08) |
                                                     grepl("Anxiety", Consensus_Dx$DX_09))
Consensus_Dx$Anx_All <- as.numeric(grepl("Anxiety", Consensus_Dx$DX_01_Cat) | grepl("Anxiety", Consensus_Dx$DX_01) |
                                                     grepl("Anxiety", Consensus_Dx$DX_02_Cat) | grepl("Anxiety", Consensus_Dx$DX_02) |
                                                     grepl("Anxiety", Consensus_Dx$DX_03_Cat) | grepl("Anxiety", Consensus_Dx$DX_03) |
                                                     grepl("Anxiety", Consensus_Dx$DX_04_Cat) | grepl("Anxiety", Consensus_Dx$DX_04) |
                                                     grepl("Anxiety", Consensus_Dx$DX_05_Cat) | grepl("Anxiety", Consensus_Dx$DX_05) |
                                                     grepl("Anxiety", Consensus_Dx$DX_06_Cat) | grepl("Anxiety", Consensus_Dx$DX_06) |
                                                     grepl("Anxiety", Consensus_Dx$DX_07_Cat) | grepl("Anxiety", Consensus_Dx$DX_07) |
                                                     grepl("Anxiety", Consensus_Dx$DX_08_Cat) | grepl("Anxiety", Consensus_Dx$DX_08) |
                                                     grepl("Anxiety", Consensus_Dx$DX_09_Cat) | grepl("Anxiety", Consensus_Dx$DX_09))
Consensus_Dx$Depression <- as.numeric(grepl("Depressive Disorder", Consensus_Dx$DX_01) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_02) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_03) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_04) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_05) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_06) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_07) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_08) |
                                                        grepl("Depressive Disorder", Consensus_Dx$DX_09))
Consensus_Dx$ASD_ADHD_I <- as.numeric(Consensus_Dx$ASD == 1 & Consensus_Dx$ADHD_I == 1)
Consensus_Dx$ASD_ADHD_H <- as.numeric(Consensus_Dx$ASD == 1 & Consensus_Dx$ADHD_H == 1)
Consensus_Dx$ASD_ADHD_C <- as.numeric(Consensus_Dx$ASD == 1 & Consensus_Dx$ADHD_C == 1)
Consensus_Dx$ASD_ADHD_All <- as.numeric(Consensus_Dx$ASD == 1 & (Consensus_Dx$ADHD_I == 1 | Consensus_Dx$ADHD_H == 1 | Consensus_Dx$ADHD_C == 1)) 
Consensus_Dx$ASD_noADHD <- as.numeric(Consensus_Dx$ASD == 1 & Consensus_Dx$ADHD_I == 0 & Consensus_Dx$ADHD_H == 0 & Consensus_Dx$ADHD_C == 0)
Consensus_Dx$NoDx <- as.numeric(Consensus_Dx$NoDX == 1)
Consensus_Dx$NoDxDropout <- as.numeric(Consensus_Dx$NoDX == 3)
# Includes Major Depressive Disorder, Persistent Depressive Disorder, and Other Specified Depressive Disorder
Dx_Cat <- Consensus_Dx[Consensus_Dx$NoDX != 3, c("EID", "ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", "GAD", "Learning", "Learning_Reading", "Learning_Math", "Learning_Writing", 
                                                 "Anx_Soc", "Anx_All",
                                                                                 "Depression", "ASD_ADHD_I", "ASD_ADHD_H", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD",
                                                                                 "NoDx", "NoDxDropout")]

# Merge dataframes for processing speed tasks
PSTasks <- Reduce(Merge, list(EEG_behav_data[c("EID", "EEG_Standard")], NIH_Scores[, c("EID", "NIH7_Pattern")], WISC[, c("EID", "WISC_Coding_Standard", "WISC_SS_Standard")]))


PSTasks <- na.omit(PSTasks[PSTasks$EID %in% Dx_Cat$EID, ])
row.names(PSTasks) <- PSTasks$EID


### Run PCA on processing speed measures
setwd("R:/Data Feb 2019")

### Look at Processing Speed Outliers
PSTasks_long <- melt(PSTasks, id.vars = c("EID"))

# Generate boxplots for processing speed measures
ggplot(PSTasks_long, aes(x = variable, y = value)) + geom_boxplot() +
  labs(x = "Measure", y = "Score")
ggsave("Plots/ProcSpeedMeasures.png")

# Mark participants that have an outlier score within 2 and 3 standard deviations
for (measure in c("EEG_Standard", "NIH7_Pattern", "WISC_Coding_Standard", "WISC_SS_Standard")) {
  mean <- mean(PSTasks[, measure])
  sd <- sd(PSTasks[, measure])
  PSTasks[, paste0(measure, "_outlier2SD")] <- (PSTasks[, measure] > mean + 2 * sd) | (PSTasks[, measure] < mean - 2 * sd)
  PSTasks[, paste0(measure, "_outlier3SD")] <- (PSTasks[, measure] > mean + 3 * sd) | (PSTasks[, measure] < mean - 3 * sd)
}

# Generate dataframes with PS and dx data for participants with outliers
PSTasks_num_outliers <- colSums(PSTasks[, c("EEG_Standard_outlier2SD", "NIH7_Pattern_outlier2SD", 
                                            "WISC_Coding_Standard_outlier2SD", "WISC_SS_Standard_outlier2SD", 
                                            "EEG_Standard_outlier3SD", "NIH7_Pattern_outlier3SD", 
                                            "WISC_Coding_Standard_outlier3SD", "WISC_SS_Standard_outlier3SD")])
PSTasks_outliers_2SD <-  PSTasks[rowSums(PSTasks[, c(6, 8, 10, 12)]) > 0, c(1:5)]
PSTasks_outliers_3SD <-  PSTasks[rowSums(PSTasks[, c(7, 9, 11, 13)]) > 0, c(1:5)]

PSTasks_outliers_2SD <- merge(PSTasks_outliers_2SD, Dx_Cat, by = "EID", all.x = TRUE, all.y = FALSE)
PSTasks_outliers_3SD <- merge(PSTasks_outliers_3SD, Dx_Cat, by = "EID", all.x = TRUE, all.y = FALSE)

write.csv(PSTasks_outliers_2SD, "Results/ProcSpeedOutliers2SD.csv", row.names = FALSE)
write.csv(PSTasks_outliers_3SD, "Results/ProcSpeedOutliers3SD.csv", row.names = FALSE)

# Plot distribution of diagnoses for 2SD outliers
PSTasks_outliers_2SD_dx <- data.frame(dx = names(PSTasks_outliers_2SD[, c(6:15, 21)]),
                                      n = colSums(PSTasks_outliers_2SD[, c(6:15, 21)]))
ggplot(PSTasks_outliers_2SD_dx, aes(dx, n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Plots/Outliers/Outliers2SD_dx_dist.png")

PSTasks_outliers_2SD_dx <- PSTasks_outliers_2SD[, c(1, 6:15, 21)]
rownames(PSTasks_outliers_2SD_dx) <- PSTasks_outliers_2SD_dx$EID
PSTasks_outliers_2SD_dx$EID <- NULL
png("Plots/Outliers/Outliers2SD_dx_heatmap.png")
par(oma = c(2, 1, 1, 2))
heatmap(as.matrix(PSTasks_outliers_2SD_dx), 
        Rowv = NA, Colv = NA, col = inferno(256), scale = "none")
dev.off()

# Run PCA, and only remove 3SD outliers
PSTasks_rm_outliers <- PSTasks[rowSums(PSTasks[, c(7, 9, 11, 13)]) == 0, c(1:5)]

PSTasks[, c("EEG_Standard_outlier2SD", "NIH7_Pattern_outlier2SD", 
            "WISC_Coding_Standard_outlier2SD", "WISC_SS_Standard_outlier2SD", 
            "EEG_Standard_outlier3SD", "NIH7_Pattern_outlier3SD", 
            "WISC_Coding_Standard_outlier3SD", "WISC_SS_Standard_outlier3SD")] <- NULL

PSTask_orig <- PSTasks
PSTasks <- PSTasks_rm_outliers
PSTasks$EID <- NULL

pca <- prcomp(~ WISC_Coding_Standard + WISC_SS_Standard + NIH7_Pattern + EEG_Standard, data = PSTasks, scale. = TRUE)
summary(pca)
png("Plots/PCA/PCA_biplot.png")
biplot(pca)
dev.off()

pca_components <- data.frame(pca$x)

# Run varimax rotation on PC1 (There are three methods and they all have the
# same output)
ncomp <- 4

# Method 1
pca_rotated <- psych::principal(PSTasks, rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_rotated$scores[1:5,])  # Scores returned by principal()

# Method 2
pca <- prcomp(PSTasks, center=T, scale=T)
rawLoadings <- pca$rotation[,1:ncomp] %*% diag(pca$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings <- t(pracma::pinv(rotatedLoadings))
scores <- scale(PSTasks) %*% invLoadings
print(scores[1:5,]) # Scores computed via rotated loadings

# Method 3
scores <- scale(pca$x[,1:ncomp]) %*% varimax(rawLoadings)$rotmat
print(scores[1:5,])

pca_components_rot <- data.frame(scores)

png("Plots/PCA/PCA_variable_biplot.png")
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
dev.off()

# Aki- look at parallel analysis
# Also, PC2 contains less than 25% of variance (i.e. proportion of variance
# shared by the 4 measures)
# Rotations

## PCA plots
pca_components$EID <- row.names(pca_components)
names(pca_components_rot) <- c("RC1", "RC2", "RC3", "RC4")
pca_components_rot$EID <- row.names(pca_components_rot)
pca_results <- merge(pca_components, pca_components_rot, by = "EID")
PSTasks$EID <- row.names(PSTasks)
PSTasks <- Merge(PSTasks, pca_results)
hist(PSTasks$PC1, xlab = "PC1", main = "") # PC1 histogram
hist(PSTasks$PC2, xlab = "PC2", main = "") # PC2 histogram

# PC1/PC2 scatterplot
ggplot(PSTasks, aes(PC1, PC2)) + 
  geom_point() +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
  annotate("text", -4, 5, label = paste0("r = ", signif(cor(PSTasks$PC1, PSTasks$PC2, method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_PC2.png",
       width = 6, height = 6, units = "in")

# PC1/Barratt scatterplot
plot_data <- merge(pca_results, Barratt, all.x = TRUE)
plot_data <- na.omit(plot_data)
ggplot(plot_data, aes(PC1, Barratt_Total)) + 
  geom_point() +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 80)) +
  annotate("text", -4, 80, label = paste0("r = ", signif(cor(plot_data$PC1, plot_data$Barratt_Total, use = "complete.obs", method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_Barratt.png",
       width = 6, height = 6, units = "in")

# Save PC1 output
write.csv(PSTasks, "Results/PCA.csv", row.names = FALSE)

### Get demographic information
PSTasks_demos <- merge(PSTasks, Basic_Demos)
print(paste0("Age: ", round(mean(PSTasks_demos$Age), 2), "+-", 
             round(sd(PSTasks_demos$Age), 2)))
print(paste0("Male: ", nrow(PSTasks_demos) - sum(PSTasks_demos$Sex),
             "; Female: ", sum(PSTasks_demos$Sex)))

PSTasks_demos <- merge(PSTasks_demos, Barratt, all.x = TRUE)

pct_prof <- sum(PSTasks_demos$Barratt_P1_Occ > 35 | PSTasks_demos$Barratt_P2_Occ > 35, na.rm = TRUE)/nrow(PSTasks_demos) * 100
print(paste0("Percentage of children with managerial or professional occupation for one or both parents: ", round(pct_prof, 4)))
sum(is.na(PSTasks_demos$Barratt_P1_Occ) & is.na(PSTasks_demos$Barratt_P2_Occ))

PSTasks_demos <- merge(PSTasks_demos, Race_Ethnicity, all.x = TRUE)
### Ethnicity:
### 0 - White/Caucasian
### 1 - Black/African American
### 2 - Hispanic
### 3 - Asian
### 4 - Indian
### 5 - Native American Indian
### 6 - American Indian/Alaskan Native
### 7 - Native Hawaiian/Other Pacific Islander
### 8 - Two or more races
### 9 - Other race 
### 10 - Unknown

### Race 
### 
### 0 - Not Hispanic or Latino
### 1 - Hispanic or Latino
### 2 - Decline to specify
### 3 - Unknown
summary(factor(PSTasks_demos$Child_Race))/nrow(PSTasks_demos) * 100
summary(factor(PSTasks_demos$Child_Ethnicity))/nrow(PSTasks_demos) * 100 

### Summarize age and sex by diagnoses
PSTasks_demos_dx <- merge(PSTasks_demos[, c("EID", "Sex", "Age", "Barratt_Total")],
                          Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C",
                                                           "ASD", "Learning_Reading", "Learning_Math","Anx_All",
                                                           "Depression", "NoDx")])
PSTasks_demos_dx$OtherDX <- as.numeric(rowSums(PSTasks_demos_dx[, c("ADHD_I", "ADHD_C",
                                                           "ASD", "Learning_Reading", "Learning_Math", "Anx_All",
                                                           "Depression", "NoDx")]) == 0)
# Note: n = x, x  if Barratt is included

dx_demos_summary <- data.frame(disorder = character(), n = numeric(), n_male = numeric(), 
                               n_female = numeric(), mean_age = numeric(), sd_age = numeric(),
                               mean_barratt = numeric(), sd_barratt = numeric(),
                               stringsAsFactors = FALSE)

for (disorder in c("ADHD_I", "ADHD_C",
                   "ASD", "Learning_Reading", "Learning_Math", "Anx_All",
                   "Depression", "NoDx", "OtherDX", "Total")) {
  if (disorder == "Total") {
    temp <- PSTasks_demos_dx
  } else {
    temp <- PSTasks_demos_dx[which(PSTasks_demos_dx[, disorder] == 1), 
                           c("EID", "Sex", "Age", "Barratt_Total")]
  }
  n <- nrow(temp)
  n_female <- sum(temp$Sex)
  n_male <- n - n_female
  mean_age <- mean(temp$Age)
  sd_age <- sd(temp$Age)
  mean_barratt <- mean(temp$Barratt_Total, na.rm = TRUE)
  sd_barratt <- sd(temp$Barratt_Total, na.rm = TRUE)
  dx_demos_summary <- rbind(dx_demos_summary, 
                            data.frame(disorder = disorder,
                                       n = n,
                                       n_male = n_male,
                                       n_female = n_female,
                                       mean_age = round(mean_age, 2), 
                                       sd_age = round(sd_age, 2),
                                       mean_barratt = round(mean_barratt, 2),
                                       sd_barratt = round(sd_barratt, 2),
                                       stringsAsFactors = FALSE))
}

write.csv(dx_demos_summary, "Results/dx_demos_summary.csv", row.names = FALSE)

### Venn Diagrams
venn_data <- PSTasks_demos_dx
venn_data_list <- list(
                       ADHD_I = venn_data$EID[which(venn_data$ADHD_I==1)],
                       ADHD_C = venn_data$EID[which(venn_data$ADHD_C==1)],
                       ASD = venn_data$EID[which(venn_data$ASD==1)],
                       Learning = venn_data$EID[which(venn_data$Learning==1)],
                       Anx_All = venn_data$EID[which(venn_data$Anx_All==1)],
                       Depression = venn_data$EID[which(venn_data$Depression==1)]
                       )
png("Plots/VennDiagram/venndiagram_n.png", width = 640, height = 640)
venn(venn_data_list, col = rainbow(6, start = 0.001, end = 0.999),
     zcolor = "style", opacity = 0.7, cexil = 1, cexsn = 1)
dev.off()

### UpSet Plot
PSTasks_demos_dx$Sex[PSTasks_demos_dx$Sex == 1] <- "Female"
PSTasks_demos_dx$Sex[PSTasks_demos_dx$Sex == 0] <- "Male"
PSTasks_demos_dx$OtherDx <- as.numeric(!PSTasks_demos_dx$NoDx & PSTasks_demos_dx$OtherDX)
PSTasks_demos_dx$OtherDX <- NULL

querySex <- function(row, sex) {
  data <- (row["Sex"] == sex)
}


tiff("Plots/Upset/Upset.tiff")
upset(PSTasks_demos_dx[, 2:13],
      main.bar.color = "gray23",
      att.color = "gray23",
      queries = list(list(query = querySex, params = list("Female"), color = "red", active = TRUE)),
      nsets = 6, 
      sets = c("Anx_All", "ADHD_I", "ADHD_C", "Learning_Reading", "Learning_Math", "ASD", "Depression", "OtherDx", "NoDx"),
      keep.order = TRUE,
      boxplot.summary = c("Age", "Barratt_Total"),
      order.by = c("degree", "freq"), decreasing = c(TRUE, TRUE),
      text.scale = c(2, 2, 1.5, 0.8, 1.5, 2)
      )
dev.off()


###merging data sets into on Full
FullDataSet<- Reduce(Merge, list(Basic_Demos, NIH_Scores, EEG_behav_data, Pegboard, WISC, WIAT, YSR,
                                 CELF, Conners_SR, SWAN, ASSQ, SRS, SCQ, ARI_P, ARI_S,
                                 MFQ_SR, MFQ_P, SCARED_SR, SCARED_P, CBCL, WAIS, WASI))


### Visualize PSI data

# Plot PS measures vs PC/RC/WISC/GP
plot_data <- Reduce(Merge, list(PSTasks, WISC[, c("EID", "WISC_FSIQ", "WISC_GAI")], Pegboard[, c("EID", "peg_z_d")]))
plot_data <- plot_data[!is.na(plot_data$PC1) & !is.na(plot_data$PC2), ]
plot_data <- plot_data[, c("EID", "EEG_Standard", "NIH7_Pattern", "WISC_Coding_Standard",
                           "WISC_SS_Standard", "PC1", "PC2", "RC1", "RC2", "WISC_FSIQ", 
                           "WISC_GAI", "peg_z_d")]
plot_data_long <- melt(plot_data, id.vars = c("EID", "PC1", "PC2", "RC1", "RC2", "WISC_FSIQ", "WISC_GAI", "peg_z_d"), variable_name = "Measure1")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value1"
plot_data_long <- melt(plot_data_long, id.vars = c("EID", "Measure1", "value1"), variable_name = "Measure2")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value2"

plot_data_long$Measure1 <- factor(plot_data_long$Measure1)
levels(plot_data_long$Measure1) <- c("WISC Symbol \nSearch (EEG)", "NIH Pattern \nComparison", 
                                     "WISC Coding", "WISC Symbol \nSearch (Clinician)")

plot_data_long$Measure2 <- factor(plot_data_long$Measure2)
levels(plot_data_long$Measure2) <- c("PC1", "PC2", "RC1", "RC2", "WISC FSIQ", 
                                     "WISC GAI", "Grooved Pegboard \nZ-score")

ggplot(plot_data_long, aes(x = value1, y = value2)) + geom_point() + 
  facet_grid(Measure2 ~ Measure1, scales = "free_y") + 
  labs(x = "value", y = "value")
ggsave("Plots/Correlations/PSVars_PC1_WISC_GP.png", 
       width = 6, height = 6, units = "in")

# Corresponding r-values
r_values <- round(cor(plot_data[, c("PC1", "PC2", "WISC_FSIQ", "WISC_GAI", "peg_z_d")], 
                      plot_data[, c("EEG_Standard", "NIH7_Pattern", "WISC_Coding_Standard", "WISC_SS_Standard")], 
                      use = "complete.obs", method = "pearson"), 3)
write.csv(r_values, "Results/r_values.csv")

# Corresponding p-values
cor.test(plot_data$PC1, plot_data$EEG_Standard, method = "pearson")
cor.test(plot_data$PC1, plot_data$NIH7_Pattern, method = "pearson")
cor.test(plot_data$PC1, plot_data$WISC_Coding_Standard, method = "pearson")
cor.test(plot_data$PC1, plot_data$WISC_SS_Standard, method = "pearson")
cor.test(plot_data$PC1, plot_data$peg_z_d, method = "pearson")
cor.test(plot_data$WISC_FSIQ, plot_data$WISC_GAI, method = "pearson")

# Plot processing speed measures against principal and rotated components
plot_data_long <- plot_data_long[plot_data_long$Measure2 %in% c("PC1", "PC2", "RC1", "RC2"), ]

ggplot(plot_data_long, aes(x = value1, y = value2)) + geom_point() + 
  facet_grid(Measure2 ~ Measure1, scales = "free_y") + 
  labs(x = "value", y = "value")
ggsave("Plots/Correlations/PSVars_PC_RC.png", 
       width = 6.5, height = 6, units = "in")

r_values <- round(cor(plot_data[, c("PC1", "PC2", "RC1", "RC2")], 
                      plot_data[, c("EEG_Standard", "NIH7_Pattern", "WISC_Coding_Standard", "WISC_SS_Standard")], 
                      use = "complete.obs", method = "pearson"), 3)
write.csv(r_values, "Results/r_values_RC.csv")

### Scatter plots for Principal components, intelligence, and GP

plot_data <- Reduce(Merge, list(PSTasks[, c("EID", "PC1", "PC2")], 
                                WISC[, c("EID", "WISC_FSIQ", "WISC_GAI")], 
                                Pegboard[, c("EID", "peg_z_d")]))
plot_data <- plot_data[!is.na(plot_data$PC1), ]
plot_data[, -1] <- scale(plot_data[, -1]) # scale data before plotting
plot_data_long <- melt(plot_data, id.vars = c("EID"), variable_name = "measure1")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value1"
plot_data_long <- Merge(plot_data_long, plot_data)
plot_data_long <- melt(plot_data_long, id.vars = c("EID", "measure1", "value1"), variable_name = "measure2")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value2"

plot_data_long$measure1 <- factor(plot_data_long$measure1)
levels(plot_data_long$measure1) <- c("PC1", "PC2", "WISC FSIQ", 
                                     "WISC GAI", "Grooved Pegboard \nZ-score")

plot_data_long$measure2 <- factor(plot_data_long$measure2)
levels(plot_data_long$measure2) <- c("PC1", "PC2", "WISC FSIQ", 
                                     "WISC GAI", "Grooved Pegboard \nZ-score")

ggplot(plot_data_long, aes(x = value1, y = value2)) + geom_point() + 
  facet_grid(measure2 ~ measure1, scales = "free") + 
  labs(x = "", y = "")
ggsave("Plots/Correlations/PC1_WISC_GP.png", 
       height = 8, width = 8, units = "in")
correlations <- cor(plot_data[, c("PC1", "PC2", "WISC_FSIQ", "WISC_GAI", "peg_z_d")], use = "complete.obs", method = "pearson")
for (measure in c("WISC_FSIQ", "WISC_GAI", "peg_z_d")) {
  r <- round(cor(plot_data$PC1, plot_data[, measure], use = "complete.obs", method = "pearson"), 2)
  print(paste0("Correlation between PC1 and ", measure, ": r = ", r))
}
write.csv(correlations, "Results/r_values_PC_WISC_GP.csv")


# Scatterplots for rotated components, intelligence, and GP

plot_data <- Reduce(Merge, list(PSTasks[, c("EID", "RC1", "RC2")], 
                                WISC[, c("EID", "WISC_FSIQ", "WISC_GAI")], 
                                Pegboard[, c("EID", "peg_z_d")]))
plot_data <- plot_data[!is.na(plot_data$RC1), ]
plot_data[, -1] <- scale(plot_data[, -1]) # scale data before plotting
plot_data_long <- melt(plot_data, id.vars = c("EID"), variable_name = "measure1")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value1"
plot_data_long <- Merge(plot_data_long, plot_data)
plot_data_long <- melt(plot_data_long, id.vars = c("EID", "measure1", "value1"), variable_name = "measure2")
names(plot_data_long)[names(plot_data_long) == "value"] <- "value2"

plot_data_long$measure1 <- factor(plot_data_long$measure1)
levels(plot_data_long$measure1) <- c("RC1", "RC2", "WISC FSIQ", 
                                     "WISC GAI", "Grooved Pegboard \nZ-score")

plot_data_long$measure2 <- factor(plot_data_long$measure2)
levels(plot_data_long$measure2) <- c("RC1", "RC2", "WISC FSIQ", 
                                     "WISC GAI", "Grooved Pegboard \nZ-score")

ggplot(plot_data_long, aes(x = value1, y = value2)) + geom_point() + 
  facet_grid(measure2 ~ measure1, scales = "free") + 
  labs(x = "", y = "")
ggsave("Plots/Correlations/RC_WISC_GP.png", 
       height = 8, width = 8, units = "in")
correlations <- cor(plot_data[, c("RC1", "RC2", "WISC_FSIQ", "WISC_GAI", "peg_z_d")], use = "complete.obs", method = "pearson")
for (measure in c("WISC_FSIQ", "WISC_GAI", "peg_z_d")) {
  r <- round(cor(plot_data$RC1, plot_data[, measure], use = "complete.obs", method = "pearson"), 2)
  print(paste0("Correlation between PC1 and ", measure, ": r = ", r))
}
write.csv(correlations, "Results/r_values_RC_WISC_GP.csv")

### Total N = 1253 (remove 3SD outliers)

## Create correlation heatmap plots
correlationPlot <- function(df, cols, dir, filename) {
  if (sum(!(cols %in% names(df))) > 0) {
    stop("Column names not found in data frame.")
  }
  if (!file.exists(dir)) {
    stop(paste0(dir, " does not exist"))
  }
  correlationvars <- df[, cols]
  rvalues <- cor(correlationvars, use="pairwise.complete.obs")
  
  # Get P and N values for correlations
  significance <- corr.test(correlationvars, adjust="fdr")
  pvalues <- significance$p
  nvalues <- significance$n
  
  # Set plot parameters, color, and labels
  
  par(mfrow=c(1,1))
  col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                             "cyan", "#007FFF","#00007F"))  
  colnames(rvalues) <- cols
  rownames(rvalues) <- cols
  
  # PLOT CORRELATION MATRIX
  
  filename <- paste0(dir, "/", filename)
  png(filename, width = 960, height = 960)
  corrplot(rvalues, p.mat = pvalues, method="color", insig = "blank",
           addrect=2, tl.cex = 0.6, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
           tl.col="black", tl.srt=60, type="upper")
  dev.off()
  
  print(paste0("Correlation saved in ", dir))
  
  values <- list(pvalues, nvalues, rvalues)
  values
}

df_corr <- Reduce(Merge, list(Basic_Demos, Barratt, CBCL, MFQ_P, MFQ_SR, SCARED_P, SCARED_SR, 
                              ARI_P, ARI_S, SWAN, ASSQ, SRS, SCQ, asd_fa, WISC, CELF, Pegboard,
                              PSTasks[, c("EID", "EEG_Standard", "NIH7_Pattern", "PC1", "PC2", "RC1", "RC2")]))

df_corr$Sex <- as.numeric(df_corr$Sex)
names(df_corr)[which(names(df_corr) == "peg_z_d")] <- "GP"
#dir <- "/Volumes/data/Research/Healthy Brain Network/AACAP/2017/Proc Speed/Data June 2018/Plots/Correlations"
dir <- "R:/Data Feb 2019/Plots/Correlations"
corr_data <- correlationPlot(df_corr, c("Sex", "Age", "Barratt_Total", "MFQ_P_Total_AgeSex_Ctrl", 
                                        "MFQ_SR_Total_AgeSex_Ctrl", "SCARED_P_Total_AgeSex_Ctrl", "SCARED_SR_Total_AgeSex_Ctrl", 
                                        "ARI_P_Total_Score_AgeSex_Ctrl", "ARI_S_Total_Score_AgeSex_Ctrl", "SWAN_IN_AgeSex_Ctrl", 
                                        "SWAN_HY_AgeSex_Ctrl", "ASSQ_Total", "SRS_Total", "SCQ_Total", "GP", 
                                        "PC1", "PC2", "WISC_FSIQ"), 
                             dir, "corr.png")
names(df_corr)[which(names(df_corr) == "GP")] <- "peg_z_d"

# Save p, n, and r values
write.csv(corr_data[[1]], "Results/CorrPlot/pvalues.csv", row.names = FALSE)
write.csv(corr_data[[2]], "Results/CorrPlot/nvalues.csv", row.names = FALSE)
write.csv(corr_data[[3]], "Results/CorrPlot/rvalues.csv", row.names = FALSE)


# ICC Analysis
PS_names <- c("EEG_Standard", "NIH7_Pattern", "WISC_Coding_Standard",
              "WISC_SS_Standard", "PC1", "PC2", "RC1", "RC2")
icc_df <- matrix(data = NA, nrow = 8, ncol = 8)
row.names(icc_df) <- PS_names -> colnames(icc_df)
PSTasks_scaled <- PSTasks
PSTasks_scaled[, -1] <- scale(PSTasks[, -1])
for (row in PS_names) {
  for (col in PS_names) {
    if (row == col) {
      next
    } else {
      ICC_output <- ICC(PSTasks_scaled[, c(row, col)])
      icc_df[row, col] <- ICC_output$results["Single_fixed_raters", "ICC"]
    }
  }
}
icc_df <- signif(icc_df, 3)
write.csv(icc_df, "Results/PSTasks_ICC.csv", row.names = TRUE, na = "")

### standardizeBeta - we may need this for regressions later
standardizeBeta <- function(model) {
  b <- summary(model)$coef[-1, 1]
  sx <- unlist(lapply(model$model[-1], sd))
  sy <- unlist(lapply(model$model[1], sd))
  beta <- b * (sx/sy)
  return(beta)
}

### Categorical Regression

# General steps for each regression:
# 1. Merge revelant datasets
# 2. Loop through IQ measures and PS measures
# 3. Run linear model and append stats to dataframe
# 4. Save stats to csv


Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning_Reading", "Learning_Math", "Anx_Soc", "Anx_All", 
                                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, 
                                        Basic_Demos, Barratt[, c("EID", "Barratt_Total")],
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

# Diagnoses
model <- lm(PC1 ~ WISC_GAI, data = Dx_Cat_Regression)
Dx_Cat_Regression$PC1_Ctrl_GAI <- resid(model)

model <- lm(PC1 ~ WISC_FSIQ, data = Dx_Cat_Regression)
Dx_Cat_Regression$PC1_Ctrl_FSIQ <- resid(model)

ps_measures <- c("PC1", "PC2", "RC1", "RC2", "EEG_Standard", "WISC_SS_Standard",
                 "WISC_Coding_Standard", "NIH7_Pattern")
iq_measures <- c("WISC_FSIQ", "WISC_GAI", "")




for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
    
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                  direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical/", iq_measure, 
            ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Categorical Regression with individual diagnoses
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning_Reading", "Learning_Math", "Anx_Soc", "Anx_All", 
                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, Basic_Demos, Barratt[, c("EID", "Barratt_Total")], WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard, SWAN[, c("EID", "SWAN_IN")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

diagnoses <- c("ADHD_I", "ADHD_C", "ASD", "Learning_Reading", "Learning_Math", "Anx_All", "Depression")

coeffs_all <- data.frame(variable = character(), dx = character(),
                         Estimate = numeric(),	
                         Std_Error = numeric(),	t_value = numeric(),
                         p_value = numeric(),	sig = numeric(), 
                         stringsAsFactors = FALSE)
stats_all <- data.frame(dx = character(),
                        f_statistic = numeric(), df_reg = numeric(),
                        df_error = numeric(), r_sq = numeric(),
                        r_sq_adj = numeric(),
                        stringsAsFactors = FALSE)
for (dx in diagnoses) {
  
  pct_dx <- sum(Dx_Cat_Regression[, dx])/nrow(PSTasks_demos)
  pct_wo_dx <- 1 - pct_dx
  model_eq <- paste0("lm(PC1 ~ ", dx, " + Sex + Age + Study_Site + Barratt_Total + peg_z_d, data = Dx_Cat_Regression)")

  model <- eval(parse(text = model_eq))
  coeffs <- data.frame(signif(summary(model)$coefficients, 4))
  names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
  coeffs$sig <- sig_code_v(coeffs$p_value)
  coeffs$variable <- row.names(coeffs)
  row.names(coeffs) <- NULL
  coeffs$dx <- dx
  coeffs <- coeffs[, names(coeffs_all)]
  coeffs_all <- rbind(coeffs_all, coeffs)
  
  stats <- data.frame(dx = dx,
                      f_statistic = summary(model)$fstatistic['value'], 
                      df_obs = summary(model)$df[2],
                      df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                      r_sq_adj = summary(model)$adj.r.squared,
                      stringsAsFactors = FALSE)
  stats_all <- rbind(stats_all, stats)
}
coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "dx",
                      direction = "wide")
write.csv(coeffs_all, 
          "Results/Regression/Categorical_Single_Dx/coeffs.csv", 
          row.names = FALSE)
write.csv(stats_all, 
          "Results/Regression/Categorical_Single_Dx/stats.csv", 
          row.names = FALSE)

### Categorical Regression with WIAT-Num ops
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning", "Anx_Soc", "Anx_All", 
                                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, Basic_Demos, Barratt[, c("EID", "Barratt_Total")], WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard, SWAN[, c("EID", "SWAN_IN")], WIAT[, c("EID", "WIAT_Num_Stnd")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + WIAT_Num_Stnd, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + WIAT_Num_Stnd + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_WIAT_Num/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_WIAT_Num/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Categorical Regression with SWAN-IN
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning_Math", "Learning_Reading", "Anx_Soc", "Anx_All", 
                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, Basic_Demos, Barratt[, c("EID", "Barratt_Total")], WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard, SWAN[, c("EID", "SWAN_IN")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Reading + Learning_Math + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + SWAN_IN, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Reading + Learning_Math + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + SWAN_IN + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_SWANIN/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_SWANIN/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Categorical Regression with SWAN-IN
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning", "Anx_Soc", "Anx_All", 
                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, Basic_Demos, 
                                        Barratt[, c("EID", "Barratt_Total")], 
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], 
                                        Pegboard, SWAN[, c("EID", "SWAN_IN_AgeSex_Ctrl")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + SWAN_IN_AgeSex_Ctrl, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning + Anx_All + Depression + Sex + Age + Study_Site + Barratt_Total + peg_z_d + SWAN_IN_AgeSex_Ctrl + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_SWANIN/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_SWANIN/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

# Categorical + WIAT SLD Categories
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning", "Anx_Soc", "Anx_All", 
                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, Basic_Demos, 
                                        Barratt[, c("EID", "Barratt_Total")], 
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], 
                                        Pegboard, WIAT[, c("EID", "WIAT_Num_Stnd", "WIAT_Word_Stnd")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)
Dx_Cat_Regression$Learning_Math <- as.numeric(Dx_Cat_Regression$Learning == 1 & 
                                      (Dx_Cat_Regression$WIAT_Num_Stnd < 85 & 
                                      Dx_Cat_Regression$WIAT_Word_Stnd >= 85))
Dx_Cat_Regression$Learning_Reading <- as.numeric(Dx_Cat_Regression$Learning == 1 & 
                                         (Dx_Cat_Regression$WIAT_Num_Stnd >= 85 & 
                                         Dx_Cat_Regression$WIAT_Word_Stnd < 85))
Dx_Cat_Regression$Learning_MathReading <- as.numeric(Dx_Cat_Regression$Learning == 1 & 
                                             (Dx_Cat_Regression$WIAT_Num_Stnd < 85 & 
                                             Dx_Cat_Regression$WIAT_Word_Stnd < 85))
Dx_Cat_Regression$Learning_NoMathReading <- as.numeric(Dx_Cat_Regression$Learning == 1 & 
                                             (Dx_Cat_Regression$WIAT_Num_Stnd >= 85 & 
                                                Dx_Cat_Regression$WIAT_Word_Stnd >= 85))

for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Learning_MathReading + Learning_NoMathReading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Learning_MathReading + Learning_NoMathReading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
    
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_SLD/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_SLD/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Item-Level Categorical Regression with SWAN-IN
Dx_Cat_Regression <- Reduce(Merge, list(PSTasks, Basic_Demos, Barratt[, c("EID", "Barratt_Total")],
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], 
                                        SWAN[, c("EID", "SWAN_01", "SWAN_02", "SWAN_03", "SWAN_04", "SWAN_05",
                                                 "SWAN_06", "SWAN_07", "SWAN_08", "SWAN_09", "SWAN_IN")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)


for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           Estimate = numeric(), Std_Error = numeric(),	
                           t_value = numeric(), p_value = numeric(),	
                           sig = numeric(), stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)

  for (ps_measure in ps_measures) {
    model_eq <- paste0("lm(", ps_measure, " ~ Sex + Age + Barratt_Total + Study_Site + SWAN_01 + SWAN_02 + SWAN_03 + SWAN_04 + SWAN_05 + SWAN_06 + SWAN_07 + SWAN_08 + SWAN_09, data = Dx_Cat_Regression)")
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_SWANIN_item/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_SWANIN_item/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Forward Regression
forward_reg <- df_corr[, c("Sex", "Age", "Barratt_Total", "Study_Site", "SWAN_IN_AgeSex_Ctrl", 
                           "SWAN_HY_AgeSex_Ctrl", "ASSQ_Total", "SRS_Total", "SCQ_Total", 
                           "peg_z_d", "WISC_GAI", "WISC_FSIQ", "PC1")]
forward_reg <- na.omit(forward_reg)
forward_reg$Study_Site <- factor(forward_reg$Study_Site)

cor.test(forward_reg$ASSQ_Total, forward_reg$SCQ_Total, method = "pearson")
ggplot(forward_reg, aes(x = ASSQ_Total, y = SCQ_Total)) + geom_point()

cor.test(forward_reg$SRS_Total, forward_reg$SCQ_Total, method = "pearson")
ggplot(forward_reg, aes(x = SRS_Total, y = SCQ_Total)) + geom_point()

fswr_calls <- data.frame(ps_measure = character(), iq_measure = character(),
                         call = character(), stringsAsFactors = FALSE)


for (iq_measure in iq_measures) {
  fswr_coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                                iq_measure = character(),	Estimate = numeric(),	
                                Std_Error = numeric(),	t_value = numeric(),
                                p_value = numeric(),	sig = numeric(), 
                                stringsAsFactors = FALSE)
  fswr_stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                               f_statistic = numeric(), df_reg = numeric(),
                               df_error = numeric(), r_sq = numeric(),
                               r_sq_adj = numeric(),
                               stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    if (iq_measure == "") {
      forward_reg <- df_corr[, c("Sex", "Age", "Barratt_Total", "Study_Site", "SWAN_IN_AgeSex_Ctrl",
                                 "SWAN_HY_AgeSex_Ctrl", "SRS_Total", "SCQ_Total", "peg_z_d", ps_measure)]
    } else {
      forward_reg <- df_corr[, c("Sex", "Age", "Barratt_Total", "Study_Site", "SWAN_IN_AgeSex_Ctrl",
                                 "SWAN_HY_AgeSex_Ctrl", "SRS_Total", "SCQ_Total", "peg_z_d", iq_measure, ps_measure)]
    }
    forward_reg <- na.omit(forward_reg)
    
    null_model <- lm(paste0(ps_measure, " ~ 1"), data=forward_reg)
    full_model <- lm(paste0(ps_measure, " ~ ."), data=forward_reg)
    fswr <- step(null_model, scope=list(lower=null_model, upper=full_model), direction="forward")
    call <- as.character(fswr$call)
    fswr_calls <- rbind(fswr_calls, data.frame(ps_measure = ps_measure,
                                               iq_measure = iq_measure,
                                               call = call[2], 
                                               stringsAsFactors = FALSE))
    model <- lm(call[2], data = forward_reg)
    
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(fswr_coeffs_all)]
    fswr_coeffs_all <- rbind(fswr_coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    fswr_stats_all <- rbind(fswr_stats_all, stats)
  }
  fswr_coeffs_all$iq_measure <- NULL
  fswr_stats_all$iq_measure <- NULL
  fswr_coeffs_all <- reshape(fswr_coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(fswr_coeffs_all, 
            paste0("Results/Regression/FSWR/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(fswr_stats_all, 
            paste0("Results/Regression/FSWR/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}
write.csv(fswr_calls, "Results/Regression/FSWR/fswr_calls.csv", row.names = FALSE)

### Run regression with signficant FSWR items
Dx_Cat_Regression <- df_corr[, c("Sex", "Age", "Barratt_Total", "Study_Site", "SWAN_IN_AgeSex_Ctrl", 
                                 "SWAN_HY_AgeSex_Ctrl", "ASSQ_Total", "SRS_Total", "SCQ_Total", 
                                 "peg_z_d", "WISC_GAI", "WISC_FSIQ", "PC1")]
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

coeffs_all <- data.frame(variable = character(),	FSWR_item = character(),
                         Estimate = numeric(), CI = character(),	
                         Std_Error = numeric(),	t_value = numeric(),
                         p_value = numeric(),	sig = numeric(), 
                         stringsAsFactors = FALSE)
stats_all <- data.frame(FSWR_item = character(),
                        f_statistic = numeric(), df_reg = numeric(),
                        df_error = numeric(), r_sq = numeric(),
                        r_sq_adj = numeric(),
                        stringsAsFactors = FALSE)

for (FSWR_item in c("ASSQ_Total", "SRS_Total", "SCQ_Total", "SWAN_IN_AgeSex_Ctrl", "SWAN_HY_AgeSex_Ctrl")) {
  model_eq <- paste0("lm(PC1 ~ Sex + Age + Barratt_Total + Study_Site + ", FSWR_item, ", data = Dx_Cat_Regression)")
  model <- eval(parse(text = model_eq))
  coeffs <- data.frame(signif(summary(model)$coefficients, 4))
  names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
  coeffs$sig <- sig_code_v(coeffs$p_value)
  coeffs$variable <- row.names(coeffs)
  coeffs$variable[coeffs$variable == FSWR_item] <- "FSWR_item"
  coeffs$FSWR_item <- FSWR_item
  
  coeffs$CI <- paste0("(", coeffs$Estimate - 2*coeffs$Std_Error, ", ", coeffs$Estimate + 2*coeffs$Std_Error, ")")
  row.names(coeffs) <- NULL
  coeffs <- coeffs[, names(coeffs_all)]
  coeffs_all <- rbind(coeffs_all, coeffs)
  
  stats <- data.frame(FSWR_item = FSWR_item,
                      f_statistic = summary(model)$fstatistic['value'], 
                      df_obs = summary(model)$df[2],
                      df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                      r_sq_adj = summary(model)$adj.r.squared,
                      stringsAsFactors = FALSE)
  stats_all <- rbind(stats_all, stats)
}
coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "FSWR_item",
                      direction = "wide")
row.names(stats_all) <- NULL
write.csv(coeffs_all, 
          paste0("Results/Regression/Categorical_FSWR_item/", iq_measure, 
                 ifelse(iq_measure == "", "", "_"), "coeffs_single_item.csv"), 
          row.names = FALSE)
write.csv(stats_all, 
          paste0("Results/Regression/Categorical_FSWR_item/", iq_measure, 
                 ifelse(iq_measure == "", "", "_"), "stats_single_item.csv"), 
          row.names = FALSE)

# Run forward step-wise regression with SWAN items
fswr_calls <- data.frame(ps_measure = character(), iq_measure = character(),
                         call = character(), stringsAsFactors = FALSE)

for (iq_measure in iq_measures) {
  fswr_coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                                Estimate = numeric(),	
                                Std_Error = numeric(),	t_value = numeric(),
                                p_value = numeric(),	sig = numeric(), 
                                stringsAsFactors = FALSE)
  fswr_stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                               f_statistic = numeric(), df_reg = numeric(),
                               df_error = numeric(), r_sq = numeric(),
                               r_sq_adj = numeric(),
                               stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    forward_reg <- df_corr[, c("SWAN_01", "SWAN_02", "SWAN_03", "SWAN_04",
                               "SWAN_05", "SWAN_06", "SWAN_07", "SWAN_08",
                               "SWAN_09", "SWAN_10", "SWAN_11", "SWAN_12",
                               "SWAN_13", "SWAN_14", "SWAN_15", "SWAN_16",
                               "SWAN_17", "SWAN_18",
                               "Sex", "Age", "Barratt_Total", "Study_Site",
                               ps_measure)]
    forward_reg <- na.omit(forward_reg)
    forward_reg$Study_Site <- as.factor(forward_reg$Study_Site)
    
    null_model <- lm(paste0(ps_measure, " ~ 1"), data=forward_reg)
    full_model <- lm(paste0(ps_measure, " ~ ."), data=forward_reg)
    fswr <- step(null_model, scope=list(lower=null_model, upper=full_model), direction="forward")
    call <- as.character(fswr$call)
    fswr_calls <- rbind(fswr_calls, data.frame(ps_measure = ps_measure,
                                               iq_measure = iq_measure,
                                               call = call[2], 
                                               stringsAsFactors = FALSE))
    model <- lm(call[2], data = forward_reg)
    
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(fswr_coeffs_all)]
    fswr_coeffs_all <- rbind(fswr_coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    fswr_stats_all <- rbind(fswr_stats_all, stats)
  }
  fswr_coeffs_all$iq_measure <- NULL
  fswr_stats_all$iq_measure <- NULL
  fswr_coeffs_all <- reshape(fswr_coeffs_all, idvar = "variable", timevar = "ps_measure",
                             direction = "wide")
  write.csv(fswr_coeffs_all, 
            paste0("Results/Regression/FSWR_SWANIN_item/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(fswr_stats_all, 
            paste0("Results/Regression/FSWR_SWANIN_item/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}
write.csv(fswr_calls, "Results/Regression/FSWR_SWANIN_item/fswr_calls.csv", row.names = FALSE)

### New regression: PC1 <- WIAT Numerical Ops + WIAT Word Reading + SWAN IN +
###                 Age + Sex + Site + Barratt + GP

WIAT_Regression <- Reduce(Merge, list(PSTasks, Basic_Demos, Dx_Cat,
                                      Barratt[, c("EID", "Barratt_Total")], 
                                      WIAT[, c("EID", "WIAT_Num_Stnd", "WIAT_Word_Stnd")], 
                                      Pegboard, SWAN[, c("EID", "SWAN_IN")]))
WIAT_Regression <- na.omit(WIAT_Regression)
WIAT_Regression <- WIAT_Regression[WIAT_Regression$Learning == 1, ]


model <- lm(PC1 ~ WIAT_Num_Stnd + WIAT_Word_Stnd + SWAN_IN + Age + 
              Sex + Study_Site + Barratt_Total + peg_z_nd, data = WIAT_Regression)
coeffs <- data.frame(signif(summary(model)$coefficients, 4))
names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
coeffs$sig <- sig_code_v(coeffs$p_value)
coeffs$variable <- row.names(coeffs)
row.names(coeffs) <- NULL
coeffs <- coeffs[,c("variable", "Estimate", "Std_Error", "t_value", "p_value", "sig")]

stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                    f_statistic = summary(model)$fstatistic['value'], 
                    df_obs = summary(model)$df[2],
                    df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                    r_sq_adj = summary(model)$adj.r.squared,
                    stringsAsFactors = FALSE)

write.csv(coeffs, 
          paste0("Results/Regression/WIAT/", iq_measure, 
                 ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
          row.names = FALSE)
write.csv(stats, 
          paste0("Results/Regression/WIAT/stats.csv"), 
          row.names = FALSE)

# Run PCA on WISC subscales to generate "g"
WISC_subscales <- WISC_PCA[, c("EID", "WISC_BD_Scaled", "WISC_Similarities_Scaled",
             "WISC_MR_Scaled", "WISC_DS_Scaled", "WISC_Coding_Scaled",
             "WISC_Vocab_Scaled", "WISC_FW_Scaled", "WISC_VP_Scaled",
             "WISC_PS_Scaled", "WISC_SS_Scaled", "WISC_FSIQ", "WISC_GAI")]
WISC_subscales <- na.omit(WISC_subscales)
row.names(WISC_subscales) <- WISC_subscales$EID
WISC_subscales$EID <- NULL

WISC_components <- prcomp(~ WISC_BD_Scaled + 
                            WISC_Similarities_Scaled + 
                            WISC_MR_Scaled + 
                            WISC_DS_Scaled +
                            WISC_Coding_Scaled +
                            WISC_Vocab_Scaled +
                            WISC_FW_Scaled +
                            WISC_VP_Scaled +
                            WISC_PS_Scaled +
                            WISC_SS_Scaled, data = WISC_subscales, scale. = TRUE)
summary(WISC_components)
biplot(WISC_components)
fviz_pca_var(WISC_components,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
WISC_PC <- data.frame(WISC_components$x)

ncomp <- 10
pca_rotated <- psych::principal(WISC_subscales[, 1:10], rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_rotated$scores)

WISC_PC_rot <- data.frame(pca_rotated$scores)[, c("RC1", "RC2", "RC3",
                                                           "RC4", "RC5", "RC6",
                                                           "RC7", "RC8", "RC9",
                                                           "RC10")]

WISC_PC$EID <- row.names(WISC_PC)
WISC_PC_rot$EID <- row.names(WISC_PC_rot)

WISC_PC_all <- merge(WISC_PC, WISC_PC_rot, by = "EID")

WISC_subscales$EID <- row.names(WISC_subscales)
WISC_PC_all <- merge(WISC_subscales, WISC_PC_all, by = "EID")

WISC_PC_corr <- cor(WISC_PC_all[, c("WISC_FSIQ", "WISC_GAI", "PC1", "RC1")], 
                    method = "pearson")
write.csv(WISC_PC_corr, "Results/G/WISC_PCA_corr.csv", row.names = FALSE)

cor.test(WISC_PC_all$WISC_GAI, WISC_PC_all$WISC_FSIQ, method = "pearson")
cor.test(WISC_PC_all$PC1, WISC_PC_all$WISC_FSIQ, method = "pearson")
cor.test(WISC_PC_all$PC1, WISC_PC_all$WISC_GAI, method = "pearson")

### Look at Consensus Dx for SLD kids
Consensus_Dx_SLD <- Consensus_Dx[Consensus_Dx$EID %in% 
                                   PSTasks_demos_dx$EID[PSTasks_demos_dx$Learning == 1], ]
Consensus_Dx_SLD <- merge(Consensus_Dx_SLD, WIAT, all.x = TRUE)
Consensus_Dx_SLD <- Consensus_Dx_SLD[, -c(158:174)]
write.csv(Consensus_Dx_SLD, "Results/Consensus_Dx_SLD.csv", row.names = FALSE, na = "")

## categorical regression using 2 subtypes of SLD instead of one
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "Learning", "Learning_Reading", "Learning_Math", "Anx_All", 
                                      "Depression" )]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, 
                                        Basic_Demos, Barratt[, c("EID", "Barratt_Total")],
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)


for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Reading + Learning_Math + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Reading + Learning_Math + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d + ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
    
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_Learning/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_Learning/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### More plots
plot_data <- merge(PSTasks_demos, Pegboard, by = "EID", all.x = TRUE)
plot_data <- plot_data[, c("EID", "PC1", "Age", "Sex", "Study_Site", "Barratt_Total", "peg_z_d")]
plot_data <- na.omit(plot_data)

# Scatterplot of Grooved Pegboard and PC1 after controlling for demos
model <- lm(PC1 ~ Age + Sex + Study_Site + Barratt_Total, data = plot_data)
plot_data$PC1_demo_ctrl <- resid(model)
model <- lm(peg_z_d ~ Age + Sex + Study_Site + Barratt_Total, data = plot_data)
plot_data$peg_z_d_demo_ctrl <- resid(model)

ggplot(plot_data, aes(x = PC1_demo_ctrl, y = peg_z_d_demo_ctrl)) +
  geom_point() +
  annotate("text", -4, 5, label = paste0("r = ", signif(cor(plot_data$PC1_demo_ctrl, plot_data$peg_z_d_demo_ctrl, method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_GP_Demo_Ctrl.png")

# Scatterplot of Grooved Pegboard and PC1 after controlling for demos and significant categorical predictors
plot_data <- merge(plot_data, Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "Learning_Math", "Learning_Reading")],
                   by = "EID", all.x = TRUE)

model <- lm(PC1 ~ Age + Sex + Study_Site + Barratt_Total + ADHD_I + ADHD_C + Learning_Math + Learning_Reading, data = plot_data)
plot_data$PC1_demo_dx_ctrl <- resid(model)
model <- lm(peg_z_d ~ Age + Sex + Study_Site + Barratt_Total + ADHD_I + ADHD_C + Learning_Math + Learning_Reading, data = plot_data)
plot_data$peg_z_d_demo_dx_ctrl <- resid(model)

ggplot(plot_data, aes(x = PC1_demo_dx_ctrl, y = peg_z_d_demo_dx_ctrl)) +
  geom_point() +
  annotate("text", -3.5, 5, label = paste0("r = ", signif(cor(plot_data$PC1_demo_dx_ctrl, plot_data$peg_z_d_demo_dx_ctrl, method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_GP_Demo_Dx_Ctrl.png")

### Run regression with signficant SWAN items
Dx_Cat_Regression <- Reduce(Merge, list(PSTasks, Basic_Demos, Barratt[, c("EID", "Barratt_Total")],
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], 
                                        SWAN[, c("EID", "SWAN_01", "SWAN_02", "SWAN_03", "SWAN_04", "SWAN_05",
                                                 "SWAN_06", "SWAN_07", "SWAN_08", "SWAN_09", "SWAN_10",
                                                 "SWAN_11", "SWAN_12", "SWAN_13", "SWAN_14", "SWAN_15",
                                                 "SWAN_16", "SWAN_17", "SWAN_18")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

coeffs_all <- data.frame(variable = character(),	SWAN_item = character(),
                         Estimate = numeric(), CI = character(),	
                         Std_Error = numeric(),	t_value = numeric(),
                         p_value = numeric(),	sig = numeric(), 
                         stringsAsFactors = FALSE)
stats_all <- data.frame(SWAN_item = character(),
                        f_statistic = numeric(), df_reg = numeric(),
                        df_error = numeric(), r_sq = numeric(),
                        r_sq_adj = numeric(),
                        stringsAsFactors = FALSE)

for (SWAN_item in c("SWAN_02", "SWAN_06", "SWAN_09", "SWAN_16", "SWAN_18")) {
  model_eq <- paste0("lm(PC1 ~ Sex + Age + Barratt_Total + Study_Site + ", SWAN_item, ", data = Dx_Cat_Regression)")
  model <- eval(parse(text = model_eq))
  coeffs <- data.frame(signif(summary(model)$coefficients, 4))
  names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
  coeffs$sig <- sig_code_v(coeffs$p_value)
  coeffs$variable <- row.names(coeffs)
  coeffs$variable[coeffs$variable == SWAN_item] <- "SWAN_item"
  coeffs$SWAN_item <- SWAN_item
  
  coeffs$CI <- paste0("(", coeffs$Estimate - 2*coeffs$Std_Error, ", ", coeffs$Estimate + 2*coeffs$Std_Error, ")")
  row.names(coeffs) <- NULL
  coeffs <- coeffs[, names(coeffs_all)]
  coeffs_all <- rbind(coeffs_all, coeffs)
  
  stats <- data.frame(SWAN_item = SWAN_item,
                      f_statistic = summary(model)$fstatistic['value'], 
                      df_obs = summary(model)$df[2],
                      df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                      r_sq_adj = summary(model)$adj.r.squared,
                      stringsAsFactors = FALSE)
  stats_all <- rbind(stats_all, stats)
}
coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "SWAN_item",
                      direction = "wide")
row.names(stats_all) <- NULL
write.csv(coeffs_all, 
          paste0("Results/Regression/Categorical_SWANIN_item/", iq_measure, 
                 ifelse(iq_measure == "", "", "_"), "coeffs_single_item.csv"), 
          row.names = FALSE)
write.csv(stats_all, 
          paste0("Results/Regression/Categorical_SWANIN_item/", iq_measure, 
                 ifelse(iq_measure == "", "", "_"), "stats_single_item.csv"), 
          row.names = FALSE)

### Correlations between PC1 and Card Sort and Flanker
plot_data <- merge(PSTasks_demos, NIH_Scores[, c("EID", "NIH7_Card", "NIH7_Flanker")], by = "EID", all.x = TRUE)
plot_data <- plot_data[, c("EID", "PC1", "NIH7_Card", "NIH7_Flanker")]
plot_data <- na.omit(plot_data)
ggplot(plot_data, aes(x = PC1, y = NIH7_Card)) +
  geom_point() +
  annotate("text", -4, 175, label = paste0("r = ", signif(cor(plot_data$PC1, plot_data$NIH7_Card, method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_NIHCard.png")

ggplot(plot_data, aes(x = PC1, y = NIH7_Flanker)) +
  geom_point() +
  annotate("text", -4, 175, label = paste0("r = ", signif(cor(plot_data$PC1, plot_data$NIH7_Flanker, method = "pearson"), 3)))
ggsave("Plots/Correlations/PC1_NIHFlanker.png")


### Categorical Regression with NIH Tasks
Dx_Cat_Regression <- Consensus_Dx[, c("EID", "ADHD_I", "ADHD_C", "ASD", "GAD", "Learning_Reading", "Learning_Math", "Anx_Soc", "Anx_All", 
                                      "Depression", "ASD_ADHD_I", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")]
Dx_Cat_Regression <- Reduce(Merge, list(Dx_Cat_Regression, PSTasks, 
                                        Basic_Demos, Barratt[, c("EID", "Barratt_Total")],
                                        WISC[, c("EID", "WISC_GAI", "WISC_FSIQ")], Pegboard,
                                        NIH_Scores[, c("EID", "NIH7_Card", "NIH7_Flanker")]))
Dx_Cat_Regression <- na.omit(Dx_Cat_Regression)
Dx_Cat_Regression$Study_Site <- factor(Dx_Cat_Regression$Study_Site)

# Diagnoses
model <- lm(PC1 ~ WISC_GAI, data = Dx_Cat_Regression)
Dx_Cat_Regression$PC1_Ctrl_GAI <- resid(model)

model <- lm(PC1 ~ WISC_FSIQ, data = Dx_Cat_Regression)
Dx_Cat_Regression$PC1_Ctrl_FSIQ <- resid(model)

ps_measures <- c("PC1", "PC2", "RC1", "RC2", "EEG_Standard", "WISC_SS_Standard",
                 "WISC_Coding_Standard", "NIH7_Pattern")
iq_measures <- c("WISC_FSIQ", "WISC_GAI", "")


for (iq_measure in iq_measures) {
  coeffs_all <- data.frame(variable = character(),	ps_measure = character(),
                           iq_measure = character(),	Estimate = numeric(),	
                           CI = character(),
                           Std_Error = numeric(),	t_value = numeric(),
                           p_value = numeric(),	sig = numeric(), 
                           stringsAsFactors = FALSE)
  stats_all <- data.frame(ps_measure = character(), iq_measure = character(),
                          f_statistic = numeric(), df_reg = numeric(),
                          df_error = numeric(), r_sq = numeric(),
                          r_sq_adj = numeric(),
                          stringsAsFactors = FALSE)
  for (ps_measure in ps_measures) {
    model_eq <- ""
    if (iq_measure == "") {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d + NIH7_Card + NIH7_Flanker, data = Dx_Cat_Regression)")
    } else {
      model_eq <- paste0("lm(", ps_measure, " ~ ADHD_I + ADHD_C + ASD + Learning_Math + Learning_Reading + Anx_All + Depression + Sex + Age + Barratt_Total + Study_Site + peg_z_d + NIH7_Card + NIH7_Flanker, ", iq_measure, ", data = Dx_Cat_Regression)") 
    }
    model <- eval(parse(text = model_eq))
    coeffs <- data.frame(signif(summary(model)$coefficients, 4))
    names(coeffs) <- c("Estimate", "Std_Error", "t_value", "p_value")
    coeffs$sig <- sig_code_v(coeffs$p_value)
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- NULL
    coeffs$ps_measure <- ps_measure
    coeffs$iq_measure <- iq_measure
    coeffs$CI <- paste0("(", coeffs$Estimate - 2*coeffs$Std_Error, ", ", coeffs$Estimate + 2*coeffs$Std_Error, ")")
    coeffs <- coeffs[, names(coeffs_all)]
    coeffs_all <- rbind(coeffs_all, coeffs)
    
    stats <- data.frame(ps_measure = ps_measure, iq_measure = iq_measure,
                        f_statistic = summary(model)$fstatistic['value'], 
                        df_obs = summary(model)$df[2],
                        df_pred = summary(model)$df[1] - 1, r_sq = summary(model)$r.squared,
                        r_sq_adj = summary(model)$adj.r.squared,
                        stringsAsFactors = FALSE)
    stats_all <- rbind(stats_all, stats)
    
  }
  coeffs_all$iq_measure <- NULL
  stats_all$iq_measure <- NULL
  coeffs_all <- reshape(coeffs_all, idvar = "variable", timevar = "ps_measure",
                        direction = "wide")
  write.csv(coeffs_all, 
            paste0("Results/Regression/Categorical_NIH/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "coeffs.csv"), 
            row.names = FALSE)
  write.csv(stats_all, 
            paste0("Results/Regression/Categorical_NIH/", iq_measure, 
                   ifelse(iq_measure == "", "", "_"), "stats.csv"), 
            row.names = FALSE)
}

### Correlation between PC1 and Study Site
cor.test(PSTasks_demos$PC1, PSTasks_demos$Study_Site, method = "pearson")
