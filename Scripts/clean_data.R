# clean_data.R
# clean data for processing speed project
# Author: Bonhwang Koo
# Last updated 3/3/2019
# Eliza- To score EEG data, run lines 8, 11, 17-518


require(dplyr)

##importing/cleaning EEG WISC-IV data:
setwd("R:/Data Feb 2019/Data")

# Import behavioral data
# for Mac
#setwd("/Volumes/data/Research/Healthy Brain Network/AACAP/2017/Proc Speed/Data Feb 2019/Data")

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv, stringsAsFactors = FALSE), envir = .GlobalEnv)

Merge <- function(x, y){
  df <- merge(x, y, by= "EID", all.x= TRUE, all.y= TRUE)
  return(df)
}


# Calculate EEG WISC Scores
# Set directory to folder with WISC SS files
setwd("R:/Data Feb 2019/Data/EEG WISC")

temp = list.files(pattern="*.csv")
ids = unlist(lapply(strsplit(temp, split = '_'), '[', 1)) # Shorten strings in temp
n = length(ids)

key = c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1,
        1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
        1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1,
        0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1,
        1, 0, 1, 0, 0, 0)

EEG_behav_data <- data.frame(EID = character(n), Ones = numeric(n),
                             zeroes = numeric(n), EEG_Raw_Score = numeric(n),
                             stringsAsFactors = FALSE)
names(EEG_behav_data) <- c("EID","Ones", "Zeroes", "EEG_Raw_Score")
count = 1

#for loop: for every file in "files", take the first 12 characters of the file 
#and make it a row in column 1. count all the 0's in the file and make that number 
#a row in column 2. count all the 1's in the file and make that number a row in column 3
for (file in temp) {
  id <- strsplit(file, split = '_')[[1]][1]
  data <- read.csv(file)
  
  resp = c(data$activated_resp1[1:15], data$activated_resp2[1:15],
           data$activated_resp3[1:15], data$activated_resp4[1:15])
  if (length(resp) != length(key)) {
    print(file)
  }
  EEG_behav_data$EID[count] = id
  EEG_behav_data$Ones[count] = sum(resp == key, na.rm = TRUE)
  EEG_behav_data$Zeroes[count] = sum(resp != key & resp != 2, na.rm = TRUE)
  EEG_behav_data$EEG_Raw_Score[count] = EEG_behav_data$Ones[count] - EEG_behav_data$Zeroes[count]
  count = count + 1
}

# Convert EEG WISC Scores to Standard Scores
EEG_behav_data <- EEG_behav_data[EEG_behav_data$EID %in% Basic_Demos$EID, ]
EEG_behav_data <- Merge(EEG_behav_data, Basic_Demos[c("EID", "Age")])
EEG_behav_data$Age_years = floor(EEG_behav_data$Age)
EEG_behav_data$Age_months = (EEG_behav_data$Age %% 1) * 12
EEG_behav_data$EEG_Standard = case_when(
  (EEG_behav_data$Age_years == 8 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:1) ~ 55,
              EEG_behav_data$EEG_Raw_Score == 2 ~ 60,
              EEG_behav_data$EEG_Raw_Score == 3 ~ 65,
              EEG_behav_data$EEG_Raw_Score == 4 ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(5:6) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(7:8) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(9:10) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(11:12) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(13:14) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(15:16) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(17:18) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(19:20) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(29:31) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(32:33) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(34:60) ~ 145
  ) , ##8:4- 8:7
  (EEG_behav_data$Age_years == 8 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:1) ~ 55,
              EEG_behav_data$EEG_Raw_Score == 2 ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(3:4) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(5:6) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(7:8) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(9:10) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(11:12) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(13:14) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(15:16) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(17:18) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(19:20) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(29:31) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(32:33) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(36:60) ~ 145
  ), ##8:8-8:11
  (EEG_behav_data$Age_years == 8 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:1) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(2:3) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(4:5) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(6:7) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(31:32) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(33:34) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(35:36) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(37:60) ~ 145
  ), ##9:0-9:3
  (EEG_behav_data$Age_years == 9 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:2) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(3:4) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(5:6) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(7:8) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(9:10) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(11:12) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(13:14) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(15:16) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(17:18) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(19:20) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(29:31) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(32:33) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(36:38) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(39:60) ~ 145
  ) , ##9:4- 9:7
  (EEG_behav_data$Age_years == 9 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:3) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(4:5) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(6:7) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(31:32) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(33:34) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(35:36) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(37:38) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(39:60) ~ 145
  ) , ##9:8-9:11
  (EEG_behav_data$Age_years == 9 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:3) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(4:5) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(6:7) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(40:60) ~ 145
  ) , ##10:0-10:3
  (EEG_behav_data$Age_years == 10 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:5) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(6:7) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(31:32) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(33:34) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(35:36) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(37:38) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(39:41) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(42:60) ~ 145
  ) , ##10:4-10:7
  (EEG_behav_data$Age_years == 10 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:5) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(6:7) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(42:60) ~ 145
  ) , ##10:8-10:11
  (EEG_behav_data$Age_years == 10 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:5) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(6:8) ~ 60,
              EEG_behav_data$EEG_Raw_Score == 9 ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(18:20) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(42:60) ~ 145
  ) , ##11:0-11:3
  (EEG_behav_data$Age_years == 11 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:7) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(8:9) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(22:23) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(24:25) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(44:60) ~ 145
  ) , ##11:4-11:7
  (EEG_behav_data$Age_years == 11 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:8) ~ 55,
              EEG_behav_data$EEG_Raw_Score == 9 ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(18:20) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(44:60) ~ 145
  ) , ##11:8-11:11
  (EEG_behav_data$Age_years == 11 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:8) ~ 55,
              EEG_behav_data$EEG_Raw_Score == 9 ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(18:20) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(44:60) ~ 145
  ) , ##12:0-12:7
  (EEG_behav_data$Age_years == 12 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:9) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(22:24) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(46:60) ~ 145
  ) , ##12:8-12:11
  (EEG_behav_data$Age_years == 12 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:9) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(10:11) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(20:22) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(27:29) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(30:31) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(32:33) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(46:60) ~ 145
  ) , ##13:0-13:3
  (EEG_behav_data$Age_years == 13 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:11) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(22:24) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(46:47) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(48:60) ~ 145
  ) , ##13:4-13:11
  (EEG_behav_data$Age_years == 13 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:11) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(12:13) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(20:22) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(23:24) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(27:29) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(30:31) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(32:33) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(46:47) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(48:60) ~ 145
  ) , ##14:0-14:7
  (EEG_behav_data$Age_years == 14 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 8))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:13) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(22:24) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(31:32) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(33:35) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(46:47) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(48:49) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(50:60) ~ 145
  ) , ##14:8:14:11
  (EEG_behav_data$Age_years == 14 & (EEG_behav_data$Age_months >= 8 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:13) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(14:15) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(22:24) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(44:46) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(47:48) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(49:50) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(51:60) ~ 145
  )  , ##15:0-15:3
  (EEG_behav_data$Age_years == 15 & (EEG_behav_data$Age_months >= 0 & EEG_behav_data$Age_months < 4))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:14) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(15:16) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(17:18) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(19:20) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(23:25) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(31:32) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(33:35) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(46:47) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(48:49) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(50:52) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(53:60) ~ 145
  ) , ##15:4- 15:11
  (EEG_behav_data$Age_years == 15 & (EEG_behav_data$Age_months >= 4 & EEG_behav_data$Age_months < 12))
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:14) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(15:16) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(17:18) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(19:20) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(21:22) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(23:25) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(26:27) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(28:30) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(36:37) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(38:39) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(40:41) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(42:43) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(44:46) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(47:48) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(49:50) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(51:52) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(53:60) ~ 145
  ) , ##16:0-16:11
  (EEG_behav_data$Age_years == 16)
  ~ case_when(EEG_behav_data$EEG_Raw_Score %in% c(0:15) ~ 55,
              EEG_behav_data$EEG_Raw_Score %in% c(16:17) ~ 60,
              EEG_behav_data$EEG_Raw_Score %in% c(18:19) ~ 65,
              EEG_behav_data$EEG_Raw_Score %in% c(20:21) ~ 70,
              EEG_behav_data$EEG_Raw_Score %in% c(22:24) ~ 75,
              EEG_behav_data$EEG_Raw_Score %in% c(25:26) ~ 80,
              EEG_behav_data$EEG_Raw_Score %in% c(27:28) ~ 85,
              EEG_behav_data$EEG_Raw_Score %in% c(29:30) ~ 90,
              EEG_behav_data$EEG_Raw_Score %in% c(31:33) ~ 95,
              EEG_behav_data$EEG_Raw_Score %in% c(34:35) ~ 100,
              EEG_behav_data$EEG_Raw_Score %in% c(36:38) ~ 105,
              EEG_behav_data$EEG_Raw_Score %in% c(39:40) ~ 110,
              EEG_behav_data$EEG_Raw_Score %in% c(41:43) ~ 115,
              EEG_behav_data$EEG_Raw_Score %in% c(44:45) ~ 120,
              EEG_behav_data$EEG_Raw_Score %in% c(46:47) ~ 125,
              EEG_behav_data$EEG_Raw_Score %in% c(48:49) ~ 130,
              EEG_behav_data$EEG_Raw_Score %in% c(50:51) ~ 135,
              EEG_behav_data$EEG_Raw_Score %in% c(52:53) ~ 140,
              EEG_behav_data$EEG_Raw_Score %in% c(54:60) ~ 145
  )  
)

EEG_behav_data <- EEG_behav_data[!is.na(EEG_behav_data$EEG_Standard),]
EEG_behav_data <- EEG_behav_data[, c("EID", "EEG_Raw_Score", "EEG_Standard")]
write.csv(EEG_behav_data, "../../Data Cleaned/EEG_behav_data.csv", row.names = FALSE)

## WISC - get standard scores for SS and coding

for (subscale in c("WISC_BD", "WISC_Similarities",
                   "WISC_MR", "WISC_DS", "WISC_Coding",
                   "WISC_Vocab", "WISC_FW", "WISC_VP",
                   "WISC_PS", "WISC_SS")) {
  WISC[, paste0(subscale, "_Standard")] = case_when(
    WISC[, paste0(subscale, "_Scaled")] == 1 ~ 55,
    WISC[, paste0(subscale, "_Scaled")] == 2 ~ 60,
    WISC[, paste0(subscale, "_Scaled")] == 3 ~ 65,
    WISC[, paste0(subscale, "_Scaled")] == 4 ~ 70,
    WISC[, paste0(subscale, "_Scaled")] == 5 ~ 75,
    WISC[, paste0(subscale, "_Scaled")] == 6 ~ 80,
    WISC[, paste0(subscale, "_Scaled")] == 7 ~ 85,
    WISC[, paste0(subscale, "_Scaled")] == 8 ~ 90,
    WISC[, paste0(subscale, "_Scaled")] == 9 ~ 95,
    WISC[, paste0(subscale, "_Scaled")] == 10 ~ 100,
    WISC[, paste0(subscale, "_Scaled")] == 11 ~ 105,
    WISC[, paste0(subscale, "_Scaled")] == 12 ~ 110,
    WISC[, paste0(subscale, "_Scaled")] == 13 ~ 115,
    WISC[, paste0(subscale, "_Scaled")] == 14 ~ 120,
    WISC[, paste0(subscale, "_Scaled")] == 15 ~ 125,
    WISC[, paste0(subscale, "_Scaled")] == 16 ~ 130,
    WISC[, paste0(subscale, "_Scaled")] == 17 ~ 135,
    WISC[, paste0(subscale, "_Scaled")] == 18 ~ 140,
    WISC[, paste0(subscale, "_Scaled")] == 19 ~ 145
  )  
}


setwd("R:/Data Feb 2019")
WISC_GAI_Conversion <- read.csv("Scripts/WISC_GAI_Conversion.csv", stringsAsFactors = FALSE)
names(WISC_GAI_Conversion)[1] <- "GAI_Sum_Scaled_Scores"
WISC$WISC_GAI_Sum <- rowSums(WISC[, c("WISC_BD_Scaled", "WISC_Similarities_Scaled", "WISC_MR_Scaled", "WISC_Vocab_Scaled", "WISC_FW_Scaled")])
WISC$WISC_GAI <- WISC_GAI_Conversion$GAI[match(WISC$WISC_GAI_Sum, WISC_GAI_Conversion$GAI_Sum_Scaled_Scores)]

for (asmt in temp) {
  name <- gsub("*.csv$", "", asmt)
  df <- get(name)
  write.csv(df, paste0("Data Cleaned/", asmt), row.names = FALSE)
}


# Look for duplicates
ARI_P <- ARI_P[!duplicated(ARI_P), ]
ARI_P$EID[duplicated(ARI_P$EID)]
write.csv(ARI_P, "Data Cleaned/ARI_P.csv", row.names = FALSE)

ARI_S <- ARI_S[!duplicated(ARI_S), ]
ARI_S$EID[duplicated(ARI_S$EID)]
write.csv(ARI_S, "Data Cleaned/ARI_S.csv", row.names = FALSE)

ASSQ <- ASSQ[!duplicated(ASSQ), ]
ASSQ$EID[duplicated(ASSQ$EID)]
write.csv(ASSQ, "Data Cleaned/ASSQ.csv", row.names = FALSE)

Barratt <- Barratt[!duplicated(Barratt), ]
Barratt$EID[duplicated(Barratt$EID)]
write.csv(Barratt, "Data Cleaned/Barratt.csv", row.names = FALSE)

Basic_Demos <- Basic_Demos[!duplicated(Basic_Demos), ]
Basic_Demos$EID[duplicated(Basic_Demos$EID)]
write.csv(Basic_Demos, "Data Cleaned/Basic_Demos.csv", row.names = FALSE)

CBCL <- CBCL[!duplicated(CBCL), ]
CBCL$EID[duplicated(CBCL$EID)]
write.csv(CBCL, "Data Cleaned/CBCL.csv", row.names = FALSE)

CBCL <- CBCL[!duplicated(CBCL), ]
CBCL$EID[duplicated(CBCL$EID)]
write.csv(CBCL, "Data Cleaned/CBCL.csv", row.names = FALSE)

CELF <- CELF[!duplicated(CELF), ]
CELF$EID[duplicated(CELF$EID)]
write.csv(CELF, "Data Cleaned/CELF.csv", row.names = FALSE)

C3SR <- C3SR[!duplicated(C3SR), ]
C3SR$EID[duplicated(C3SR$EID)]
C3SR <- C3SR[!duplicated(C3SR$EID, fromLast = TRUE), ]
write.csv(C3SR, "Data Cleaned/Conners_SR.csv", row.names = FALSE)

Consensus_Dx <- Consensus_Dx[!duplicated(Consensus_Dx), ]
Consensus_Dx$EID[duplicated(Consensus_Dx$EID)]
write.csv(Consensus_Dx, "Data Cleaned/Consensus_Dx.csv", row.names = FALSE)

Pegboard <- Pegboard[!duplicated(Pegboard), ]
Pegboard$EID[duplicated(Pegboard$EID)]
write.csv(Pegboard, "Data Cleaned/Pegboard.csv", row.names = FALSE)

MFQ_P <- MFQ_P[!duplicated(MFQ_P), ]
MFQ_P$EID[duplicated(MFQ_P$EID)]
write.csv(MFQ_P, "Data Cleaned/MFQ_P.csv", row.names = FALSE)

MFQ_SR <- MFQ_SR[!duplicated(MFQ_SR), ]
MFQ_SR$EID[duplicated(MFQ_SR$EID)]
write.csv(MFQ_SR, "Data Cleaned/MFQ_SR.csv", row.names = FALSE)

NIH_Scores <- NIH_Scores[!duplicated(NIH_Scores), ]
NIH_Scores$EID[duplicated(NIH_Scores$EID)]
write.csv(NIH_Scores, "Data Cleaned/NIH_Scores.csv", row.names = FALSE)

Race_Ethnicity <- Race_Ethnicity[!duplicated(Race_Ethnicity), ]
Race_Ethnicity$EID[duplicated(Race_Ethnicity$EID)]
write.csv(Race_Ethnicity, "Data Cleaned/Race_Ethnicity.csv", row.names = FALSE)

SCARED_P <- SCARED_P[!duplicated(SCARED_P), ]
SCARED_P$EID[duplicated(SCARED_P$EID)]
write.csv(SCARED_P, "Data Cleaned/SCARED_P.csv", row.names = FALSE)

SCARED_SR <- SCARED_SR[!duplicated(SCARED_SR), ]
SCARED_SR$EID[duplicated(SCARED_SR$EID)]
write.csv(SCARED_SR, "Data Cleaned/SCARED_SR.csv", row.names = FALSE)

SCQ <- SCQ[!duplicated(SCQ), ]
SCQ$EID[duplicated(SCQ$EID)]
write.csv(SCQ, "Data Cleaned/SCQ.csv", row.names = FALSE)

SRS <- SRS[!duplicated(SRS), ]
SRS$EID[duplicated(SRS$EID)]
write.csv(SRS, "Data Cleaned/SRS.csv", row.names = FALSE)

SWAN <- SWAN[!duplicated(SWAN), ]
SWAN$EID[duplicated(SWAN$EID)]
write.csv(SWAN, "Data Cleaned/SWAN.csv", row.names = FALSE)

WAIS <- WAIS[!duplicated(WAIS), ]
WAIS$EID[duplicated(WAIS$EID)]
write.csv(WAIS, "Data Cleaned/WAIS.csv", row.names = FALSE)

WASI <- WASI[!duplicated(WASI), ]
WASI$EID[duplicated(WASI$EID)]
write.csv(WASI, "Data Cleaned/WASI.csv", row.names = FALSE)

WIAT <- WIAT[!duplicated(WIAT), ]
WIAT$EID[duplicated(WIAT$EID)]
write.csv(WIAT, "Data Cleaned/WIAT.csv", row.names = FALSE)

WISC <- WISC[!duplicated(WISC), ]
WISC$EID[duplicated(WISC$EID)]
write.csv(WISC, "Data Cleaned/WISC.csv", row.names = FALSE)

YSR <- YSR[!duplicated(YSR), ]
YSR$EID[duplicated(YSR$EID)]
write.csv(YSR, "Data Cleaned/YSR.csv", row.names = FALSE)
