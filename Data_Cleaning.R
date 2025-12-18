#Code by Philip Weber
#Set Seed
set.seed(123)

#Load Data
library(readr)
df <- read_csv("data_raw/smoking_driking_dataset_Ver01.csv")
head(df)

#check type of variables
class(df$sex)
class(df$weight)
class(df$age)
class(df$SMK_stat_type_cd) #should be categorical
class(df$LDL_chole)
class(df$HDL_chole)
class(df$SBP)
class(df$DRK_YN)
str(df$DRK_YN)

 #convert smoking status from numerical to factor variable
df$SMK_stat_type_cd <- as.factor(df$SMK_stat_type_cd)
#check
str(df$SMK_stat_type_cd)

#Rename smoking status
library(dplyr)
df <- df %>%
  rename(smoking_status=SMK_stat_type_cd)
str(df$smoking_status)

#New Variable BMI
df_new <- df %>%
  select(sex, weight, height, age, smoking_status, LDL_chole, HDL_chole, SBP, DRK_YN) %>%
  mutate(BMI=(weight/(height)**2)*10000)

head(df_new)

#change levels-name of smoking status
df_new <- df_new %>%
  mutate(smoking_status = recode(smoking_status, "1"="never smoked",
         "2"="former smoker",
         "3"="current smoker"))

str(df_new$smoking_status)

#check for missing values
anyNA(df_new)
levels(df_new$smoking_status)
unique(df_new$sex)
unique(df_new$weight)
unique(df_new$height)
unique(df_new$age)
unique(df_new$DRK_YN)
unique(df_new$LDL_chole)
unique(df_new$HDL_chole)
unique(df_new$SBP)
#there are no missing values in this data frame

#write csv, save clean data for further analysis
write_csv(df_new, "data_clean/Smoking_Health_clean.csv")