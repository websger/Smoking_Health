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
  dplyr::select(sex, weight, height, age, smoking_status, LDL_chole, HDL_chole, SBP, DRK_YN) %>%
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

#Pre-Analysis
#Viewing all Variables
library(ggplot2)

table(df_new$sex)
hist(df_new$height)
hist(df_new$weight)
hist(df_new$age)
table(df_new$smoking_status)
ggplot(data=df_new, aes(x=smoking_status)) +
  geom_bar()

table(df_new$DRK_YN)

hist(df_new$BMI)
min(df_new$LDL_chole)
max(df_new$LDL_chole)

#Pre-Analysis H1: The likelihood of currently smoking decreases with increasing age. covariate: sex
#Library
library(MASS)      # polr()
library(ordinal)   # clm()
library(splines)   # ns()
library(ggeffects) # Plots
library(lmtest)
library(reshape2)

#Viewing variables
hist(df_new$age)
table(df_new$smoking_status)
table(df_new$sex)

#Linearity of regression
df_new$smoking_status <- ordered(df_new$smoking_status,
  levels = c("never smoked",
    "former smoker",
    "current smoker"))

df_new$age_con <- scale(df_new$age, center = TRUE, scale = TRUE) # centered age: subtract mean age from each value

# Fit linear ordinal model (standard)
model_lin_h1 <- clm(smoking_status ~ age_con + sex,
                 data = df_new,
                 control = clm.control(gradTol = 0.001))

# Fit model with natural spline for age_con (df = 3)
model_spline_h1 <- clm(smoking_status ~ ns(age_con, df = 3) + sex,
                    data = df_new,
                    control = clm.control(gradTol = 0.001))

# Likelihood ratio test to check linearity
lrtest(model_lin_h1, model_spline_h1)
# Interpretation: p < 0.05 → linearity assumption violated → spline model fits better

# Check proportional-odds assumption
nominal_test(model_spline_h1)
# No significant results - therefore proportional-odds assumption holds. Analysis is feasible. 

# Final model for interpretation
summary(model_spline_h1)
# Interpretation: Coefficients show the effect of age_con (spline) on the log-odds of being in a higher smoking category, accounting for the proportional-odds assumption.


#Pre-Analysis H2:
#Library
library(hexbin)
library(car)

#Viewing variables
hist(df_new$weight)
mean(df_new$weight)
H2_set <- df_new %>% group_by(smoking_status) %>% 
  summarise(mean(weight), sd(weight), mean(age))

#normal distribution
qqnorm(df_new$weight)
qqline(df_new$weight)
#weight looks normally distributed within

#cheack for linearity
model_h2 <- lm(weight ~ age + sex + smoking_status, data = df_new)

bin_h2 <- hexbin(model_h2$fitted.values, resid(model_h2), xbins = 50)

plot(bin_h2,
     colramp = colorRampPalette(c("grey","blue"))) #two clouds because of sex separation

#homoscedasticity
df_res <- data.frame(fit = model_h2$fitted.values, resid = resid(model_h2))

ggplot(df_res, aes(x = fit, y = resid)) +
  geom_hex(bins = 50) +
  scale_fill_gradient(low = "grey", high = "blue") +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted (Hexbin)") #two clouds because of sex separation in model_h2
#clouds have no specific shape. homoscedasticity is satisfied - width of cloud is stable

#normality of residuals
qqnorm(resid(model_h2))
qqline(resid(model_h2))

#multicollinearity
vif(model_h2)
#all values <5, therefore no multicollinearity