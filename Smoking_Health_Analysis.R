#Code by Philip Weber
#Set Seed
set.seed(123)

#Install packages
install.packages("MASS")
install.packages("ordinal")
install.packages("ggeffects")
install.packages("lmtest")
install.packages("reshape")
install.packages("tidyverse")
install.packages("hexbin")
install.packages("car")
install.packages("effectsize")

#Load Data
library(tidyverse)
library(ggplot2)
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
df <- df %>%
  rename(smoking_status=SMK_stat_type_cd)
str(df$smoking_status)

#New Variable BMI
df_new <- df %>%
  dplyr::select(sex, weight, height, age, smoking_status, LDL_chole, HDL_chole, SBP, DRK_YN) %>%
  mutate(BMI=(weight/(height)**2)*10000) #height in m therefore multiplication with 10000

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

#Analysis of Hypotheses
#Viewing all Variables

table(df_new$sex)
hist(df_new$height)
hist(df_new$weight)
hist(df_new$age)
table(df_new$smoking_status)

ggplot(data=df_new, aes(x=smoking_status)) +
  geom_bar() +
  theme_minimal()

table(df_new$DRK_YN)

hist(df_new$BMI)
min(df_new$LDL_chole)
max(df_new$LDL_chole)

#H1
#Pre-Analysis H1: The likelihood of currently smoking decreases with increasing age. covariate: sex
#Library
library(MASS)      # polr()
library(ordinal)   # clm()
library(splines)   # ns()
library(ggeffects) # Plots
library(lmtest)
library(reshape2)

#Linearity of regression
df_new$smoking_status <- ordered(df_new$smoking_status,
  levels = c("never smoked",
    "former smoker",
    "current smoker"))

df_new$age_con <- scale(df_new$age, center = TRUE, scale = TRUE) 
#centered age: subtract mean age from each value

#effects of sex on smoking status
age_smoking <- table(df_new$sex, df_new$smoking_status)
df_age_smoking <- as.data.frame(age_smoking)
head(df_age_smoking)
df_age_smoking <- df_age_smoking %>% rename("sex"="Var1", "smoking_status"="Var2", "n"="Freq") 
df_age_smoking <- df_age_smoking %>% mutate(smoking_percent = n/sum(n)*100)

ggplot(df_age_smoking, aes(x = smoking_status, y=smoking_percent, fill=sex)) +
  geom_col(position = "dodge") +
  labs(x = "Smoking status",
    y = "Percent",
    fill = "Sex") +
  theme_minimal()

#Fit linear ordinal model (standard)
model_lin_h1 <- clm(smoking_status ~ age_con + sex,
                 data = df_new,
                 control = clm.control(gradTol = 0.001))

#Fit model with natural spline for age_con (df = 3)
model_spline_h1 <- clm(smoking_status ~ ns(age_con, df = 3) + sex,
                    data = df_new,
                    control = clm.control(gradTol = 0.001))

#Likelihood ratio test to check linearity
lrtest(model_lin_h1, model_spline_h1)
#Interpretation: p < 0.05 → linearity assumption violated → spline model fits better

#Check proportional-odds assumption
nominal_test(model_spline_h1)
#No significant results - therefore proportional-odds assumption holds. Analysis is feasible. 

#Analysis of H1
#Final model for interpretation
summary(model_spline_h1)
#Interpretation: Coefficients show the effect of age_con (spline) on the log-odds of being in a 
#higher smoking category, accounting for the proportional-odds assumption.
#Age has an effect on smoking_status

#Visual Analysis of H1. Age has an effect on smoking_status but at what age does it change? 
age_seq <- seq(from = min(df_new$age, na.rm = TRUE),
  to   = max(df_new$age, na.rm = TRUE),
  length.out = 100)

newdata<- expand.grid(age = age_seq,
  sex = c("Female", "Male"))

newdata$age_con <- scale(newdata$age,
  center = attr(df_new$age_con, "scaled:center"),
  scale  = attr(df_new$age_con, "scaled:scale"))

pred_probs <- predict(model_spline_h1,
  newdata = newdata,
  type = "prob")

pred_df <- cbind(newdata, as.data.frame(pred_probs))

ggplot(pred_df, aes(x = age, y = `fit.current.smoker`, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Age (years)",
    y = "Predicted probability of current smoking",
    color = "Sex") +
  theme_minimal()
#highest probability of currently smoking around 40. Especially in men. Increase until 40 and then decrease. 

ggplot(pred_df, aes(x = age, y = `fit.former.smoker`, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Age (years)",
    y = "Predicted probability of former smoker",
    color = "Sex") +
  theme_minimal()
#pretty stable number of former smokers. Decline in number of former smokers around 60

ggplot(pred_df, aes(x = age, y = `fit.never.smoked`, color = sex)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Age (years)",
    y = "Predicted probability of non smoker",
    color = "Sex") +
  theme_minimal()
#increase in probability of never smokers after age 40. 


#H2
#Pre-Analysis H2: weight decreases with smoking status. covariates: age, sex
#Library
library(hexbin)
library(car)

#Viewing variables
mean(df_new$weight)
H2_set <- df_new %>% group_by(smoking_status) %>% 
  summarise(mean(weight), sd(weight), mean(age))

#normal distribution
qqnorm(df_new$weight)
qqline(df_new$weight)
#"weight" looks normally distributed within quantiles -2 and 2

#check for linearity
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
plot(density(rstandard(model_h2))) #residuals normally distributed

#multicollinearity
vif(model_h2)
#all values <5, therefore no multicollinearity

#Analysis of H2
summary(model_h2)
library(effectsize)
eta_squared(model_h2, partial = TRUE)
#age and smoking_status both have significant but small effects on weight. They are probobly just
#significant because of the huge sample size. Sex has a significant effect with an effect size of d=0.34

df_smoking_weight <- df_new %>% 
  group_by(smoking_status, sex) %>% mutate(mean_weight = mean(weight))

df_smoking_weight <- df_smoking_weight %>%
  transmute(sex, mean_weight, smoking_status)

ggplot(df_smoking_weight, aes(x=smoking_status, y=mean_weight, fill=sex))+
  geom_col(position = "dodge")+
  labs(title = "Mean Weight by Smoking Status and Sex", 
       x = "Smoking Status", 
       y = "Mean Weight") +
  theme_minimal()


#H3
#Pre-Analysis H3: smoking status predicts higher LDL and lower HDL. covariates: age, sex, alcohol consumption
#Viewing Variables
H3_set <- df_new %>% group_by(smoking_status) %>% 
  summarise(mean(LDL_chole), sd(LDL_chole), mean(HDL_chole), sd(HDL_chole), mean(age), sd(age))
table(df_new$sex)
table(df_new$DRK_YN)

#check for linearity
qqnorm(df_new$LDL_chole) #one extreme outlier detected
df_new %>% filter(LDL_chole > 1000) #values above 1000mg/dl extremely unlikely
max(df_new$LDL_chole)

qqnorm(df_new$HDL_chole) #one extreme outlier detected
df_new %>% filter(HDL_chole > 500) #values above 500mg/dl extremely unlikely

df_new_h3 <- df_new %>% filter(LDL_chole < 1000) %>% filter(HDL_chole < 500)
#One extreme LDL cholesterol value (maximum observed) was excluded for h3

qqnorm(df_new_h3$LDL_chole)
qqline(df_new_h3$LDL_chole) #normal distribution visually acceptable
qqnorm(df_new_h3$HDL_chole)
qqline(df_new_h3$HDL_chole) #normal distribution visually acceptable

ggplot(df_new, aes(age, LDL_chole)) +
  geom_hex(bins = 50) +
  scale_fill_gradient(
    low = "grey",
    high = "blue") +
  geom_smooth(method = "lm", color = "red", linewidth = 1) +
  labs(title = "linearity of Age & LDL-cholesterin")

ggplot(df_new, aes(age, HDL_chole)) +
  geom_hex(bins = 50) +
  scale_fill_gradient(
    low = "grey",
    high = "blue") +
  geom_smooth(method = "lm", color = "red", linewidth = 1) +
  labs(title = "linearity of Age & HDL-cholesterin")

#check for multicollinearity
model_h3_LDL <- lm(LDL_chole ~ age + sex + DRK_YN + smoking_status, data=df_new_h3)
vif(model_h3_LDL)
#VIF <5 therefore no multicollinearity

model_h3_HDL <- lm(HDL_chole ~ age + sex + DRK_YN + smoking_status, data=df_new_h3)
vif(model_h3_HDL)
#VIF <5 therefore no multicollinearity

#check normal distribution of residuals
plot(density(rstandard(model_h3_LDL))) 
plot(density(rstandard(model_h3_HDL)))
#shows normal distribution of residuals

#Given the large sample size (n=991299), formal tests of multivariate normality were not calculated, 
#as even trivial deviations lead to statistical significance. Visual inspection of Q–Q plots and 
#residual distributions indicated no severe violations relevant for the MANCOVA.

#Analysis of H3
model_mancova <- manova(cbind(LDL_chole, HDL_chole) ~ smoking_status + age + sex,data = df_new)
model_mancova
summary(model_mancova, test = "Pillai")

summary(model_h3_HDL)
summary(model_h3_LDL)
p_hdl <- anova(model_h3_HDL)["smoking_status", "Pr(>F)"]
p_ldl <- anova(model_h3_LDL)["smoking_status", "Pr(>F)"]
p.adjust(c(HDL = p_hdl, LDL = p_ldl), method = "holm")
#smoking status has significant effects on both HDL and LDL-cholesterol

#Visualization
ggplot(data=df_new_h3, aes(x=smoking_status, y=LDL_chole, fill=sex))+
  geom_col(position="dodge")+
  labs(title="Effect of smoking status on LDL-cholesterol, separated by sex", 
       x = "Smoking Status", 
       y = "LDL cholesterol")+
  theme_minimal()

ggplot(data=df_new_h3, aes(x=smoking_status, y=HDL_chole, fill=sex))+
  geom_col(position="dodge") +
  labs(title="Effect of smoking status on HDL-cholesterol, separated by sex", 
       x = "Smoking Status", 
       y = "HDL cholesterol")+
  theme_minimal()
#we see a big difference between men and women in how smoking status affects HDL and LDL cholesterol