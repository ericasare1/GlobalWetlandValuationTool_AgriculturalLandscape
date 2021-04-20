
rm(list=ls(all=TRUE))

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest, caret)

# Import data
#-----------------------------------------------
df_acre <- read_csv("data/final_estimation.csv") %>%
  filter(!is.na(acreage)) %>% 
  mutate(
    lnprov = log(provisioning_US_ha +1),
    lnregul = log(regulation_US_ha +1),
    year = year - 1995 - 1,
    lnyear = log(year + 2),
    lnacre = log(acreage + 1),
    lnpop = log(pop_Density +1),
    lnpcgdp = log(pcgdp + 1),
    lnagprod = log(agProd),
    lnamphibians = log(amphibians +1),
    lnbirds = log(birds +1),
    low_income = ifelse(low_lowmid_uppmid_high ==1,1,0),
    high_income = ifelse(low_lowmid_uppmid_high ==3,1,0),
    lat_mod = latitude + 38,
    log_mod = longitude + 128,
    lnlat = log(lat_mod),
    lnlong = log(log_mod)
  )

 

df <- read_csv("data/final_estimation.csv") %>%
  mutate(
    acreage = ifelse(acreage == NA, 0, acreage),
    lnprov = log(provisioning_US_ha +1),
    lnregul = log(regulation_US_ha +1),
    year = year - 1995 - 1,
    lnyear = log(year + 2),
    lnacre = log(acreage + 1),
    lnpop = log(pop_Density +1),
    lnpcgdp = log(pcgdp + 1),
    lnagprod = log(agProd),
    lnamphibians = log(amphibians +1),
    lnbirds = log(birds +1),
    low_income = ifelse(low_lowmid_uppmid_high ==1,1,0),
    high_income = ifelse(low_lowmid_uppmid_high ==3,1,0) 
  )

colSums(is.na(df))

df_prov_acre <- df_acre %>% filter(id_prov == 1)
df_prov <- df %>% filter(id_prov == 1)

df_regul_acre <- df_acre %>% filter(id_regul == 1) 
df_regul <- df %>% filter(id_regul == 1)



str(df) #check the structure of variables
colnames(df)     

#function to scale variables to be between 0 and 1
normalized <- function(x) {
  (x- min(x))/(max(x) - min(x))
}

# Variable Diagnostics
#A. checking the distribution of dependent variable
qqp(df_prov$lnprov, "norm")

#B. Plot of distribution of lnwtp within study clusters:  .
df$id_study <- as.character(df$studyid)     # create a character variable to identify obser. per study     
ggplot(df_prov, aes(x= studyid, y = lnprov)) +
  geom_boxplot() +
  theme_bw(base_size = 14)


#C. checking for outliers
boxplot(df_prov$lnprov) 
boxplot(df_regul$lnregul) 

#1. Correlation matrix Provisioning
df_cor_prov <- df_prov_acre %>% 
  dplyr::select(lnprov, lnacre, lnyear, lnpop, lnagprod, amphibians, birds, peer_review,
                 methodology, wl_policy, ecosystemservicesgoal, usepenalties,
                 useincentives, high_income, latitude, longitude)

cormat_prov <- round(cor(df_cor_prov),2)
head(cormat_prov)
#Reshape above matrix
library(reshape2)
melted_cormat_prov <- melt(cormat_prov)
head(melted_cormat_prov)
#correlation heat map
ggplot(data = melted_cormat_prov, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#1. Correlation matrix regulating
df_cor_reg <- df_regul_acre %>% 
  dplyr::select(lnregul, lnacre, lnyear, lnpop, lnagprod, amphibians, birds, peer_review,
                methodology, wl_policy, ecosystemservicesgoal, usepenalties,
                useincentives, high_income, latitude, longitude)

cormat_reg <- round(cor(df_cor_reg),2)
head(cormat_reg)
#Reshape above matrix
library(reshape2)
melted_cormat_reg <- melt(cormat_reg)
head(melted_cormat_reg)
#correlation heat map
ggplot(data = melted_cormat_reg, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Mixed models

#B. Mixed Model: Random Coefficient Model 
#1. Full - Random intercept model (...)
mixed_full_prov <- lmer(lnprov ~ lnacre + lnpop +
                          lnagprod + high_income +
                          peer_review + methodology +
                          lnamphibians + lnbirds +
                          wl_policy + ecosystemservicesgoal +
                          usepenalties + 
                          latitude + longitude + (1 |studyid), data= df_prov_acre)
ranova(mixed_full_prov) 


mixed_full_reg <- lmer(lnregul ~ lnacre + lnpop +
                          lnagprod + high_income +
                          peer_review + methodology +
                          amphibians +
                          wl_policy + ecosystemservicesgoal +
                          usepenalties + 
                          latitude + longitude + (1 |studyid), data= df_regul_acre)
#Model Diagnostics
#checking if the random coefficient model is really significant
ranova(mixed_full_reg) 

#<<<<<<<<<<<<<<<<< OLS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<
# .......................Provisioning Model Estimation....................
#A.1. log -log prov
lm_full_prov <- lm(lnprov ~ lnacre + lnpop +
                     lnagprod + high_income +
                     peer_review + methodology +
                     lnamphibians + lnbirds +
                     wl_policy + ecosystemservicesgoal +
                     usepenalties + useincentives + latitude + longitude, data= df_prov_acre)

#A.1. log -linear prov
lm_full_prov_ll <- lm(lnprov ~ acreage + pop_Density +
                     agProd + high_income +
                     peer_review + methodology +
                     amphibians + birds +
                     wl_policy + ecosystemservicesgoal +
                     usepenalties + useincentives + latitude + longitude, data= df_prov_acre)
summary(lm_full_prov_ll)
lmtest::bptest(lm_full_prov_ll)  # Breusch-Pagan test
car::vif(lm_full_prov)

#B. Regulation Model
lm_full_reg <- lm(lnregul ~ lnacre  + lnpop +
                     lnagprod + high_income +
                     peer_review + methodology +
                    lnamphibians + lnbirds +
                     wl_policy + 
                    latitude + longitude, data= df_regul_acre)


lm_full_reg_ll <- lm(lnregul ~ acreage + pop_Density +
                   agProd + high_income +
                    peer_review + methodology +
                    amphibians + birds +
                    wl_policy + 
                    latitude + longitude, data= df_regul_acre)

summary(lm_full_reg_ll)
lmtest::bptest(lm_full_reg)  # Breusch-Pagan test
car::vif(lm_full_reg)

#extracting AIC info for both models to examine how they compared to each other regarding model fit
extractAIC(lm_full_prov)
extractAIC(lm_full_prov_ll)
extractAIC(lm_full_reg)
extractAIC(lm_full_reg_ll)

# Model Results in Table
stargazer(lm_full_prov, lm_full_prov_ll, lm_full_reg, lm_full_reg_ll,
          type = "html",
          out="output/model_results.doc",
          style = "qje",
          single.row = TRUE)

#Transfer Errors
df_prediction_prov <- data.frame(fit = predict(lm_full_prov)) 

transfer_error_prov <- df_prediction_prov %>%
  tibble(wtp = exp(df_prov_acre$lnprov)) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100))

median(transfer_error_prov$TE_MA)
median(transfer_error_prov$TE_UnitTransfer)
write_csv(transfer_error_prov, "data/transfer_error_prov.csv")

#reg
df_prediction_reg <- data.frame(fit = predict(lm_full_reg))

transfer_error_reg <- df_prediction_reg %>%
  tibble(wtp = exp(df_regul_acre$lnregul) - 1) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100))


median(transfer_error_reg$TE_MA)
median(transfer_error_prov$TE_UnitTransfer)
write_csv(transfer_error_reg, "data/transfer_error_reg.csv")

#.....Cross validation Methods
#updating r
install.packages("lme4", dependencies = TRUE)
library(lme4)
methods(sigma)
install.packages("pbkrtest", dependencies = TRUE)
library(caret)
# package to compute 
# cross - validation methods
library(caret)

# setting seed to generate a
# reproducible random sampling
set.seed(123)

# define training control which
# generates parameters that further
# control how models are created
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)

# building the model and
# predicting the target varibale
# as per the Naive Bayes classifier
model <- train(lnprov ~ lnacre + lnpop +
                 lnagprod + high_income +
                 peer_review + methodology +
                 lnamphibians + lnbirds +
                 wl_policy + ecosystemservicesgoal +
                 usepenalties + useincentives + latitude + longitude,
               data = df_prov_acre,
               method = "lm",
               trControl = train_control)


