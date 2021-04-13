
rm(list=ls(all=TRUE))

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest)

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
    high_income = ifelse(low_lowmid_uppmid_high ==3,1,0)   
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

colnames(df_prov)
ols <- lm(lnprov ~ lnacre + lnyear + lnpop +
            lnagprod + amphibians + birds + peer_review +
            methodology + wl_policy + ecosystemservicesgoal +
            usepenalties + useincentives  + high_income + low_income + latitude + longitude, data  = df_prov_acre)
ols_reg1 <- lm(lnregul ~ lnacre + lnyear + lnpop +
            lnagprod + amphibians + birds + peer_review +
            methodology + wl_policy + ecosystemservicesgoal +
            usepenalties + useincentives  + high_income + latitude + longitude, data  = df_regul_acre)
ols_reg <- lm(lnregul ~ lnacre + lnyear + lnpop +
            lnagprod + amphibians + birds + peer_review +
            methodology + wl_policy + high_income + latitude + longitude, data  = df_regul_acre)
car::vif(ols_reg) 
summary(ols_reg)
lmtest::bptest(ols_reg)  # Breusch-Pagan test

#a)....Checking for multicollinearity with Variance Inflation Factors
colnames(df_prov)
ols <- rlm(provisioning_US_ha ~ acreage + year + pop_Density +
             pcgdp + agProd + amphibians + birds + peer_review +
             methodology + wl_policy + latitude + longitude, data  = df_prov)
car::vif(ols) 
summary(ols)

# .......................Provisioning Model Estimation....................
#A. OLS
#1. With only the log of the change in baseline and post improvement wetland acres 
#b. Full OLS Model
lm_full_prov <- lm(lnprov ~ lnacre + lnyear + lnpop +
                lnagprod + high_income + low_income +
                peer_review + methodology +
                amphibians +
                wl_policy + ecosystemservicesgoal +
                usepenalties + useincentives + latitude + longitude, data= df_prov_acre)
summary(lm_full)

#OLS Model Diagnostics
#I. Graphical Approach To Check for  fit
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_full, las = 1)
#II. Statistical testing: Test of Homoscedasticity 
lmtest::bptest(lm_full)  # Breusch-Pagan test

#B. Mixed Model: Random Coefficient Model 
#1. Full - Random intercept model (...)
mixed_full_prov <- lmer(lnprov ~ lnacre + lnyear + lnpop +
                     lnagprod + low_income + high_income +
                     peer_review + methodology +
                     amphibians +
                     wl_policy + ecosystemservicesgoal +
                     usepenalties + 
                     latitude + longitude + (1 |studyid), data= df_prov_acre)
#Model Diagnostics
#checking if the random coefficient model is really significant
ranova(mixed_full) 
#Inter Class Correlation
performance::icc(mixed_full) 

#Checking for Multicollinearity
performance::check_collinearity(mixed_full)

#show estimated results
summary(mixed_full)
#c. checking for heteroscedasticity
#c1. Graphical way
plot(fitted(mixed_full), resid(mixed_full, type = "pearson"))# this will create the plot
abline(0,0, col="red")
#c2. Statistical test
performance::check_heteroscedasticity(mixed_full)
#Normality of residuals
qqnorm(resid(mixed_full)) 
qqline(resid(mixed_full), col = "red") # add a perfect fit line

#Model Performance
#a. Root mean squared error
performance::rmse(mixed_full)
#b. R square
performance::r2(mixed_full)

##1. Mixed model with robust standard errors

#bayesian
priors<-c(set_prior("normal(0,10)", class="b"),#prior for the beta's
          set_prior("inv_gamma(.5,.5)", class="sigma"))#prior for the residual std. deviation


#Full
bayesian_mixed_full_prov = brm(
  lnprov ~ lnacre + lnyear + lnpop +
    lnagprod + high_income + low_income +
    peer_review + methodology +
    amphibians +
    wl_policy + ecosystemservicesgoal +
    usepenalties + useincentives + latitude + longitude, 
  data= df_prov_acre, 
  prior = priors,
  cores = 4,
  warmup = 1000, 
  iter = 5000,
  control = list(adapt_delta = 0.98))

#Model Diasgnostics
#model fit
pp_check(bayesian_mixed_full)
#covergence
plot(bayesian_mixed_full)

summary(bayesian_mixed_full)

#Model performance
performance::performance_rmse(bayesian_mixed_full) #compyes 

#............Transfer Error
df_prediction_prov <-  data.frame(predict(bayesian_mixed_full_prov, df_prov_acre, allow_new_levels =TRUE)) 

transfer_error <- df_prediction_prov %>%
  tibble(lnprov = df_prov_acre$lnprov) %>% 
  mutate(TE_MA = as.numeric((abs(lnprov - Estimate)/lnprov)*100),
         TE_BT = as.numeric((abs(lnprov - mean(lnprov, na.rm = TRUE))/lnprov)*100))
median(transfer_error$TE_MA)
median(transfer_error$TE_BT)

# ...................Regulation Model Estimation
#A. OLS
#1. With only the log of the change in baseline and post improvement wetland acres 
#b. Full OLS Model
lm_full_reg <- lm(lnregul ~ lnacre + lnyear + lnpop +
                    lnagprod + amphibians + peer_review +
                    methodology + wl_policy + high_income + latitude + longitude, data=df_regul_acre)
summary(lm_full_reg)
car::vif(lm_full_reg)
#OLS Model Diagnostics
#I. Graphical Approach To Check for  fit
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_full_reg, las = 1)
#II. Statistical testing: Test of Homoscedasticity 
lmtest::bptest(lm_full_reg)  # Breusch-Pagan test
car::vif(lm_full_reg)
#B. Mixed Model: Random Coefficient Model 
#1. Full - Random intercept model (...)
mixed_full_reg <- lmer(lnregul ~ lnacre + lnyear + lnpop +
                     lnagprod + amphibians + peer_review +
                     methodology + wl_policy + high_income + latitude + longitude +
                     (1|studyid), data=df_regul_acre)
#birds eliminated cos of singularity
#Model Diagnostics
#checking if the random coefficient model is really significant
ranova(mixed_full_reg) 
#Inter Class Correlation
performance::icc(mixed_full_reg) 

#Checking for Multicollinearity
performance::check_collinearity(mixed_full_reg)

#show estimated results
summary(mixed_full_reg)
#c. checking for heteroscedasticity
#c1. Graphical way
plot(fitted(mixed_full), resid(mixed_full, type = "pearson"))# this will create the plot
abline(0,0, col="red")
#c2. Statistical test
performance::check_heteroscedasticity(mixed_full_reg)
#Normality of residuals
qqnorm(resid(mixed_full)) 
qqline(resid(mixed_full), col = "red") # add a perfect fit line

#Model Performance
#a. Root mean squared error
performance::rmse(mixed_full)
#b. R square
performance::r2(mixed_full)

##1. Mixed model with robust standard errors

#bayesian
priors<-c(set_prior("normal(0,10)", class="b"),#prior for the beta's
          set_prior("inv_gamma(.5,.5)", class="sigma"))#prior for the residual std. deviation


#Full
bayesian_mixed_full_reg = brm(
  lnregul ~ lnacre + lnyear + lnpop +
    lnagprod + amphibians + peer_review +
    methodology + wl_policy + high_income + latitude + longitude, 
  data= df_regul_acre, 
  prior = priors,
  cores = 4,
  warmup = 1000, 
  iter = 5000,
  control = list(adapt_delta = 0.98))

#Model Diasgnostics
#model fit
pp_check(bayesian_mixed_full_reg)
#covergence
plot(bayesian_mixed_full_reg)

summary(bayesian_mixed_full_reg)

#Model performance
performance::performance_rmse(bayesian_mixed_full) #compyes 

#............Transfer Error
df_prediction_reg <-  data.frame(predict(bayesian_mixed_full_reg, df_regul_acre, allow_new_levels =TRUE)) 
plot(df_prediction_reg$Estimate, df_regul_acre$lnregul)

transfer_error_reg <- df_prediction_reg %>%
  tibble(lnregul = df_regul_acre$lnregul) %>% 
  mutate(TE_MA = as.numeric((abs(lnregul - Estimate)/lnregul)*100),
         TE_BT = as.numeric((abs(lnregul - mean(lnregul))/lnregul)*100))
median(transfer_error_reg$TE_MA, na.rm = TRUE)
median(transfer_error_reg$TE_BT)


#................Summary of Results 
#. Summary Stats


rm(list=ls(all=TRUE))

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, gtsummary)

#Summary Stats for whole data
sum_whole <-  df %>% descr(stats = "common") %>% tb()

# Grouped summary statis
grouped_canada <- df %>% group_by(canada) %>% descr(stats = "common") %>% tb()

#saving data
write_csv(sum_whole, "output/tables/sum_whole1.csv")
write_csv(grouped_canada, "output/tables/grouped_canada1.csv")
#.2..Regression Results

class(mixed_full_prov) <- "lmerMod"
class(mixed_full_reg) <- "lmerMod"


stargazer(lm_full_prov, mixed_full_prov,
          type = "html",
          out="lm_mixed_meta_ag.doc",
          style = "qje",
          single.row = TRUE)

tidy(bayesian_mixed_full_prov)

stargazer(lm_full_reg, mixed_full_reg,
          type = "html",
          out="reg_lm_mixed_meta_ag.doc",
          style = "qje",
          single.row = TRUE)

tidy(bayesian_mixed_full_reg)
 
  
  