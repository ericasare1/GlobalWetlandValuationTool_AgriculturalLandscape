
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

#1. Correlation matrix
df_cor_prov <- df_prov_acre %>% 
  dplyr::select(lnacre, lnyear, lnpop, lnagprod, amphibians, birds, peer_review,
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

