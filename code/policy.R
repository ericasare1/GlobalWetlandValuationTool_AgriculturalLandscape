library(tidyverse)

df_prov_acre <- read_csv("data/provision_12_16.csv") %>%
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
    middle_income = ifelse(low_lowmid_uppmid_high ==3,1,0),
        high_income = ifelse(low_lowmid_uppmid_high ==3,1,0),
    lat_mod = latitude + 38,
    log_mod = longitude + 128,
    lnlat = log(lat_mod),
    lnlong = log(log_mod)
  ) %>% dplyr::select(nation, Paper,acreage, pop_Density, agProd, high_income, peer_review, amphibians, birds ,ecosystemservicesgoal ,latitude ,longitude)

view(df_prov_acre)
write_csv(df_prov_acre, "output/prov_pred_metadata.csv")


df_regul_acre <- read_csv("data/regulation_12_16.csv") %>%
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
    middle_income = ifelse(low_lowmid_uppmid_high ==3,1,0),
    high_income = ifelse(low_lowmid_uppmid_high ==3,1,0),
    lat_mod = latitude + 38,
    log_mod = longitude + 128,
    lnlat = log(lat_mod),
    lnlong = log(log_mod)
  ) %>% dplyr::select(nation, Paper, acreage, pop_Density, agProd, high_income, peer_review, methodology, birds, amphibians,
                 wl_policy, useincentives, usepenalties, ecosystemservicesgoal, latitude, longitude)

view(df_regul_acre)
write_csv(df_regul_acre, "output/regul_pred_metadata.csv")


library(merTools)
#https://www.rdocumentation.org/packages/merTools/versions/0.5.2/topics/predictInterval
prov_pred <- data.frame(predict(lm_full_prov_restln, newdata = df_prov_acre, interval="predict", level = 0.95, seed = 235, nsims = 1000)) %>%
  mutate(
         nation = df_prov_acre$nation,
         study = df_prov_acre$Paper,
         Area = df_prov_acre$acreage,
         lat = df_prov_acre$latitude,
         long = df_prov_acre$longitude,
         wtp = exp(fit),
         wtp_lwer = exp(lwr),
         wtp_upr = exp(upr)
         
  )

prov_pred %>% View()
write_csv(prov_pred, "output/resultspolicy_prov.csv")

#regulating
#https://www.rdocumentation.org/packages/merTools/versions/0.5.2/topics/predictInterval
reg_pred <- data.frame(predict(lm_full_reg_rest, newdata = df_regul_acre, interval="predict", level = 0.95, seed = 235, nsims = 1000)) %>%
  mutate(
    nation = df_regul_acre$nation,
    study = df_regul_acre$Paper,
    Area = df_regul_acre$acreage,
    lat = df_regul_acre$latitude,
    long = df_regul_acre$longitude,
    wtp = exp(fit),
    wtp_lwer = exp(lwr),
    wtp_upr = exp(upr)
  )

reg_pred %>% View()
write_csv(reg_pred, "output/resultspolicy_reg.csv")
