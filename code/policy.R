library(tidyverse)

df_acre <- read_csv("data/policy_data.csv") %>%
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

df_prov_acre <- df_acre %>% filter(id_prov == 1) %>% mutate(mean_lnprov = mean(lnprov), value =  exp(lnprov))
df_regul_acre <- df_acre %>% filter(id_regul == 1) %>% mutate(value =  exp(lnregul))

df_regul <- df_regul_acre %>%
  dplyr::select(lnregul, acreage, pop_Density, agProd, high_income, birds, peer_review, methodology, 
                birds, amphibians, wl_policy, useincentives, usepenalties, 
                ecosystemservicesgoal, latitude, longitude, lnacre, lnpop, lnbirds, lnamphibians,lnagprod) %>%
  mutate(mean_lnreg = mean(lnregul)) %>%
  filter(!is.na(.))

library(merTools)
#https://www.rdocumentation.org/packages/merTools/versions/0.5.2/topics/predictInterval
prov_pred <- data.frame(predict(lm_full_prov_restln, newdata = prov_data, interval="predict", level = 0.95, seed = 235, nsims = 1000)) %>%
  mutate(acre = exp(prov_data$lnacre),
         wtp = exp(fit)/acre,
         wtp_lwer = exp(lwr)/acre,
         wtp_upr = exp(upr)/acre,
         id = prov_data$id,
         nation = prov_data$nation,
         study = prov_data$Paper
  )

prov_pred %>% View()
write_csv(prov_pred, "output/policy_prov.csv")

#regulating
#https://www.rdocumentation.org/packages/merTools/versions/0.5.2/topics/predictInterval
reg_pred <- data.frame(predict(lm_full_reg_rest, newdata = reg_data, interval="predict", level = 0.95, seed = 235, nsims = 1000)) %>%
  mutate(acre = exp(reg_data$lnacre),
         wtp = exp(fit)/acre,
         wtp_lwer = exp(lwr)/acre,
         wtp_upr = exp(upr)/acre,
         id = reg_data$id,
         nation = reg_data$nation,
         study = reg_data$Paper
  )

reg_pred %>% View()
write_csv(reg_pred, "output/policy_reg.csv")
