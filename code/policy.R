library(tidyverse)
lm_full_prov_restln
write.csv(df_prov_acre, "data/prov_policy.csv")
write.csv(df_regul_acre, "data/reg_policy.csv")

lm_full_reg_rest
df_regul

prov_data <- read.csv("data/prov_policy.csv")
reg_data <- read.csv("data/reg_policy.csv")

reg_data %>% View()
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
