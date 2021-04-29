
rm(list=ls(all=TRUE))

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,brms, rstanarm, rstan, sjstats, lmerTest, caret)

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

df_acre %>% View()
unique(df_acre$nation)
nrow(df_acre)

str(df) #check the structure of variables

#Creating provisioning and regulating data sets
df_prov_acre <- df_acre %>% filter(id_prov == 1) %>% mutate(mean_lnprov = mean(lnprov))
df_regul_acre <- df_acre %>% filter(id_regul == 1) 

#function to scale variables to be between 0 and 1
normalized <- function(x) {
  (x- min(x))/(max(x) - min(x))
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Variable Diagnostics <<<<<<<<<<<<<<
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
                  high_income, latitude, longitude)

cormat_prov <- round(cor(df_cor_prov),2)
#highlyCorrelated_pro <- findCorrelation(cor(as.matrix(df_cor_prov)), cutoff= 0.7, verbose = FALSE, names = T)

df_corel <- data.frame(cormat_prov) %>% View

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
highlyCorrelated_reg <- findCorrelation(cor(as.matrix(df_cor_reg)), cutoff= 0.6, verbose = FALSE, names = T)

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

#checking if the random coefficient model is really significant
ranova(mixed_full_prov) 


mixed_full_reg <- lmer(lnregul ~ lnacre + lnpop +
                          lnagprod + high_income +
                          methodology +
                          amphibians +
                          wl_policy + ecosystemservicesgoal +
                          usepenalties + 
                          latitude + longitude + (1 |studyid), data= df_regul_acre)
ranova(mixed_full_reg) 

# Mixed Model Results both models
class(mixed_full_prov) <- "lmerMod"
class(mixed_full_reg) <- "lmerMod"

stargazer(mixed_full_prov, mixed_full_reg,
          type = "html",
          out="output/mixed_model_results.doc",
          style = "qje",
          single.row = TRUE)

#<<<<<<<<<<<<<<<<< OLS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<
# .......................Provisioning Model Estimation....................
#A.1. log -log prov

lm_full_prov <- lm(lnprov ~ lnacre + lnpop +
                          lnagprod + high_income +
                          peer_review + methodology + 
                          lnbirds + lnamphibians + 
                          wl_policy + useincentives + usepenalties +
                          ecosystemservicesgoal + latitude + longitude, data= df_prov_acre)

lm_full_prov_rest <- lm(lnprov ~ lnacre + lnpop +
                     lnagprod + high_income +
                     peer_review +
                     lnamphibians + useincentives + 
                     ecosystemservicesgoal + latitude + longitude, data= df_prov_acre)
summary(lm_full_prov)
summary(lm_full_prov_rest)

#heteroscedasticity test
lmtest::bptest(lm_full_prov)  # Breusch-Pagan test
lmtest::bptest(lm_full_prov_rest)  # Breusch-Pagan test
#multicollinearity
car::vif(lm_full_prov)
car::vif(lm_full_prov_rest)


#A.1. log -linear prov
lm_full_prov_ln <- lm(lnprov ~ acreage + pop_Density +
                      agProd + high_income +
                      peer_review + methodology +
                      birds + amphibians +
                      wl_policy + useincentives + usepenalties +
                      ecosystemservicesgoal + latitude + longitude, data= df_prov_acre)

lm_full_prov_restln <- lm(lnprov ~ acreage + pop_Density +
                            agProd + high_income +
                            peer_review + amphibians + birds +
                            ecosystemservicesgoal +
                            latitude + longitude, data= df_prov_acre)
summary(lm_full_prov_ln)
summary(lm_full_prov_restln)

#heteroscedasticity test
lmtest::bptest(lm_full_prov_ln)  # Breusch-Pagan test
lmtest::bptest(lm_full_prov_restln)  # Breusch-Pagan test
#multicollinearity
car::vif(lm_full_prov_ln)
car::vif(lm_full_prov_restln)

#extracting AIC info for both models to examine how they compared to each other regarding model fit
extractAIC(lm_full_prov)
extractAIC(lm_full_prov_rest)
extractAIC(lm_full_prov_ln)
extractAIC(lm_full_prov_restln) #best
summary(lm_full_prov_restln)

summary(lm_full_prov_restln)
car::vif(lm_full_prov_rest)

#model output for provisioning model
stargazer(lm_full_prov, lm_full_prov_rest, lm_full_prov_ln, lm_full_prov_restln, 
          type = "html",
          out="output/prov_model_results.doc",
          style = "qje",
          single.row = TRUE)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<mean value error
set.seed(123) #set random seed for reprocibility of results

#splitting a data set to do 10 fold cv using the cvTools package
fold_cv = function(data,k){
  folds=cvTools::cvFolds(nrow(df),K=k)
  invisible(folds)
}

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV

fold <- df_prov_acre %>% fold_cv(., k=10)
str(fold)
#creating a temp data to store results
temp2 <- df_prov_acre %>% 
  mutate(Fold=rep(0,nrow(df_prov_acre)),
         holdoutpred=rep(0,nrow(df_prov_acre)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)),
         MAE=rep(0,nrow(.)),
         R2=rep(0,nrow(.)),
         AIC=rep(0,nrow(.)),
         BIC=rep(0,nrow(.)))

for(i in 1:10){
  train=temp2[fold$subsets[fold$which != i], ]  #set the first n-1 dataset for training
  test=temp2[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  mod_1 = lm(lnprov ~ acreage + pop_Density +
               agProd + high_income +
               peer_review + amphibians + birds +
               ecosystemservicesgoal +
               latitude + longitude, data = train)
  newpred = predict(mod_1, newdata = test)   #predict with first test data
  true = rep(mean(test$lnprov),nrow(test))   # find the original true dependent var from testdata
  error=(true-newpred) #deviations of true from predicted dependent variable
  
#different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  
#storing results from the cross validation looping
  temp2[fold$subsets[fold$which == i], ]$holdoutpred <- newpred
  temp2[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp2[fold$subsets[fold$which == i], ]$MSE=mse
  temp2[fold$subsets[fold$which == i], ]$MAE=mae
  temp2[fold$subsets[fold$which == i], ]$R2=R2
  temp2[fold$subsets[fold$which == i], ]$AIC=AIC(mod_1)
  temp2[fold$subsets[fold$which == i], ]$BIC=BIC(mod_1)
  temp2[fold$subsets[fold$which == i], ]$Fold=i
}
temp2

temp2 %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp2 %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)

#using 
set.seed(123)
# creating training data as 80% of the dataset
random_sample_prov <- createDataPartition(df_prov_acre$lnprov,
                                         p = 0.8, list = FALSE)

# generating training dataset
# from the random_sample
training_dataset_prov <- df_prov_acre[random_sample_prov, ]

# generating testing dataset
# from rows which are not
# included in random_sample
testing_dataset_prov <- df_prov_acre[-random_sample_prov, ]

model_vs_prov <- lm(lnprov ~ acreage + pop_Density +  #estimate the linear model
                     agProd + high_income +
                     peer_review + 
                     ecosystemservicesgoal +
                     latitude + longitude, 
                   data = training_dataset_prov)

# predicting the target variable
predictions_prov <- predict(model_vs_prov, testing_dataset_prov)
# computing model performance metrics
data.frame( RMSE_prov1 = RMSE(predictions_prov, testing_dataset_prov$mean_lnprov),
            MAE_prov1 = MAE(predictions_prov, testing_dataset_prov$mean_lnprov))

#repeated Cv
set.seed(10000)
cv_model_prov_restln <- train(lnprov ~ acreage + pop_Density +
                                agProd + high_income +
                                peer_review + amphibians + birds +
                                ecosystemservicesgoal +
                                latitude + longitude,
                              data = df_prov_acre,
                              method = "lm",
                              trControl = train_control)
print(cv_model_prov_restln)

#B. <<<<<<<<<<<<<<<<< Regulation Model
df_regul <- df_regul_acre %>%
  dplyr::select(lnregul, acreage, pop_Density, agProd, high_income, birds, peer_review, methodology, 
                  birds, amphibians, wl_policy, useincentives, usepenalties, 
                   ecosystemservicesgoal, latitude, longitude, lnacre, lnpop, lnbirds, lnamphibians,lnagprod) %>%
  mutate(mean_lnreg = mean(lnregul)) %>%
  filter(!is.na(.))

lm_full_reg <- lm(lnregul ~ lnacre + lnpop +
                     lnagprod + high_income +
                     peer_review + methodology + 
                     lnbirds + lnamphibians + 
                     wl_policy + useincentives + usepenalties +
                     ecosystemservicesgoal + latitude + longitude, data= df_regul)
lmtest::bptest(lm_full_reg)  # Breusch-Pagan test
car::vif(lm_full_reg)

lm_full_reg_rest <- lm(lnregul ~ lnacre + lnpop +
                         lnagprod + high_income +
                         methodology + 
                         lnamphibians + 
                         wl_policy + latitude + longitude,
                       data = df_regul)

lmtest::bptest(lm_full_reg_rest)  # Breusch-Pagan test
 car::vif(lm_full_reg_rest)

summary(lm_full_reg)
summary(lm_full_reg_rest)


lm_full_reg_ln <- lm(lnregul ~ acreage + pop_Density +
                        agProd + high_income +
                        peer_review + methodology +
                        birds + amphibians +
                        wl_policy + useincentives + usepenalties +
                        ecosystemservicesgoal + latitude + longitude, data= df_regul)
lmtest::bptest(lm_full_reg_ln)  # Breusch-Pagan test
car::vif(lm_full_reg_ln)

lm_full_reg_restln <- lm(lnregul ~ acreage + pop_Density +
                           agProd + high_income +
                           methodology + 
                           wl_policy + 
                           latitude + longitude,
                         data= df_regul)
lmtest::bptest(lm_full_reg_restln)  # Breusch-Pagan test
car::vif(lm_full_reg_restln)
summary(lm_full_reg_restln) 
summary(lm_full_reg_rest) 

extractAIC(lm_full_reg)
extractAIC(lm_full_reg_rest)
extractAIC(lm_full_reg_ln)  #best
extractAIC(lm_full_reg_restln)

#model output for the regulating 
stargazer(lm_full_reg, lm_full_reg_rest, lm_full_reg_ln, lm_full_reg_restln, 
          type = "html",
          out="output/reg_model_results.doc",
          style = "qje",
          single.row = TRUE)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<mean value error
set.seed(123) #set random seed for reprocibility of results

#splitting a data set to do 10 fold cv using the cvTools package
fold_cv = function(data,k){
  folds=cvTools::cvFolds(nrow(data),K=k)
  invisible(folds)
}

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV

fold_reg <- df_regul %>% fold_cv(., k=10)
str(fold_reg)
#creating a temp data to store results
temp_reg <- df_regul %>% 
  mutate(Fold=rep(0,nrow(df_regul)),
         holdoutpred=rep(0,nrow(df_regul)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train_reg=temp_reg[fold_reg$subsets[fold_reg$which != i], ]  #set the first n-1 dataset for training
  test_reg=temp_reg[fold_reg$subsets[fold_reg$which == i], ]  # set first 1/1oth dataset for test
  mod_reg = lm(lnregul ~ lnacre + lnpop +
                 lnagprod + high_income +
                 methodology + 
                 lnamphibians + 
                 wl_policy + latitude + longitude,
                 data= train_reg)
  newpred_reg = predict(mod_reg, newdata = test_reg) #predict with first test data
  true_reg = test_reg$mean_lnreg #find the original true dependent var from testdata
  error_reg= (true_reg - newpred_reg) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse_reg=sqrt(mean(error_reg^2)) 
  mae_reg=mean(abs(error_reg))
  #storing results from the cross validation looping
  temp_reg[fold_reg$subsets[fold_reg$which == i], ]$holdoutpred_reg <- newpred_reg
  temp_reg[fold_reg$subsets[fold_reg$which == i], ]$RMSE_reg=rmse_reg
  temp_reg[fold_reg$subsets[fold_reg$which == i], ]$MSE_reg=mse_reg
  temp_reg[fold_reg$subsets[fold_reg$which == i], ]$Fold_reg=i
}
temp2

temp2 %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp2 %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)

#using tle valu
set.seed(123)
# creating training data as 80% of the dataset
random_sample_reg <- createDataPartition(df_regul$lnregul,
                                     p = 0.8, list = FALSE)

# generating training dataset
# from the random_sample
training_dataset_reg <- df_regul[random_sample, ]

# generating testing dataset
# from rows which are not
# included in random_sample
testing_dataset_reg <- df_regul[-random_sample, ]

model_vs_reg <- lm(lnregul ~ lnacre + lnpop +
                 lnagprod + high_income +
                 methodology + 
                 lnamphibians + 
                 wl_policy + latitude + longitude, 
               data = training_dataset)

# predicting the target variable
predictions_reg <- predict(model_vs, testing_dataset)
# computing model performance metrics
data.frame( RMSE_reg1 = RMSE(predictions_reg, testing_dataset_reg$mean_lnreg),
            MAE_reg1 = MAE(predictions_reg, testing_dataset_reg$mean_lnreg))

#repeated Cv
set.seed(10000)
cv_model_reg_rest <- train(lnregul ~ lnacre + lnpop +
                                lnagprod + high_income +
                                methodology + 
                               lnamphibians + 
                             wl_policy + latitude + longitude,
                              data = df_regul_acre,
                              method = "lm",
                              trControl = train_control)
print(cv_model_reg_rest)

cv_model_reg_restln <- train(lnregul ~ acreage + pop_Density +
                               agProd + high_income +
                               methodology + amphibians +
                               wl_policy + 
                               latitude + longitude,
                              data = df_regul_acre,
                              method = "lm",
                              trControl = train_control)
print(cv_model_reg_restln)

# Model Results in Table
stargazer(lm_full_prov, lm_full_prov_ll, lm_full_reg, lm_full_reg_ll,
          type = "html",
          out="output/model_results.doc",
          style = "qje",
          single.row = TRUE)

#.......Geo-spatial analysis----Create map of study locations
pacman::p_load(sf, tmaptools, tmap, raster, sp, rgdal)

#Converting provision data to shp file
#require(raster)
df_prov_lat_log <- df_prov_acre %>%
  dplyr::select(latitude, longitude)

points_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
coords <- df_prov_lat_log[c("longitude", "latitude")]

prov_shp <- SpatialPointsDataFrame(
  coords = coords,
  df_prov_lat_log,
  proj4string = points_crs)

utm_prov <- spTransform(prov_shp, CRS("+init=epsg:32616")) #transform to UTM

writeOGR(utm_prov, dsn="data/provision_shp/prov_shp/", layer= "prov.shp", 
         driver="ESRI Shapefile", overwrite_layer=TRUE)

#Converting regulation data to shp file
df_reg_lat_log <- df_regul_acre %>%
  dplyr::select(latitude, longitude)

points_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
coords_reg <- df_reg_lat_log[c("longitude", "latitude")]

reg_shp <- SpatialPointsDataFrame(
  coords = coords_reg,
  df_reg_lat_log,
  proj4string = points_crs)

utm_reg <- spTransform(reg_shp, CRS("+init=epsg:32616")) #transform to UTM

writeOGR(utm_reg, dsn="data/provision_shp/reg_shp/", layer= "reg.shp", 
         driver="ESRI Shapefile", overwrite_layer=TRUE)












