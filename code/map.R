#Load appropriate libraries
pacman::p_load(sf, tmaptools, tmap, tidyverse)

#loading shapefile
sf_shp <- sf::st_read("Data/RawData/Canada_Land_Parcels/Canada_Land_Parcels.shp") 
sf_shp %>% View()

#making sure there are no errors with shapefile geometry
sf_shp = st_make_valid(sf_shp)  #Correcting for bad geometries if any
check = st_is_valid(sf_shp)     # Checking if the above step worked
check

#Get centroid geometry of shapefile features (point shapefile)
sf_shp_centroid <- st_centroid(sf_shp) 
sf_shp_centroid %>% View()
# Save the point shapefile (centroids) and extract lat and long information
st_write(sf_shp_centroid, "Data/DerivedData/sf_shp_centroid.csv",  layer_options = "GEOMETRY=AS_XY")
  
sf_shp_centroid1 <- read_csv("Data/DerivedData/sf_shp_centroid.csv") 

#select only lat (X), Long (Y) and join id: Legal_en (section-township-range-meridian)
sf_shp_centroid1 <- sf_shp_centroid1 %>%
  select(X, Y, SECT, LEGAL_EN)

sf_shp_centroid1 %>% View()

#<<<<<<<<<<loading farmland data
farmlandData <- read_csv("Data/DerivedData/df_farm.csv")
nrow(farmlandData)

#Separate legal land description into separate columns
farmlandData <- farmlandData %>%
  separate(legal, into = c('QS', "SEC", 'PTWP', 'PRGE', "PMER" ),  sep = "\\-" , remove = F) 

farmlandData %>% View()
#Observations: Separation worked well except LLD starting with Quarter-sections "RL" eg: RL 07-45-27-W2
#             and "PRL" eg: PRL 05-45-05-W3. In both cases the section 
#             (figures after the letters did not separate)
# Therefore: The section column for the above cases contained township values
#            The township contained range values
#           The range contained the meridian values
#           The meridian contained NA

#Solving the above problem
farmlandData <- farmlandData %>% separate(QS, into = c("QS1", "SEC1"), remove = F) 

farmlandData <- farmlandData %>%
  mutate(TWP = ifelse(QS1 == "RL" | QS1 == "PRL", SEC, PTWP),
         RGE = ifelse(QS1 == "RL" | QS1 == "PRL", PTWP, PRGE),
         MER = ifelse(QS1 == "RL" | QS1 == "PRL", PRGE, PMER),
         SECT = ifelse(QS1 == "RL" | QS1 == "PRL", SEC1, SEC)) %>%
  unite_("LEGAL_EN", c("SECT","TWP", "RGE", "MER"), sep = "-", remove = F) %>%
  relocate(SECT, TWP, RGE, MER, LEGAL_EN, legal, .after = PMER)
  
#Comparing the legal and computed legal (LEGAL_EN) I am satisfied and can proceed with the inner join
farmlandData %>% filter(QS1 == "RL" | QS1 == "PRL") %>% View()
farmlandData  %>% View()


# Join
farmlandData_leftjoin <- left_join(farmlandData, sf_shp_centroid1, by = "LEGAL_EN") %>%
  relocate(X, Y, SECT.y, .after = legal)

nrow(farmlandData) 
nrow(farmlandData_leftjoin)

#Do we have duplicates in the legal land descriptions
farmlandData_leftjoin1 %>% 
  group_by(legal) %>% 
  filter(n()>1) %>% 
  summarize(n=n()) %>%
  View()

#maintain only distinct legal land descriptions
farmlandData_leftjoin1 <- farmlandData_leftjoin %>% distinct(legal, .keep_all = TRUE) %>% 
  arrange(SEC, TWP, RGE, MER)

# Find unique values of quarter section
QS_uniqueLabels <- data.frame(QS_id = unique(farmlandData_leftjoin1$QS1))

#finding right scale to do conversion: Online converter: https://legallandconverter.com/cgi-bin/shopats201703.cgi
#NW-01-01-01-W3    Y: 49.010588  X: 106.022142
#NE-01-01-01-W3    Y: 49.010561  X: 106.011001
#From our cal: centroid of 01-01-01-W3: Y: 49.00696   X: 106.0166

scale_x = abs(106.0166 - 106.022142)
scale_y = abs(49.00696 - 49.010588)

long = 106.022142 
lat = 49.010588

# adjusting the values of lat and long based on quartersection labels: Am assuming long = X, Lat = Y
farmlandData_leftjoin1 <- farmlandData_leftjoin1 %>%
  dplyr::mutate(
    long = X,
    long = ifelse(QS1 == "NE", X + scale_x, long),
    long = ifelse(QS1 == "PNE", X + scale_x, long),
    long = ifelse(QS1 == "NW", X + scale_x, long),
    long = ifelse(QS1 == "SE", X - scale_x, long),
    long = ifelse(QS1 == "PSE", X - scale_x, long),
    long = ifelse(QS1 == "SW", X - scale_x, long),
    long = ifelse(QS1 == "PSW", X - scale_x, long),
    
    lat = Y,
    lat = ifelse(QS1 == "NE", Y + scale_y, lat),
    lat = ifelse(QS1 == "PNE", Y + scale_y, lat),
    lat = ifelse(QS1 == "NW", Y - scale_y, lat), 
    lat = ifelse(QS1 == "SE", Y + scale_y, lat),
    lat = ifelse(QS1 == "PSE", Y + scale_y, lat),
    lat = ifelse(QS1 == "SW", Y - scale_y, lat),  
    lat = ifelse(QS1 == "PSW", Y - scale_y, lat)
    ) %>%
  relocate(lat, long, .after = Y) %>%
  dplyr::select(legal, lat, long)

farmlandData_leftjoin1 %>% View()

write_csv(farmlandData_leftjoin1, "Data/DerivedData/LLD_Lat_long.csv")
