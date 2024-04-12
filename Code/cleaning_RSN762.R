#this is to clean up file 762_RSN_FuelLoad_2019, and make it a wide instead of long#

#load packages
library(dplyr)
library(tidyr)

#set working directory
setwd("/Users/wk78/Documents/R/RSN")

#read in the file
RSN_fuel_load <- read.csv("762_RSN_FuelLoad_2019.csv")
RSN_siteinfo <- read.csv("737_RSN_SiteInformation.csv")

#make a unique ID so that site/transect/plot level data can be merged
RSN_fuel_load <- RSN_fuel_load %>%
  unite(ID, Site, Transect, Plot, sep = "_", remove = FALSE)

#removed unwanted columns so that the pivor will work (?)
RSN_fuel_load_short <- select(RSN_fuel_load, ID, FuelType, FuelLoad.kg.m2)

#pivot
RSN_fuel_load_new <- RSN_fuel_load_short %>%
  pivot_wider(names_from = FuelType, values_from = FuelLoad.kg.m2)

#re-add the columns I merged
RSN_fuel_load <- RSN_fuel_load_new %>%
  separate(ID, into = c("Site", "Transect", "Plot"), sep = "_")

#save new clean dataset
write.csv(RSN_fuel_load, file = "762_RSN_FuelLoad_2019_clean.csv")


#check numbers of sites
RSN_fuel_load <- read.csv("762_RSN_FuelLoad_2019_clean.csv")

num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_fuel <- RSN_fuel_load %>% 
  summarise(unique_count = n_distinct(Site))
#n = 28

# Compare unique values between the 606 and 737
values_fuel <- RSN_fuel_load %>% distinct(Site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_fuel$Site)
diff_siteinfo
# 65 unique values 

diff_fuel <- setdiff(values_fuel$Site, values_siteinfo$sitename)
diff_fuel
# no unique value


