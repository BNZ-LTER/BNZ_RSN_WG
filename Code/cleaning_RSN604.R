## cleaning up RSN datafiles ##

#this is for file 604_RSN_Releve#

#load packages
library(dplyr)
library(tidyr)

#set working directory
setwd("/Users/wk78/Documents/R/RSN")

#read in the file
RSN_siteinfo <- read.csv("737_RSN_SiteInformation.csv")
RSN_releve <- read.csv("604_RSN_Releve.csv")
RSN_active <- read.csv("605_RSN_ActiveLayerDepths_2021.csv")
RSN_releve$site

num_sites <- RSN_releve %>% 
  summarise(unique_count = n_distinct(site))
#n = 84

num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_active <- RSN_active %>% 
  summarise(unique_count = n_distinct(site))
#n = 91

# Compare unique values between the 604 and 737
values_releve <- RSN_releve %>% distinct(site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_releve$site)
diff_siteinfo
# unique values are: "FP5A" "FP5C" "FP5D" "HR1A" "SL1A" "SL1B" "UP4A" "UP4B" "UP4C"

# Compare unique values between the 605 and 737
values_active <- RSN_active %>% distinct(site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_active$site)
diff_siteinfo
# unique values are: "BFY1"  "BFY10" "BFY11" "BFY12" "BFY13" "BFY4"  "BFY9"  "DCY15" "DCY17" "DCY5"  "DCY7" 
#"DCY8"  "GRI3"  "HR1A"  "UP4D"  "WDI4" 
