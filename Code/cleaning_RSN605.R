## cleaning up RSN datafiles ##

#this is for file 604_RSN_Releve#

#load packages
library(dplyr)
library(tidyr)

#set working directory
setwd("/Users/wk78/Documents/R/RSN")

#read in the file
RSN_siteinfo <- read.csv("737_RSN_SiteInformation.csv")
RSN_active <- read.csv("605_RSN_ActiveLayerDepths_2021.csv")
RSN_active$site

num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_active <- RSN_active %>% 
  summarise(unique_count = n_distinct(site))
#n = 91, but some of them are mispellings

# fix mispellines - now n=84
RSN_active$site <- gsub("i", "I", RSN_active$site)
RSN_active$site <- gsub("y", "Y", RSN_active$site)
RSN_active$site <- gsub("m", "M", RSN_active$site)

# Compare unique values between the 605 and 737
values_active <- RSN_active %>% distinct(site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_active$site)
diff_siteinfo
# unique values are: "BFY1"  "BFY10" "BFY11" "BFY12" "BFY13" "BFY4"  "BFY9"  "DCY15" "DCY17" "DCY5"  "DCY7" 
#"DCY8"  "GRI3"  "HR1A"  "UP4D"  "WDI4" 

diff_active <- setdiff(values_active$site, values_siteinfo$sitename)
diff_active
# uniaue value is: "FP5B"

write.csv(RSN_active, file = "605_RSN_ActiveLayerDepths_2021.csv")
