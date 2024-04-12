## cleaning up RSN datafiles ##

#this is for file 783_denit#

#load packages
library(dplyr)
library(tidyr)

#set working directory
setwd("/Users/wk78/Documents/R/RSN")

#read in the file
RSN_siteinfo <- read.csv("737_RSN_SiteInformation.csv")
RSN_denit <- read.csv("783_denit.csv")
RSN_denit$Site

num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_denit <- RSN_denit %>% 
  summarise(unique_count = n_distinct(Site))
#n = 46
RSN_denit$Site

# Compare unique values between the 783 and 737
values_denit <- RSN_denit %>% distinct(Site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_denit$Site)
diff_siteinfo
# 78 unique values 

diff_denit <- setdiff(values_denit$Site, values_siteinfo$sitename)
diff_denit
# 31 unique values
#this means that there are only 15 RSN sites used here



