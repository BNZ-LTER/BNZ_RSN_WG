## cleaning up RSN datafiles ##
#created by WK 
#updated by WK on 12/21/23

#this is for file TreeInventorySampleHistory_1989-2023

#load packages
library(dplyr)
library(tidyr)

#read in the file
RSN_siteinfo <- read_csv("Data/raw/737_RSN_SiteInformation.csv")
RSN_treeinv <- read_csv("Data/raw/TreeInventorySampleHistory_1989-2023.csv")

num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_treeinv <- RSN_treeinv %>% 
  summarise(unique_count = n_distinct(Site))
#n = 118

# Compare unique values between treeinv and 737
values_treeinv <- RSN_treeinv %>% distinct(Site)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_treeinv$Site)
diff_siteinfo
#unique values 0  

diff_treeinv <- setdiff(values_treeinv$Site, values_siteinfo$sitename)
diff_treeinv
#25 unique values



