## cleaning up RSN datafiles ##
#created by WK 
#updated by WK on 12/20/23

#this is for file MasterInventory11.19.23

#load packages
library(tidyverse)

#read in the file
rsn_siteinfo <- read_csv("Data/raw/737_RSN_SiteInformation.csv")
rsn_masterinv <- read_csv("Data/raw/MasterInventory11.19.23.csv")

##first some digging to unserstand how many of these sites are RSN sites
num_sites_info <- rsn_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_masterinv <- rsn_masterinv %>% 
  summarise(unique_count = n_distinct(SITE))
#n = 118

#double check what these 118 are
unique(rsn_masterinv$SITE)
#looks like there might be some mispellings. let's clean them up
rsn_masterinv$SITE <- sub("i", "I", rsn_masterinv$SITE)
rsn_masterinv$SITE <- sub("p", "P", rsn_masterinv$SITE)
rsn_masterinv$SITE <- sub("b", "B", rsn_masterinv$SITE)
#now n = 115

# Compare unique values between treeinv and 737
values_masterinv <- rsn_masterinv %>% distinct(SITE)
values_siteinfo <- rsn_siteinfo %>% distinct(sitename)

#find sites in rsn_737 that are not in masterinv
diff_siteinfo <- setdiff(values_siteinfo$sitename, values_masterinv$SITE)
diff_siteinfo
#5 unique values

#find sites in masterinv that are not in rsn_737
diff_masterinv <- setdiff(values_masterinv$SITE, values_siteinfo$sitename)
diff_masterinv
#27 unique values

## we are missing tree inventory data for 5 RSN sites: DCY1, DCY10, DCT14, DCY7, HR1A



