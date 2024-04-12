## cleaning up RSN datafiles ##

#this is for file 2023NewSiteNetwork

#load packages
library(dplyr)
library(tidyr)
install.packages("sf")
install.packages("units") #had to download this so that sf would install
#had to download udunits2 in terminal in order to install units
library(sf)

#set working directory
setwd("/Users/wk78/Documents/R/RSN")

#read in the file
RSN_siteinfo <- read.csv("737_RSN_SiteInformation.csv")
RSN_fuel <- read.csv("762_RSN_FuelLoad_2019_clean.csv")
RSN_site_names <- read.csv("RSN_site_names.csv")
RSN_summ <- read.csv("2023NewSiteNetwork_Complete_SD.csv")
RSN_summ$prelimID <- RSN_summ$Site.Name

RSN_summ <- merge(RSN_summ, RSN_site_names, by = "prelimID", all=TRUE)
RSN_summ <- select(RSN_summ, prelimID, sitename, Date, Site.Name, Fire.Name, Observers,              
                  Weather, Location.Notes, GPS.Coordinates..North., GPS.Coordinates..East., Slope..deg., Aspect..deg.,          
                  Elevation..m., Comments, Topographic.Position, Site.Contour, Evidence.of.thermokarst,
                  Water.Flux, Est.age.of.oldest.trees, Site.Moisture, Surficial.Geology, Surficial.Geomorphology,
                  Other.Notes)

RSN_summ <- RSN_summ %>%
  filter(!is.na(Date))

RSN_summ <- select(RSN_summ, sitename, Site.Name, Date, Fire.Name, Observers,              
                   Weather, Location.Notes, GPS.Coordinates..North., GPS.Coordinates..East., Slope..deg., Aspect..deg.,          
                   Elevation..m., Comments, Topographic.Position, Site.Contour, Evidence.of.thermokarst,
                   Water.Flux, Est.age.of.oldest.trees, Site.Moisture, Surficial.Geology, Surficial.Geomorphology,
                   Other.Notes)

#fixing coordinates

#make new column combining coordinates
#RSN_summ <- RSN_summ %>%
 #mutate(UTM = paste(GPS.Coordinates..North., GPS.Coordinates..East., sep = " "))

RSN_summ$UTM_n <- as.numeric(RSN_summ$GPS.Coordinates..North.)
RSN_summ$UTM_e <- as.numeric(RSN_summ$GPS.Coordinates..East.)
utm_coords <- RSN_summ[, c("UTM_n", "UTM_e")]
#utm_coords <- RSN_summ[, c("GPS.Coordinates..North.", "GPS.Coordinates..East.")]

cleaned_utm_coords <- utm_coords[complete.cases(utm_coords$UTM_n, utm_coords$UTM_e), ]

##convert to sf
utm_coords_sf <- st_as_sf(cleaned_utm_coords, coords = c("UTM_n", "UTM_e"), crs = 32606)  # CRS for UTM Zone 06N

# Transform coordinates from UTM to lat-long (WGS84)
utm_coords_sf_transformed <- st_transform(utm_coords_sf, crs = 4326)  # CRS for WGS84

# Extract transformed coordinates
transformed_coords <- st_coordinates(utm_coords_sf_transformed)
transformed_coords$n_dd <- transformed_coords$X
transformed_coords$w_dd <- transformed_coords$Y
RSN_summ <- cbind(RSN_summ, transformed_coords)


num_sites_info <- RSN_siteinfo %>% 
  summarise(unique_count = n_distinct(sitename))
#n = 93

num_sites_summ <- RSN_summ %>% 
  summarise(unique_count = n_distinct(Site.Name))
#n = 39

# Compare unique values between the summ and 737
values_summ <- RSN_summ %>% distinct(sitename)
values_siteinfo <- RSN_siteinfo %>% distinct(sitename)

diff_siteinfo <- setdiff(values_siteinfo$sitename, values_summ$sitename)
diff_siteinfo
#unique values (65)  
#this means that 28 sites from RSN_summ are from RSN, the difference between 39 and 28 are 
#sites that do not have "modern" site names

diff_summ <- setdiff(values_summ$sitename, values_siteinfo$sitename)
diff_summ
# no unique value


write.csv(RSN_summ, file = "2023NewSiteNetwork_Complete_SD_clean.csv")


