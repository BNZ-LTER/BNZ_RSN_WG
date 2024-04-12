## checking the coordinates for RSN sites; using file 737 and Jamie's RSN coordinate file rsn_6_6_2016
#created by WK 12/20/23
#updated by WK 12/20/23

library(tidyverse)

#reading in the two files
rsn737 <- read_csv("Data/raw/737_RSN_SiteInformation.csv")
rsn_coords <- read_csv("Data/raw/rsn_6_6_2016.csv")

#let's make a single coordinate column
rsn737 <- rsn737 %>%
  mutate(coords = paste(n_dd, w_dd, sep = ", "))

rsn_coords <- rsn_coords %>%
  mutate(coords = paste(Latitude, Longitude, sep = ", "))

#find common unique_ids between the two datasets
common_coords <- intersect(rsn_coords$coords, rsn737$coords)
#we have 87 common values
#the differences might be small...next step will be to check on map

# Find coords in rsn_coords that are not in rsn737
unique_to_coords <- setdiff(rsn_coords$coords, rsn737$coords)
#6 of them: UP4C, GSM5, SL1A, SL1B, FP5C, HR1A

# Find coords in trsn737 that are not in rsn_coords
unique_to_737 <- setdiff(rsn737$coords, rsn_coords$coords)
#6 of them: FP5C, GSM5, HR1A, SL1A, SL1B, UP4C
#same 6 sites, gonna double check them in arcgis

weird_coords<- data.frame(
  coords_rsn = c("65.153934, -147.491332", "64.998562, -147.65685", "64.654212, -148.281189", "64.648267, -148.286093",
                 "64.713612, -148.147237", "65.170917, -147.543548"),
  coords_737 = c("64.713614, -148.147263", "64.998563, -147.65685", "65.170919, -147.543574", "64.654214, -148.281215",
                "64.648269, -148.286119", "65.153934, -147.491333"))

write_csv(weird_coords, file = ("Data/working/rsn_weird_coords.csv"))
#double checked these on the map and they are max 1.5m off. 

#I am going to use the rsn_737 version since it's already archived, and change the coordinates in rsn_coords
rsn_coords[rsn_coords == "64.713612, -148.147237"] <- "64.713614, -148.147263" # FP5C
rsn_coords[rsn_coords == "64.998562, -147.65685"] <- "64.998563, -147.65685" #GSM5 
rsn_coords[rsn_coords == "65.170917, -147.543548"] <- "65.170919, -147.543574" #HR1A 
rsn_coords[rsn_coords == "64.654212, -148.281189"] <- "64.654214, -148.281215" #SL1A 
rsn_coords[rsn_coords == "64.648267, -148.286093"] <- "64.648269, -148.286119" #SL1B 
rsn_coords[rsn_coords == "65.153934, -147.491332"] <- "65.153934, -147.491333" #UP4C 

#last double check
common_coords <- intersect(rsn_coords$coords, rsn737$coords)
#success! 93 values in common

#save this file as the new rsn_coords in the working data file
rsn_coords <- select(rsn_coords, -coords)

write_csv(rsn_coords, file = "Data/working/rsn_coord.csv")

