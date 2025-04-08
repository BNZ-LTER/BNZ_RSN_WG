#RSN Masterfile Creation####
#Created by Weronika Konwent
#Modified by Zach Madsen
#Date 4-4-2025
#Note: Open file using Rproject inside BNZ_RSN_WG for streamless data access

##Tools####
load('libary_function.RData') #Load install & load library function
library.func(c('dplyr', 'matrixStats', 'devtools')) #Installs only if missing then loads
                                        #Continue adding to list if more is needed 
                                        #Then re-run function


##Data####
#Original compiled masterfile in 2015
rsn_master <- read.csv("Data/raw/737_RSN_SiteInformation.csv")

#Organic depth layer data (better one according to Jaime)
rsn_soils_606 <- read.csv("Data/raw/606_RSN_SoilsOL_2015.csv")

#Additional organic depth layers
short_cores <- read.csv("Data/raw/short_core_inventory.csv")

#Organic layer information that needs to be converted to organic depth
pf_cores <- read.csv("Data/raw/CoreProcessingData_GuelphandAlaskaProcessing_Jan2024.csv")

#Additional site level data for newer sites
jfsp_sitedata <- read.csv("Data/raw/342_JFSP_sitedata_2011.csv")

#Terresa data for additional site info
tkn_site_level <- read.csv("Data/working/tknforrsn_enviro.csv")

#Tree core data for determining fire year
tree_age <- read.csv("Data/raw/tree_age.csv")

#Tree core data from new site network for determining fire year
nsn_tree_cores <- read.csv("Data/raw/nsn_tree_cores.csv")

#Modified site names to help match orignial with new names
site_names_df <- read.csv("Data/working/RSN_site_names.csv")
site_names_df <- site_names_df[-c(94:999),]

#Teresa tree core for establishing site age
tkn_age <- read.csv("Data/raw/139_treecores_rings.csv")

###Step 1: Compare data we have already compiled and find what is missing####
#Clean up orignal master data sheet with desired features
rsn_master <- rsn_master %>% 
  #select useful columns
  select(ecoregion, sitename, prelimID, n_dd, w_dd, slope, aspect, elevation, 
         canopy_typ, topo, moist, severity, ph, sol, age, burn_year) %>%
  #change all values to lowercase
  mutate(
    canopy_typ = tolower(canopy_typ), 
    topo = tolower(topo), 
    moist = tolower(moist), 
    age = tolower(age),
    #change burn_year to numeric value by making years absolute
    burn_year = replace(burn_year, burn_year == "Pre 1930", NA), 
    ### derived age for these sites is the most conservative estimate
    #all values set to 1930 are actually fires pre-1930
    burn_year = as.numeric(burn_year),
    #Jamie had some additional information on prelimIDs. there are some in this dataset that could be amended
    prelimID = case_when(
      prelimID == "UP4A" ~ "TKN0001",
      prelimID == "UP4B" ~ "TKN0103",
      prelimID == "UP4C" ~ "TKN0110",
      prelimID == "UP4D" ~ "TKN0134", 
      TRUE ~ prelimID
      )
    )

##Assess site names that have desired traits between site names of missing traits
##Note: All columns from 1-9 are already filled

#Start with topography indicator
topo.complete <- rsn_master %>% 
  filter(!topo %in% '') %>% 
  select(sitename)
topo.incomplete <- rsn_master %>% 
  filter(!sitename %in% topo.complete$sitename) %>% 
  select(sitename)
##62 incomplete, 31 complete

#Next distinguish moisture type
moist.complete <- rsn_master %>% 
  filter(!moist %in% '') %>% 
  select(sitename)
moist.incomplete <- rsn_master %>% 
  filter(!sitename %in% moist.complete$sitename) %>% 
  select(sitename)
##All complete (may need to update)

#Severity for each site
severity.complete <- rsn_master %>% 
  filter(!severity %>% is.na()) %>% 
  select(sitename)
severity.incomplete <- rsn_master %>% 
  filter(!sitename %in% severity.complete$sitename) %>% 
  select(sitename)
#30 complete

#Next is ph for sites
ph.complete <- rsn_master %>% 
  filter(!ph %in% 0) %>% 
  select(sitename)
ph.incomplete <- rsn_master %>% 
  filter(!sitename %in% ph.complete$sitename)
#31 complete

#Finally organic soil depth
org.complete <- rsn_master %>% 
  filter(!sol %in% 0) %>% 
  select(sitename)
org.incomplete <- rsn_master %>% 
  filter(!sitename %in% org.complete$sitename) %>% 
  select(sitename)
#31 Complete


###Step 2: Compare raw data from different sources to what we have/don't have####
##Start with organic depth layers
#Short core data sheet uses current site names
short_cores$Site 








