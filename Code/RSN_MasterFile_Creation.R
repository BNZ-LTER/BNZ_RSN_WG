#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#RSN Masterfile Creation----
#Created by Weronika Konwent
#Modified by Zach Madsen
#Date 4-4-2025
#Note: Open file using Rproject inside BNZ_RSN_WG for streamless data access
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#==============================================================================#
##Libraries----
#==============================================================================#

load('libary_function.RData') #Load install & load library function
library.func(c('dplyr', 'matrixStats', 'lubridate', 'tidyverse')) #Installs only if missing then loads
                                        #Continue adding to list if more is needed 
                                        #Then re-run function

#==============================================================================#
##Load Data----
#==============================================================================#

#Original compiled masterfile in 2015
rsn_master <- read.csv("Data/raw/737_RSN_SiteInformation.csv")

#New site network masterfile
nsn_master <- read.csv('Data/raw/NewSiteNetwork_Information.csv')
#Clean topo
nsn_master <- nsn_master %>% 
  mutate(
    Topo = TP[match(Topographic.Position, Key)]
  )

#NSN organic depth
nsn.org <- read.csv('Data/raw/NewSiteNetwork_OrganicDepth.csv')

#Organic depth layer data (better one according to Jaime)
rsn_soils_606 <- read.csv("Data/raw/606_RSN_SoilsOL_2015.csv")

#Additional organic depth layers
short_cores <- read.csv("Data/raw/short_core_inventory.csv")

#Organic layer information that needs to be converted to organic depth
pf_cores <- read.csv("Data/raw/CoreProcessingData_GuelphandAlaskaProcessing_Jan2024.csv")

#Additional site level data for newer sites
jfsp_sitedata <- read.csv("Data/raw/342_JFSP_sitedata_2011.csv")

#Clean JFSP column names
jfsp_sitedata <- jfsp_sitedata %>%
  rename(
    severity = CBI.total,
    topo = slope.pos,
    moist = moist.2008,
    slope = slope.deg,
    org.depth = Resid.org
  ) %>% 
  mutate(
    PrelimID = paste0(burn, site)
  ) %>%
  mutate(
    moist = case_when(
      moist == 1 ~ 'xeric',
      moist == 2 ~ "subxeric", 
      moist == 3 ~ "subxeric-mesic",
      moist == 4 ~ "mesic",
      moist == 5 ~ "submesic",
      moist == 6 ~ "subhygric",
      TRUE ~ 'Check'
    ),
    topo = case_when(
      topo == 0 ~ "upper slope", 
      topo == 1 ~ "middle slope", 
      topo == 2 ~ "toe slope",
      topo == 3 ~ "flat", 
      TRUE ~ 'Check'
    )
  )

#Teresa data for additional site info
tkn_site_level <- read.csv("Data/working/tknforrsn_enviro.csv")

#Clean Teresa column names
tkn_site_level <- tkn_site_level %>%
  rename(
    ph = X1.1.pH,
    org.depth = total.O..cm.,
    prelimID = Site,
    topo = topographic.position,
    moist = site.moisture,
    elevation = elevation..m.,
    slope = slope..deg.
  )  %>%
  mutate(
    moist = case_when(
      moist == 3 ~ "xeric", 
      moist == 4 ~ "subxeric", 
      moist == 5 ~ "subxeric-mesic", 
      moist == 6 ~ "mesic",
      moist == 7 ~ "mesic-subhygric", 
      moist == 8 ~ "subhygric", 
      TRUE ~ 'Check'
    ),
    topo = case_when(
      topo == 1 ~ "summit", 
      topo == 3 ~ "side slope", 
      topo == 4 ~ "toe slope", 
      topo == 5 ~ "valley bottom",
      topo == 9 ~ "lowland", 
      TRUE ~ 'check'
    )
  ) 

#Tree core data for determining fire year
tree_age <- read.csv("Data/raw/tree_age.csv")

#Tree core data from new site network for determining fire year
nsn_tree_cores <- read.csv("Data/raw/nsn_tree_cores.csv")

#Modified site names to help match orignial with new names
site_names_df <- read.csv("Data/working/RSN_site_names.csv")
site_names_df <- site_names_df[-c(94:999),]

#Teresa tree core for establishing site age
tkn_age <- read.csv("Data/raw/139_treecores_rings.csv")

#==============================================================================#
##Process Data----
#==============================================================================#

#################################################################-
###Step 1: Assess the Original RSN Master's Missing Attributes----
#################################################################-

#-----------------------------------------------------------------------#
####1.1: Clean up original master data sheet with desired features----
#-----------------------------------------------------------------------# 

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

#-----------------------------------------------------#
####1.2: Names of Known vs Unknown Site Attributes----
#-----------------------------------------------------#
##Note: All columns from 1-9 are already filled

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####1.2.1: Topography---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

topo.complete <- rsn_master %>% 
  filter(!topo %in% '') %>% 
  select(sitename, prelimID)
topo.incomplete <- rsn_master %>% 
  filter(!sitename %in% topo.complete$sitename) %>% 
  select(sitename, prelimID)
##62 incomplete, 31 complete

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####1.2.2: Severity---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

severity.complete <- rsn_master %>% 
  filter(!severity %>% is.na()) %>% 
  select(sitename, prelimID)
severity.incomplete <- rsn_master %>% 
  filter(!sitename %in% severity.complete$sitename) %>% 
  select(sitename, prelimID)
#30 complete

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####1.2.3: Ph---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ph.complete <- rsn_master %>% 
  filter(!ph %in% 0) %>% 
  select(sitename, prelimID)
ph.incomplete <- rsn_master %>% 
  filter(!sitename %in% ph.complete$sitename) %>% 
  select(sitename, prelimID)
#31 complete

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####1.2.4: Moisture---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

moist.complete <- rsn_master %>% 
  filter(!moist %in% '') %>% 
  select(sitename, prelimID)
moist.incomplete <- rsn_master %>% 
  filter(!sitename %in% moist.complete$sitename) %>% 
  select(sitename, prelimID)
##All complete (may need to update)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####1.2.5: Organic Soil Depth---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

org.complete <- rsn_master %>% 
  filter(!sol %in% 0) %>% 
  select(sitename, prelimID, sol)
org.incomplete <- rsn_master %>% 
  filter(!sitename %in% org.complete$sitename) %>% 
  select(sitename, prelimID)
#31 Complete

##############################################################-
###Step 2: Compare Common Fields/Values Between Data Sheets----
##############################################################-

#--------------------------------------------------------------------#
####2.1: Create Function Comparing Attribute Values Between Sites----
#--------------------------------------------------------------------#
compare_numeric_values <- function(df1, df2,
                                   site_col1 = "", site_col2 = "",
                                   variable_col1 = "", variable_col2 = "",
                                   tolerance = 0.10) {
  # Rename site and variable columns for internal consistency
  df1_clean <- df1 %>%
    rename(sites = all_of(site_col1), value_df1 = all_of(variable_col1))
  
  df2_clean <- df2 %>%
    rename(sites = all_of(site_col2), value_df2 = all_of(variable_col2))
  
  # Join and compare
  joined <- df1_clean %>%
    full_join(df2_clean, by = "sites") %>%
    mutate(
      value_match = near(value_df1, value_df2, tol = tolerance)
    )
  
  return(joined)
}

#-----------------------------------------------------#
####2.2: Organic Depth Data Comparison----
#-----------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.2.1: Short Core vs RSN Master Org---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Short core data sheet uses prelimID site names
(unique(short_cores$Site) %in% org.complete$prelimID) %>% sum() #23 sites have sol and organic depth
sum(short_cores$Site %>% unique() %in% org.incomplete$prelimID) #Additional 25 sites for only organic depth

#Summarize short core data sheet to get depth mean, std, replica count, and year
short.core.sum <- short_cores %>% 
  mutate(
    Date = myd(Date),
    Year = year(Date)
  ) %>% 
  group_by(Site) %>% 
  summarize(
    depth.mean = mean(Org.Depth, na.rm = TRUE) %>% round(2),
    depth.rep = n(),
    depth.std = sd(Org.Depth, na.rm = TRUE) %>% round(2),
    Year = mean(Year, na.rm = TRUE) %>% round(0)
  ) %>% 
  select(Site, depth.mean, depth.rep, depth.std, Year)

#Compare organic depth values with sol values
short.joined <- compare_numeric_values(df1 = org.complete, df2 = short.core.sum,
                                       site_col1 = 'prelimID', site_col2 = 'Site',
                                       variable_col1 = 'sol', variable_col2 = 'depth.mean', tolerance = 0.10)

#Compare total matches of organic depth
sum(short.joined$value_match, na.rm = TRUE) #0
                                            #Let's stick with org depth since we know replicas and std

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.2.2: RSN 606 Soil vs RSN Master Org---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Compare common sites from original rsn master and missing sites
sum(unique(rsn_soils_606$site_name) %in% org.complete$sitename) #22 sites are in original
sum(unique(rsn_soils_606$site_name) %in% org.incomplete$sitename) #30 sites fill in missing

#Summarize rsn 606 sol
soil606.sum <- rsn_soils_606 %>% 
  mutate(
    Year = sapply(strsplit(date, "/"), function(x) tail(x, 1)) %>% as.numeric()
  ) %>% 
  group_by(site_name) %>% 
  summarize(
    depth.mean = mean(organic_depth, na.rm = TRUE) %>% round(2),
    depth.rep = n(),
    depth.std = sd(organic_depth, na.rm = TRUE) %>% round(2),
    Year = mean(Year, na.rm = TRUE) %>% round(0),
    PrelimID = unique(alternative_name)
  ) %>% 
  select(site_name, depth.mean, depth.rep, depth.std, Year, PrelimID)

#Compare site organic depth between original master and rsn 606
soil606.joined <- compare_numeric_values(df1 = org.complete, df2 = soil606.sum,
                                       site_col1 = 'sitename', site_col2 = 'site_name',
                                       variable_col1 = 'sol', variable_col2 = 'depth.mean', 
                                       tolerance = 0.10)

#How many match?
sum(soil606.joined$value_match, na.rm = TRUE) #2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.2.3: Short Core vs RSN 606 Soil---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare site organic depth between short core and rsn 606
soilshort.606.joined <- compare_numeric_values(df1 = short.core.sum, df2 = soil606.sum,
                                            site_col1 = 'Site', site_col2 = 'PrelimID',
                                            variable_col1 = 'depth.mean', variable_col2 = 'depth.mean', 
                                            tolerance = 0.10)
#How many match?
sum(soilshort.606.joined$value_match, na.rm = TRUE) #2   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.2.4: TKN & JFSP vs RSN Master Org---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare site organic depth between jfsp and rsn master orig
jfsp.rsnmaster.joined <- compare_numeric_values(df1 = org.complete, df2 = jfsp_sitedata,
                                               site_col1 = 'prelimID', site_col2 = 'PrelimID',
                                               variable_col1 = 'sol', variable_col2 = 'org.depth', 
                                               tolerance = 0.10)
#How many match?
sum(jfsp.rsnmaster.joined$value_match, na.rm = TRUE) #0

#Compare site organic depth between jfsp and rsn master orig
tkn.rsnmaster.joined <- compare_numeric_values(df1 = org.complete, df2 = tkn_site_level,
                                                site_col1 = 'sitename', site_col2 = 'prelimID',
                                                variable_col1 = 'sol', variable_col2 = 'org.depth', 
                                                tolerance = 0.10)
#How many match?
sum(jfsp.rsnmaster.joined$value_match, na.rm = TRUE) #0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.2.5: NSN vs RSN Master Org---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(nsn.org$Site.Name) %in% org.complete$prelimID) #25 sites are in original
sum(unique(nsn.org$Site.Name) %in% org.incomplete$prelimID) #2 sites fill in missing

#Summarize
nsn.org.sum <- nsn.org.cleaned %>%
  pivot_longer(
    cols = starts_with("Thickness"),
    names_to = "measurement",
    values_to = "depth"
  ) %>%
  group_by(Site.Name) %>%
  summarize(
    depth.mean = round(mean(depth, na.rm = TRUE), 2),
    depth.rep = sum(!is.na(depth)),
    depth.std = round(sd(depth, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  rename(site_name = Site.Name) %>%
  mutate(Year = 2011)

#Summarize RSN 606 sol
nsn.org.sum <- nsn.org %>% 
  group_by(Site.Name) %>% 
  summarize(
    depth.mean = mean(Thickness.1, Thickness.2, Thickness.3, Thickness.4, 
                      Thickness.5, Thickness.6, Thickness.7, Thickness.8,
                      Thickness.9, Thickness.10, na.rm = TRUE) %>% round(2),
    depth.rep = n(),
    depth.std = sd(Thickness.1, Thickness.2, Thickness.3, Thickness.4, 
                   Thickness.5, Thickness.6, Thickness.7, Thickness.8,
                   Thickness.9, Thickness.10, na.rm = TRUE) %>% round(2),
    ) %>% 
  select(site_name, depth.mean, depth.rep, depth.std) %>% 
  mutate(
    Year = 2011
  )

#Remove characters
nsn.org.cleaned <- nsn.org %>%
  mutate(across(
    starts_with("Thickness"),
    ~ as.numeric(gsub("[^0-9.]", "", .x))  # remove non-numeric characters
  ))

#Compare site organic depth between original master and rsn 606
soil606.joined <- compare_numeric_values(df1 = org.complete, df2 = soil606.sum,
                                         site_col1 = 'sitename', site_col2 = 'site_name',
                                         variable_col1 = 'sol', variable_col2 = 'depth.mean', 
                                         tolerance = 0.10)




#How many match?
sum(soil606.joined$value_match, na.rm = TRUE) #2

#------------------------------------------------------# 
####2.3: pH Data Comparison----  
#------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.3.1: JFSP vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(jfsp_sitedata$PrelimID) %in% ph.complete$prelimID) #0/31 sites are in original
sum(unique(jfsp_sitedata$PrelimID) %in% ph.incomplete$prelimID) #25/62 sites fill in missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.3.2: TKN vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tkn_site_level$prelimID) %in% ph.complete$prelimID) #0/31 sites are in original
sum(unique(tkn_site_level$prelimID) %in% ph.incomplete$prelimID) #18/62 sites fill in missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.3.3: TKN vs JFSP---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tkn_site_level$prelimID) %in% jfsp_sitedata$PrelimID) #0 sites match
sum(unique(short.core.sum$Site) %in% jfsp_sitedata$PrelimID) #0 sites match

#-------------------------------------------------------------------#
####2.4: Severity Data Comparison----
#-------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.4.1: JFSP vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(jfsp_sitedata$PrelimID) %in% severity.complete$prelimID) #25/30 sites are in original
sum(unique(jfsp_sitedata$PrelimID) %in% severity.incomplete$prelimID) #0/63 sites fill in missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.4.2: TKN vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tkn_site_level$prelimID) %in% severity.complete$prelimID) #0/30 sites are in original
sum(unique(tkn_site_level$prelimID) %in% severity.incomplete$prelimID) #18/63 sites fill in missing


#-------------------------------------------------------------------#
####2.5: Topography Data Comparison----
#-------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.5.1: JFSP vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(jfsp_sitedata$PrelimID) %in% topo.complete$prelimID) #0/31 sites are in original
sum(unique(jfsp_sitedata$PrelimID) %in% topo.incomplete$prelimID) #25/62 sites fill in missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.5.2: TKN vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tkn_site_level$prelimID) %in% topo.complete$prelimID) #0/31 sites are in original
sum(unique(tkn_site_level$prelimID) %in% topo.incomplete$prelimID) #18/62 sites fill in missing


#-------------------------------------------------------------------#
####2.6: Moisture Data Comparison----
#-------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.6.1: JFSP vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(jfsp_sitedata$PrelimID) %in% moist.complete$prelimID) #0/31 sites are in original
sum(unique(jfsp_sitedata$PrelimID) %in% moist.incomplete$prelimID) #25/62 sites fill in missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.6.2: TKN vs RSN Master Original---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tkn_site_level$prelimID) %in% moist.complete$prelimID) #0/31 sites are in original
sum(unique(tkn_site_level$prelimID) %in% mosist.incomplete$prelimID) #18/62 sites fill in missing


#-------------------------------------------------------------------#
####2.7: Tree Core Data Comparison----
#-------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.6.1: Tree age vs RSN Master Original vs others---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(tree_age$Site.Name) %in% rsn_master$prelimID) #28/93 sites are in original

#Compare tree age to nsn tree
sum(unique(tree_age$Site.Name) %in% unique(nsn_tree_cores$Site.Name))#40

#Compare tree age to Terresa data
sum(unique(tree_age$Site.Name) %in% unique(tkn_age$Site)) #0

#Summarize tree age data
#Summarize short core data sheet to get depth mean, std, replica count, and year
tree.age.sum <- tree_age %>% 
  group_by(Site.Name) %>% 
  summarize(
    pith.mean = mean(Org.Depth, na.rm = TRUE) %>% round(2),
    pith.rep = n(),
    pith.std = sd(Org.Depth, na.rm = TRUE) %>% round(2)
  ) %>% 
  select(Site, depth.mean, depth.rep, depth.std)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####2.6.2: NSN vs RSN Master Original & Terresa---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Compare common sites from original rsn master and missing sites
sum(unique(nsn_tree_cores$Site.Name) %in% unique(nsn_tree_cores$Site.Name))#41/93

#Compare tree age to Terresa data
sum(unique(nsn_tree_cores$Site.Name) %in% unique(tkn_age$Site)) #0

############################################################-
###Step 3: Choose Highest Quality Data for New RSN Master----
############################################################-


##################################################-
###Step 4: Create Site Burn Year with Core Data----
##################################################-


##########################################################-
###Step 5: Complete Organic Depth with Permafrost Cores----
##########################################################-


#==============================================================================#
##Save Data----
#==============================================================================#


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#End Script----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
