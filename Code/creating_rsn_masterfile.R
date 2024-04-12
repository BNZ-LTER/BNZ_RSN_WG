## creating an RSN masterfile ##
#created by Wk on 12/20/23
#updated by WK on 2/15/24

#this is where I pull all of the interesting data from various datafiles into one RSN masterfile
#all datasets come in their raw form

install.packages("matrixStats")
library(tidyverse)
library(dplyr)
library(matrixStats)

#file 737 has a lot of what we will want to start with
rsn_master <- read_csv("Data/raw/737_RSN_SiteInformation.csv")
rsn_master <- select(rsn_master, ecoregion, sitename, prelimID, n_dd, w_dd, slope, aspect, elevation, canopy_typ, topo, moist, severity,
                     ph, sol, treeden, treeba, seedlingde, age, burn_year)

### SOIL ###
#let's add in sol, from file 606
rsn_soils <- read_csv("Data/raw/606_RSN_SoilsOL_2015.csv")
#all of these sites are rsn sites

rsn_soils <- select(rsn_soils, site_name, organic_depth)

#let's make it so that each site is it's own row
rsn_soils <- rsn_soils %>%
  group_by(site_name) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = organic_depth, names_prefix = "org_depth")

#make an average, rounded row 
rsn_soils$ave_orgd <- rowMeans(rsn_soils[, -1], na.rm = TRUE)
rsn_soils$ave_orgd <- round(rsn_soils$ave_orgd, 1)

#make rsn_soils mergable with rsn_master
rsn_soils <- select(rsn_soils, site_name, ave_orgd)
rsn_soils <- rename(rsn_soils, sitename = site_name)

#join the two datasets
rsn_master <- full_join(rsn_master, rsn_soils, by = "sitename")


rsn_master <- select(rsn_master, ecoregion, sitename, prelimID, n_dd, w_dd, slope, aspect, elevation, canopy_typ, topo, moist, severity,
                     ph, sol, ave_orgd, treeden, treeba, seedlingde, age, burn_year)
#looking at sol and ave_orgd, there are some differences in depth and they cover most but not all of the same sites
#need to know where sol raw data came from
#this completes our soil portion of the masterfile

### AGE ###
#now let's tackle stand age
tree_age <- read_csv("Data/working/nsn_tree_cores_all.csv")

#first let's check that all of the sites here are RSN sites
#find common unique_ids between the two datasets
common_rsn <- intersect(tree_age$Site_Name, rsn_master$prelimID)
#we have 28 rsn sites in tree_age - could the missing 12 be because of alterntaive prelimIDs?

#find values in Site_Name in tree_age that are not in master
unique_to_tree_age <- setdiff(tree_age$Site_Name, rsn_master$prelimID)
#12 of them -> looks like this isn't an issue of alt prelimIDs

#so I guess let's keep just the rsn sites
tree_age <- tree_age[tree_age$Site_Name %in% common_rsn, ]

#next, this file uses old site names so let's change them to the correct version
site_names_df <- read_csv("Data/working/RSN_site_names.csv")
site_names_df <- site_names_df[site_names_df$prelimID %in% common_rsn, ]
tree_age <- rename(tree_age, prelimID = "Site_Name")
tree_age <- full_join(tree_age, site_names_df, by = "prelimID")

#let's get down to retrieving age from this file
#make a unique_id, so that we can group by site, cohort, and Species

tree_age <- select(tree_age, sitename, cohort, Species, tree_num, ave_age)
tree_age <- tree_age %>%
  mutate(unique_id = paste(sitename, cohort, Species, sep = "_"))

tree_age_ave <- select(tree_age, unique_id, ave_age)

#pivot wider so that each row has all the ages for a unique _id
tree_age_ave <- tree_age_ave %>%
  group_by(unique_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = ave_age, names_prefix = "age")

#average the ages
tree_age_ave$ave_age <- rowMeans(tree_age_ave[, -1], na.rm = TRUE)
tree_age_ave$ave_age <- round(tree_age_ave$ave_age, 0)

#now let's group them by site, and not by species
tree_age_ave <- separate(tree_age_ave, unique_id, into = c("sitename", "cohort", "Species"), sep = "_")
tree_age_site <- select(tree_age_ave, sitename, ave_age)

#pivot wider so that each row has all the ages for a site, species mixed
tree_age_site <- tree_age_site %>%
  group_by(sitename) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = row, values_from = ave_age, names_prefix = "age")

#average stand age
tree_age_site$age_ave <- rowMeans(tree_age_site[2:7], na.rm = TRUE)
tree_age_site$age_ave <- round(tree_age_site$age_ave, 0)

#minimum stand age
tree_age_site$age_min <- rowMins(as.matrix(tree_age_site[2:7]), na.rm = TRUE)
tree_age_site$age_min <- round(tree_age_site$age_min, 0)

#maximum stand age
tree_age_site$age_max <- rowMaxs(as.matrix(tree_age_site[2:7]), na.rm = TRUE)
tree_age_site$age_max <- round(tree_age_site$age_max, 0)

#median stand age
tree_age_site$age_median <- rowMedians(as.matrix(tree_age_site[2:7]), na.rm = TRUE)
tree_age_site$age_median <- round(tree_age_site$age_median, 0)

tree_age_stats <- select(tree_age_site, sitename, age_ave, age_min, age_max, age_median)

#join to rsn_master
rsn_master <- full_join(rsn_master, tree_age_stats, by = "sitename")

#this gives us ages for 28 of the 93 rsn sites, and looking at the data is seems like they're mostly intermediate
#it's been a solid few years since 2011, when these were collected/processed (?) - how do we want to account 
#for this?
#we also need to choose which number we want to represent the age

########################

#now let's add age for mature sites from Teresa's data
tkn_age <- read_csv("Data/raw/139_treecores_rings.csv")

#first let's check which of the sites here are RSN sites
#find common unique_ids between the two datasets
common_rsn <- intersect(tkn_age$Site, rsn_master$prelimID)
#we have 14 rsn sites in tkn_age

#let's keep just these 14 sites
tkn_age <- tkn_age[tkn_age$Site %in% common_rsn, ]

#next, this file uses old site names so let's change them to the correct version
site_names_df <- read_csv("Data/working/RSN_site_names.csv")
site_names_df <- site_names_df[site_names_df$prelimID %in% common_rsn, ]
tkn_age <- rename(tkn_age, prelimID = "Site")

tkn_age <- full_join(tkn_age, site_names_df, by = "prelimID")

#let's get down to retrieving age from this file
#make a unique_id, so that we can group by site, and tree # - all trees are picmar
tkn_age <- rename(tkn_age, tree_num = "Tree #")
tkn_age <- rename(tkn_age, est_age = "Est. inner date of est.")
tkn_age <- select(tkn_age, sitename, tree_num, est_age)

tkn_age_site <- tkn_age %>%
  pivot_wider(names_from = tree_num, values_from = est_age, names_prefix = "tree")
#hmmmm, looks like there are some sites that have two entries for tree #4, or have a tree 4 and are missing a tree 3
#we need to do some cleaning up of these data

#what if we do a combination group by site and number
tkn_age_unique <- tkn_age %>%
  group_by(sitename, tree_num) %>%
  mutate(row = row_number()) %>%
  ungroup()

#then we pivot, so that each site that has a duplicate tree number gets an extra row. 
tkn_age_site <- tkn_age_unique %>%
  pivot_wider(names_from = tree_num, values_from = est_age, names_prefix = "tree")

#then we can target each row 2 to change it to tree5
#this doesn't fix our problem with missing tree 3s, but that doesn't matter since we'll be taking summary stats anyway
tkn_age_site <- tkn_age_site %>%
  group_by(sitename) %>%
  mutate(tree5 = ifelse(any(row == 2), lead(tree4), tree5)) %>%
  filter(row == 1) %>%
  ungroup()

#phew. ok. that was a lot. I will keep the process until we hear from teresa that the assumptions I'm making are correct

tkn_age_site <- select(tkn_age_site, sitename, tree1, tree2, tree3, tree4, tree5, tree6)
#ok, next question - should we change the year to an age, and then do stats? or do stats and the change to age?

#now we can create summary stats
#average stand age
tkn_age_site$age_ave <- rowMeans(tkn_age_site[2:7], na.rm = TRUE)
tkn_age_site$age_ave <- 2024 - tkn_age_site$age_ave
tkn_age_site$age_ave <- round(tkn_age_site$age_ave, 0)

#maximum stand age - we're using the minimum function, since we're subtracting
tkn_age_site$age_max <- rowMins(as.matrix(tkn_age_site[2:7]), na.rm = TRUE)
tkn_age_site$age_max <- 2024 - tkn_age_site$age_max
tkn_age_site$age_max <- round(tkn_age_site$age_max, 0)

#maximum stand age - we're using the maximum function, since we're subtracting
tkn_age_site$age_min <- rowMaxs(as.matrix(tkn_age_site[2:7]), na.rm = TRUE)
tkn_age_site$age_min <- 2024 - tkn_age_site$age_min
tkn_age_site$age_min <- round(tkn_age_site$age_min, 0)

#median stand age
tkn_age_site$age_median <- rowMedians(as.matrix(tkn_age_site[2:7]), na.rm = TRUE)
tkn_age_site$age_median <- 2024 - tkn_age_site$age_median
tkn_age_site$age_median <- round(tkn_age_site$age_median, 0)

tkn_age_stats <- select(tkn_age_site, sitename, age_ave, age_min, age_max, age_median)

#join to rsn_master
#first we just gotta combine the age data
age_data <- select(rsn_master, sitename, age_ave, age_min, age_max, age_median)
age_data <- full_join(age_data, tkn_age_stats, by = "sitename")
#unfortunately the full_join duplicates all of the age columns, so we have to now coalesce them
age_data <- age_data %>%
  mutate(age_ave = coalesce(age_ave.x, age_ave.y), age_min = coalesce(age_min.x, age_min.y), 
         age_max = coalesce(age_max.x, age_max.y), age_median = coalesce(age_median.x, age_median.y)) %>%
  select(-starts_with(c("age_ave.", "age_min.", "age_max.", "age_median.")))  # Remove duplicated columns

#let's add a column - all of the current ages are gleaned from cores/cookies, and all other ages will be derived 
#from last disturbance
age_data <- age_data %>%
  mutate(age_source = ifelse(is.na(age_ave), "disturbance", "core"))

#we now have age from cores for 42 of the 93 rsn sites
#next we'll get the derived age from disturbance, but first we have to combine back to the master df
rsn_master <- select(rsn_master, ecoregion, sitename, prelimID, n_dd, w_dd, slope, aspect, elevation, canopy_typ, topo, moist, severity,
                     ph, sol, ave_orgd, treeden, treeba, seedlingde, burn_year, age)
rsn_master <- full_join(rsn_master, age_data, by = "sitename")

#let's get the derived age
#but first we have to make sure the burn_year only contains number. Let's change "Pre-1930" to 1930, and add a
#NOTE: all derived ages from 1930 are the minimum age possible, and likely to be much older
rsn_master <- rsn_master %>%
  mutate(burn_year = ifelse(burn_year == "Pre 1930", "1930", burn_year))
rsn_master$burn_year <- as.numeric(rsn_master$burn_year)
rsn_master <- rsn_master %>%
  mutate(derived_age = 2024 - burn_year)

#so we know the young sites from 2004 were burned in 2004, so those trees are max 20 years old. all other sites
#we do not know the actual date of, so we will leave as NA
#there are 26 sites from 2004
rsn_master = rsn_master %>%
  mutate(age_ave = case_when(age == "Young" ~ derived_age, TRUE~age_ave), age_min = case_when(age == "Young" ~ derived_age, TRUE~age_min),
         age_max = case_when(age == "Young" ~ derived_age, TRUE~age_max), age_median = case_when(age == "Young" ~ derived_age, TRUE~age_median)) 

#let's fill in the derived ages for the other 4 age stats for those sites without core data
#rsn_master <- rsn_master %>%
 # mutate(age_ave = coalesce(age_ave, derived_age), age_min = coalesce(age_min, derived_age),
  #       age_max = coalesce(age_max, derived_age), age_median = coalesce(age_median, derived_age))

#we still need to choose which value we want to represent age here, but otherwise, 
#this concludes the AGE portion of the masterfile

### TREE DENSITY ###
#we're using an output of Xanthe's with biomass and density already calculated, by site
tree_inv <- read_csv("Data/working/rsn_bio_dens_bysite.csv")

#first let's filter out just rsn sites
common_rsn <- intersect(tree_inv$SITE, rsn_master$sitename)
#there are 84 rsn sites in tree_inv

#find sites in master that are not in tree_inv
rsn_not_in_inv <- setdiff(rsn_master$sitename, tree_inv$SITE)
#9 sites -> "BFY8"  "DCM1"  "DCY1"  "DCY10" "DCY14" "DCY7"  "FP5C"  "HR1A"  "SL1B" 

site_names_df <- read_csv("Data/working/RSN_site_names.csv")
site_names_df <- site_names_df[site_names_df$sitename %in% common_rsn, ]
tree_inv <- tree_inv[tree_inv$SITE %in% common_rsn, ]

#let's keep site, d.index, decid.bio, decid.dens, and class
decid_index <- select(tree_inv, SITE, decid.bio, decid.dens, D.Index, Class)

#let's change SITE to sitename for ease of joining
decid_index <- rename(decid_index, sitename = "SITE")

#now let's join this to the master_rsn
rsn_master <- full_join(rsn_master, decid_index, by = "sitename")
#for plotting purpose, let's keep age, decid.bio, decid.dens, D.Index and class
bio_dens_age_bysite <- select(rsn_master, sitename, age_ave, decid.bio, decid.dens, D.Index, Class)

#temporarily write this in to share with folks
write_csv(bio_dens_age_bysite, file = "Data/working/bio_dens_age_bysite.csv")
#write_csv(rsn_master, file = "Data/working/rsn_master_021624.csv")


