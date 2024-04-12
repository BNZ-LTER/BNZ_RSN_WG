##code to combine tree age with NewSiteNewtowk_Complete
#created by WK 12/20/23
#updated by WK 12/20/23

library(tidyverse)

#read in file - this is the file with age from tree cores. we want to integrate it with NSN
tree_age <- read_csv("Data/raw/tree_age.csv")
nsn_tree_cores <- read_csv("Data/raw/nsn_tree_cores.csv")

#let's clean up these files a bit
colnames(nsn_tree_cores)

#first we must get rid of all of the spaces in the column names
colnames(nsn_tree_cores) <- gsub(" ", "_", colnames(nsn_tree_cores))
colnames(tree_age) <- gsub(" ", "_", colnames(tree_age))

nsn_tree_cores <- select(nsn_tree_cores, Date, Fire_Name, Site_Name, Observers, Weather, "Cohort_#", Species, 
                         "Tree_#", "Ht_of_Core_(cm)", "DCH_(cm)", "DBH_(cm)", "Cookie/Core?", Condition_of_Core, 
                         "Fire_Scar?", Age, Additional_Notes)
nsn_tree_cores <- nsn_tree_cores[-c(679:999), ]

#let's change all the column names with special characters
nsn_tree_cores <- nsn_tree_cores %>%
  rename(cohort = "Cohort_#", tree_num = "Tree_#", core_ht = "Ht_of_Core_(cm)", dch = "DCH_(cm)",
  dbh = "DBH_(cm)", sample_type = "Cookie/Core?", fire_scar = "Fire_Scar?")

tree_age <- tree_age %>%
  rename(cohort = "Cohort_#", tree_num = "Tree_#", core_ht = "Ht_of_Core_(cm)", dch = "DCH_(cm)",
         dbh = "DBH_(cm)", sample_type = "Cookie/Core?", fire_scar = "Fire_Scar?", field_notes = "Feild_Notes")

####

#we have a different number of rows in these two datasets - let's see what's missing
#first let's make a unique id for both of these
nsn_tree_cores <- nsn_tree_cores %>%
  mutate(unique_id = paste(Site_Name, cohort, tree_num, sep = "-"))

tree_age <- tree_age %>%
  mutate(unique_id = paste(Site_Name, cohort, tree_num, sep = "-"))

#find common unique_ids between the two datasets
common_values <- intersect(nsn_tree_cores$unique_id, tree_age$unique_id)
#we have 669 common values

# Find values in unique_id in nsn_tree_cores that are not in tree_age
unique_to_nsn <- setdiff(nsn_tree_cores$unique_id, tree_age$unique_id)
#8 of them: "BD-1-4-1"  "BD-2-3-1"  "GR-10-4-3" "WD-8-4-1"  "WD-8-4-2"  "WD-8-4-3"  "WD-8-4-4"  "WD-8-4-5" 

# Find values in unique_id in tree_age that are not in nsn
unique_to_tree_age <- setdiff(tree_age$unique_id, nsn_tree_cores$unique_id)
#17 of them
#this in interesting - I did not think there were going to be any unique values in tree_age
#on closer inspection, there's a weird site here "BD-RB". Also a weird species "CBSNBC

#check to see if there is an issue with duplicates?
unique_tree_age <- unique(tree_age$unique_id)
unique_nsn <- unique(nsn_tree_cores$unique_id)

duplicates_unique_id <- nsn_tree_cores[duplicated(nsn_tree_cores$unique_id) | duplicated(nsn_tree_cores$unique_id, fromLast = TRUE), ]
#that doesn't seem to be the case, but did find one duplicate, in BD-1-2-1, which had two cores taken because the tree was so big

###

#what is the best way to combine these datasets? probably combine common values to nsn, and leave those 8 unique 
#values as NAs
comm_tree_age <- tree_age[tree_age$unique_id %in% common_values, ]

comm_tree_age <- select(comm_tree_age, unique_id, "Times_Counted?", Age_1, Age_2, Age_3, Avg, field_notes, Counting_Notes, Age_Notes)

nsn_tree_cores_all <- full_join(comm_tree_age, nsn_tree_cores, by = "unique_id")

#let's clean up the rows 
#first let's redo the average
nsn_tree_cores_all$ave_age <- rowMeans(nsn_tree_cores_all[, c("Age_1", "Age_2", "Age_3")], na.rm = TRUE)
nsn_tree_cores_all$ave_age <- round(nsn_tree_cores_all$ave_age, 2)
nsn_tree_cores_all <- rename(nsn_tree_cores_all, age_count = "Times_Counted?") 

nsn_tree_cores_all <- select(nsn_tree_cores_all, Date, Fire_Name, Site_Name, Observers, Weather, cohort, Species, 
                             tree_num, core_ht, dch, dbh, sample_type, Condition_of_Core, fire_scar, age_count,
                             Age_1, Age_2, Age_3, ave_age, field_notes, Counting_Notes, Age_Notes)

#save in working data folder
write_csv(nsn_tree_cores_all, file = "Data/working/nsn_tree_cores_all.csv")
