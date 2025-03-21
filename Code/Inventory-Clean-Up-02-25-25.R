#Load Libraries and Data####
load('RSN/libary_function.RData')
library.func(libs = c('dplyr', 'tidyr', 'ggplot2', 'ggiraph', 'lubridate', 'purrr', 'htmlwidgets'))

##Step 1: Load and Prepare Dataset##############################################
MI <- read.csv("RSN/Data/raw/MasterInventory11.19.23.csv")
Sps.Grw <- read.csv('RSN/Data/Informative/Species_DBH_GrowthRate.csv')
MI.24 <- read.csv('RSN/Data/Raw/All 2024 tree data_QC_011324.csv')

#Get columns ready for both data sets
MI <- MI %>%
  mutate(
    DATE = mdy(DATE),
    Year = year(DATE),
    Year = as.integer(Year),
    DBH = as.numeric(DBH),
    DBH = replace_na(DBH, -8888),
    SITE = toupper(SITE),
    UniqueID = paste(SITE, PLOT, TREE, sep = '_'),
    STATUS = case_when(
      DBH == -7777 ~ 'Standing/Dead',
      DBH == -8888 ~ 'Missing',
      DBH == -9999 ~ 'Fallen/Dead',
      TRUE ~ 'Alive'
    ),
    DBH = ifelse(DBH < 0, NA, DBH),
    ReplaceID = paste(UniqueID, Year, sep = '_')
  ) %>% 
  select(-OfficeNotes, -DAMAGE, -LOCATION, -SEVERITY)

#Prep 2024 Data
New.dat <- MI.24 %>% 
  select(-DATE, -DBH, -NOTES, -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8) %>% 
  mutate(
    TREE = ifelse(is.na(NEW.TREE.TAG), TREE, NEW.TREE.TAG),
    SITE = toupper(SITE),
    UniqueID = paste(SITE, PLOT, TREE, sep = '_')
  ) %>% 
  rename(
    DBH = New.DBH,  
    DATE = DATE..new.,
    NOTES = New.Notes
  ) %>% 
  select(-NEW.TREE.TAG) 

#Fix 2024 data columns
New.dat <- New.dat %>% 
  mutate(
   STATUS = case_when(
      DBH == -7777 ~ 'Standing/Dead',
      DBH == -8888 ~ 'Missing',
      DBH == -9999 ~ 'Fallen/Dead',
      TRUE ~ 'Alive'
    ),
    DATE = ifelse(DATE == 'Jul-24', '7/01/24', DATE),
    DATE = mdy(DATE),
    Year = year(DATE),
    Year = as.integer(Year),
   DBH = ifelse(DBH<0, NA, DBH),
    ReplaceID = paste(UniqueID, Year, sep = '_')
  ) %>% 
  select(-Office.Notes, -DAMAGE, -LOCATION, -SEVERITY)

#Combine the two datasets
MI <- rbind(MI, New.dat) %>% 
  arrange(UniqueID, Year)

key.words <- c('now', 'new tag', 'new number', 'new #', 'was', 'old#', 'old #', 'actually', '185',
               '238', '245', '6442', '679', '8084')

# Search for notes containing any of the keywords
matching_notes <- MI %>%
  filter(grepl(paste(key.words, collapse = "|"), NOTES, ignore.case = TRUE))

desired.notes <- unique(matching_notes$NOTES) %>% sort()
desired.notes2 <- desired.notes[-c(9, 18:26, 29, 31, 33:35, 39:45, 53, 59:60, 79:82, 88:89,
                                   93:95, 98:103, 111:112, 116, 118:126, 131, 146, 152,
                                   196:199, 202:206, 208, 329:332)]

change.tag <- MI %>% 
  filter(UniqueID %in% UniqueID[NOTES %in% desired.notes2]) %>% 
  arrange(UniqueID, Year)

##Step 2: Detect Missing and Duplicate DBH values###############################
##Begin by identifying duplicates
#Group by year and Unique ID to find duplicates and then identify them by order
# Uncomment if wanting to see step by step, but not necessary
# duplicates <- MI %>% 
#   group_by(UniqueID, Year) %>% 
#   mutate(Duplicate.count = n(),
#          Duplicate.number = row_number(UniqueID),
#          Duplicate = Duplicate.count > 1
#          ) %>% 
#   filter(Duplicate) %>% 
#   select(UniqueID, Year, DBH, Duplicate.number) %>% 
#   ungroup() 
# 
# #View duplicates
# print(duplicates, n = 76) #76 duplicates
# 
# #Search for False missing duplicates
# duplicates <- duplicates %>% 
#   group_by(UniqueID, Year, DBH) %>% 
#   mutate(DBH.Duplicate = n() > 1,
#          False.Missing = ifelse(UniqueID == 'UP1C_3_92'& Year == 2023 & Duplicate.number == 1, 
#                                 FALSE, 
#                                 !DBH.Duplicate & DBH < 0
#                                 )
#          ) %>% 
#   ungroup()  
#                                 
# #View false missing duplicated values
# print(duplicates, n = 76)
# 
# #How many duplicate dbh are there?
# sum(duplicates$DBH.Duplicate)/2 #10
# 
# #Number of false missing
# sum(duplicates$False.Missing) #7



###Fix Stage 1: Remove duplicate DBH entries & False Missing Values#############
MI2 <- MI %>%
  #Identify all duplicates Unique ID that occur in the same year
  group_by(UniqueID, Year) %>% 
  #Count the number of duplicates, rank them by order, and create T/F column
  mutate(Duplicate.count = n(),
         Duplicate.number = row_number(UniqueID),
         Duplicate = Duplicate.count > 1
  ) %>% 
  #Group by DBH values for duplicate entries
  group_by(UniqueID, Year, DBH) %>% 
  #Identify duplicate DBH entries for same Unique ID
  #Also identify false missing duplicates
  mutate(DBH.Duplicate = n() > 1,
         False.Missing = ifelse(UniqueID == 'UP1C_3_92' & Year == 2023 & Duplicate.number == 1, 
                                FALSE, 
                                Duplicate & !DBH.Duplicate & DBH < 0
         )
  ) %>% 
  #Regroup by UniqueID only for final logical
  group_by(UniqueID, Year) %>% 
  #Create a column that determines if the row should be removed or not
  mutate(
    Remove.Field = (DBH.Duplicate & Duplicate.number == 2)|False.Missing
  ) %>% 
  #Ungroup data for final clean
  ungroup() %>% 
  #Filter out duplicates
  filter(!Remove.Field) %>% 
  # #Redo Duplicate T/F field to match
  # group_by(UniqueID, Year) %>% 
  # mutate(
  #   Duplicate = n() > 1
  # ) %>%
  # ungroup() %>%
  #Remove columns conditions for removed duplicates
  select(-Duplicate.count, -DBH.Duplicate, -False.Missing)


##Step 3:Locate Missing and Duplicate Values & Identify Correlations Between Two####
MI2 <- MI2 %>%  
  #Redo Duplicate T/F field to match
  group_by(UniqueID, Year) %>%
  mutate(
    Duplicate = n() > 1
  ) %>%
  group_by(UniqueID) %>%
  arrange(Year) %>% 
  mutate(
    ID.Seq = row_number(UniqueID),
    Missing.Place = case_when(
      DBH < 0 & ID.Seq == min(ID.Seq) ~ 'First',
      DBH < 0 & ID.Seq == max(ID.Seq) ~ 'Last',
      DBH < 0 ~ 'Middle',
      TRUE ~ 'NMis'
    ),
    Duplicate.Place = case_when(
      Duplicate & ID.Seq == min(ID.Seq) ~ 'First',
      Duplicate & ID.Seq == max(ID.Seq) ~ 'Last',
      Duplicate ~ 'Middle',
      TRUE ~ 'NDup'
    )
  ) %>% 
  ungroup()

sum(MI2$Duplicate) #42 Duplicates Remaining

# Initialize counters for the total switches
total_first_switches <- 0
total_last_switches <- 0

# Modify the Missing.Place based on prior and next conditions
MI2 <- MI2 %>% 
  group_by(UniqueID) %>% 
  group_modify(~{
    df <- .x
    
    # Initialize counters for switches in this group
    group_first_switches <- 0
    group_last_switches <- 0
    
    # While loop for both 'Last' and 'First' modifications
    while(any(df$Missing.Place == 'Middle' & lead(df$Missing.Place) == 'Last', na.rm = TRUE) |
          any(df$Missing.Place == 'Middle' & lag(df$Missing.Place) == 'First', na.rm = TRUE)) {
      
      # Apply the changes and count the switches
      df <- df %>% 
        mutate(
          # Change 'Middle' to 'Last' if next is 'Last'
          Missing.Place = replace(Missing.Place,
                                  Missing.Place == 'Middle' & lead(Missing.Place) == 'Last',
                                  'Last'),
          
          
          # Change 'Middle' to 'First' if previous is 'First'
          Missing.Place = replace(Missing.Place,
                                  Missing.Place == 'Middle' & lag(Missing.Place) == 'First',
                                  'First'),
          
        )
      
      
    }
    
    # Count how many 'Middle' were changed to 'Last' and 'First'
    group_last_switches <- max(sum(df$Missing.Place == 'Last') - 1, 0)  # Subtract 1 for the initial 'Last'
    group_first_switches <- max(sum(df$Missing.Place == 'First') - 1, 0)  # Subtract 1 for the initial 'First'
    
    # Update the global counters
    total_first_switches <<- total_first_switches + group_first_switches
    total_last_switches <<- total_last_switches + group_last_switches
    
    return(df)
  }) %>% 
  ungroup()

# Print the total number of switches separately for First and Last
print(paste("Total number of switches to First across all UniqueIDs: ", total_first_switches)) #5 were double missing for first two values??
print(paste("Total number of switches to Last across all UniqueIDs: ", total_last_switches)) #1749 sequential missing values that were the last observation
sum(MI2$Missing.Place == 'First') #83
sum(MI2$Missing.Place == 'Last') #12115
sum(MI2$Missing.Place == 'Middle') #604

###Use this next section to dig into the data to understand the final modifications###
##Need to uncomment to proceed, but not neccessary to fix data##
# #Look at data to verify no match ups
# check.dat <- MI2 %>% 
#   group_by(SITE, PLOT) %>% 
#   arrange(UniqueID, Year) %>% 
#   filter(sum(Duplicate)>0) %>% 
#   ungroup() %>% 
#   select(UniqueID, SITE, PLOT, Year, Duplicate, Duplicate.Place, DBH, SPECIES, NOTES)
# 
# #Identify duplicates that have a species that doesn't match the rest of observations
# check.dat <- check.dat %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Correct.Species = ifelse(Duplicate.Place == 'First',
#                              SPECIES==lead(SPECIES, 2L),
#                              ifelse(Duplicate.Place == 'Middle',
#                                     ifelse(lead(Duplicate),
#                                            SPECIES == lag(SPECIES),
#                                            SPECIES == lead(SPECIES)),
#                                     ifelse(Duplicate.Place == 'Last',
#                                            SPECIES == lag(SPECIES, 2L),
#                                            TRUE))),
#     Correct.Species = replace_na(Correct.Species, TRUE)
#     )
# 
# #Subset correct species
# c.species <- check.dat %>% 
#   subset(Correct.Species)
# 
# #Subset incorrect species
# ic.species <- check.dat %>% 
#   subset(!Correct.Species)
# 
# #Subset those fields and examine 
# matched.missing <- c.species %>%
#   filter(DBH == -9999) %>%  # Keep only rows where DBH is negative
#   semi_join(ic.species, by = c("SITE", "PLOT", "Year", "SPECIES"))  # Match on everything else
# 
# View(matched.missing) #Two missing trees were true matches
# 
# #Subset out matching trees and compare if duplicate fits
# m.species2 <- c.species %>% 
#   filter(UniqueID %in% matched.missing$UniqueID)
# 
# View(m.species2) #One duplicates make sense for the missing value
# 
# #Assess if other incorrect trees are duplicate dbh for other trees in same site/plot/year
# 
# matched_DBH <- semi_join(c.species, ic.species, by = c("SITE", "PLOT", "Year", "SPECIES", "DBH")) #2 match
# 
# View(matched_species)
# 
# #For so few values, this is getting un-necessary
# ##Identify closest DBH value
# c.species <- c.species %>% 
#   group_by(UniqueID, Year) %>% 
#   mutate(
#     Duplicate = n() > 1
#   ) %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Near.DBH = ifelse(Duplicate.Place == 'First',
#                              lead(DBH, 2L)-DBH,
#                              ifelse(Duplicate.Place == 'Middle',
#                                     ifelse(lead(Duplicate.Place) == 'Last',
#                                            DBH - lag(DBH),
#                                            ifelse(lead(Duplicate.Place) == 'Middle',
#                                                        lead(DBH, 2L)-DBH,
#                                                        lead(DBH)-DBH)),
#                                     ifelse(Duplicate.Place == 'Last',
#                                            DBH - lag(DBH, 2L),
#                                            0))),
#     Near.DBH = replace_na(Near.DBH, 0)
#   ) %>% 
#   group_by(UniqueID, Year) %>% 
#   mutate(
#     Is.Negative = any(Near.DBH < 0),
#     Closest = ifelse(Is.Negative,
#                      Near.DBH >= 0,
#                      Near.DBH == min(Near.DBH)),
#     Investigate = Near.DBH < -1000
#   ) %>% 
#   ungroup()
# 
# i.species <- c.species %>% 
#   filter(UniqueID %in% UniqueID[Investigate])
# sum(c.species$Investigate)
# 
# c.species <- c.species %>% 
#   filter(!Investigate)
# 
# #Test if closest values are equal between T/F
# sum(!c.species$Closest)
# sum(c.species$Closest&c.species$Duplicate) #Equal!
# 
# #Remove Duplicates
# f.species <- c.species %>% 
#   filter(Closest) %>% 
#   group_by(UniqueID, Year) %>% 
#   mutate(
#     Duplicate = n() > 1
#   )
# sum(f.species$Duplicate) #0 Remaining


###Fix Stage 2: Remove remaining duplicates and correct middle missing##########
#Manually fix the one matching duplicate and missing value
condition <- MI2$UniqueID == 'MDI2_9_533' & MI2$Year == 2018
condition2 <- MI2$UniqueID == 'MDI2_9_542' & MI2$Year == 2018 & MI2$SPECIES == 'PICMAR'
MI2$DBH[condition] <- MI2$DBH[condition2]
                                                                       
#Go through and fix/remove duplicates                                                                      
MI3 <- MI2 %>% 
  group_by(UniqueID) %>% 
  arrange(UniqueID, Year) %>% 
  mutate(
    Correct.Species = ifelse(Duplicate.Place == 'First',
                             SPECIES==lead(SPECIES, 2L),
                             ifelse(Duplicate.Place == 'Middle',
                                    ifelse(lead(Duplicate),
                                           SPECIES == lag(SPECIES),
                                           SPECIES == lead(SPECIES)),
                                    ifelse(Duplicate.Place == 'Last',
                                           SPECIES == lag(SPECIES, 2L),
                                           TRUE))),
    Correct.Species = replace_na(Correct.Species, TRUE),
    Near.DBH = ifelse(Duplicate.Place == 'First',
                      lead(DBH, 2L)-DBH,
                      ifelse(Duplicate.Place == 'Middle',
                             ifelse(lead(Duplicate.Place) == 'Last',
                                    DBH - lag(DBH),
                                    ifelse(lead(Duplicate.Place) == 'Middle',
                                           lead(DBH, 2L)-DBH,
                                           lead(DBH)-DBH)),
                             ifelse(Duplicate.Place == 'Last',
                                    DBH - lag(DBH, 2L),
                                    0))),
    Near.DBH = replace_na(Near.DBH, 0)
  ) %>% 
  group_by(UniqueID, Year) %>% 
  mutate(
    Is.Negative = any(Near.DBH < 0),
    Closest = ifelse(Is.Negative,
                     Near.DBH >= 0,
                     Near.DBH == min(Near.DBH)),
    Investigate = Near.DBH < -1000
  ) %>% 
  ungroup() %>% 
  filter(Correct.Species & UniqueID %in% UniqueID[!Investigate] & Closest) %>% 
  group_by(UniqueID, Year) %>% 
  mutate(Duplicate = n() > 1) %>% 
  ungroup()

sum(MI3$Duplicate) #There are now 0 duplicates remaining!

#Separate missing/dead observations that are first or last observation and save
##Fix missing observations that are in between non-missing observations linearly 
Missing.data <- MI3 %>% 
  filter(Missing.Place %in% c('First', 'Last'))

write.csv(Missing.data, 
          'RSN/Data/working/BNZ-Missing_Dead-Trees.csv', 
          row.names = FALSE)

#Change dead and missing DBH values to NA
MI3 <- MI3 %>%
  mutate(DBH = replace(DBH, DBH <= 0, NA_real_))

#Fix middle missing values between to known values
MI4 <- MI3 %>%
  group_by(UniqueID) %>%
  arrange(Year) %>%  # Ensure ordering before processing
  mutate(
    # Fill 'Middle' DBH using the average of the neighboring values
    DBH = if_else(Missing.Place == 'Middle', (lag(DBH) + lead(DBH)) / 2, DBH),
    
    # Update Missing.Place if 'Middle' has been filled
    Missing.Place = replace(Missing.Place, Missing.Place == 'Middle' & !is.na(DBH), 'NMis')
  ) %>%
  ungroup() %>% 
  select(-Duplicate.number, -Duplicate, -Duplicate.Place, -Remove.Field, 
         -Correct.Species, -Near.DBH, -Is.Negative, -Closest, -Investigate)

#Some middle values are consecutive and need further work
sum(MI4$Missing.Place == 'Middle') #42 left

#Investigate further
missing.dat <- MI4 %>% 
  filter(MI4$UniqueID %in% MI4$UniqueID[MI4$Missing.Place == 'Middle'& is.na(MI4$DBH)]) %>% 
  arrange(UniqueID, Year)

#Determine average growth for individuals with multiple missing values
missing.dat <- missing.dat %>% 
  group_by(UniqueID) %>% 
  arrange(UniqueID, Year) %>% 
  mutate(
    # Get max and min DBH values, handling NA cases
    max_dbh = max(DBH, na.rm = TRUE),
    min_dbh = min(DBH, na.rm = TRUE),
    
    # Get years corresponding to max and min DBH values, filtering out NA cases
    max_year = max(Year[DBH == max_dbh & !is.na(DBH)], na.rm = TRUE),
    min_year = min(Year[DBH == min_dbh & !is.na(DBH)], na.rm = TRUE),
    
    # Compute Average Growth safely
    Average.Growth = ifelse(is.finite(max_dbh) & is.finite(min_dbh) & max_year != min_year, 
                            (max_dbh - min_dbh) / (max_year - min_year), 
                            NA_real_)
  ) %>%
  select(-max_dbh, -min_dbh, -max_year, -min_year) %>% # Drop temp variables
  ungroup()

#Sequentially fix each missing value by adding the average growth/year to the previous value
for(i in 1:nrow(missing.dat)){
  #Calculate Year Difference
  Year.Dif <- missing.dat$Year[i]-missing.dat$Year[i-1]
  
  #Calculate total growth per year difference
  Growth <- missing.dat$Average.Growth[i]*Year.Dif
  
  #Update DBH of middle missing values to be a linear tragectory
  missing.dat$DBH[i] <- ifelse(missing.dat$Missing.Place[i] == 'Middle', 
                       missing.dat$DBH[i-1] + Growth, 
                       missing.dat$DBH[i])
}

#Round the data
missing.dat$DBH <- missing.dat$DBH %>% round(2)

#Count how many are left
sum(missing.dat$Missing.Place == 'Middle'& is.na(missing.dat$DBH)) #0 are left

#Take the fixed values and enter them into original dataset
MI5 <- MI4 %>%
  mutate(
    DBH = if_else(UniqueID %in% missing.dat$UniqueID & 
                    Missing.Place == "Middle", 
                  missing.dat$DBH[match(UniqueID, missing.dat$UniqueID)], 
                  DBH),
    Missing.Place = replace(Missing.Place, Missing.Place == 'Middle' & !is.na(DBH), 'NMis')
  )

#Count how many middle missing values there are
sum(MI5$Missing.Place == 'Middle') ##All middle dead/missing observations have been corrected

#Investigate dead/missing trees that are first observations
F.Missing <- MI5 %>% 
  filter(UniqueID %in% UniqueID[Missing.Place == 'First']) %>% 
  arrange(UniqueID, Year) %>% 
  mutate(
    TREE = as.numeric(TREE)
  )

#There are remaining tree observations that are incorrectly labeled
##Separate and save for later
Unk.Trees <- MI5 %>% 
  mutate(
    TREE = as.numeric(TREE)
  ) %>% 
  filter(is.na(TREE)) %>% 
  arrange(UniqueID, Year)

View(Unk.Trees) ##66 observations need to be separated

#Separate weird data and save
Weird.data <- MI5 %>% 
  filter(ReplaceID %in% Unk.Trees$ReplaceID & SPECIES == '?')

write.csv(Weird.data, 'RSN/Data/working/BNZ_PoorLabel_Trees.csv', row.names = FALSE)

#Remove weird data from main data
MI6 <- MI5 %>% 
  filter(!ReplaceID %in% Unk.Trees$ReplaceID & SPECIES != '?')


# ##Step 4: Detect DBH Spike and Dip Anomalies####################################
# #Find obvious decimal errors
# MI6 <- MI6 %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Decimal.Error = if_else(!is.na(DBH) & lag(DBH) >= 1 & !is.na(lag(DBH)) & DBH >= lag(DBH) * 10, TRUE, FALSE)
#   ) %>% 
#   ungroup()
# sum(MI5$Decimal.Error) #8 decimal spikes
# 
# #Remove one decimal place from errors
# MI7 <- MI6 %>% 
#   mutate(
#     DBH = ifelse(Decimal.Error, DBH/10, DBH)
#   )
# 
# #Test if removal was successful
# MI7 <- MI7 %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Decimal.Error = if_else(!is.na(DBH) & lag(DBH) >= 1 & !is.na(lag(DBH)) & DBH >= lag(DBH) * 10, TRUE, FALSE)
#   ) %>% 
#   ungroup()
# 
# #Count decimal errors
# sum(MI7$Decimal.Error) #0 decimal spikes
# 
# #Join the sps growth error to assess spikes
# MI7 <- MI7 %>%
#   left_join(Sps.Grw %>% select(Sps.Code, Max.Growth.Error), 
#             by = c("SPECIES" = "Sps.Code")) %>%
#   mutate(species_growth = Max.Growth.Error) %>%
#   select(-Max.Growth.Error)  # Remove the extra column if not needed
# 
# #Determine spikes and dips
# MI7 <- MI7 %>%
#   group_by(UniqueID) %>%
#   arrange(UniqueID, Year) %>%
#   mutate(
#     dbh_change = c(NA, diff(DBH)),# Yearly differences (change in DBH)
#     dbh_change = replace_na(dbh_change, 0),
#     year_diff = c(NA, diff(Year)),  # Year-to-year differences
#     year_diff = replace_na(year_diff, 0),
#     dbh_year_change = ifelse(!is.finite(dbh_change/year_diff), 0, dbh_change/year_diff),
#     is_spike = dbh_year_change > species_growth & !is.na(lead(dbh_year_change)) & lead(dbh_year_change) <= -0.5,
#     is_spike = !is_spike & dbh_change >= 2 &  !is.na(lead(dbh_change)) & lead(dbh_change) <= -1,
#     is_spike = replace_na(is_spike, FALSE),
#     is_dip = ifelse(lag(is_spike), FALSE, dbh_change < -0.5),
#     is_dip = replace_na(is_dip, FALSE),
#     ) %>% 
#   ungroup() %>% 
#   mutate(
#     # Final anomaly flags and types
#     is_anomalous = is_spike | is_dip,
#     is_anomalous = replace_na(is_anomalous, FALSE),
#     anomalous_type = case_when(
#       is_spike ~ 'Spike', 
#       is_dip ~ 'Dip', 
#       TRUE ~ 'NA'                                                  
#       )
#   )
# 
# #Total dips
# sum(MI7$is_dip) #413
# 
# #Total spikes
# sum(MI7$is_spike) #24
# 
# #Total of dips and spikes
# sum(MI7$is_anomalous) #437
# 
# #Separate anomalous data to further investigate
# anom.dat <- MI7 %>% 
#   filter(UniqueID %in% UniqueID[is_anomalous]) %>% 
#   arrange(UniqueID, Year)
# 
# #See how many dips are a result of a dead tree that has DBH
# sum(anom.dat$is_dip & anom.dat$DA == 0) #Only 3
# 
# #How many of the dips have notes attached with them?
# sum(anom.dat$is_dip & anom.dat$NOTES != '') #70
# 
# #Separate the dips with notes
# weird.dips <- anom.dat %>% 
#   filter(is_dip & NOTES != '') %>% 
#   arrange(UniqueID, Year)
# 
# #Separate the spikes with notes
# weird.spikes <- anom.dat %>% 
#   filter(is_spike & NOTES != '') %>% 
#   arrange(UniqueID, Year)
# 
# #List the notes to search for
# side.notes <- c(unique(weird.dips$NOTES), unique(weird.spikes$NOTES)) %>% 
#   unique()
# 
# #Take out anomolies with notes, as well as their other observations
# MI8 <- MI7 %>% 
#   filter(UniqueID %in% UniqueID[!NOTES %in% side.notes]) %>% 
#   arrange(UniqueID, Year)
# 
# #Put those anomalies in a separate file and save
# look.at.dat <- MI7 %>% 
#   filter(UniqueID %in% UniqueID[NOTES %in% side.notes]) %>% 
#   arrange(UniqueID, Year)
# 
# #Save anomalies with notes for physicall examination later
# write.csv(look.at.dat, 'RSN/Data/working/BNZ_Spike&Dip_Notes.csv', row.names = FALSE)
# 
# #Separate the rest of the anomalous data and their other observations to physically look at in graphs
# anom.dat2 <- MI8 %>% 
#   filter(UniqueID %in% UniqueID[is_anomalous]) %>% 
#   arrange(UniqueID, Year)
# 
# ##Collect site names for the loop
# site.name <- unique(anom.dat2$SITE)
# 
# ##Loop through each site and plot each Plot and the trees within them DBH and save
# for(i in 1:length(site.name)){
#   graph.dat <- subset(anom.dat2, SITE == site.name[i])
#   
#   mi.plot <- graph.dat %>%
#     ggplot(mapping = aes(
#       x = Year,
#       y = DBH, 
#       group = UniqueID
#     )) +
#     geom_line(color = 'black') +
#     geom_point(aes(
#       colour = anomalous_type, 
#       fill = anomalous_type,
#       shape = anomalous_type
#     ), size = 3) + # Adjust size as needed
#     scale_shape_manual(values = c("Dip" = 21, "Spike" = 21, 'NA' = 1)) + # Hollow for NA
#     scale_fill_manual(values = c("Dip" = "red", "Spike" = "blue", 'NA' = 'white')) + # No fill for NA
#     scale_colour_manual(values = c("Dip" = "black", "Spike" = "black", 'NA' = "black")) + # Outline color for points
#     scale_alpha_manual(values = c('Dip' = 1, 'Spike' = 1, 'NA' = 0.01)) + # Transparent for NA
#     ggtitle(paste('Site', site.name[i], 'Raw')) +
#     theme_classic() +
#     facet_wrap(~PLOT)
#   
#   ggsave(mi.plot,
#          file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Raw/Cleaned/', 
#                        site.name[i], '_Error_Original.png'),
#          width = 10, height = 6, units = 'in')
# }
# 
# #Repeat this step until satisfied with identification
# anom.dat3 <- anom.dat2 %>%
#   group_by(UniqueID) %>%
#   arrange(UniqueID, Year) %>%
#   mutate(
#     high_spike = ifelse(
#       !is.na(lead(DBH)) & !is.na(lag(DBH)),
#       DBH > (((lag(DBH) + lead(DBH))/2)+1.5) & abs(lead(dbh_change)) < dbh_change & lag(dbh_change) > -1,
#       !is.na(lag(dbh_change)) & lag(dbh_change) > -1 & dbh_year_change > species_growth*1.5),
#     high_spike = replace_na(high_spike, FALSE),
#     extreme_dip = !is.na(lag(high_spike)) & !lag(high_spike) & dbh_change < -1.5,
#     extreme_dip = replace_na(extreme_dip, FALSE),
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     # Final anomaly flags and types
#     is_anomalous2 = high_spike | extreme_dip,
#     is_anomalous2 = replace_na(is_anomalous2, FALSE),
#     anomalous_type2 = case_when(
#       high_spike ~ 'Extreme Spike', 
#       extreme_dip ~ 'Extreme Dip', 
#       TRUE ~ 'NA'                                                  
#     )
#   ) %>% 
#   filter(UniqueID %in% UniqueID[anomalous_type2 != 'NA']) %>% 
#   arrange(UniqueID, Year)
# 
# ##Collect site names for the loop
# site.name2 <- unique(anom.dat3$SITE)
# 
# ##Loop through each site and plot each Plot and the trees within them DBH and save
# for(i in 1:length(site.name2)){
#   graph.dat <- subset(anom.dat3, SITE == site.name2[i])
#   
#   mi.plot <- graph.dat %>%
#     ggplot(mapping = aes(
#       x = Year,
#       y = DBH, 
#       group = UniqueID
#     )) +
#     geom_line(color = 'black') +
#     geom_point(aes(
#       colour = anomalous_type2, 
#       fill = anomalous_type2,
#       shape = anomalous_type2
#     ), size = 3) + # Adjust size as needed
#     scale_shape_manual(values = c("Extreme Dip" = 21, "Extreme Spike" = 21, 'NA' = 1)) + # Hollow for NA
#     scale_fill_manual(values = c("Extreme Dip" = "red", "Extreme Spike" = "blue", 'NA' = 'white')) + # No fill for NA
#     scale_colour_manual(values = c("Extreme Dip" = "black", "Extreme Spike" = "black", 'NA' = "black")) + # Outline color for points
#     scale_alpha_manual(values = c('Extreme Dip' = 1, 'Extreme Spike' = 1, 'NA' = 0.01)) + # Transparent for NA
#     ggtitle(paste('Site', site.name2[i], 'Raw')) +
#     theme_classic() +
#     facet_wrap(~PLOT)
#   
#   ggsave(mi.plot,
#          file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Raw/Cleaned2/', 
#                        site.name2[i], '_Error_Original.png'),
#          width = 10, height = 6, units = 'in')
# }
# 
# #Find how many are noted as dead and or new growth
# sum(anom.dat3$extreme_dip & anom.dat3$DA == 0) #0
# sum(anom.dat3$extreme_dip & anom.dat3$NOTES != '') #14
# 
# sum(anom.dat3$high_spike & anom.dat3$NOTES != '') #3
# 
# 
# #Investigate notes
# unique(anom.dat3$NOTES[anom.dat3$extreme_dip])
# unique(anom.dat3$NOTES[anom.dat3$high_spike])
# 
# #Separate extreme dip notes for investigation
# extreme.dip <- anom.dat3 %>% 
#   filter(UniqueID %in% UniqueID[(high_spike|extreme_dip) & NOTES != ''])
# 
# #Save for later looking
# write.csv(extreme.dip, 'RSN/Data/working/BNZ-Extremes_with_notes.csv')
# 
# #Remove noted data
# anom.dat4 <- anom.dat3 %>% 
#   filter(UniqueID %in% UniqueID[anomalous_type2 != 'NA' & NOTES == '']) #Remove 80 variables
# 
# #View one more time
# ##Collect site names for the loop
# site.name3 <- unique(anom.dat4$SITE)
# 
# ##Loop through each site and plot each Plot and the trees within them DBH and save
# for(i in 1:length(site.name3)){
#   graph.dat <- subset(anom.dat4, SITE == site.name3[i])
#   
#   mi.plot <- graph.dat %>%
#     ggplot(mapping = aes(
#       x = Year,
#       y = DBH, 
#       group = UniqueID
#     )) +
#     geom_line(color = 'black') +
#     geom_point(aes(
#       colour = anomalous_type2, 
#       fill = anomalous_type2,
#       shape = anomalous_type2
#     ), size = 3) + # Adjust size as needed
#     scale_shape_manual(values = c("Extreme Dip" = 21, "Extreme Spike" = 21, 'NA' = 1)) + # Hollow for NA
#     scale_fill_manual(values = c("Extreme Dip" = "red", "Extreme Spike" = "blue", 'NA' = 'white')) + # No fill for NA
#     scale_colour_manual(values = c("Extreme Dip" = "black", "Extreme Spike" = "black", 'NA' = "black")) + # Outline color for points
#     scale_alpha_manual(values = c('Extreme Dip' = 1, 'Extreme Spike' = 1, 'NA' = 0.01)) + # Transparent for NA
#     ggtitle(paste('Site', site.name3[i], 'Raw')) +
#     theme_classic() +
#     facet_wrap(~PLOT)
#   
#   ggsave(mi.plot,
#          file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Raw/Cleaned3/', 
#                        site.name3[i], '_Error_Original.png'),
#          width = 10, height = 6, units = 'in')
# }
# 
# #Set aside dips with only two observations
# anom.dat4 <- anom.dat4 %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Obs.count = sum(!is.na(DBH))
#   ) %>% 
#   ungroup()
# 
# sum(anom.dat4$Obs.count == 2)
# 
# #Separate the two observations for further investigation
# two.anomalous <- anom.dat4 %>% 
#   filter(Obs.count == 2)
# 
# #Save
# write.csv(two.anomalous, 'RSN/Data/working/BNZ_Dip_Two-Obs.csv', row.names = FALSE)
# 
# #Remove from data
# anom.dat5 <- anom.dat4 %>% 
#   filter(Obs.count != 2)
# 
# #Need to visualize again
# ##Collect site names for the loop
# site.name3 <- unique(anom.dat5$SITE)
# 
# ##Loop through each site and plot each Plot and the trees within them DBH and save
# for(i in 1:length(site.name3)){
#   graph.dat <- subset(anom.dat5, SITE == site.name3[i])
#   
#   mi.plot <- graph.dat %>%
#     ggplot(mapping = aes(
#       x = Year,
#       y = DBH, 
#       group = UniqueID
#     )) +
#     geom_line(color = 'black') +
#     geom_point(aes(
#       colour = anomalous_type2, 
#       fill = anomalous_type2,
#       shape = anomalous_type2
#     ), size = 3) + # Adjust size as needed
#     scale_shape_manual(values = c("Extreme Dip" = 21, "Extreme Spike" = 21, 'NA' = 1)) + # Hollow for NA
#     scale_fill_manual(values = c("Extreme Dip" = "red", "Extreme Spike" = "blue", 'NA' = 'white')) + # No fill for NA
#     scale_colour_manual(values = c("Extreme Dip" = "black", "Extreme Spike" = "black", 'NA' = "black")) + # Outline color for points
#     scale_alpha_manual(values = c('Extreme Dip' = 1, 'Extreme Spike' = 1, 'NA' = 0.01)) + # Transparent for NA
#     ggtitle(paste('Site', site.name3[i], 'Raw')) +
#     theme_classic() +
#     facet_wrap(~PLOT)
#   
#   ggsave(mi.plot,
#          file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Raw/Cleaned3/', 
#                        site.name3[i], '_Error_Original.png'),
#          width = 10, height = 6, units = 'in')
# }
# 
# #Determine the dips that should be fixed vs potential new growth
# anom.dat5 <- anom.dat5 %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     New.Growth = extreme_dip & ifelse(!is.na(lead(DBH)), lead(DBH) < lag(DBH), ID.Seq == Obs.count),
#     Fix.Dip = extreme_dip & !New.Growth
#   )
# 
# #Separate new growth
# new.growth <- anom.dat5 %>% 
#   filter(UniqueID %in% UniqueID[New.Growth]) %>% 
#   arrange(UniqueID, Year)
# 
# #Save to look at later
# write.csv(new.growth, 'RSN/Data/working/BNZ_NewGrowth.csv', row.names = FALSE)
# 
# #Separate new growth from working data
# anom.dat6 <- anom.dat5 %>% 
#   filter(!UniqueID %in% UniqueID[New.Growth]) %>% 
#   arrange(UniqueID, Year)
# 
# #Fix Data
# anom.dat6 <- anom.dat6 %>% 
#   group_by(UniqueID) %>% 
#   arrange(UniqueID, Year) %>% 
#   mutate(
#     Fix = Fix.Dip|high_spike,
#     Fix.DBH = ifelse(Fix, (lead(DBH)+(lag(DBH)))/2, DBH)
#   ) %>% 
#   ungroup()
# 
# #Visualize
# site.name3 <- unique(anom.dat6$SITE)
# 
# ##Loop through each site and plot each Plot and the trees within them DBH and save
# for(i in 1:length(site.name3)){
#   graph.dat <- subset(anom.dat6, SITE == site.name3[i])
#   
#   mi.plot <- graph.dat %>%
#     ggplot(mapping = aes(
#       x = Year,
#       y = Fix.DBH, 
#       group = UniqueID
#     )) +
#     geom_line(color = 'black') +
#     geom_point(aes(
#       colour = anomalous_type2, 
#       fill = anomalous_type2,
#       shape = anomalous_type2
#     ), size = 3) + # Adjust size as needed
#     scale_shape_manual(values = c("Extreme Dip" = 21, "Extreme Spike" = 21, 'NA' = 1)) + # Hollow for NA
#     scale_fill_manual(values = c("Extreme Dip" = "red", "Extreme Spike" = "blue", 'NA' = 'white')) + # No fill for NA
#     scale_colour_manual(values = c("Extreme Dip" = "black", "Extreme Spike" = "black", 'NA' = "black")) + # Outline color for points
#     scale_alpha_manual(values = c('Extreme Dip' = 1, 'Extreme Spike' = 1, 'NA' = 0.01)) + # Transparent for NA
#     ggtitle(paste('Site', site.name3[i], 'Raw')) +
#     theme_classic() +
#     facet_wrap(~PLOT)
#   
#   ggsave(mi.plot,
#          file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Fixed/', 
#                        site.name3[i], '_Fixed.png'),
#          width = 10, height = 6, units = 'in')
# }
# #Everything looks great! Let's save the work for future
# ##Also prepare list of species set aside
# 
# #Save Desired Data
# AF <- anom.dat6 %>% 
#   mutate(
#     DBH = Fix.DBH
#   ) %>% 
#   filter(Fix) %>% 
#   select(ReplaceID, DBH) %>% 
#   arrange(ReplaceID)
# 
# write.csv(AF, 'RSN/Data/Informative/Anomalous_Tree_Fix.csv', row.names = FALSE)
# 
# #Compile list of species to remove from main data
# Remove.Names <- c(new.growth$UniqueID, two.anomalous$UniqueID, extreme.dip$UniqueID, look.at.dat$UniqueID)
# Remove.Names2 <- data.frame(UniqueID = Remove.Names %>% unique())
# 
# #Save the Set Aside Data (SAD)
# SAD <- Remove.Names2
# write.csv(SAD, 'RSN/Data/Informative/Mannually-Assess-Names.csv', row.names = FALSE)

###Fix Stage 3: Correct Data Spikes and Dips####################################
#Find obvious decimal errors
MI7 <- MI6 %>% 
  group_by(UniqueID) %>% 
  arrange(UniqueID, Year) %>% 
  mutate(
    Decimal.Error = if_else(!is.na(DBH) & lag(DBH) >= 1 & !is.na(lag(DBH)) & DBH >= lag(DBH) * 10, TRUE, FALSE),
    DBH = ifelse(Decimal.Error, DBH/10, DBH)
  ) %>% 
  ungroup() %>% 
  select(-Decimal.Error, -Missing.Place)

#Repeat this step until satisfied with identification
MI7 <- MI7 %>%
  group_by(UniqueID) %>%
  arrange(UniqueID, Year) %>%
  mutate(
    dbh_change = c(NA, diff(DBH)),# Yearly differences (change in DBH)
    dbh_change = replace_na(dbh_change, 0),
    obs.count = n(),
    high_spike = case_when(
      !is.na(lead(DBH)) & !is.na(lag(DBH)) ~ 
        (DBH > (((lag(DBH) + lead(DBH))/2)+1.5)) & 
        (abs(lead(dbh_change)) < dbh_change) & (lag(dbh_change) > -1),
      obs.count == 2 & ID.Seq == 2 ~ dbh_change >= 4,
      dbh_change >= 10 ~ TRUE,
      TRUE ~ FALSE
      ),
    high_spike = replace_na(high_spike, FALSE),
    extreme_dip = ifelse(lag(high_spike), FALSE, dbh_change <= -1),
    extreme_dip = replace_na(extreme_dip, FALSE),
    high_spike = ifelse(lag(extreme_dip), FALSE, high_spike),
    New.Growth = extreme_dip & ifelse(!is.na(lead(DBH)), 
                                      lead(DBH) < lag(DBH), 
                                      ID.Seq == obs.count|is.na(lead(DBH))),
    Fix.Dip = extreme_dip & !New.Growth,
    Fix = Fix.Dip|high_spike,
    Last.obs = Fix & (ID.Seq == obs.count|is.na(lead(DBH)))
  ) %>% 
  ungroup() %>% 
  mutate(
    # Final anomaly flags and types
    is_anomalous2 = high_spike | Fix.Dip,
    is_anomalous2 = replace_na(is_anomalous2, FALSE),
    anomalous_type2 = case_when(
      high_spike ~ 'Extreme Spike', 
      Fix.Dip ~ 'Extreme Dip', 
      TRUE ~ 'NA'                                                  
    )
  )

sum(MI7$high_spike) #215
sum(MI7$extreme_dip) #178
sum(MI7$New.Growth) #154
sum(MI7$Fix.Dip) #24
sum(MI7$Fix) #239
sum(MI7$Last.obs) #97

#Distinguish Dips vs New Growth
MI7 <- MI7 %>% 
  group_by(UniqueID, SPECIES) %>% 
  arrange(UniqueID, Year, SPECIES) %>% 
  mutate(
    mean.dbh.change = mean(dbh_change[!Fix.Dip & !high_spike & !is.na(dbh_change)])
    ) %>% 
  ungroup()


#Fix Data
MI8 <- MI7 %>% 
  group_by(UniqueID) %>% 
  arrange(UniqueID, Year) %>% 
  mutate(
    DBH = case_when(
      Fix & !is.na(lag(DBH)) & !is.na(lead(DBH)) ~ (lead(DBH)+(lag(DBH)))/2,
      Fix & Last.obs ~ lag(DBH) + mean.dbh.change,
      TRUE ~ DBH)
  ) %>% 
  ungroup() %>% 
  filter(!UniqueID %in% UniqueID[New.Growth]) 


#New Growth Trees
New.Growth <- MI7 %>% 
  filter(UniqueID %in% UniqueID[New.Growth]) 

write.csv(New.Growth, 'RSN/Data/working/BNZ_NewGrowth.csv', row.names = FALSE)

##Step 5: Visualize Data for any other errors and fix###########################

site.name <- unique(MI$SITE)

##Loop through each site and plot each Plot and the trees within them DBH and save
for(i in 1:length(site.name)){
  graph.dat <- subset(MI, SITE == site.name[i])

  mi.plot <- graph.dat %>%
    ggplot(mapping = aes(
      x = Year,
      y = DBH,
      group = UniqueID
    )) +
    geom_line(aes(color = factor(SPECIES))) +
    geom_point(color = 'black', fill = 'black')+
    ggtitle(paste('Site', site.name[i], 'Original')) +
    theme_classic() +
    facet_wrap(~PLOT)

  ggsave(mi.plot,
         file = paste0('RSN/Data/Informative/DBH_Plot_Adjustment/Raw/',
                       site.name[i], '_Original.png'),
         width = 10, height = 6, units = 'in')
}

##Step 6: Save Data#############################################################
MI9 <- MI8 %>% 
  select(-obs.count, -ID.Seq, -dbh_change, -high_spike, -extreme_dip, -New.Growth,
         -Fix.Dip, -is_anomalous2, -anomalous_type2, -mean.dbh.change, -Fix, -Last.obs, -ReplaceID)

MI9 <- MI9 %>% 
  group_by(SITE, PLOT) %>% 
  mutate(
    Plot.count = n(),
    Plot.size = 100,
    Site.size = Plot.count * Plot.size
  ) %>% 
  ungroup() %>% 
  select(-Plot.count)
write.csv(MI9, 'RSN/Data/working/BNZ_Inventory_ZMCorrected.csv', row.names = FALSE)
#End of Scrip###################################################################






