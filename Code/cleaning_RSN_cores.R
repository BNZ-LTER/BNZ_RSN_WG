#quick summary stats of coreprocessing2018 and core_status datafiles, to capture extent of RSN soil sampling
#created by WK 4/25/24
#updated by __________

#this is the datafile from Guelph. it seems like it has ALL permafrost cores collected
rsn_cores <- read_csv("Data/raw/Coring_RSN_20150508.csv")

#check how many sites are in this dataset
n_distinct(rsn_cores$Site)
#32

#smush so that we have one row per core. make a core depth by taking the max number at the end of each core
rsn_cores %>%
  group_by(Site, Core_number) %>%
  summarize(depth = max(Bottom_depth)) -> rsn_cores_sum

#save this file
write_csv(rsn_cores_sum, "Data/working/rsn_cores_summary.csv")

#read in core_status file I have been compiling
rsn_cores.a <- read_csv("Data/working/core_status.csv")

#filter by permafrost cores, count how many sites there are
rsn_cores.a %>%
  filter(core_type == "pf") %>%
  summarise(num_distinct_sites = n_distinct(site)) -> test
#33 pf core sites

#filter by short cores, count how many sites there are
rsn_cores.a %>%
  filter(core_type == "short") %>%
  summarise(num_distinct_sites = n_distinct(site)) -> test
#48 short core sites

#make a df with just pf
rsn_cores.a %>% 
  filter(core_type == "pf") -> rsn_cores_pf

#make a df with just short
rsn_cores.a %>% 
  filter(core_type == "short") -> rsn_cores_short

#make list of pf and short sites
values_pf <- rsn_cores_pf %>% distinct(site)
values_short <- rsn_cores_short %>% distinct(site)

#find sites in pf that are not in short
setdiff(values_pf$site, values_short$site)
#there are 31 sites in pf that are not in short

#find sites in short that are not in pf
setdiff(values_short$site, values_pf$site)
# there are 46 sites in short that are not in pf

#check which sites are shared
shared_sites <- intersect(rsn_cores_pf$site, rsn_cores_short$site)
#the only two shared sites are DC33 and GS-2

