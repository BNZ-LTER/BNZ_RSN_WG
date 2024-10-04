## cleaning and plotting on Melissa Boyd's cafi_merge dataset to get biomass and density by stand age ##
#this is meant to help with choosing new RSN sites summer 2024
#created by Wk on 2/15/24
#updated by WK on 2/15/24

cafi <- read_csv("Data/raw/CAFI_merge.csv")

#remove all Kenai plots - we only want to work in the interior boreal
cafi <- cafi[cafi$ecoregion != "MARINE WEST COAST FOREST", ]
#this leaves us with 115 sites

#let's save cafi coordinates into separate file rn
cafi_coords <- cafi %>% 
  select(!c(Region_PSP, biomass_kgM2, density_m2, decidBA, conifBA, scientific, surveyyr, ecoregion, ASPC, SLOP, SOMO, SOORCM, SOOICM, SOOECM, SOOACM, ELEV, DF))

#ok, back to our usual
#remove info that is unimportant to us rn
cafi <- cafi %>% 
  select(!c(decidBA, conifBA, Longitude, Latitude, scientific, surveyyr, ecoregion, ASPC, SLOP, SOMO, SOORCM, SOOICM, SOOECM, SOOACM, ELEV, DF))

cafi = cafi %>%
  group_by(StdAge, common, site) %>%
  mutate(row = row_number()) %>%
  summarise(mean_biomass = mean(biomass_kgM2), mean_density = mean(density_m2))

cafi = cafi %>%
  mutate(class = case_when(common == "Alaskan birch" ~ "Decid", common == "Balsam poplar" ~ "Decid",
                            common == "Trembling aspen" ~ "Decid", common == "Black spruce" ~ "Spruce", 
                            common == "White spruce" ~ "Spruce",  common == "Tamarack" ~ "Spruce"))

cafi = cafi %>%
  group_by(StdAge, class, site) %>%
  mutate(row = row_number()) %>%
  summarise(biomass = mean(mean_biomass), density = mean(mean_density))

cafi = cafi %>%
  group_by(StdAge, class, site) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = class, values_from = c(biomass, density), names_sep = "_")

#cafi <= select(cafi, site, class, biomass_Decid, density_Decid)

cafi.a =cafi %>%
  replace(is.na(.),0) %>% 
  mutate(rel.conif.dens=density_Spruce/(density_Spruce+density_Decid), 
         rel.conif.bio=biomass_Spruce/(biomass_Spruce+biomass_Decid), 
         rel.decid.dens=1-rel.conif.dens, 
         rel.decid.bio=1-rel.conif.bio, 
         D.Index=(rel.decid.dens+rel.decid.bio)/2, 
         Class=ifelse(D.Index<0.33, "Spruce", 
                      ifelse(D.Index>0.66, "Decid", "Mixed")))

cafi.b = cafi.a %>%
  mutate(StdAge = case_when(StdAge == 0 ~ NA, TRUE~StdAge))

#join the coordinates back in
cafi.b <- cafi.b %>%
  left_join(cafi_coords, by = "site") %>%
  group_by(site, StdAge.x, Class) %>%
  slice(1) %>%
  ungroup()

cafi.b %>% 
  rename(StdAge = "StdAge.x") %>% 
  select(site, Longitude, Latitude, StdAge, D.Index, Class) -> cafi.b

##this is code from wk_RSN_inventory exploration. it changes cafi column names to be better merged with RSN data
# #read in cafi data
# cafi.b <- read_csv("Data/working/cafi.b.csv")
# cafi.b %>% 
#   mutate(ID = "CAFI",
#          Class = tolower(Class)) %>% 
#   rename(sitename = "site",
#          age_ave = "StdAge", 
#          d.index = "D.Index",
#          class = "Class") %>% 
#   select(ID, sitename, age_ave, d.index, class) -> cafi.c
# cafi.c$sitename=as.character(cafi.c$sitename)

#there are 56 plots with ages
#D.Index v age
f=ggplot(cafi.b, aes(x=StdAge, y=D.Index, color=Class))+geom_point()+theme_bw()
f= f+ labs(x="Stand Age", y="Deciduous Index")+ geom_point(size=3,aes(fill=Class, shape=Class)) + 
  scale_color_manual(values=c("gold1","#77D153FF", "#443983FF"))
f

##edits
f <- ggplot(cafi.b, aes(x = StdAge, y = D.Index, color = Class)) +
  geom_point() +
  theme_bw() +
  labs(x = "Stand Age", y = "Deciduous Index") +
  geom_point(size = 3, aes(fill = Class, shape = Class)) +
  scale_color_manual(values = c("gold1", "#77D153FF", "#443983FF")) +
  scale_x_continuous(breaks = seq(0, 300, by = 10), 
                     limits = c(0, 300), 
                     labels = function(x) ifelse(x %% 50 == 0, as.character(x), ""))  # Setting breaks and labels
f

#decid.bio v age
f=ggplot(cafi.b, aes(x=StdAge, y=biomass_Decid, color=Class))+geom_point()+theme_bw()
f= f+ labs(x="Stand Age", y="Deciduous Biomass")+ geom_point(size=3,aes(fill=Class, shape=Class)) + scale_color_manual(values=c("gold1","#77D153FF", "#443983FF"))
f

#edits
f <- ggplot(cafi.b, aes(x = StdAge, y = biomass_Decid, color = Class)) +
  geom_point() +
  theme_bw() +
  labs(x = "Stand Age", y = "Deciduous Biomass (kg m2)") +
  geom_point(size = 3, aes(fill = Class, shape = Class)) +
  scale_color_manual(values = c("gold1", "#77D153FF", "#443983FF")) +
  scale_x_continuous(breaks = seq(0, 300, by = 10),
                     limits = c(0, 300),
                     labels = function(x) ifelse(x %% 50 == 0, as.character(x), "")) +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10,12))
f

#decid.dens v age
f=ggplot(cafi.b, aes(x=StdAge, y=density_Decid, color=Class))+geom_point()+theme_bw()
f= f+ labs(x="Stand Age", y="Deciduous Density")+ geom_point(size=3,aes(fill=Class, shape=Class)) + scale_color_manual(values=c("gold1","#77D153FF", "#443983FF"))
f

#edits
f <- ggplot(cafi.b, aes(x = StdAge, y = density_Decid, color = Class)) +
  geom_point() +
  theme_bw() +
  labs(x = "Stand Age", y = "Deciduous Density") +
  geom_point(size = 3, aes(fill = Class, shape = Class)) +
  scale_color_manual(values = c("gold1", "#77D153FF", "#443983FF")) +
  scale_x_continuous(breaks = seq(0, 300, by = 10),
                     limits = c(0, 300),
                     labels = function(x) ifelse(x %% 50 == 0, as.character(x), "")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75))
f


write_csv(cafi.b, file = "Data/working/cafi.b.csv")
  