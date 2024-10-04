##Dataset used: CHRONOSEQUENCE
#Only used Heather Alexander's intermediate and old aged tree inventory data that became a part of chrono (excluded young for model purposes)
#Got site environmental data from Melanie Jean's Chronosequence dataset
#Both datasets from BNZ archive

##########################
library(reshape)
library(dplyr)
library(tidyverse)
library(data.table)
########### set Working directory
setwd("~/MS THESIS/Final_RawData/SoilDepthModel/Chrono data")

rm(list = ls())


## Load in Dataframes

##As=Heather Alexander's portion of chrono data
datA=read.csv("546_TreeShrubs_IntMat_edit030223.csv") #https://www.lter.uaf.edu/data/data-detail/id/546
datAA=read.csv("801_Jean_SiteEnvironmentalData.csv") #https://www.lter.uaf.edu/data/data-detail/id/801


## Now to work on the Heather Alexander intermediate aged stand from Melanie's Data
names(datA)
names(datA)[names(datA) == 'Site'] <- 'site'
# datub=subset(datA,site=="UBHW2")
names(datA)[names(datA) == 'DBH_cm'] <- 'dbh'
names(datA)[names(datA) == 'BD_cm'] <- 'bd'
datA=datA%>%
  dplyr::select(site:Transect,Subsection:Spp,TS,bd:Alive)
datA$project = "chrono"
datA$project=as.factor(datA$project)

#Get rid of dead trees
#There's only like a dozen rows where alive is blank, I'll just keep them
unique(datA$Alive)
datA=subset(datA,(Alive== "Y" | Alive == "       Y" | Alive=="")) 

#Get rid of shrubs and also seemingly erroneous "PY"
unique(datA$TS)
datA=subset(datA,(TS== "Tree" | TS == "Tree " )) 

#Get rid of salix mislabelled as a tree
unique(datA$Spp)
datA=subset(datA, !(Spp=="SA"))
datA$Spp[datA$Spp == 'BN '] <- 'BN' #Correct name
datA$Spp[datA$Spp == 'PM '] <- 'PM' #Correct name

unique(datA$site)
datA$site[datA$site == 'SC '] <- 'SC' #Correct name

datA$site=as.factor(datA$site)
datA$Spp=as.factor(datA$Spp)
datA$dbh=as.numeric(datA$dbh)

unique(datA$Subsection)
#Get rid of 5m subsections bc they will all get eliminated by dbh cut off anyway and would mess up subplot names
#Also get rid of site UBHW2 because data is irreconcilable with other data- safest bet
#Also get rid of row of data with no subsection- no data in this row
datA=datA%>%
  subset(!(Subsection== "0_5" |Subsection=="5_10"|Subsection=="10_15"|Subsection== "15_20" |Subsection=="20_25"|Subsection=="25_30"|Subsection== "20_30" |Subsection=="30_40"|Subsection=="40_50"|Subsection== "50_60" |Subsection=="0_10"|Subsection=="10_20"|Subsection=="0-10"|Subsection==""))
unique(datA$Subsection)

#Correct remaining subsection names
datA=datA%>%
  mutate(Subsection=ifelse(Subsection=="19-Oct","0_20",
                           ifelse(Subsection=="40_60 ","40_60",                                                               ifelse(Subsection=="90-102"|Subsection=="90-103" | Subsection=="90-104"| Subsection=="90-105","80_100",Subsection))))
unique(datA$Subsection)


datA=separate(datA,Subsection,c(NA,"Subsection"),sep="_")

names(datA)
datA <- unite(datA,plot,site,YearBurn,Transect,sep="",remove=FALSE)

#Correct some plot names
unique(datA$plot)
datA$plot[datA$plot == 'MATDEC1NANA'] <- 'MATDEC1'
datA$plot[datA$plot == 'MATDEC2NANA'] <- 'MATDEC2'
datA$plot[datA$plot == 'MATDEC3NANA'] <- 'MATDEC3'
datA$plot[datA$plot == 'MATDEC4NANA'] <- 'MATDEC4'
datA$plot[datA$plot == 'MATDEC5NANA'] <- 'MATDEC5'
datA$plot[datA$plot == 'TKN103NANA'] <- 'TKN103'
datA$plot[datA$plot == 'TKN117NANA'] <- 'TKN117'
datA$plot[datA$plot == 'TKN125NANA'] <- 'TKN125'
datA$plot[datA$plot == 'TKN137NANA'] <- 'TKN137'
datA$plot[datA$plot == 'TKN139NANA'] <- 'TKN139'
datA$plot[datA$plot == 'UBHW1NANA'] <- 'UBHW1'
datA$plot=as.factor(datA$plot)

datA <- unite(datA,plot,project,plot,sep="_",remove=FALSE)
datA <- unite(datA,subplot,plot,Subsection,sep="_",remove=FALSE)
datA$plot=as.factor(datA$plot)
datA$subplot=as.factor(datA$subplot)





datL=datA
#Get rid of everything that was too small to have a dbh taken or didn't make same threshold as FIA db minimum DBH cut off (DBH>=1" or 2.54cm)
datA1=subset(datA, (dbh>2.54)) 

names(datA1)
datA1=datA1%>%
  dplyr::select(project,plot,subplot,site:Spp,dbh)

unique(datA1$Spp)
# #Ok time for total biomass:
#Healther Alexander 2012 equations except for larch
datA1=datA1%>%
  mutate(biomass=ifelse(Spp=="BN", (164.18*(dbh)^2.29),
                        ifelse(Spp=="PM", (271.46*(dbh)^1.84),
                               ifelse(Spp=="PT", (134.1*(dbh)^2.26),
                                      ifelse(Spp=="PB", (133.71*(dbh)^2.29),
                                             ifelse(Spp=="PG", (96.77*(dbh)^2.4),
                                                    ifelse(Spp=="LL", 1000*(exp(-2.3012+2.3853*log(dbh))),NA)))))))

names(datA1)
datA2=datA1%>%
  group_by(project,plot,site, YearBurn,subplot,Subsection)%>%
  summarise(Biomass.all=sum(biomass))

datA3=datA1%>%
  group_by(project,plot,site, YearBurn,subplot,Subsection,Spp)%>%
  summarise(Biomass.SP=sum(biomass))

datA3 <- spread(datA3, Spp,Biomass.SP)

datA4 <- full_join(datA2, datA3, by = c("project","plot","subplot","Subsection","site","YearBurn"))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.con=sum(LL, PG , PM,na.rm=TRUE))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.dec=sum(BN,PT,PB,na.rm=TRUE))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.bir=sum(BN,na.rm=TRUE))

datA4<- datA4 %>% 
  rowwise%>%
  mutate(biomass.asp=sum(PT,na.rm=TRUE))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.ws=sum(PG,na.rm=TRUE))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.bs=sum(PM,na.rm=TRUE))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.pb=sum(PB,na.rm=TRUE))

#These plot subsections are 10mx2m or 20m^2 so get biomass/m2 by  dividing by 20
datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.all.m2=(Biomass.all/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.con.m2=(biomass.con/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.dec.m2=(biomass.dec/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.bir.m2=(biomass.bir/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.asp.m2=(biomass.asp/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.ws.m2=(biomass.ws/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.bs.m2=(biomass.bs/20))

datA4<- datA4 %>%
  rowwise%>%
  mutate(biomass.pop.m2=(biomass.pb/20))

datA4<- datA4%>%
  rowwise%>%
  mutate(RelBiomass.dec=(biomass.dec/Biomass.all))

datA4<- datA4 %>%
  rowwise%>%
  mutate(RelBiomass.bir=(biomass.bir/Biomass.all))

datA4<- datA4 %>%
  rowwise%>%
  mutate(RelBiomass.asp=(biomass.asp/Biomass.all))

datA4<- datA4 %>%
  rowwise%>%
  mutate(RelBiomass.pop=(biomass.pb/Biomass.all))

names(datA4)
datA4=datA4%>%
  dplyr::select(project:Subsection,biomass.all.m2:RelBiomass.pop)

##Now density
names(datA1)
datA6=datA1%>%
  rowwise%>%
  group_by(plot, subplot, project,Subsection,site, YearBurn,Spp)%>%
  summarise(Stems.SP=n())


datA5=datA1%>%
  rowwise%>%
  group_by(plot,subplot, project,site,YearBurn,Subsection)%>%
  summarise(Stems.all=n())


datA6 <- spread(datA6, Spp,Stems.SP)

datA7 <- full_join(datA5, datA6, by = c("project","plot","subplot","Subsection","site","YearBurn"))

names(datA7)
datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.con=sum(LL , PG , PM,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.dec=sum(BN,PT,PB,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.bir=sum(BN,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.asp=sum(PT,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.ws=sum(PG,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.bs=sum(PM,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(Stems.pb=sum(PB,na.rm=TRUE))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.all.m2=(Stems.all/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.con.m2=(Stems.con/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.dec.m2=(Stems.dec/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.bir.m2=(Stems.bir/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.asp.m2=(Stems.asp/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.ws.m2=(Stems.ws/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.bs.m2=(Stems.bs/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(density.pop.m2=(Stems.pb/20))

datA7<- datA7 %>%
  rowwise%>%
  mutate(RelDensity.dec=(Stems.dec/Stems.all))

datA7<- datA7 %>%
  rowwise%>%
  mutate(RelDensity.asp=(Stems.asp/Stems.all)) 

datA7<- datA7 %>%
  rowwise%>%
  mutate(RelDensity.bir=(Stems.bir/Stems.all))

datA7<- datA7 %>%
  rowwise%>%
  mutate(RelDensity.pop=(Stems.pb/Stems.all))

names(datA7)
datA7=datA7%>%
  dplyr::select(plot:Subsection,density.all.m2:RelDensity.pop)

datA8 <- full_join(datA7, datA4, by = c("project","plot","subplot","Subsection","site","YearBurn"))
names(datA8)
str(datA8)
datA8$Subsection=as.factor(datA8$Subsection)

## Calculate MM Deciduous index
datA8$DECID_INDEX = (((datA8$RelBiomass.dec + datA8$RelDensity.dec)/2)*100)
datA8$BIRCH_INDEX = (((datA8$RelBiomass.bir + datA8$RelDensity.bir)/2)*100)
datA8$ASPEN_INDEX = (((datA8$RelBiomass.asp + datA8$RelDensity.asp)/2)*100)
datA8$POP_INDEX = (((datA8$RelBiomass.pop + datA8$RelDensity.pop)/2)*100)

names(datA8)
datA8=datA8%>%
  dplyr::select(plot:density.pop.m2,biomass.all.m2:biomass.pop.m2,DECID_INDEX:POP_INDEX)
##Ok tree inv data is good, time to wrangle this site data


str(datAA)
datAA$project="chrono"
datAA<- unite(datAA,plot,project,SiteCode,sep="_",remove=FALSE)
datAA$plot=as.factor(datAA$plot)
unique(datAA$plot)
unique(datA8$plot)

names(datAA)
datAA=datAA%>%
  dplyr::select(plot:SiteCode,FireName:Age,Latitude:ElevationMax,MoistureClass:OLDepth,project)

#Match site data to sites we have tree data for
datAA1 <- left_join(datA8, datAA, by = c("plot","project"))

names(datAA1)
#Using common 1-3 moisture class for decid soil model to  merge disparate sampling
datAA1=mutate(datAA1, MoistClass=ifelse(MoistureClass > 4,
                                        "3",
                                        ifelse(MoistureClass<=4 & MoistureClass >2,
                                               "2",
                                               ifelse(MoistureClass<=2,"1","NA"))))




names(datAA1)
datAA1=datAA1%>%
  dplyr::select(project,plot:subplot,site,Subsection:ElevationMax,OLDepth:MoistClass)

names(datAA1)
names(datAA1)[names(datAA1) == 'FireName'] <- 'burn'
names(datAA1)[names(datAA1) == 'BurnYear'] <- 'burn_yr'
names(datAA1)[names(datAA1) == 'YearSampled'] <- 'samp_yr'
names(datAA1)[names(datAA1) == 'Age'] <- 'StdAge'
names(datAA1)[names(datAA1) == 'Latitude'] <- 'latitude'
names(datAA1)[names(datAA1) == 'Longitude'] <- 'longitude'
names(datAA1)[names(datAA1) == 'Aspect'] <- 'aspect'
names(datAA1)[names(datAA1) == 'SlopeDegree'] <- 'slope'
names(datAA1)[names(datAA1) == 'ElevationMax'] <- 'elevation'
names(datAA1)[names(datAA1) == 'OLDepth'] <- 'solCM'

str(datAA1)

datAA1$project=as.factor(datAA1$project)
datAA1$burn=as.factor(datAA1$burn)
datAA1$burn_yr=as.numeric(datAA1$burn_yr)
datAA1$samp_yr=as.numeric(datAA1$samp_yr)
datAA1$StdAge=as.numeric(datAA1$StdAge)
datAA1$aspect=as.numeric(datAA1$aspect)
datAA1$MoistClass=as.numeric(datAA1$MoistClass)


names(datAA1)
datAA1=datAA1%>%
  dplyr::select(project:subplot,density.all.m2:POP_INDEX,burn:MoistClass)
## Ok at this point Melanie Jean Data is ready to go

names(datAA1)
names(datAA1)[names(datAA1) == 'density.all.m2'] <- 'density'
names(datAA1)[names(datAA1) == 'density.con.m2'] <- 'density.con'
names(datAA1)[names(datAA1) == 'density.dec.m2'] <- 'density.dec'
names(datAA1)[names(datAA1) == 'density.bir.m2'] <- 'density.br'
names(datAA1)[names(datAA1) == 'density.asp.m2'] <- 'density.as'
names(datAA1)[names(datAA1) == 'density.pop.m2'] <- 'density.bp'
names(datAA1)[names(datAA1) == 'density.ws.m2'] <- 'density.ws'
names(datAA1)[names(datAA1) == 'density.bs.m2'] <- 'density.bs'
names(datAA1)[names(datAA1) == 'biomass.all.m2'] <- 'biomass'
names(datAA1)[names(datAA1) == 'biomass.con.m2'] <- 'biomass.con'
names(datAA1)[names(datAA1) == 'biomass.dec.m2'] <- 'biomass.dec'
names(datAA1)[names(datAA1) == 'biomass.asp.m2'] <- 'biomass.as'
names(datAA1)[names(datAA1) == 'biomass.bir.m2'] <- 'biomass.br'
names(datAA1)[names(datAA1) == 'biomass.ws.m2'] <- 'biomass.ws'
names(datAA1)[names(datAA1) == 'biomass.pop.m2'] <- 'biomass.bp'
names(datAA1)[names(datAA1) == 'biomass.bs.m2'] <- 'biomass.bs'


#Make Canopy Dominance categories
datAA1=datAA1%>%
  mutate(DCM.cat=ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                        ifelse(DECID_INDEX>=66, "deciduous", "conifer")))

datAA1=datAA1%>%
  mutate(ABMC.cat=ifelse(DECID_INDEX<33, "conifer", 
                         ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                                ifelse(DECID_INDEX>=66&ASPEN_INDEX>BIRCH_INDEX&ASPEN_INDEX>POP_INDEX,"aspen",
                                       ifelse(DECID_INDEX>=66&BIRCH_INDEX>ASPEN_INDEX&BIRCH_INDEX>POP_INDEX,"birch","poplar")))))

names(datAA1)

datAA2=datAA1%>%
  dplyr::select(project,plot,subplot,burn,latitude,longitude,aspect,slope,elevation,MoistClass,StdAge,solCM,biomass,biomass.dec,biomass.con,biomass.as,biomass.br,biomass.bp,biomass.bs,biomass.ws,density,density.dec,density.con,density.as,density.br,density.bp,density.bs,density.ws,DECID_INDEX,ASPEN_INDEX,BIRCH_INDEX,POP_INDEX,DCM.cat,ABMC.cat)

unique(datAA2$burn)

#Fix burn scar names
datAA2$burn=as.character(datAA2$burn)
datAA2=datAA2%>%
  mutate(burn=ifelse(plot=="chrono_BD692","Big Denver",
              ifelse(plot=="chrono_BG474","Big Gerstle",
              ifelse(plot=="chrono_CD582","Chena Dome",
              ifelse(plot=="chrono_CD583","Chena Dome",
              ifelse(plot=="chrono_GC872","Granite Creek",
              ifelse(plot=="chrono_GM542","Granite Mountain",
              ifelse(plot=="chrono_GS662","Goldstream",
              ifelse(plot=="chrono_SM573","Sawtooth Mountain",
              ifelse(plot=="chrono_LG581","Livengood",
              ifelse(plot=="chrono_LG582","Livengood",
              ifelse(plot=="chrono_LG583","Livengood",
              ifelse(plot=="chrono_MATDEC1",NA,
              ifelse(plot=="chrono_MATDEC2",NA,
              ifelse(plot=="chrono_MATDEC3",NA,
              ifelse(plot=="chrono_MATDEC4",NA,
              ifelse(plot=="chrono_MATDEC5",NA,
              ifelse(plot=="chrono_WF714","Wickersham Dome",burn))))))))))))))))))

datAA2b=subset(datAA2,!is.na(solCM))

write.csv(datAA2b,"Chrono_cleaned030523.csv",row.names=FALSE)

#Pull out dat for C pool model
names(datAA2)
datpools=datAA2%>%
  dplyr::select(project:subplot,DCM.cat:ABMC.cat)
# names(datub)
# unique(datub$TS)
# datub=subset(datub,TS=="Tree")
# unique(datub$Alive)
# datub=subset(datub,Alive=="Y")
# datub=subset(datub,DBH_cm>=2.54)
write.csv(datpools,"HA_treecats_forCpools.csv",row.names=FALSE)



#### Below this point is re-doing with BD for soil C pool model
datL=datA
names(datL)
datL=datL%>%
  dplyr::select(project,plot,subplot,site:Spp,bd:dbh)

unique(datL$Spp)
# #Ok time for total biomass:
#Heather Alexander 2012 equations except for larch (used Jenkins larch formula for larch with dbh, used black spruce for larch with bd)
datL=datL%>%
  mutate(biomass=ifelse(Spp=="BN"& !is.na(dbh),(164.18*(dbh)^2.29),
                 ifelse(Spp=="BN"& is.na(dbh),(26.29*(bd)^2.68),
                 ifelse(Spp=="PM"& !is.na(dbh),(271.46*(dbh)^1.84),
                 ifelse(Spp=="PM"& is.na(dbh),(37.29*(bd)^2.4),
                 ifelse(Spp=="PT"& !is.na(dbh),(134.1*(dbh)^2.26),
                 ifelse(Spp=="PT"& is.na(dbh),(56.83*(bd)^2.49),
                 ifelse(Spp=="PB"& !is.na(dbh),(133.71*(dbh)^2.29),
                 ifelse(Spp=="PB"& is.na(dbh),(58.29*(bd)^2.44),
                 ifelse(Spp=="PG"& !is.na(dbh),(96.77*(dbh)^2.4),
                 ifelse(Spp=="PG"& is.na(dbh),(53.74*(bd)^2.45),
                 ifelse(Spp=="LL"& !is.na(dbh),(1000*(exp(-2.3012+2.3853*log(dbh)))),
                 ifelse(Spp=="LL"& is.na(dbh),(37.29*(bd)^2.4),NA)))))))))))))

names(datL)
datL2=datL%>%
  group_by(project,plot,site, YearBurn,subplot,Subsection)%>%
  summarise(Biomass.all=sum(biomass))

datL3=datL%>%
  group_by(project,plot,site, YearBurn,subplot,Subsection,Spp)%>%
  summarise(Biomass.SP=sum(biomass))

datL3 <- spread(datL3, Spp,Biomass.SP)

datL4 <- full_join(datL2, datL3, by = c("project","plot","subplot","Subsection","site","YearBurn"))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.con=sum(LL, PG , PM,na.rm=TRUE))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.dec=sum(BN,PT,PB,na.rm=TRUE))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.bir=sum(BN,na.rm=TRUE))

datL4<- datL4 %>% 
  rowwise%>%
  mutate(biomass.asp=sum(PT,na.rm=TRUE))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.ws=sum(PG,na.rm=TRUE))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.bs=sum(PM,na.rm=TRUE))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.pb=sum(PB,na.rm=TRUE))

#These plot subsections are 10mx2m or 20m^2 so get biomass/m2 by  dividing by 20
datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.all.m2=(Biomass.all/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.con.m2=(biomass.con/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.dec.m2=(biomass.dec/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.bir.m2=(biomass.bir/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.asp.m2=(biomass.asp/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.ws.m2=(biomass.ws/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.bs.m2=(biomass.bs/20))

datL4<- datL4 %>%
  rowwise%>%
  mutate(biomass.pop.m2=(biomass.pb/20))

datL4<- datL4%>%
  rowwise%>%
  mutate(RelBiomass.dec=(biomass.dec/Biomass.all))

datL4<- datL4 %>%
  rowwise%>%
  mutate(RelBiomass.bir=(biomass.bir/Biomass.all))

datL4<- datL4 %>%
  rowwise%>%
  mutate(RelBiomass.asp=(biomass.asp/Biomass.all))

datL4<- datL4 %>%
  rowwise%>%
  mutate(RelBiomass.pop=(biomass.pb/Biomass.all))

names(datL4)
datL4=datL4%>%
  dplyr::select(project:Subsection,biomass.all.m2:RelBiomass.pop)

##Now density
names(datL)
datL6=datL%>%
  rowwise%>%
  group_by(plot, subplot, project,Subsection,site, YearBurn,Spp)%>%
  summarise(Stems.SP=n())


datL5=datL%>%
  rowwise%>%
  group_by(plot,subplot, project,site,YearBurn,Subsection)%>%
  summarise(Stems.all=n())


datL6 <- spread(datL6, Spp,Stems.SP)

datL7 <- full_join(datL5, datL6, by = c("project","plot","subplot","Subsection","site","YearBurn"))

names(datL7)
datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.con=sum(LL , PG , PM,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.dec=sum(BN,PT,PB,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.bir=sum(BN,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.asp=sum(PT,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.ws=sum(PG,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.bs=sum(PM,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(Stems.pb=sum(PB,na.rm=TRUE))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.all.m2=(Stems.all/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.con.m2=(Stems.con/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.dec.m2=(Stems.dec/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.bir.m2=(Stems.bir/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.asp.m2=(Stems.asp/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.ws.m2=(Stems.ws/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.bs.m2=(Stems.bs/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(density.pop.m2=(Stems.pb/20))

datL7<- datL7 %>%
  rowwise%>%
  mutate(RelDensity.dec=(Stems.dec/Stems.all))

datL7<- datL7 %>%
  rowwise%>%
  mutate(RelDensity.asp=(Stems.asp/Stems.all)) 

datL7<- datL7 %>%
  rowwise%>%
  mutate(RelDensity.bir=(Stems.bir/Stems.all))

datL7<- datL7 %>%
  rowwise%>%
  mutate(RelDensity.pop=(Stems.pb/Stems.all))

names(datL7)
datL7=datL7%>%
  dplyr::select(plot:Subsection,density.all.m2:RelDensity.pop)

datL8 <- full_join(datL7, datL4, by = c("project","plot","subplot","Subsection","site","YearBurn"))
names(datL8)
str(datL8)
datL8$Subsection=as.factor(datL8$Subsection)

## Calculate MM Deciduous index
datL8$DECID_INDEX = (((datL8$RelBiomass.dec + datL8$RelDensity.dec)/2)*100)
datL8$BIRCH_INDEX = (((datL8$RelBiomass.bir + datL8$RelDensity.bir)/2)*100)
datL8$ASPEN_INDEX = (((datL8$RelBiomass.asp + datL8$RelDensity.asp)/2)*100)
datL8$POP_INDEX = (((datL8$RelBiomass.pop + datL8$RelDensity.pop)/2)*100)

names(datL8)
datL8=datL8%>%
  dplyr::select(plot:density.pop.m2,biomass.all.m2:biomass.pop.m2,DECID_INDEX:POP_INDEX)
##Ok tree inv data is good, time to wrangle this site data

toplot=datL8
# 
#Make Canopy Dominance categories
datL8=datL8%>%
  mutate(DCM.cat=ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                        ifelse(DECID_INDEX>=66, "deciduous", "conifer")))

datL8=datL8%>%
  mutate(ABMC.cat=ifelse(DECID_INDEX<33, "conifer", 
                         ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                                ifelse(DECID_INDEX>=66&ASPEN_INDEX>BIRCH_INDEX&ASPEN_INDEX>POP_INDEX,"aspen",
                                       ifelse(DECID_INDEX>=66&BIRCH_INDEX>ASPEN_INDEX&BIRCH_INDEX>POP_INDEX,"birch","poplar")))))

names(datL8)

datL9=datL8%>%
  dplyr::select(project,plot,subplot,DCM.cat,ABMC.cat)
# 
names(datL9)
datL9=subset(datL9,!is.na(DCM.cat))

names(datAA2b)
dal=datAA2b%>%
  dplyr::select(plot:subplot,StdAge)
datL10=full_join(datL9,dal)
datL10=subset(datL10,!is.na(DCM.cat))

write.csv(datL10,"HA_treecats_forCpools_alltrees.csv",row.names=FALSE)


##Redo tree cats for C pools, get stand cat for averaged to plot level (for comparison to pub'd data)
names(toplot)
toplot=toplot%>%
  group_by(plot)%>%
  mutate(DECID_INDEX=mean(DECID_INDEX))%>%
  mutate(BIRCH_INDEX=mean(BIRCH_INDEX))%>%
  mutate(ASPEN_INDEX=mean(ASPEN_INDEX))%>%
  mutate(POP_INDEX=mean(POP_INDEX))
toplot=toplot%>%
  dplyr::select(plot,DECID_INDEX:POP_INDEX)
toplot=unique(toplot)
#Make Canopy Dominance categories
toplot=toplot%>%
  mutate(DCM.cat=ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                        ifelse(DECID_INDEX>=66, "deciduous", "conifer")))

toplot=toplot%>%
  mutate(ABMC.cat=ifelse(DECID_INDEX<33, "conifer", 
                         ifelse(DECID_INDEX>=33&DECID_INDEX<66, "mixed",
                                ifelse(DECID_INDEX>=66&ASPEN_INDEX>BIRCH_INDEX&ASPEN_INDEX>POP_INDEX,"aspen",
                                       ifelse(DECID_INDEX>=66&BIRCH_INDEX>ASPEN_INDEX&BIRCH_INDEX>POP_INDEX,"birch","poplar")))))

write.csv(toplot,"HA_treecats_forCpools_alltrees_toplot.csv",row.names=FALSE)
