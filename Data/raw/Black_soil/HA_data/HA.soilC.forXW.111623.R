#Start by running everything to line 160 

### Soil C Pool Model

library(reshape)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)


setwd("~/MS THESIS/Final_RawData/SoilDepthModel/CPool_HA")
rm(list=ls())
## Load in soil data####
datA=read.csv("HAlexander_SOILS.csv")
# datB=read.csv("HA_treecats_forCpools.csv") #Use this instead to do trees dbh>1" only
datB=read.csv("HA_treecats_forCpools_alltrees.csv") #This is all trees

names(datA)
names(datB)

names(datA)[names(datA) == 'Site'] <- 'site'
unique(datA$site)
unique(datB$plot)

#Adjust site names to match tree data
datA=subset(datA,!site=="")
datA$project="chrono"
datA <- unite(datA,site,site,Transect,sep="",remove=TRUE)
datA <- unite(datA,plot, project,site,sep="_",remove=FALSE)
datA <- unite(datA,subplot,plot,Subsection,sep="_",remove=FALSE)
names(datA)
unique(datA$Subsection)

unique(datA$plot)
unique(datB$plot)

###### select columns we want
names(datA)
unique(datA$Soil.Type) # 2 is mineral, 3 is org plus mineral, > keep only 1
datA=subset(datA,Soil.Type=="1_OM")

#Can get rid of UBHW2 subsection=0 and subsection=50, because we don't have those in the other data (gets rid of two measures)
datA=subset(datA,!(Subsection=="0"|Subsection=="50"))
names(datA)
datA=datA%>%
  dplyr::select(project,plot,subplot,Horizon:Horizon.Depth..cm.,Soil.Bulk.Density..g.cm3.,Soil.C...,Soil...Coarse..g.C.m.2.) #soilcoarse is soil+coarse

names(datA)
str(datA)
datA$plot=as.factor(datA$plot)
datA$subplot=as.factor(datA$subplot)
datA$project=as.factor(datA$project)
datA$Horizon=as.factor(datA$Horizon)
datA$Horizon.Depth..cm.=as.numeric(datA$Horizon.Depth..cm.)
datA$Soil.Bulk.Density..g.cm3.=as.numeric(datA$Soil.Bulk.Density..g.cm3.)
datA$Soil.C...=as.numeric(datA$Soil.C...)

names(datB)
unique(datB$plot) 
unique(datA$plot)

names(datA)
datA1 <- full_join(datA, datB, by = c("project","plot","subplot"))

#Throw out rows that weren't in both
names(datA1)
datA1=subset(datA1,!is.na(Horizon))
p=datA1
datA1=subset(datA1,!is.na(DCM.cat))
datA1=subset(datA1,!ABMC.cat=="poplar") #Removed 6 rows out of 1400 in dbh version, removed 0 rows in all trees version
#datA1=subset(datA1,!ABMC.cat=="conifer") #Removed conifer data from model

str(datA1)
datA1$DCM.cat=as.factor(datA1$DCM.cat)
datA1$ABMC.cat=as.factor(datA1$ABMC.cat)

#Throw out totals horizons
unique(datA1$Horizon)
datA2=subset(datA1,Horizon=="1_0-5"|Horizon=="2_5-10"|Horizon=="3_10+")

names(datA2)
##New divergence as of 12-7-22: don't need to get avg depths of horizons, j modelling pools by 5cm increments.
##Instead should look at avg depth by forest cat and CI to see if it gels with soil depth model


datA3b=subset(datA2,Horizon.Depth..cm.>0) #don't need
rm(datA1,datA,datB,datA2)

#Look at bulk density compared with %C
names(datA3b)
datA3b=subset(datA3b,!Soil.Bulk.Density..g.cm3.>.5)

datA3b$ABMC.cat<-factor(datA3b$ABMC.cat, levels=c("conifer","mixed", "aspen", "birch"))
g=ggplot(data=subset(datA3b,!ABMC.cat=="conifer"), aes(x=Soil.Bulk.Density..g.cm3.,y=Soil.C...,color=ABMC.cat))+
  geom_point()+
  scale_color_manual(values=c("indianred4","goldenrod1","chocolate3"))+
  theme_bw()+
  geom_smooth(method="lm",se=FALSE)+
  ylab("Organic Soil Carbon Concentration (%)")+
  xlab(expression('Organic Soil Bulk Density (g m'^-2*')'))+
  facet_wrap(~ABMC.cat)
g
g=g+theme(strip.text.x = element_text(size = 14)) +theme(strip.background = element_rect(fill="white", size=0.18))+
  theme(legend.text=element_text(size=14))+
  theme(axis.title.x = element_blank())+ theme(panel.border = element_rect(colour = "black", fill=NA, size=0.18))+
  theme(axis.ticks.length=unit(.05, "cm"), axis.ticks = element_line(colour = "black", size = 0.18)) +
  theme(panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.18))+ guides(color=guide_legend(title="Forest Type"))+theme(legend.title=element_blank()) + theme(legend.position="top", legend.spacing.x = unit(0.1, 'cm'))+
  theme(axis.title.x = element_text( size=14, vjust=1.5), axis.text.x  = element_text(size=14)) +
  theme(axis.title.y = element_text( size=14, vjust=1.5), axis.text.y  = element_text(size=14))+ guides(color=FALSE)+ theme(legend.background = element_rect(size=0.5, linetype="solid", colour ="white"))
g






#First check out avg depths
datA3b= datA3b%>%
  group_by(subplot)%>%
  mutate(totaldepth=sum(Horizon.Depth..cm.))%>%
  mutate(totalC=sum(Soil...Coarse..g.C.m.2.))

d= datA3b%>%
  group_by(ABMC.cat)%>%
  summarize(meanC=mean(totalC))

datA3b=datA3b%>%
  mutate(depthclass=ifelse(totaldepth<=5,"0_5",
                           ifelse(totaldepth>5&totaldepth<=10,"5_10",
                                  ifelse(totaldepth>10,"10+",NA))))

less5=subset(datA3b,depthclass=="0_5"&Horizon=="1_0-5")
less10=subset(datA3b,depthclass=="5_10"&Horizon=="1_0-5"|depthclass=="5_10"&Horizon=="2_5-10")
more10=subset(datA3b,depthclass=="10+"&Horizon=="1_0-5"|depthclass=="10+"&Horizon=="2_5-10"|depthclass=="10+"&Horizon=="3_10+")

x=bind_rows(less5,less10)
x=bind_rows(x,more10)

less5=less5%>%
  group_by(ABMC.cat,Horizon)%>%
  summarize(meandepth=mean(Horizon.Depth..cm.),sedepth=plotrix::std.error(Horizon.Depth..cm.),meanBD=mean(Soil.Bulk.Density..g.cm3.),seBD=plotrix::std.error(Soil.Bulk.Density..g.cm3.),meanpercC=mean(Soil.C...),sepercC=plotrix::std.error(Soil.C...),meanC=mean(Soil...Coarse..g.C.m.2.),seC=plotrix::std.error(Soil...Coarse..g.C.m.2.),n=n())

less10=less10%>%
  group_by(ABMC.cat,Horizon)%>%
  summarize(meandepth=mean(Horizon.Depth..cm.),sedepth=plotrix::std.error(Horizon.Depth..cm.),meanBD=mean(Soil.Bulk.Density..g.cm3.),seBD=plotrix::std.error(Soil.Bulk.Density..g.cm3.),meanpercC=mean(Soil.C...),sepercC=plotrix::std.error(Soil.C...),meanC=mean(Soil...Coarse..g.C.m.2.),seC=plotrix::std.error(Soil...Coarse..g.C.m.2.),n=n())

more10=more10%>%
  group_by(ABMC.cat,Horizon)%>%
  summarize(meandepth=mean(Horizon.Depth..cm.),sedepth=plotrix::std.error(Horizon.Depth..cm.),meanBD=mean(Soil.Bulk.Density..g.cm3.),seBD=plotrix::std.error(Soil.Bulk.Density..g.cm3.),meanpercC=mean(Soil.C...),sepercC=plotrix::std.error(Soil.C...),meanC=mean(Soil...Coarse..g.C.m.2.),seC=plotrix::std.error(Soil...Coarse..g.C.m.2.),n=n())

less5$class="shallow"
less10$class="intermediate"
more10$class="deep"

data=bind_rows(less5,less10)
data=bind_rows(data,more10)


#2####
rm(g,less10,less5,more10,p,x,d,datA3b)
#HERE data= C#s ####