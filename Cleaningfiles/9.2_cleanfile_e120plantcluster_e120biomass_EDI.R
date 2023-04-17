# Package ID: knb-lter-cdr.273.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/273/10/8cbc85eae79f460bafd377ffc940f0f8" 
# download.file(inUrl1,"Data/data_e120plantcluster_e120biomass.csv",method="auto")
generate_e120_biomass <- function(e120_biomass_raw){
  # targets::tar_load(e120_biomass_raw)
dt1 <-read.csv(e120_biomass_raw,header=F 
               ,skip=1
               ,sep="\t"  
               , col.names=c(
                 "Exp",     
                 "Year",     
                 "Month",     
                 "Plot",     
                 "Strip",     
                 "Substrip",     
                 "Date",     
                 "Species",     
                 "NumSp",     
                 "SpNum",     
                 "FgNum",     
                 "Fgset",     
                 "C3",     
                 "C4",     
                 "Forb",     
                 "Legume",     
                 "Woody",     
                 "Achmi",     
                 "Agrsm",     
                 "Amoca",     
                 "Andge",     
                 "Asctu",     
                 "Elyca",     
                 "Koecr",     
                 "Lesca",     
                 "Liaas",     
                 "Luppe",     
                 "Monfi",     
                 "Panvi",     
                 "Petpu",     
                 "Poapr",     
                 "Queel",     
                 "Quema",     
                 "Schsc",     
                 "Sornu",     
                 "Biomass..paren.g.per.m2.paren.",     
                 "Note"    ), check.names=TRUE)
library(tidyverse)
####
#The aboveground biomass is aggregated into a plot level average 
#Strip 1 is the sorted strip
#Strip 2 is the un-sorted strip 

#I am using years post 2009 to get an average of the biomass once
#the system approaches it appx. equilibrium. 
#There were different sample procedures used earlier in the experiment
#before the plots reached a stable state.
#2009 was not sorted due to limitations and therefore 2010 is a good 
#cutoff to use the following script to clean the data.
#These rules should persist as new data is collected. 
######
#subset years
dat1 <- dt1 %>% filter(Year > 2009)
#generate meta data
meta_e120 <- dat1 %>% 
  select(Plot,NumSp,Fgset,C3,C4,Forb,Legume) %>% 
  distinct()
#create fgset 
meta_e120$Fgset <- str_replace_all(string = meta_e120$Fgset,pattern = "W",replacement = "")
meta_e120$Fgset <- str_replace_all(string = meta_e120$Fgset,pattern = "3|4",replacement = "G")
meta_e120$Fgset <- str_replace_all(string = meta_e120$Fgset,pattern = "GG",replacement = "G")
#get number of functional groups per plot
meta_e120$FgNum <- nchar(meta_e120$Fgset)
#
unique(dat1$Year)
length(unique(dat1$Year))
#check that there is no an additional sampling in June
unique(dat1$Month)
#check that there is only two strips
unique(dat1$Strip)#I do not consider the one NA plot
#
tmp <- dat1 %>% filter(is.na(Strip)==TRUE)
#2013 has the strip order opposite so I subset the data and rearrange it
dat1_tmp <- dat1 %>% filter(Year!=2013)
dat1_tmp2 <- dat1 %>% filter(Year==2013)
#
dat1_tmp2$Strip <- ifelse(dat1_tmp2$Strip==1,2,1)
#
dat2 <- bind_rows(dat1_tmp,dat1_tmp2)
NROW(dat2)==NROW(dat1)
#get strip 1 mean biomass
strip1 <- dat2 %>% filter(Strip==1)
#the general rule is (has been) to remove litter and non-vascular plants
#but to keep the non-planted species
#This rule was confirmed with Dave Tilman 
#the oaks may or may not be removed
#the oaks went mainly extinct due to the frequent burning
remove <- c("Miscellaneous litter","Mosses & lichens","Mosses",
            "Fungi")
"%ni%" <- Negate("%in%")
sort(unique(strip1$Species))
#remove non-vascular plants 
strip1_2 <- strip1 %>% 
  filter(Species %ni% remove)
#calculate the biomass for strip 1 as sum of the sorted strip
strip1_sum <- strip1_2 %>% 
  group_by(Plot,Year) %>% 
  summarise(sum=sum(Biomass..paren.g.per.m2.paren.))
#strip 2
#This is the unsorted strip
strip2 <- dat2 %>% filter(Strip==2)
##
unique(strip2$Species)
##
#tmp <- strip2 %>% filter(Species=="Miscellaneous grasses")
#these two observations are
#removed as I do not know why they were sorted separately
#most likely there was a reason and the should not be included 
##
strip2_1 <-strip2 %>% 
  filter(Species=="Green matter"|
           Species == "Green matter (alive)"|
           Species =="Unsorted biomass") %>% 
  select(Plot,Year,Species,Biomass..paren.g.per.m2.paren.) 
#
table(strip2_1$Year)
#
error <- strip2_1 %>% filter(Year==2012)
error_plot <- table(error$Plot)==2
plot <- error %>% filter(Plot %in% names(error_plot[error_plot==TRUE]))
#these plots have a trivial amount of biomass with the same species name
#that I am including in the plot level average
#
strip2_2 <- strip2_1%>% 
  group_by(Year,Plot)%>%
  summarise(Biomass=sum(Biomass..paren.g.per.m2.paren.))#some plots in 2012 need to be grouped
#
strip2_3 <- strip2_2 %>% select(Plot,Year,Biomass)
table(strip2_3$Year)
table(strip1_sum$Year)
all(table(strip2_3$Year)==table(strip1_sum$Year))==TRUE
#
both <- left_join(strip1_sum,strip2_3)
unique(dat1$Year)
#create plot level means
out <- both %>%
  rowwise() %>%
  mutate(AbvBioAnnProd_20102018mean=round(mean(c(sum, Biomass)),3)) %>% 
  select(Plot,Year,AbvBioAnnProd_20102018mean) %>% 
  group_by(Plot) %>% 
  summarise(AbvBioAnnProd_20102018mean=round(mean(AbvBioAnnProd_20102018mean),2))
#
out <- left_join(out,meta_e120)
write_csv(x = out,file = "Data_derived/data_e120plantcluster_e120biomass.csv")
return("Data_derived/data_e120plantcluster_e120biomass.csv")
}