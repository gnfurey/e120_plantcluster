# Package ID: knb-lter-cdr.715.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Species trait tissue chemistry: Biodiversity II: Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  George Furey - Norwegian University of Life Science 
# Data set creator:  David Tilman - University of Minnesota 
# Metadata Provider:  George Furey - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

get_tissuechem_e120 <- function(x){
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/715/3/2644f9afd5adb7840c732b1ac3aca6d8" 
download.file(inUrl1,"Data/data_e120plantclust_tissuetrait_meta_edi.csv",method="auto")
return("Data/data_e120plantclust_tissuetrait_meta_edi.csv")
}
#check local file with EDI 
# chem1 <- read.csv("Data/data_e120plantclust_tissuetrait_meta_edi.csv")
# chem2 <- read.csv("Data/data_e120plantclust_tissuetrait_meta.csv")
# all.equal(chem1,chem2)
# 
# 
