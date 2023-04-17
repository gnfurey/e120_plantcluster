# Package ID: knb-lter-cdr.414.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant traits: Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

get_aaf_e120 <- function(x){
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/414/8/f09cbe352ad7b89c4bd60f5a772dee10"
  download.file(inUrl1,"Data/data_e120plantcluster_RAW_aafe120.csv",method="auto")
  return("Data/data_e120plantcluster_RAW_aafe120.csv")
}
