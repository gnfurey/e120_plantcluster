# Package ID: knb-lter-cdr.273.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
get_biomass_e120 <- function(x){
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/273/10/8cbc85eae79f460bafd377ffc940f0f8"
  download.file(inUrl1,"Data/data_e120plantcluster_e120biomass_raw.csv",method="auto")
  return("Data/data_e120plantcluster_e120biomass_raw.csv")
}

