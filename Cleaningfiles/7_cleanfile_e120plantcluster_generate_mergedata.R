#####
mergefunction <- function(e120_species,
                          chem,
                          cadotte,
                          trydat,
                          datatype){
  ###
  # library(targets)
  # library(tidyverse)
  # source("Functions/00_e120plantcluster_functions.R")
  # tar_load(e120_species)
  # tar_load(e120chem)
  # tar_load(aafe120)
  # tar_load(trydata_clean)
  # chem <- e120chem
  # cadotte <- aafe120
  # trydat <- trydata_clean
  ###
  #read in species list
  e120 <- read.csv(e120_species)
  #get short name
  colnames(e120) <- str_replace(string = colnames(e120),
                               pattern = coll("Specid_5"),
                               replacement = "Species")
  #get only pertinent meta data
  dat.m <- e120 %>% select(Species,funcgroup,
                         funcgroup_noc3,Family,Clade,
                         ccesr_name,Output.Taxon)
  #get chemical traits
  chem_dat <- read.csv(chem)
  #get species mean
  chem_dat.m <- chem_dat %>% group_by(Species) %>% 
    summarise(across(.cols = Aluminum:Zinc,.fns = mean,na.rm=TRUE)) %>% 
    ungroup()
  #join data table
  dat.m <- left_join(dat.m,chem_dat.m)
  #read in local leaf traits 
  aafe120 <- read.csv(file = cadotte)
  #merge together 
  dat.m <- left_join(dat.m,aafe120)
  #####
  try <- read.csv(trydat)
  # try <- trydat
  try$AccSpeciesName <- ifelse(try$AccSpeciesName=="Achillea millefolium",
                               "Achillea millefolium(lanulosa)",try$AccSpeciesName)
  try$AccSpeciesName <- ifelse(try$AccSpeciesName=="Koeleria macrantha",
                               "Koeleria cristata",try$AccSpeciesName)
  #
  try$Species <- get_fiveID(try$AccSpeciesName)
  try$AccSpeciesName <- NULL
  #####
  try.w <- try %>% select(Species,VarName,val) %>% 
    pivot_wider(names_from = "VarName",values_from = "val")
  #####
  dat.m <- left_join(dat.m,try.w)
  #
  colnames(dat.m)
  dat.ml_all <- dat.m %>%
    pivot_longer(Aluminum:StomataConductance_mass,
                 names_to = "VarName",values_to = "val")
  if(datatype=="mean"){
  write_csv(x = dat.m,
            file = "Data_derived/data_e120plantcluster_meantraits.csv")
  return("Data_derived/data_e120plantcluster_meantraits.csv")
  }
  if(datatype=="long"){
    write_csv(x = dat.ml_all,
              "Data_derived/data_e120plantcluster_meantraits_long.csv")
    return("Data_derived/data_e120plantcluster_meantraits_long.csv")
  }
}