###
namemake <- function(trynames,
                     aafenames,
                     e120chem1,datatype){
  library(tidyverse)
  library(targets)
  ######
  # tar_load(e120chem)
  # tar_load(aafe120_name)
  # tar_load(trydata_names)
  # e120chem1=e120chem
  # aafenames=aafe120_name
  # trynames=trydata_names
  # datatype="csv"
  #####
  try <- read.csv(trynames)
  try <- try %>% distinct()
  colnames(try)
  try <- try %>% select(TraitID,TraitNameClean,UnitName,Dataset,VarName)
  colnames(try)
  colnames(try)[2] <- "TraitName"
  #
  #takenfrom
  #https://www.cedarcreek.umn.edu/research/data/dataset?aafe120
  # aafenames <- "Data/aafe120_Traitmeta_clean.csv"
  aafe <- read.csv(aafenames)
  colnames(aafe)
  # aafe$VarName <- NULL
  aafe$OrigVarName <- NULL
  aafe$TraitID <- 000
  aafe$Dataset <- "Local"
  ###
  # e120chem1 <- e120chem
  dat <- read.csv(e120chem1)
  colnames(dat)
  varName <- c("Aluminum","Boron","Calcium",
               "Carbon","Copper","Iron",
               "Magnesium","Manganese","Nitrogen",
               "Phosphorus","Potassium","Sodium",
               "Sulfur","Zinc")
  varName==colnames(dat)[3:16]
  e120chem <- data.frame(VarName=varName)
  e120chem$TraitName <- paste("% Whole Aboveground",e120chem$VarName,"in dry mass",sep=" ")
  e120chem$UnitName <- "Percent"
  e120chem$Dataset <- "Local"
  # e120chem$varName <- NULL
  e120chem$TraitID <- 000
  #
  colnames(aafe)
  colnames(try)
  colnames(e120chem)
  tab <- rbind(aafe,try,e120chem)
  ###
  unique(tab$UnitName)
  tab$UnitName <- ifelse(tab$UnitName=="squareCentimeters","cm2",tab$UnitName)
  tab$UnitName <- ifelse(tab$UnitName=="centimetersSquaredPerGram","cm2 g-1",tab$UnitName)
  ###
  library(gt)
  tab <-  tab %>% arrange(Dataset,TraitName)
  tab %>% arrange(Dataset,TraitName) %>%  gt()
  tab <- tab %>% select(TraitName,VarName,UnitName,TraitID,Dataset)
  #
  tab$Category <- ""
  tab$Category <- ifelse(str_detect(tab$TraitName, 
                                    "Whole Aboveground ")==TRUE,"Chemical",
                         tab$Category)
  tab$Category <- ifelse(tab$VarName=="LeafNitrogen","Chemical",tab$Category)
  #
  tab$VarName
  tab$Category <- ifelse(tab$VarName %in% 
                           c("LeafDelta15Nitrogen",
                             "LeafDelta13Carbon",
                             "StomataConductance_area",
                             "StomataConductance_mass",
                             "LeafPhoto_area",
                             "LeafPhoto_mass"),
                         "Metabolic",tab$Category)
  tab$Category <- ifelse(tab$Category =="",
                         "Morphological",tab$Category)
  tab <- tab %>% select(TraitName,Category,everything()) %>% 
    arrange(Category,Dataset,TraitName)
  # tab %>% gt()
  tabword <- tab 
  tabword <- tabword %>% arrange(Category)
  tabword$TraitID <- NULL
  #
  #
  if(datatype=="csv"){
    write.csv(x = tab,file = "Tables/table_e120plantcluster_Traitnames.csv",
              row.names = FALSE)
  return("Tables/table_e120plantcluster_Traitnames.csv")
  }
  if(datatype=="word"){
    tablefun(mytab = tabword,
             tabname = "Supplemental Table 2" ,
             filename = "Tables/table_e120plantcluster_SupplementalTable2.docx")
    return("Tables/table_e120plantcluster_SupplementalTable2.docx")
  }
}