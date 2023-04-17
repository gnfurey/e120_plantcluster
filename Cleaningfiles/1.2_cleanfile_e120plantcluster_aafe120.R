# Package ID: knb-lter-cdr.414.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant traits: Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  David Tilman -
# Metadata Provider:    - Cedar Creek LTER
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/414/8/f09cbe352ad7b89c4bd60f5a772dee10"
# download.file(inUrl1,"Data/data_e120plantclust_RAW_aafe120.csv",method="curl")
cleanaafe120 <- function(x, datatype, aafe_meta1) {
  source("Functions/00_e120plantcluster_functions.R")
  #read in data
  dt1 <- read.csv(x,
    header = F,
    skip = 1,
    sep = "\t",
    col.names = c(
      "Year",
      "Species",
      "Area_of_leaf_blade_cm2",
      "P.per.A",
      "P_A.astrix.L",
      "SLA_cm2.per.g",
      "Seed_weight_.paren.g.paren.",
      "height_.paren.m.paren."
    ), check.names = TRUE
  )
  colnames(dt1)
  #rename annoying variable names 
  colnames(dt1)[colnames(dt1) == "Area_of_leaf_blade_cm2"] <-
    "Local_LeafBladeArea"
  colnames(dt1)[colnames(dt1) == "P.per.A"] <-
    "Local_P.A"
  colnames(dt1)[colnames(dt1) == "P_A.astrix.L"] <-
    "Local_P.A.L"
  colnames(dt1)[colnames(dt1) == "Seed_weight_.paren.g.paren."] <-
    "Local_SeedDryMass"
  colnames(dt1)[colnames(dt1) == "height_.paren.m.paren."] <-
    "Local_Height"
  colnames(dt1)[colnames(dt1) == "SLA_cm2.per.g"] <-
    "Local_SLA_pet_ex"
  colnames(dt1)
  #
  library(tidyverse)
  dt1$Species <- as.character(dt1$Species)
  #use Achmi ccesr name 
  dt1$Species <- ifelse(dt1$Species == "Achillea millefolium",
    "Achillea millefolium(lanulosa)",
    dt1$Species
  )
  dt1$Species
  dt1$Specid_5 <- get_fiveID(dt1$Species)
  dt1$Specid_5
  dt1 <- dt1 %>% filter(is.na(Specid_5) == FALSE)
  dt1$Species <- dt1$Specid_5
  dt1$Specid_5 <- NULL
  dt1$Year <- NULL
  colnames(dt1)
  #
  # write.csv(x=dt1,file = "Data/data_e120plantclust_aafe120.csv",row.names = FALSE)
  #I pulled extra meta data from this website link. 
  # https://www.cedarcreek.umn.edu/research/data/dataset?aafe120
  library(targets)
  # tar_load(aafe_meta)
  # aafe_meta1 <- aafe_meta
  # aafe <- read.csv("Data/aafe120_Traitmeta.csv")
  aafe <- read.csv(aafe_meta1)
  colnames(aafe)
  colnames(aafe) <- c("OrigVarName", "TraitName", "UnitName")
  colnames(aafe)
  aafe$VarName <- ""
  aafe$VarName <- ifelse(aafe$OrigVarName == "Area_of_leaf_blade_cm2",
    "Local_LeafBladeArea", aafe$VarName
  )
  #
  unique(aafe$OrigVarName)
  #
  aafe$VarName <- ifelse(aafe$OrigVarName == "P/A",
    "Local_P.A", aafe$VarName
  )
  #
  aafe$VarName <- ifelse(aafe$OrigVarName == "P_A*L",
    "Local_P.A.L", aafe$VarName
  )
  #
  aafe$VarName <- ifelse(aafe$OrigVarName == "Seed_weight_(g)",
    "Local_SeedDryMass", aafe$VarName
  )
  #
  #
  aafe$VarName <- ifelse(aafe$OrigVarName == "height_(m)",
    "Local_Height", aafe$VarName
  )
  #
  aafe$VarName <- ifelse(aafe$OrigVarName == "SLA_cm2/g",
    "Local_SLA_pet_ex", aafe$VarName
  )
  #
  aafe$TraitName
  #
  aafe[aafe$TraitName == "Average plant height at maturity (m)", "TraitName"] <-
    "Average plant height at maturity (USDA)"
  aafe[aafe$TraitName == "Leaf laminar area (cm^2)", "TraitName"] <-
    "Leaf laminar area (Local)"
  aafe[aafe$TraitName == "Perimeter per area times leaf laminar length (unitless)", "TraitName"] <-
    "Leaf perimeter per area times leaf laminar length (Local)"
  aafe[aafe$TraitName == "permeter per area (cm-1)", "TraitName"] <-
    "Leaf perimeter per area (Local)"
  aafe[aafe$TraitName == "Seed weight (g)", "TraitName"] <-
    "Seed dry mass (Local)"
  aafe[aafe$TraitName == "Specific leaf length (cm^2/g)", "TraitName"] <-
    "Leaf area per leaf dry mass (petiole excluded) (Local)"
  #####
  if (datatype == "data") {
    write.csv(
      x = dt1,
      file = "Data_derived/data_e120plantcluster_aafe120.csv", row.names = FALSE
    )
    return("Data_derived/data_e120plantcluster_aafe120.csv")
  }
  if (datatype == "names") {
    write.csv(
      x = aafe, row.names = FALSE,
      file = "Data_derived/data_e120plantcluster_aafe120_Traitmeta_clean.csv"
    )
    return("Data_derived/data_e120plantcluster_aafe120_Traitmeta_clean.csv")
  }
}
####
