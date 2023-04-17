#
tryclean <- function(x,datatype){
  library(rtry)
  library(tidyverse)
  #invert %in% 
  "%ni%" <- Negate("%in%")
  #import data 
  # dat <- rtry_import(
  #   "Data/19226_27012022110447/19226.txt",
  #   separator = "\t",
  #   encoding = "Latin-1",
  #   quote = "",
  #   showOverview = TRUE
  # )
  dat <- rtry_import(
    x,
    separator = "\t",
    encoding = "Latin-1",
    quote = "",
    showOverview = TRUE
  )
  #remove duplicates uploaded from different databases
  dat_no_dup <- rtry_remove_dup(dat, showOverview = TRUE)
  #"Sometimes R may show a column 28, which should be empty. 
  #This column is then an artefact due to different software (MySQL >> R)."
  dat_no_dup$V28 <- NULL
  #new data
  dat.t <- dat_no_dup
  #
  dat.t$AccSpeciesName <- as.character(dat.t$AccSpeciesName)
  #I requested both Andropogon gerardii and Andropogon gerardi
  dat.t$AccSpeciesName <- ifelse(dat.t$AccSpeciesName=="Andropogon gerardii",
                                 "Andropogon gerardi",dat.t$AccSpeciesName)
  #view unique traits
  sort(unique(dat.t$TraitName))
  #
  colnames(dat.t)
  ####
  #remove NA values for original values
  dat.t1 <- dat.t %>% 
    filter(is.na(OrigValueStr)==FALSE)
  #check removed values
  dat.t1_check <- dat.t %>% 
    filter(is.na(OrigValueStr)==TRUE)
  #View(dat.t1_check)
  #all values also have NA for StdValues
  #
  #get only traits and not other meta data
  dat.t2 <- dat.t1 %>% 
    filter(TraitName!="")
  #check removed values
  dat.t2_check <- dat.t1 %>% 
    filter(TraitName=="")
  unique(dat.t2_check$TraitID)#NA
  #check
  #retain those traits submitted with units
  dat.t3 <- dat.t2 %>% 
    filter(OrigValueStr!="") 
  #those without units 
  dat.t3_check <- dat.t2 %>% 
    filter(OrigValueStr=="") 
  #several traits had std value but no original values
  dat.t3_check_category <- dat.t3_check %>% 
    filter(is.na(StdValue)==TRUE)
  # View(dat.t3_check_category)
  category_traits <- unique(dat.t3_check_category$TraitID)
  sort(unique(dat.t3_check_category$TraitName))
  #
  dat.t4 <- dat.t2 %>% 
    filter(TraitID %ni% category_traits)
  #in dat.t4 that were removed by filter(OrigValueStr!="")
  #in t4 but not in t3
  diff_dat.t4_t3 <- setdiff(dat.t4,dat.t3)
  all(is.na(diff_dat.t4_t3$StdValue))==FALSE
  #in t3 but not in t4
  diff_dat.t3_t4 <- setdiff(dat.t3,dat.t4)
  all(is.na(diff_dat.t3_t4$StdValue))==FALSE
  #
  #remove those without standard values
  dat.t5 <- dat.t4 %>% 
    filter(is.na(StdValue)==FALSE)
  #check those removed
  dat.t5_check <- dat.t4 %>% 
    filter(is.na(StdValue)==TRUE) %>% 
    arrange(TraitName)
  #Some traits have units and other do not
  #retain those with units
  dat.t5_check_units <- dat.t5_check %>% 
    filter(OrigUnitStr!="")
  # View(dat.t5_check_units)
  sort(unique(dat.t5_check_units$TraitName))
  #check those without units
  dat.t5_check_units_check <- dat.t5_check %>% 
    filter(OrigUnitStr=="")
  # View(dat.t5_check_units_check)
  sort(unique(dat.t5_check_units_check$TraitName))#these are categorical
  #I am also not including ratio variables
  #
  # dat.t5#dataset with NA in std traits
  # dat.t5_check_units#dataset with original values with units, but not standard units
  #
  dat.t6 <- dat.t5 %>% 
    mutate(StdValue=as.numeric(StdValue))
  #remove traits that should be standardized but are not
  dat.t5_check_units_dat.t6_variables <- dat.t5_check_units %>%
    filter(TraitName %ni%
             unique(dat.t6$TraitName))
  # View(dat.t5_check_units_dat.t6_variables)
  unique(dat.t5_check_units_dat.t6_variables$StdValue)
  #move original values to standard
  dat.t5_merge <- dat.t5_check_units_dat.t6_variables %>% 
    mutate(TraitVal= ifelse(is.na(StdValue)==TRUE,OrigValueStr,StdValue),
           UnitName=OrigUnitStr) 
  #
  dat.t5_merge$TraitVal <- as.numeric(dat.t5_merge$TraitVal)  
  #non numeric traits 
  dat.t5_merge_nonnumeric <- dat.t5_merge %>% 
    filter(is.na(TraitVal)==TRUE)
  dat.t5_merge_noNA <- dat.t5_merge %>% 
    filter(is.na(TraitVal)==FALSE)
  #
  checkIDs <- unique(dat.t5_merge_noNA$TraitID)
  ####
  unitfun <- function(x){
    # x <- checkIDs[1]
    tmp <- dat.t5_merge_noNA %>% 
      filter(TraitID==x)
    unit <- unique(tmp$OrigUnitStr)
    unit
  }
  #
  origunits <- map(.x = checkIDs,.f = unitfun)
  origunits <- set_names(origunits,checkIDs) 
  origunits_count <- enframe(map(.x = origunits,.f = length))
  origunits_count$value <- unlist(origunits_count$value)
  colnames(origunits_count)[1] <- "TraitID"
  #####
  dat.t6$TraitVal <- dat.t6$StdValue
  dat.t7 <- bind_rows(dat.t6,dat.t5_merge_noNA)
  ####
  unique(dat.t7$ValueKindName)
  allspecies <- dat.t7 %>% 
    filter(ValueKindName %in%
             c("Single",
               "Mean",
               "Best estimate",
               "Median",
               "",
               "Species mean",
               # "Class mean",
               "Site specific mean")) 
  unique(allspecies$ValueKindName)
  #
  # tmp <- allspecies %>% filter(ValueKindName=="Class mean")
  #
  allspecies_1 <- allspecies %>% 
    group_by(AccSpeciesName,TraitID,TraitName) %>% 
    count()
  allspecies_w <- allspecies_1 %>% ungroup() %>%  select(-TraitName) %>% 
    pivot_wider(names_from = "TraitID",values_from = "n")
  traitlist <- allspecies_w %>% 
    select(where(~!any(is.na(.))))
  traitlist_l <- traitlist %>% 
    pivot_longer(-AccSpeciesName,names_to = "TraitID",
                 values_to = "n")
  ####
  dat.t8 <- dat.t7 %>% 
    filter(TraitID %in% traitlist_l$TraitID)
  sort(unique(dat.t8$TraitName))
  dat.t8_check <- dat.t7 %>% 
    filter(TraitID %ni% traitlist_l$TraitID)
  sort(unique(dat.t8_check$TraitID))
  #check those without standardized units
  dat.t8_origunit <- dat.t8 %>% 
    filter(TraitID %in% origunits_count$TraitID)
  unique(dat.t8_origunit$TraitName)
  #remove unstandard leaf shape
  #I noticed that some leaf width values have different units that cannot be easily
  #rectified so I removed those values
  unique(dat.t8$TraitName)
  #
  leafshape <- dat.t8 %>% 
    filter(TraitName == "Leaf shape") %>% 
    arrange(DataName)
  unique(leafshape$DataName)
  #
  nouseleaf <- c("Leaf form coefficient (perimeter/area)",
                 "Leaf form index (area/length^2)",
                 "Leaf perimeter per leaf area")
  dat.t9 <- dat.t8 %>% 
    filter(DataName %ni% nouseleaf)
  dat.t9_check <- dat.t8 %>% 
    filter(DataName %in% nouseleaf)
  ##
  leafshape2 <- dat.t9 %>% 
    filter(TraitName == "Leaf shape") %>% 
    arrange(DataName)
  unique(leafshape2$DataName)
  unique(leafshape2$OrigUnitStr)
  #
  dat.t10 <- dat.t9 %>% 
    filter(ValueKindName %in%
             c("Single",
               "Mean",
               "Best estimate",
               "Median",
               "",
               "Species mean",
               "Site specific mean"))
  unique(dat.t10$ValueKindName)
  #remove max, min, upper quartile and lower quartile
  ###
  #error risk 
  #calculate those traits by species observations with a 
  #sample of at least three
  counts <- dat.t10 %>% group_by(AccSpeciesID,TraitID,TraitName) %>% 
    count() %>% 
    mutate(match=paste(AccSpeciesID,TraitID,sep="_")) %>% 
    ungroup() %>% 
    select(-c(TraitName,TraitID,AccSpeciesID)) %>% 
    mutate(N_greater_3 = n>3)
  #matching column for species x traits
  dat.t10 <- dat.t10 %>% 
    mutate(match=paste(AccSpeciesID,TraitID,sep="_"))
  #merge in counts
  dat.t11 <- left_join(dat.t10,counts)  
  #remove those traits with at least three
  #and with an error risk greater than 4
  dat.t11$ErrorRisk <- ifelse(is.na(dat.t11$ErrorRisk)==TRUE,0,dat.t11$ErrorRisk)
  #
  dat.t12 <- dat.t11 %>% filter(N_greater_3==TRUE & ErrorRisk<4)
  dat.t12_extra <- dat.t11 %>% filter(N_greater_3==FALSE) %>% 
    arrange(desc(ErrorRisk))
  dat.t12_both <- bind_rows(dat.t12,dat.t12_extra)
  #those removed with an error risk greater than 4 and n >3
  dat.t12_check <- dat.t11 %>% filter(N_greater_3==TRUE & ErrorRisk>4) %>% 
    arrange(desc(ErrorRisk))
  #
  NROW(dat.t11)-NROW(dat.t12_both)==NROW(dat.t12_check)
  ###
  # unique(dat.t12$TraitName)
  # tmp <- dat.t12 %>% filter(TraitName=="Seed dry mass")
  ###
  dat.t13 <- dat.t12_both %>% 
    group_by(TraitName,AccSpeciesName) %>%  
    summarise(TraitVal=mean(TraitVal,na.rm=TRUE)) 
  dat.t13_count <- as.data.frame(table(dat.t13$TraitName,is.na(dat.t13$TraitVal)))
  #
  dat.t14 <- dat.t13%>% 
    pivot_wider(names_from = "TraitName",
                values_from="TraitVal")
  #remove values with any NA
  dat.w <- dat.t14 %>% 
    select(where(~!any(is.na(.))))
  #
  dat.w$AccSpeciesName <- ifelse(dat.w$AccSpeciesName=="Dalea purpurea",
                                 "Petalostemum purpureum",dat.w$AccSpeciesName)
  #note there are many different versions of leaf area to keep or discard 
  # dat.w$`Leaf area (in case of compound leaves: leaf, petiole included)`<- NULL
  # dat.w$`Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded` <- NULL
  # dat.w$`Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded` <- NULL
  # ####
  colnames(dat.w)
  colnames(dat.w)[2:23]
  dat.t10_out <- dat.t10 %>% 
    arrange(TraitName,AccSpeciesName) %>% 
    filter(TraitName %in% colnames(dat.w[2:23]))
  ####
  test <- dat.t1 %>%  
    filter(TraitName=="Leaf photosynthesis rate per leaf area")
  table(test$UnitName)#why are these unit not left standardized
  ####
  colnames(dat.w)
  names_try <- dat.t %>% 
    select(TraitID,TraitName,UnitName) %>% 
    distinct() %>% 
    filter(TraitName %in% colnames(dat.w[2:23])) %>% 
    arrange(TraitName)
  names_try$Dataset <- "TRY"
  names_try$UnitName <- ifelse(names_try$UnitName=="g/m2/d","micro mol m-2 s-1",names_try$UnitName)
  names_try$UnitName <- ifelse(names_try$TraitName=="Leaf shape","cm/cm",names_try$UnitName)
  names_try <- names_try %>% distinct()
  ####
  names_try <- as.data.frame(names_try)
  renamefun <- function(x,newname,newvarname){
    names_try[names_try$TraitName==x,"TraitNameClean"] <- 
      newname
    names_try[names_try$TraitName==x,"VarName"] <- 
      newvarname
    return(names_try)
  }
  names_try <- renamefun(x="Fine root diameter",
            newname = "Fine root diameter",
            newvarname =  "FineRootDiameter")
  #
  names_try <-renamefun(x="Fine root tissue density (fine root dry mass per fine root volume)",
            newname = "Fine root tissue density",
            newvarname =  "FineRootTissueDensity")
  #
  names_try <-renamefun(x="Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included",
            newname =  "Leaf area per leaf dry mass (petiole included)",
            newvarname =  "TRY_SLA_pet_inc")
  #
  names_try <-renamefun(x="Leaf carbon (C) isotope signature (delta 13C)",
            newname =  "Leaf carbon (C) isotope signature (delta 13C)",
            newvarname =  "LeafDelta13Carbon")
  #
  names_try <-renamefun(x="Leaf dry mass (single leaf)",
            newname =  "Leaf dry mass (single leaf)",
            newvarname =  "LeafDryMassSingle")
  #
  names_try <-renamefun(x="Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
            newname =  "Leaf dry mass per leaf fresh mass",
            newvarname =  "LDMC")
  #
  names_try <-renamefun(x="Leaf fresh mass",
            newname =  "Leaf fresh mass",
            newvarname =  "LeafFreshMass")
  #
  names_try <-renamefun(x="Leaf nitrogen (N) content per leaf dry mass",
            newname =    "Leaf nitrogen (N) content per leaf dry mass",
            newvarname =  "LeafNitrogen")
  #
  names_try <-renamefun(x="Leaf nitrogen (N) isotope signature (delta 15N)",
            newname =    "Leaf nitrogen (N) isotope signature (delta 15N)",
            newvarname =  "LeafDelta15Nitrogen")
  #
  names_try <-renamefun(x="Leaf photosynthesis rate per leaf area",
            newname =    "Leaf photosynthesis rate per leaf area",
            newvarname =  "LeafPhoto_area")
  names_try <-renamefun(x="Leaf photosynthesis rate per leaf dry mass",
                        newname =    "Leaf photosynthesis rate per leaf dry mass",
                        newvarname =  "LeafPhoto_mass")
  #
  names_try <-renamefun(x="Leaf shape",
                        newname =    "Leaf shape",
                        newvarname =  "LeafShape")
  #
  names_try <-renamefun(x="Leaf water content per leaf dry mass (not saturated)",
                        newname =      "Leaf water content per leaf dry mass (not saturated)",
                        newvarname =  "LeafWater")
  #
  names_try <-renamefun(x="Leaflet number per leaf",
                        newname =     "Leaflet number per leaf",
                        newvarname ="LeafletNum")
  #
  names_try <-renamefun(x="Plant height generative",
                        newname =     "Plant height generative",
                        newvarname ="PlantHeightGen")
  #
  names_try <-renamefun(x="Plant height vegetative",
                        newname =     "Plant height vegetative",
                        newvarname = "PlantHeightVeg")
  #
  names_try <-renamefun(x="Seed dry mass",
                        newname =     "Seed dry mass",
                        newvarname = "Try_SeedDryMass")
  #
  names_try <-renamefun(x="Stomata conductance per leaf area",
                        newname =     "Stomata conductance per leaf area",
                        newvarname ="StomataConductance_area")
  names_try <-renamefun(x="Stomata conductance per leaf dry mass",
                        newname =     "Stomata conductance per leaf dry mass",
                        newvarname ="StomataConductance_mass")
  names_try <-renamefun(x="Leaf area (in case of compound leaves: leaf, petiole included)",
                        newname =     " Leaf area (petiole included)",
                        newvarname ="LeafArea")
  names_try <- renamefun(x="Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
    newname =     "Leaf area per leaf dry mass (petiole undefined)",
    newvarname ="TRY_SLA_pet_undef")
  names_try <- renamefun(x="Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
                         newname =     "Leaf area per leaf dry mass (petiole excluded)",
                         newvarname ="TRY_SLA_pet_ex")
  ####
  names_try <- names_try %>% 
    select(TraitID,TraitName,TraitNameClean,VarName,everything()) %>% 
    distinct() %>% 
    arrange(TraitName)
  ####
  colnames(dat.w)
  dat.l <- dat.w %>% 
    pivot_longer(c(-AccSpeciesName),names_to = "TraitName",values_to = "val") %>% 
    ungroup() %>% 
    arrange(TraitName)
  dat.l <- left_join(dat.l,names_try)
  dat.l <- dat.l %>% ungroup() %>%  arrange(TraitName)
  #
  write_csv(x = names_try,file = "Data_derived/data_e120plantcluster_datanames_try.csv")
  write_csv(x=dat.t12_both,file ="Data_derived/data_e120plantcluster_longtry.csv")
  write_csv(x=dat.l,file = "Data_derived/data_e120plantcluster_tryclean.csv")
  #
  if(datatype=="data"){
    return("Data_derived/data_e120plantcluster_tryclean.csv")
  }
  if(datatype=="names"){
    return("Data_derived/data_e120plantcluster_datanames_try.csv")
  }
  if(datatype=="long"){
    return("Data_derived/data_e120plantcluster_longtry.csv")
    
  }
#
}