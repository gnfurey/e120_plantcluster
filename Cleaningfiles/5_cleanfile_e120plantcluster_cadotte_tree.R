#
# library(phytools)
# library(tidyverse)
readtree_cadotte <- function(cadotte_tree,datatype){
  library(ape)
  library(tidyverse)
  library(ggdendro)
  library(dendextend)
  library(tidyverse)
  # x <- "Data/cadotte_tree.txt"
  # tar_load(cadotte_tree)
  test <- read.tree(file=cadotte_tree)
  mytree <- as.phylo(test)
  mytree$tip.label
  newtree <- drop.tip(phy = mytree,
                       tip = c(
    "Magnolia_grandiflora",
    "Amborella_trichopoda",
    "Bouteloua_gracilis",
    "Sporobolus_cryptandrus",
    "Buchloe_dactyloides",
    "Elymus_canadensis",
    "Pascopyrum_smithii",
    "Anemone_cylindrica",
    "Salvia_officinalis",
    "Asclepias_incarnata",
    "Coreopsis_palmata",
    "Rudbeckia_hirta",
    "Lactuca_sativa",
    "Solidago_nemoralis",
    "Symphyotrichum_cordifolium",
    "Euphorbia_pubentissima",
    "Quercus_serrata",
    "Quercus_ellipsoidalis",
    "Quercus_macrocarpa",
    "Vicia_villosa",
    "Astragalus_canadensis",
    "Symphyotrichum_oolentangiense",
    "Dalea_candida")
  )
  matchnames <- data.frame(cadotte_name = newtree$tip.label)
  matchnames$new_name <- matchnames$cadotte_name
  matchnames$new_name <- str_replace_all(string = matchnames$new_name,
                                         pattern = "Oligoneuron_rigidum",
                                         replacement = "Solidago_rigida")
  matchnames$new_name <- str_replace_all(string = matchnames$new_name,
                                         pattern = "Amorpha_canadesis",
                                         replacement = "Amorpha_canescens")
  matchnames$new_name <- str_replace_all(string = matchnames$new_name,
                                         pattern = "Dalea_purpurea",
                                         replacement = "Petalostemum_purpureum")
  matchnames
  newtree$tip.label <-  matchnames[match(newtree$tip.label, 
                                         matchnames$cadotte_name),
                                   "new_name"]

  newtree <- chronos(newtree)
  is.ultrametric(newtree)
  write_rds(x = newtree,file = "Data_derived/cadotte_tree.rds")
  if(datatype=="tree"){
    return(newtree)}
  if(datatype=="file"){
    return("Data_derived/cadotte_tree.rds")
  }
}
