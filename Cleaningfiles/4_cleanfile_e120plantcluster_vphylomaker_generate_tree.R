#
phylomaker_tree <- function(datatype,choice,
                            e120_species) {
  library(tidyverse)
  #
  e120 <- read.csv(e120_species)
  #gerardii is the tiplabel for the backbone tree
  e120$Search <- ifelse(e120$Search=="Andropogon gerardi",
                        "Andropogon gerardii",
                        e120$Search)
  example <- data.frame(
    species = e120$Search,
    genus = e120$Input.Genus,
    family = e120$Family
  ) 
  #
  library(V.PhyloMaker)
  library(targets)
  library(tidyverse)
  result <- phylo.maker(example, scenarios = c("S1", "S2", "S3"))
  # plot(result$scenario.3)
  phylomakertree <- as.phylo(result$scenario.3)
  #####
  #get the original backbone
  backbone <-  V.PhyloMaker::GBOTB.extended
  backbone_tips <- data.frame(tip.label=backbone$tip.label)
  #a function to search the backone tree for the genus of the species
  checktips <- function(x){
  check1 <- backbone_tips %>% 
    filter(str_detect(string = tip.label,
                      pattern = example$genus[[x]])==TRUE) %>% 
    arrange(tip.label)
  check1
  return(check1)
  }
  #get the species list
  species.list1 <- result$species.list
  #get those that did have a perfect name match
  species.list1 <- species.list1 %>% 
    filter(status=="bind")
  #check them
  checks <- map(.x = as.integer(row.names(species.list1)),.f = checktips)
  checks
  checks[[1]]#no amoca
  checks[[2]]#no luppe
  checks[[3]]#no poapra
  #these species are bound using their genus age
  #########
  e120$match_search_hyphen <- str_replace_all(
    string = e120$Search,
    pattern = " ",
    replacement = "_"
  )
  matchdat <- data.frame(match_search_hyphen = phylomakertree$tip.label)
  matchdat <- left_join(matchdat, e120)
  #
  par(mar=c(1,1,1,1))
  par(mfrow = c(1, 2)) # Create a 2 x 2 plotting matrix
  matchdat$match_search_hyphen == phylomakertree$tip.label
  # plot(phylomakertree)
  phylomakertree$tip.label <- matchdat$Specid_5
  # plot(phylomakertree)
  #######
  fname <- paste("Data_derived/phylomakertree_new_tree",choice,".rds",sep="")
  write_rds(x = phylomakertree,file = fname)
  if(datatype=="tree"){
    return(phylomakertree)}
  if(datatype=="file"){
    return(fname)
  }
}
