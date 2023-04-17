#
get_e120species <- function(e120species_meta) {
  library(lcvplants)
  library(tidyverse)
  # dat <- read.csv("Data/e120_species_meta.csv")
  # tar_load(e120species_meta)#
  dat <- read.csv(e120species_meta)
  dat$Search <- dat$ccesr_name
  dat$Search <- str_replace_all(
    string = dat$Search,
    pattern = "Koeleria cristata",
    replacement = "Koeleria macrantha"
  )
  #
  dat$Search <- str_replace_all(
    string = dat$Search,
    pattern = "Petalostemum purpureum",
    replacement = "Dalea purpurea"
  )

  dat$Search <- str_replace_all(
    string = dat$Search,
    pattern = coll("Achillea millefolium(lanulosa)"),
    replacement = coll("Achillea millefolium")
  )

  #####
  species_lcvp <- lcvp_search(dat$Search, progress_bar = TRUE)
  colnames(species_lcvp)
  species_lcvp1 <- species_lcvp %>%
    select(Search, Output.Taxon, Input.Genus, Family, Order)
  ####
  dat <- left_join(dat, species_lcvp1)
  ####
  library(taxize)
  # use taxize
  # uidslist <- get_uid(dat$Search)
  # uidslist1 <- as.data.frame(uidslist)
  # write.csv(x = uidslist1, file = "Data_derived/data_e120plantcluster_ncbi_id.csv", row.names = FALSE)
  uidslist1 <- read.csv("Data_derived/data_e120plantcluster_ncbi_id.csv")
  dat$UID_codes <- uidslist1$ids
  dat$NCBI_url <- uidslist1$uri
  # open the URL
  # openfun <- function(x){
  #   system(paste("open ", uidslist1$uri[[x]],sep=""))
  # }
  # walk(.x = seq(from=1,to=15),.f = openfun)
  #
  taxize_fun <- function(x) {
    # query the itis database
    print(x)
    out <- classification(x, db = "ncbi")
    # get element of list
    out1 <- out[[1]]
    #
    keepers <- c(
      "asterids",
      "Fabaceae",
      "Poaceae"
    )
    # out2 <- out1 %>% filter(rank=="order")
    out2 <- out1 %>% filter(name %in% keepers)
    # get upper
    # keep old name
    return(out2)
  }
  ids <- dat$UID_codes
  ids <- set_names(x = ids, nm = dat$Search)
  # taxize_species_data <- map_dfr(
  #   .x = ids,
  #   .f = taxize_fun,
  #   .id = "Search"
  # )
  # write_csv(taxize_species_data,file = "Data_derived/data_e120cluster_taxize.csv")
  taxize_species_data <- read.csv("Data_derived/data_e120cluster_taxize.csv")
  colnames(taxize_species_data)[2] <- "Clade"
  colnames(taxize_species_data)[4] <- "taxize_clade_id"
  taxize_species_data$rank <- NULL
  #
  dat <- left_join(dat, taxize_species_data)
  #
  colnames(dat)
  #
  dat$funcgroup_noc3 <-
    ifelse(dat$funcgroup == "C4_Grass" |
      dat$funcgroup == "C3_Grass",
    "Grass", dat$funcgroup
    )
  #
  dat$name_hyphen <- str_replace_all(
    string = dat$ccesr_name,
    pattern = " ",
    replacement = "_"
  )
  dat$name_hyphen <- str_replace_all(
    string = dat$name_hyphen,
    pattern = coll("(lanulosa)"),
    replacement = ""
  )
  #
  dat <- dat %>% select(
    Specid, Specid_5,
    ccesr_name, name_hyphen, Search, Output.Taxon,
    Input.Genus,
    funcgroup, funcgroup_noc3,
    Clade, Order, Family,
    UID_codes, NCBI_url,
    taxize_clade_id
  )
  #
  dat$Clade <- ifelse(dat$Clade == "asterids",
    "Asterids", dat$Clade
  )
  write_csv(
    x = dat,
    file = "Data_derived/data_e120plantcluster_e120species.csv"
  )
  return("Data_derived/data_e120plantcluster_e120species.csv")
}
