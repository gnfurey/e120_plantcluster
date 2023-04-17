clean_planttraits <- function(e120plantcluster_chemtraits_source) {
  library(tidyverse)
  # tar_load(e120plantcluster_chemtraits_source)
  dat <- read.csv(e120plantcluster_chemtraits_source)
  # dat <- read.csv("Data/data_e120plantclust_tissuetrait_meta_edi.csv")
  colnames(dat) <- str_replace_all(string = colnames(dat),
                                   pattern = coll(".percent"),
                                   replacement = "")
  dat$exp <- NULL
  dat$Species.ccesr <- NULL
  dat$Species.LCVP <- NULL
  dat$Treatment <- NULL
  #
  # dat <- read.csv(e120plantcluster_chemtraits_source)
  # Subset lupin data as it was sampled a second time
  luppe <- dat %>%
    filter(Species == "Luppe")
  # Subset lupin carbon data for 2017
  carb <- luppe %>%
    filter(Year == 2017) %>%
    select(Plot, Species, Carbon)
  # Subset lupin data for 2019 - all but C
  lupp_all <- luppe %>%
    filter(Year == 2019) %>%
    select(Plot, Species, Aluminum:Calcium, Copper:Zinc)
  # join together to get 2019 all but C and 2017 C
  lupp_all <- left_join(lupp_all, carb)
  # remove lupin from original data
  dat1 <- dat %>%
    filter(Species != "Luppe") %>%
    select(-c(Year, Month))
  # join together all data
  out <- bind_rows(dat1, lupp_all) %>%
    arrange(Species)
  # output data
  write_csv(x = out, file = "Data_derived/data_e120plantclust_tissuechemistry.csv")
  return("Data_derived/data_e120plantclust_tissuechemistry.csv")
}