
generate_hypervolume <- function(chem_dat, scale1, type1,
                                 e120_species1) {
  library(targets)
  library(hypervolume)
  library(tidyverse)
  # #
  set.seed(42)
  # tar_load(e120_species)
  # tar_load(e120chem)
  # chem_dat <- e120chem
  scale1 <- FALSE
  #####
  chem <- read.csv(chem_dat)
  e120_spec <- read.csv(e120_species1)
  #####
  e120_spec1 <- e120_spec %>% 
    select(Specid_5,funcgroup_noc3,Clade)
  colnames(e120_spec1)[1] <- "Species"
  #
  chem <- left_join(chem,e120_spec1)
  #####
  table(chem$funcgroup_noc3) # methods
  table(chem$Clade)
  table(chem$Clade,chem$funcgroup_noc3)
  #
  if (scale1 == TRUE) {
    chem <- chem %>%
      mutate(across(Aluminum:Zinc, ~ scale_this(.x)))
  }
  #
  library(broom)
  library(tidyverse)
  # get Poaceae data frame
  Poaceae <- chem %>%
    filter(Clade == "Poaceae") %>%
    select(Calcium, Nitrogen, Potassium)
  # get Fabaceae data frame
  Fabaceae <- chem %>%
    filter(Clade == "Fabaceae") %>%
    select(Calcium, Nitrogen, Potassium)
  # get Asterids data frame
  Asterids <- chem %>%
    filter(Clade == "Asterids") %>%
    select(Calcium, Nitrogen, Potassium)
  # make hypervolume genetic function
  make_hv <- function(method1, dataset, names, type) {
    band <- estimate_bandwidth(data = dataset, method = method1)
    hypervolume(dataset,
      kde.bandwidth = band,
      method = type,
      name = paste(names, method1, sep = "_")
    )
  }
  # bandwidth estimators
  choice <- c("silverman", "silverman-1d", "plug-in", "cross-validation")
  # data sets
  datas <- list(Poaceae, Fabaceae, Asterids)
  # names
  names(datas) <- c("Poaceae", "Fabaceae", "Asterids")
  # grid
  grids <- expand_grid(choice, datas)
  grids$choice
  grids$name <- names(grids$datas)
  # check for errors
  all_equal(Poaceae, grids[grids$name == "Poaceae", "datas"]$datas[[1]])
  # add in grid
  # type1="gaussian"
  grids$type <- type1
  # make list
  vars <- list(grids$choice, grids$datas, grids$name, grids$type)
  # run hypervolume for each dataset for each bandwidth estimator
  vols_lists <- pmap(
    .l = vars,
    .f = make_hv
  )
  ###
}
#
