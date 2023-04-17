###
generate_rank_SuppFig2 <- function(clusters_file1,
                                   nametable_csv) {
  ###
  library(tidyverse)
  library(targets)
  ###
  # tar_load(clusters_file)
  # tar_load(nametable_csv)
  # clusters_file1 <- clusters_file
  ###
  dat <- read_csv(clusters_file1)
  ###
  name <- read.csv(nametable_csv)
  #
  namelist <- str_split(string = dat$var, pattern = "&", simplify = FALSE)
  #
  name$Category <- str_trim(name$Category)
  #
  categoryfun <- function(x) {
    # tmp <- out[[x]]
    names <- name[match(x, name$VarName), "Category"]
    out <- paste(sort(unique(names)), collapse = "&")
    out
  }
  dat$category <- as_vector(map(.x = namelist,
                                .f = categoryfun))
  #
  dat$NumTrait <- as_vector(map(.x = namelist, 
                                .f = ~ length(unique(.x))))
  #
  newdat <- dat %>%
    group_by(category) %>%
    mutate(maxcor = max(cor))
  #
  newdat$category <- as.factor(newdat$category)
  levels(newdat$category)
  newdat$category <- factor(newdat$category, levels(newdat$category)[
    c(1, 5, 7, 2, 4, 6, 3)
  ])
  levels(newdat$category)
  return(newdat)
}
