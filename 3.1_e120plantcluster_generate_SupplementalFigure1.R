#
generate_top3way_SupplementalFigure1 <- function(clusters_file1,
                                     nametable1) {
  library(tidyverse)
  library(future)
  library(furrr)
  library(cluster)
  library(targets)
  ###
  # tar_load(clusters_file)
  # tar_load(nametable_csv)
  # nametable1 <- nametable_csv
  # clusters_file1 <- clusters_file
  ###
  nametab <- read.csv(nametable1)
  # name <- read.csv(nametable1)
  dat <- read_csv(clusters_file1)
  #get list of traits 
  namelist <- str_split(string = dat$var, pattern = "&", simplify = FALSE)
  #a function to match the category of each string 
  categoryfun <- function(x) {
    # tmp <- out[[x]]
    names <- nametab[match(x, nametab$VarName), "Category"]
    out <- paste(sort(unique(names)), collapse = "&")
    out
  }
  #create categories for each threeway set
  dat$category <- as_vector(map(.x = namelist, .f = categoryfun))
  #get the lengh of traits 
  dat$NumTrait <- as_vector(map(.x = namelist, .f = ~ length(unique(.x))))
  unique(dat$NumTrait)==3#must be TRUE 
  #filter out three traits in case other set numbers desired
  three <- dat %>% filter(NumTrait == 3)
  #get top 15 
  #CHECK MANUALLY
  three_short <- three %>% slice_max(cor, n = 15)
  ###
  #a function to take the number of traits in each category
  #and then calculate the total frequency
  #this is needed for the final plot structure
  outoftotal <- function(num, dat2_choice) {
    # dat2_choice <- three_short
    # num=3
    #subset one row
    dat2use <- dat2_choice[num, ]
    #split the string 
    out <- str_split(string = dat2use$var, pattern = "&", simplify = FALSE)
    #get a simple vector 
    out1 <- as.data.frame(t(as.data.frame(flatten(flatten(out)))))
    #make a data frame
    out2 <- data.frame(VarName = unique(out1$V1))
    #give integer value
    out2$Count <- 1
    #joinw with naming table 
    counttab <- left_join(out2, nametab)
    #calculate sum 
    out3 <- counttab %>%
      group_by(Category) %>%
      mutate(
        occur = sum(Count, na.rm = TRUE),
        total = NROW(unique(VarName))
      ) %>%
      select(occur, total, Category) %>%
      distinct()
    dat2use1 <- bind_cols(dat2use, out3)
    # freq$prop <- freq$Freq/sum(freq$Freq)
    return(dat2use1)
  }
  ###
  seqs <- 1:nrow(three_short)
  seqs <- set_names(seqs, three_short$var)
  traits <- map_dfr(
    .x = seqs,
    .f = outoftotal,
    dat2_choice = three_short,
    .id = "Trait"
  )
  ###
  traits$cor <- round(traits$cor, 3)
  return(traits)
}
###