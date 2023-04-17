library(tidyverse)
library(future)
library(furrr)
library(cluster)
library(targets)
###
tar_load(clusters_file)
tar_load(nametable_csv)
nametable1 <- nametable_csv
clusters_file1 <- clusters_file
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
#get the length of traits 
dat$NumTrait <- as_vector(map(.x = namelist, .f = ~ length(unique(.x))))
unique(dat$NumTrait)==3#must be TRUE 
#filter out three traits in case other set numbers desired
three <- dat %>% filter(NumTrait == 3)
#
three2 <- three %>%
  filter(NumTrait==3) %>% 
  arrange(desc(cor)) %>% 
  mutate(rank=1:n()) %>% 
  group_by(category) %>%
  slice_max(n = 1, order_by = cor) %>%
  mutate(cor=round(cor,2)) %>% 
  arrange(desc(cor)) 
three2
