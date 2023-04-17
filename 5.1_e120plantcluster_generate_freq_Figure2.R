#####
generate_freq_Figure2 <- function(clusters_file1,
                      nametable_csv,
                      clust_stat,
                      cutoff){
  library(cluster)
  library(tidyverse)
  library(furrr)
  library(targets)
  ###
  # cutoff <- 0.8
  # tar_load(clusters_file)
  # tar_load(nametable_csv)
  # clusters_file1 <- clusters_file
  ##
  dat <- read.csv(clusters_file1)
  nametab <- read.csv(nametable_csv)
  future::plan(multisession)
  corchoice <- cutoff
  ##
  newthree <- dat %>%
    filter(cor>corchoice)
  NROW(newthree)#in figure caption
  #####
  newclassify <- function(dat2use){
    #
    # dat2use <- newthree
    #get string
    out <- str_split(string = dat2use$var,pattern = "&",simplify = FALSE)
    #get total frequency of traits 
    out1 <- as.data.frame(t(as.data.frame(flatten(flatten(out)))))
    #fill in variable
    out1$Type <- ""
    #rename to match
    colnames(out1)[1] <- "VarName"
    #join with nametable
    out2 <- left_join(out1,nametab)
    #
    freq <- as.data.frame(table(out2$VarName))
    # freq$prop <- freq$Freq/sum(freq$Freq)
    return(freq)
  }
  newthreedat <- newclassify(newthree)
  #retain dataframe and rename
  dats <- newthreedat
  #
  colnames(dats)[1] <- "VarName"
  #join with name table
  dats <- left_join(dats,nametab)
  #rearrange
  dats <- dats %>% arrange(desc(Freq))
  #make a factor 
  dats$VarName <- factor(dats$VarName,levels=unique(dats$VarName))
  #####
  #a function to get the relative proporation of each trait
  outoftotal <- function(dat2use){
    # dat2use <- newthree
    #get strings 
    out <- str_split(string = dat2use$var,pattern = "&",simplify = FALSE)
    #flatten
    out1 <- as.data.frame(t(as.data.frame(flatten(flatten(out)))))
    #get var name
    out2 <- data.frame(VarName=unique(out1$V1))
    #give integer
    out2$Count <- 1
    #merge
    counttab <- left_join(nametab,out2)
    #
    out3 <- counttab %>% group_by(Category) %>% 
      mutate(
        occur = sum(Count,na.rm=TRUE),
        total = NROW(unique(VarName))) %>% 
      select(occur,total,Category) %>% 
      distinct()
    # freq$prop <- freq$Freq/sum(freq$Freq)
    return(out3)
  }
  newthree_occur <- outoftotal(newthree)
  newthree_occur$vars <- "3"
  #
  occurdat <- newthree_occur
  outlist <- list(dats,occurdat)
  return(outlist)
}