#
generate_clusters <- function(meantraitdat1, treechoice, name) {
  source("Functions/00_e120plantcluster_functions.R")
  library(vegan)
  library(dendextend)
  library(ape)
  library(cluster)
  library(purrr)
  library(furrr)
  library(tidyverse)
  future::plan(multisession)
  set.seed(42)
  ####
  library(targets)
  ####
  # this function takes a combination of traits and gets all possible combinations
  combfun <- function(x, choosedata) {
    # dataframe from combine
    gridx <- as.data.frame(t(combn(choosedata, x)))
    # transpose the matrix
    gridy1 <- purrr::transpose(gridx) %>% simplify_all()
    # unite the list
    vecboth <- gridx %>%
      unite("name", everything(), remove = T, sep = "&")
    # set names
    names(gridy1) <- vecboth$name
    return(gridy1)
  }
  # this function takes a matrix and calculates
  # the distance matrix using euclidean distance
  # it then uses an agglomerative clustering algorithm
  # with wards distance
  # the distance matrix and algorithm can be changed
  clustplot <- function(dat, name_dat, scal, tree) {
    dist.mat <- as.matrix(dat)
    dendo1 <- function(x, scal) {
      # x=dist.mat;scal=FALSE
      if (scal == FALSE) {
        mono <- x
      } else {
        mono <- scale(x)
      }
      dimnames(mono)[[1]] <- name_dat$Species
      d1 <- daisy(mono, metric = "euclidean")
      d2 <- agnes(d1, method = "ward")
      avg_dend_obj <- as.dendrogram(d2)
      return(avg_dend_obj)
    }
    dend <- dendo1(dist.mat, scal = scal)
    return(dend)
  }
  ####
  # this function takes the trait data
  # and then runs a clustering algorithm on those traits
  # it then takes a given tree
  # and compares the trait dendrogram with the phylogenetic tree
  corfun <- function(x, choosedata) {
    ######
    allfun <- function(mat) {
      #####
      dat.m <- read.csv(meantraitdat1)
      names <- dat.m %>% select(Species)
      #####
      # arg for mat
      # vars <- dat.m %>%
      #   select(Aluminum:StomataConductance)
      # varlist <- colnames(vars)
      # mat <- combfun(1,varlist)
      ####
      data2 <- dat.m %>% select(as_vector(mat))
      dend1 <- clustplot(
        dat = data2,
        name_dat = names,
        scal = TRUE)
      #
      dend2 <- readRDS(treechoice)
      # labels(dend2) <- get_fiveID_hyphen(labels(dend2))
      ### NOTE to self
      # package dendextend uses a wrapper from ape to make a dendrogram
      # "ape.R" in the source
      # as.dendrogram(as.hclust(as.phylo(hc)))
      #
      # as.dendrogram.phylo <- function(object, ...) {
      #   # library(ape)
      #   as.dendrogram(ape::as.hclust.phylo(object))
      # }
      ###
      # dend2 <- as.dendrogram(dend2)#CHANGED TO PHYLOGRAM
      # I switched to phylogram which retains original heights
      # and was created by an evolutionary biologist
      # ?as.dendrogram.phylo
      dend2 <- phylogram::as.dendrogram.phylo(dend2)
      # labels(dend2)
      ###
      # plot(dend1)
      # plot(dend2)
      dend_list <- dendlist(dend1, dend2)
      out <- cor_cophenetic(dend1, dend2)
      outdata <- data.frame(cor = out)
      return(outdata)
    }
    ####
    # Edit here
    ####
    # dat.m <- read.csv(meantraitdat)
    # vars <- dat.m %>%
    # select(Aluminum:StomataConductance)
    ####
    mat1 <- combfun(x, choosedata)
    # mat1 <- combfun(1,varlist)
    #
    # furrr_options(seed = 42)
    out <- future_map_dfr(
      .x = mat1, .f = allfun, .id = "var",
      .options = furrr_options(seed = 42)
    )
    out <- out %>% arrange(desc(cor))
    return(out)
  }
  dat.m <- read.csv(meantraitdat1)
  colnames(dat.m)
  vars <- dat.m %>%
    select(Aluminum:StomataConductance_mass)
  varlist <- colnames(vars)
  ###
  fin3 <- corfun(x = 3, choosedata = varlist)
  #
  final <- fin3 %>% arrange(desc(cor))
  #
  library(tidyverse)
  # name <- "phylomaker"
  fname <- paste("Tables/table_e120plantcluster_Clusters_", name, ".csv", sep = "")
  write.csv(
    x = final, file = fname,
    row.names = FALSE
  )
  return(fname)
}
