#
generate_phylosignal_SuppTable3 <- function(trait_dat,
                                            nametable_csv1,
                                            tree_dat,
                                            datatype, namefile) {
  ###
  source("Functions/00_e120plantcluster_functions.R")
  library(targets)
  library(tidyverse)
  library(ape)
  library(phylobase)
  library(phylosignal)
  ###
  # tar_load(meantraitdat)
  # tar_load(nametable_csv)
  # tar_load(phylomaker_tree_dat)
  # nametable_csv1 <- nametable_csv
  # trait_dat <- meantraitdat
  # tree_dat <- phylomaker_tree_dat
  ###
  dat.m <- read.csv(trait_dat)
  nametab <- read.csv(nametable_csv1)
  ########
  newtree <- tree_dat
  newtree <- as.phylo(newtree)
  #####
  #check that tip labels match rownames of trait dataframe
  matchdata <- data.frame(Species = newtree$tip.label)
  matchdata <- left_join(matchdata, dat.m)
  traitdata <- matchdata %>% select(-c(Species:Output.Taxon))
  rownames(traitdata) <- matchdata$Species
  rownames(traitdata)
  colnames(traitdata)
  rownames(traitdata) == newtree$tip.label
  traitdata <- as.data.frame(traitdata)
  # setseed
  set.seed(42)
  # calculate the phylogenetic signal using picante
  tab <- picante::multiPhylosignal(
    x = traitdata,
    phy = newtree, reps = 9999
  )
  tab
  #adjust p values 
  tab$PIC.variance.P_adj_fdr <- p.adjust(tab$PIC.variance.P, "fdr")
  tab <- tab %>% select(K, PIC.variance.P, PIC.variance.P_adj_fdr)
  tab$Trait <- rownames(tab)
  #
  library(gt)
  #
  tab <- tab %>%
    select(Trait, K, everything()) %>%
    arrange(desc(K), PIC.variance.P_adj_fdr)
  tab %>% gt()
  tabout <- tab
  #
  nametab1 <- nametab %>% select(VarName, TraitName)
  colnames(nametab1) <- c("TraitName", "VarName")
  colnames(tab)[1] <- "TraitName"
  tab <- left_join(tab, nametab1)
  #
  tab$TraitName <- NULL
  colnames(tab)[4] <- "TraitName"
  tab <- tab %>% select(TraitName, everything())
  colnames(tab)[3:4] <- c("pval", "pval.fdr")
  #
  tab$K <- round(tab$K, 2)
  tab$pval <- pvalfun1(tab$pval)
  tab$pval.fdr <- pvalfun1(tab$pval.fdr)
  #
  tab %>% gt()
  #
  table(tab$pval.fdr < 0.05)
  #
  colnames(tabout)[3:4] <- c("pval", "pval.fdr")
  tabout <- tabout %>% arrange(desc(K), pval.fdr)
  ###
  # namefile <- "phylosignal"
  # namefile <- namefile
  table1_name <- paste("Tables/table_e120plantcluster_SupplementalTable3_", namefile, ".docx", sep = "")
  table1_name
  filename <- paste("Tables/table_e120plantcluster_SupplementalTable3_", namefile, ".csv", sep = "")
  filename
  if (datatype == "word") {
    tablefun(
      mytab = tab,
      tabname = "Table 3: Phylogenetic Signal:picate",
      filename = table1_name
    )
    return(table1_name)
  }
  if (datatype == "csv") {
    write.csv(x = tabout, file = filename, row.names = FALSE)

    return(filename)
  }
}
