#
# library(phytools)
# library(tidyverse)
readtree <- function(x,datatype){
  library(ape)
  library(tidyverse)
  library(ggdendro)
  library(dendextend)
  library(tidyverse)
  # test <- read.tree(text="((((Asclepias_tuberosa:85.81204074,Monarda_fistulosa:85.81204074)100.00:16.65029259,((Achillea_millefolium :16.78133288,Liatris_aspera:16.78133288)11.00:16.78133288,Solidago_rigida:33.56266576):68.89966757)100.00 :19.77615605,((Lupinus_perennis:48.53544118,(Amorpha_canescens:40.91335392,(Petalostemum_purpureum:20. 45667696,Petalostemum_villosum:20.45667696)NA:20.45667696)74.00:7.62208726)42.00:7.62208726,Lespedez a_capitata:56.15752844)100.00:66.08096095)100.00:37.55898026,((Panicum_virgatum:31.43279948,((Andropog on_gerardii:10.47759983,Schizachyrium_scoparium:10.47759983)52.00:10.47759983,Sorghastrum_nutans:20.955 19965)63.00:10.47759983)40.00:31.43279948,(Poa_pratensis:31.43279948,Koeleria_cristata:31.43279948)87.00: 31.43279948)100.00:96.93187069)100.00;")
  test <- read.tree(file=x)
  mytree <- as.phylo(test)
  newtree <- drop.tip(mytree,tip = "Petalostemum_villosum")
  write_rds(x = newtree,file = "Data_derived/shantree.rds")
  if(datatype=="tree"){
  return(newtree)}
  if(datatype=="file"){
    return("Data_derived/shantree.rds")
  }
}
