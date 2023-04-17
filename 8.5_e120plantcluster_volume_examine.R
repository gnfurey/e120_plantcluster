#
library(targets)
library(hypervolume)
library(gridExtra)
library(grid)
library(tidyverse)
set.seed(42)
#load in values 
tar_load(hypervolume_list)
tar_load(volume_tests_dat)
tar_load(volume_tests_dat_CI)
#get names for each vol 
hypervolume_list[[1]]@Name
Poaceae_silverman <- hypervolume_list[[1]]
hypervolume_list[[2]]@Name
Fabaceae_silverman <- hypervolume_list[[2]]
hypervolume_list[[3]]@Name
Asterids_silverman <- hypervolume_list[[3]]
#get summary
summary(Poaceae_silverman)
summary(Fabaceae_silverman)
summary(Asterids_silverman)
# volume sizes
round(get_volume(hypervolume_list[[1]]),2)
round(get_volume(hypervolume_list[[2]]),2)
round(get_volume(hypervolume_list[[3]]),2)
#in results section 
round(sum(
  get_volume(hypervolume_list[[1]]),
  get_volume(hypervolume_list[[2]]),
  get_volume(hypervolume_list[[3]])
),2)
#
names(volume_tests_dat)
?hypervolume_overlap_test
# hypervolume_overlap_test does not give back the test statistics
# that comes from hypervolume_overlap_statistics
# from the source code that gives the ggplot
#  observed = hypervolume_overlap_statistics(hypervolume_set(hv1,
# hv2, check.memory = FALSE))
hypervolume_overlap_statistics(hypervolume_set(Poaceae_silverman,
  Fabaceae_silverman,
  check.memory = FALSE
))
#extract information PoaceaeCROSSFabaceae
names(volume_tests_dat)
PoaceaeCROSSFabaceae <- volume_tests_dat[[1]]
PoaceaeCROSSFabaceae_CI <- volume_tests_dat_CI[[1]]
PoaceaeCROSSFabaceae_statistic <- hypervolume_overlap_statistics(
  hypervolume_set(Poaceae_silverman,
  Fabaceae_silverman,
  check.memory = FALSE))
PoaceaeCROSSFabaceae
PoaceaeCROSSFabaceae_statistic
#
PoaceaeCROSSFabaceae$p_values
# PoaceaeCROSSFabaceae_CI$jaccard
PoaceaeCROSSFabaceae_CI$sorensen
exec(.fn = grid.arrange, grobs = PoaceaeCROSSFabaceae$plots, ncol = 2)
#extract information PoaceaeCROSSAsterids
PoaceaeCROSSAsterids <- volume_tests_dat[[2]]
PoaceaeCROSSAsterids_CI <- volume_tests_dat_CI[[2]]
exec(.fn = grid.arrange, grobs = PoaceaeCROSSAsterids$plots, ncol = 2)
PoaceaeCROSSAsterids_statistic <- hypervolume_overlap_statistics(
  hypervolume_set(Poaceae_silverman,
  Asterids_silverman,
  check.memory = FALSE
))
PoaceaeCROSSAsterids_statistic
PoaceaeCROSSAsterids$p_values
# PoaceaeCROSSAsterids_CI$jaccard
PoaceaeCROSSAsterids_CI$sorensen
#extract information FabaceaeCROSSAsterids
FabaceaeCROSSAsterids <- volume_tests_dat[[3]]
FabaceaeCROSSAsterids_CI <- volume_tests_dat_CI[[3]]
exec(.fn = grid.arrange, grobs = FabaceaeCROSSAsterids$plots, ncol = 2)
FabaceaeCROSSAsterids_statistic <- hypervolume_overlap_statistics(
  hypervolume_set(Fabaceae_silverman,
  Asterids_silverman,
  check.memory = FALSE
))
FabaceaeCROSSAsterids_statistic
FabaceaeCROSSAsterids$p_values
# FabaceaeCROSSAsterids_CI$frac_unique_1
# FabaceaeCROSSAsterids_CI$frac_unique_2
# FabaceaeCROSSAsterids_CI$jaccard
FabaceaeCROSSAsterids_CI$sorensen
####
#extra statistics 
#distance to centroid
centroid <- function(seed){
  set.seed(seed)
  print("PoaceaeCROSSFabaceae")  
  print(round(hypervolume_distance(Poaceae_silverman,
                                   Fabaceae_silverman,type="centroid",check.memory=FALSE),2))
  print("PoaceaeCROSSAsterids")  
  print(round(hypervolume_distance(Poaceae_silverman,
                                   Asterids_silverman,type="centroid",check.memory=FALSE),2))
  print("FabaceaeCROSSAsterids")  
  print(round(hypervolume_distance(Fabaceae_silverman,
                                   Asterids_silverman,type="centroid",check.memory=FALSE),2))
}
centroid(42)
# hypervolume_distance(Poaceae_silverman,Fabaceae_silverman,type="centroid")
# hypervolume_distance(Poaceae_silverman,Asterids_silverman,type="centroid")
# hypervolume_distance(Fabaceae_silverman,Asterids_silverman,type="centroid")
#
minimum <- function(seed){
set.seed(seed)
print("PoaceaeCROSSFabaceae")  
print(hypervolume_distance(Poaceae_silverman,Fabaceae_silverman,type="minimum",check.memory=FALSE))
print("PoaceaeCROSSAsterids")  
print(hypervolume_distance(Poaceae_silverman,Asterids_silverman,type="minimum",check.memory=FALSE))
print("FabaceaeCROSSAsterids")  
print(hypervolume_distance(Fabaceae_silverman,Asterids_silverman,type="minimum",check.memory=FALSE))
}
minimum(42)
######