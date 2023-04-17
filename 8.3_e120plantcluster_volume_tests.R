volume_tests <- function(hypervolume_list1) {
  library(hypervolume)
  library(tidyverse)
  library(targets)
  set.seed(42)
  # tar_load(hypervolume_list)
  # hypervolume_list1 <- hypervolume_list
  vols_list <- hypervolume_join(
    hypervolume_list1[[1]],
    hypervolume_list1[[2]],
    hypervolume_list1[[3]]
  )
  # vols <- hv
  get_volume(vols_list[[1]])
  get_volume(vols_list[[2]])
  get_volume(vols_list[[3]])
  sum(get_volume(vols_list[[1]]), get_volume(vols_list[[2]]), get_volume(vols_list[[3]]))
  # a function to resample a hypervolume object
  resamplefun <- function(x, name, num_vol) {
    out <- hypervolume_resample(name,
      x,
      method = "bootstrap",
      n = num_vol,
      cores = 12
    )
    out
  }
  # get names of vols
  name_vols <- map(1:length(vols_list@HVList), .f = function(x) {
    vols_list[[x]]@Name
  })
  # fun the function iteratively
  resample_fun_iterate <- function(x, name_vols1, numvol1) {
    out1 <- resamplefun(x = vols_list[[x]], name = name_vols1, num_vol = numvol1)
    out1
  }
  # HOW MANY VOLUMES
  numbervol <- 50
  # the path to place the resampled volumes
  paths <- map2(
    .x = 1:length(vols_list@HVList), .y = name_vols,
    .f = resample_fun_iterate, numvol1 = numbervol
  )
  return(paths)
}
