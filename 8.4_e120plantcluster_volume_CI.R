###
volume_tests_fin <- function(hypervolume_list1, stat,paths1){
  library(hypervolume)
  set.seed(42)
  # tar_load(hypervolume_list)
  # hypervolume_list1 <- hypervolume_list
  vols_list <- hypervolume_join(hypervolume_list1[[1]],
                                hypervolume_list1[[2]],
                                hypervolume_list1[[3]])
  #function to get the 95 CI
  CI_vol <- function(boot1, boot2) {
    result <- hypervolume_overlap_confidence(boot1,
      boot2,
      CI = 0.95,
      cores = 12
    )
    return(result)
  }
  # path_grid <- expand_grid(paths,paths)
  #names 
  name_vols <- map(1:length(vols_list@HVList),.f = function(x){vols_list[[x]]@Name})
  #paths 
  paths <- paths1
  #get path in a grid 
  path_grid <- as.data.frame(t(combn(paths, 2)))
  #get names 
  name_vec <- paste(path_grid$V1, path_grid$V2, sep = "___")
  #name dataframe 
  rownames(path_grid) <- name_vec
  #get reference data frame 
  ref_frame <- data.frame(names = unlist(name_vols), number = 1:3)
  #
  test_fun <- function(x, y) {
    # x <- name_vols[[1]]
    # x <- name_vols_grid$V1[[1]]
    # y <- name_vols_grid$V2[[1]]
    #replace with your directory
    field <- "/Users/furey034/Documents/Papers/e120_plantcluster/Objects/"
    pathlist1 <- paste(field, x, sep = "")
    pathlist2 <- paste(field, y, sep = "")
    pathlists <- c(pathlist1, pathlist2)
    print(pathlists)
    #use list of paths 
    listy <- hypervolume_overlap_test(
      hv1 = vols_list[[ref_frame[ref_frame$names == x, "number"]]],
      hv2 = vols_list[[ref_frame[ref_frame$names == y, "number"]]],
      path = pathlists,
      cores = 12
    )
    listy
  }
  #get all two way combinations of vols
  name_vols_grid <- as.data.frame(t(combn(
    c(
      name_vols[[1]],
      name_vols[[2]],
      name_vols[[3]]
    ),
    2
  )))
  #control function for CI or TEST 
  if (stat == "CI") {
    out_dat <- map2(
      .x = path_grid$V1, .y = path_grid$V2,
      .f = CI_vol
    )
    system("say -v 'Samantha' 'Simulation complete'")
    return(out_dat)
  }
  if (stat == "test") {
    out <- map2(
      .x = name_vols_grid$V1,
      .y = name_vols_grid$V2, .f = test_fun
    )
    out <- set_names(out, paste(name_vols_grid$V1, name_vols_grid$V2, sep = "CROSS"))
    system("say -v 'Samantha' 'Simulation complete'")
    return(out)
  }
}
