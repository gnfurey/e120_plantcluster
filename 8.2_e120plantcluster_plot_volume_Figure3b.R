plot_hypervolume <- function(hv, 
                             e120chem1, 
                             meantraitdat1,
                             fname,e120_species1) {
  library(hypervolume)
  library(tidyverse)
  library(targets)
  # # #####
  # tar_load(e120chem)
  # e120chem1 <- e120chem
  # tar_load(meantraitdat)
  # meantraitdat1 <- meantraitdat
  # tar_load(hypervolume_list)
  # vols <- hypervolume_join(
  #   hypervolume_list[[1]],
  #   hypervolume_list[[2]],
  #   hypervolume_list[[3]]
  # )
  # tar_load(e120_species)
  # e120_species1 <- e120_species
  ###
  library(targets)
  e120_spec <- read.csv(e120_species1)
  e120_spec1 <- e120_spec %>% 
    select(Specid_5,funcgroup_noc3,Clade)
  colnames(e120_spec1)[1] <- "Species"
  ####
  vols <- hv
  # check below
  # vols[[1]]@Data
  # vols[[1]]@RandomPoints
  # plot.Hypervolume
  # plot.HypervolumeList
  # plot(vols,
  #      show.density = TRUE,
  #      show.3d=FALSE,
  #      show.contour=TRUE,
  #      show.centroid = TRUE,
  #      point.alpha.min = 1,
  #      cex.centroid = 1,
  #      contour.type = "ball")
  # plot(vols,
  #      show.density = TRUE,
  #    show.3d=TRUE,
  #    show.contour=TRUE,
  #    show.centroid = TRUE,
  #    point.alpha.min = 1,
  #    cex.centroid = 1,
  #    contour.type = "alphahull")
  #####
  # tar_load(e120chem)
  # e120chem1 <- e120chem
  newdat <- read.csv(e120chem1)
  #####
  newdat <- left_join(newdat,e120_spec1)
  table(newdat$Clade)#in figure 3 caption
  #####
  colnames(newdat)
  newdat$col <- ifelse(newdat$Clade == "Poaceae", "#1b9e77",
    ifelse(newdat$Clade == "Fabaceae", "#d95f02",
      ifelse(newdat$Clade == "Asterids", "#7570b3", "black")
    )
  )
  colormat <- newdat %>% select(Clade, col)
  # see ?rgl
  # I have made a custom plot to my preference
  # rather than use the default
  # see
  # plot.HypervolumeList
  library(rgl)
  Poaceae_hv <- as.data.frame(vols[[1]]@RandomPoints)
  Poaceae_hv$Clade <- "Poaceae"
  Fabaceae_hv <- as.data.frame(vols[[2]]@RandomPoints)
  Fabaceae_hv$Clade <- "Fabaceae"
  Asterids_hv <- as.data.frame(vols[[3]]@RandomPoints)
  Asterids_hv$Clade <- "Asterids"
  #
  random_points <- bind_rows(Poaceae_hv, Fabaceae_hv, Asterids_hv)
  random_points <- left_join(random_points, colormat)
  #
  dat.m <- read.csv(meantraitdat1)
  dat.m$col <- ifelse(dat.m$Clade == "Poaceae", "#1b9e77",
    ifelse(dat.m$Clade == "Fabaceae", "#d95f02",
      ifelse(dat.m$Clade == "Asterids", "#7570b3", "black")
    )
  )
  ###
  max(dat.m$Nitrogen)
  max(dat.m$Calcium)
  max(dat.m$Potassium)
  max(random_points$Nitrogen)
  max(random_points$Calcium)
  max(random_points$Potassium)
  ###
  clear3d()
  zoom <- read_rds("zoom.rds")
  zoom <- 0.9
  userMatrix <- read_rds("userMatrix.rds")
  windowRect <- read_rds("windowRect.rds")
  par3d(
    zoom = zoom,
    userMatrix = userMatrix,
    windowRect = windowRect,
    font = 3,
    cex = 3
  )
  plot3d(
    x = dat.m$Nitrogen, y = dat.m$Calcium, z = dat.m$Potassium,
    size = 2,
    type = "s",
    xlab = "%N",
    ylab = "%Ca",
    zlab = "%K",
    cex = 5,
    xlim = c(0, 2.5),
    ylim = c(0, 2.7),
    zlim = c(0, 3.5),
    col = dat.m$col
  )
  aspect3d(1, 1, 1)
  #
  bbox3d(
    color = c("black", "black"),
    # yat = c(0.5,1,1.5,2),
    xlen = 0, ylen = 0, zlen = 0,
    # specular = "grey",
    expand = 0.7,
    front = "lines",
    back = "lines",
    edge = "z-+",
    edge = "y+-", alpha = 1
  )
  aspect3d(1, 1, 1)
  plot3d(
    x = random_points$Nitrogen,
    y = random_points$Calcium,
    z = random_points$Potassium,
    size = 0.1,
    alpha = 0.7,
    ambient = "black",
    smooth = TRUE,
    type = "p", col = random_points$col,
    xlab = "",
    add = TRUE,
    ylab = "",
    zlab = ""
  )
  ###
  # library(tidyverse)
  par3d(
    zoom = zoom,
    userMatrix = userMatrix,
    windowRect = windowRect,
    font = 3,
    cex = 6
  )
  ###
  axes3d(
    edges = c("x--"), cex = 2,
    at = c(0, 0.7, 1.4, 2.1),
    labels = c(0, 0.7, 1.4, 2.1),
    tick = TRUE,
    nticks = 4
  )
  axes3d(
    edges = c("z--"), cex = 2,
    at = c(0, 0.7, 1.4, 2.1, 2.8, 3.5),
    labels = c(0, 0.7, 1.4, 2.1, 2.8, 3.5),
    tick = TRUE,
    nticks = 4
  )
  axes3d(
    edges = c("y+-"), cex = 2,
    at = c(0, 0.7, 1.4, 2.1, 2.8),
    labels = c(0, 0.7, 1.4, 2.1, 2.8),
    tick = TRUE,
    nticks = 4
  )
  axes3d(edges = c("y-+"),cex=2,
         at=c(0,0.7,1.4,2.1,2.8),
         labels=c(0,0.7,1.4,2.1,2.8),
         tick=TRUE,
         nticks=4)
  ###
  # zoom<-par3d()$zoom
  # userMatrix<-par3d()$userMatrix
  # windowRect<-par3d()$windowRect
  # write_rds(x=zoom,file = "zoom.rds")
  # write_rds(x=userMatrix,file="userMatrix.rds")
  # write_rds(x=windowRect,file="windowRect.rds")
  #
  #
  legend3d("topleft",
    legend = c(
      "Poaceae",
      "Fabaceae",
      "Asterids"
    ),
    cex = 3,
    pch = 16, col = c(
      "#1b9e77",
      "#d95f02", "#7570b3"
    )
  )
  rgl.snapshot(fname)
  return(fname)
  #
}
######
