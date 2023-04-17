#
generate_PCA_Figure4 <- function(SuppTable3_file1,
                   meantraitdat1,
                   nametable_csv){
  library(targets)
  library(vegan)
  library(cluster)
  library(tidyverse)
  # source("Functions/00_e120plantcluster_functions.R")
  # tar_load(meantraitdat)
  # meantraitdat1 <- meantraitdat
  # tar_load(SuppTable3_file)
  # SuppTable3_file1 <- SuppTable3_file
  # tar_load(nametable_csv)
  ####
  #function to run rda with set values 
  radfun <- function(x,xlimits,ylimits,
                     file,scalechoice,axis_scale,specs_choice){
    # x <- sel
    # xlimits <- c(-1,1)
    # ylimits <- c(-1,1)
    # file <- "test"
    # scalechoice <- TRUE
    mod <- rda(x,
               scale = scalechoice)
    print(summary(mod))
    #
    plot(mod,type="n",scaling=axis_scale,
         xlim=xlimits,ylim=ylimits)
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "white") # Color
    abline(h = 0, lty = "dotted")
    abline(v = 0, lty = "dotted")
    with(dat.m, points(mod, display = "sites", col ="Black",
                       scaling=axis_scale,cex=2,
                       pch = 21,
                       bg = colvec[Clade]))
    with(dat.m, legend("bottomleft",
                       legend = levels(Clade), bty = "n",
                       col = "Black",pt.cex=2,
                       pch = 21, pt.bg = colvec))
    ordiellipse(mod,group=dat.m$Clade,
                col=colvec,scaling=axis_scale,conf = 0.95,
                # col="white",scaling=3,conf = 0.95,
                lwd=2)
    pca_scores<-scores(mod,scaling = axis_scale)
    specs <- as.data.frame(summary(mod,scaling = axis_scale)$sites)
    specs
    specs$Species <- rownames(specs)
    #
    if(specs_choice=="TRUE"){
    text(x = specs$PC1,
         y=specs$PC2,
         label=specs$Species)
    }else{
      arrows(0,0,pca_scores$species[,1],
             pca_scores$species[,2],
             lwd=1,length=0.1)
      text(x = pca_scores$species[,1]*1.1,bg="white",
           y=pca_scores$species[,2]*1.1,
           label=rownames(pca_scores$species))
    }
    
  }
  ####
  dat.m <- read.csv(meantraitdat1)
  dat.m <- dat.m %>% arrange(Clade)
  dat.m$Clade <- as.factor(dat.m$Clade)
  levels(dat.m$Clade)
  dat.m$Clade <- as.factor(dat.m$Clade)
  # colvec <- RColorBrewer::brewer.pal(n=3,"Dark2")
  colvec <- c("#7570b3","#d95f02","#1b9e77")
  axis_scale=3
  ###
  phylosignal <- read.csv(SuppTable3_file1)
  phylosignal_k <- phylosignal %>% 
    filter(pval.fdr<0.05)
  nametab <- read.csv(nametable_csv)
  nametab1 <- nametab %>% select(VarName,TraitName)
  colnames(phylosignal_k)[1] <- "VarName"
  colnames(nametab1)
  phylosignal_k <- left_join(phylosignal_k,nametab1)
  #
  colnames(dat.m)
  ####
  # fname <- "Figures/e120plantcluster_Figure4_PCA.eps" 
  # postscript(fname,height=5,width=3.23)
  fname <- "Figures/e120plantcluster_Figure4_PCA.pdf" 
  pdf(fname,height=5,width=3.23)
  par(mfrow = c(2, 1))
  par(cex = 0.6)
  par(mar = c(1, 1,1,1), oma = c(1, 1,1,1)) 
  ###
  mat1 <- dat.m %>% select(Nitrogen,Potassium,
                           Calcium)
  rownames(mat1) <- dat.m$Species
  colnames(mat1)
  radfun(x = mat1,scalechoice = TRUE,
         xlimits = c(-1,2),
         ylimits = c(-1.3,1.9),
         axis_scale=axis_scale,
         specs_choice=FALSE)
  mtext("(a)", side = 3, line = -1.9, adj = 0.1, cex = 1)
  ####
  mat1 <- dat.m %>%
    select(one_of(phylosignal_k$VarName))
  rownames(mat1) <- dat.m$Species
  colnames(mat1)
  mod <- rda(mat1,
             scale = TRUE)
  summary(mod)
  ####
  plot(mod,type="n",
       scaling=axis_scale,
       xlab="test",
       xlim=c(-2.1,1.6),
       ylim=c(-1.8,1.5))
  title(xlab="test")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "white") # Color
  abline(h = 0, lty = "dotted")
  abline(v = 0, lty = "dotted")
  with(dat.m, points(mod, display = "sites", col ="Black",
                     scaling=axis_scale,cex=2,
                     pch = 21,
                     bg = colvec[Clade]))
  with(dat.m, legend("bottomleft",
                     legend = levels(Clade), bty = "n",
                     col = "Black",cex=1,
                     pt.cex=2,
                     pch = 21, pt.bg = colvec))
  ordiellipse(mod,group=dat.m$Clade,
              col=colvec,scaling=axis_scale,conf = 0.95,
              # col="white",scaling=3,conf = 0.95,
              lwd=2)

  pca_scores<-scores(mod,scaling = axis_scale)
  print(pca_scores)
  arrows(0,0,pca_scores$species[,1],
         pca_scores$species[,2],
         lwd=1,length=0.1)
  names <- rownames(pca_scores$species)
  text_x <- pca_scores$species[,1]*1.3
  text_y <- pca_scores$species[,2]*1.3
  #
  namedat <- data.frame(names=names,text_x=text_x,
                        text_y=text_y)
  #
  summary(mod)
  #manually adjust to avoid overplotting
  namedat[namedat$names=="LeafDelta13Carbon","text_y"] <- 
    namedat[namedat$names=="LeafDelta13Carbon","text_y"]* 6
  namedat[namedat$names=="LeafWater","text_x"] <- 
    namedat[namedat$names=="LeafWater","text_x"]* 1.2
  namedat[namedat$names=="Boron","text_x"] <- 
    namedat[namedat$names=="Boron","text_x"]* 0.8
  namedat[namedat$names=="Magnesium","text_x"] <- 
    namedat[namedat$names=="Magnesium","text_x"]* 1.2
  namedat[namedat$names=="LeafWater","text_y"] <-
  namedat[namedat$names=="LeafWater","text_y"]*1.2
  namedat[namedat$names=="Magnesium","text_y"] <-
    namedat[namedat$names=="Magnesium","text_y"]*.6
  namedat[namedat$names=="Calcium","text_x"] <-
    namedat[namedat$names=="Calcium","text_x"]*1.1
  namedat[namedat$names=="Calcium","text_y"] <-
    namedat[namedat$names=="Calcium","text_y"]*0.7
  namedat[namedat$names=="LeafShape","text_y"] <-
    namedat[namedat$names=="LeafShape","text_y"]*0.9
  namedat[namedat$names=="Nitrogen","text_y"] <-
    namedat[namedat$names=="Nitrogen","text_y"]*0.8
  text(x = namedat$text_x,bg="white",
       y=namedat$text_y,
       label=namedat$names)
  mtext("(b)", side = 3, line = -1.9, adj = 0.1, cex = 1)
  dev.off()
  #####
  return(fname)
}
###