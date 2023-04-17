#
plot_dendro_Figure1 <- function(meantraitdat1, 
                            tree_file,
                            choice,
                            matrix1,
                            fname) {
  library(targets)
  library(vegan)
  library(dendextend)
  library(ape)
  library(cluster)
  library(tidyverse)
  library(phylogram)
  ####
  set.seed(42)
  ####
  clustp <- function(mymat) {
    par(mar=c(1,1,1,1))
    #clustering function
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
    # tar_load(meantraitdat)
    # meantraitdat1 <- meantraitdat
    ####
    #load in trait data
    dat.m <- read.csv(meantraitdat1)
    #get trait names
    names <- dat.m %>% select(Species)
    #take vector of traits 
    # mymat <- c("Calcium","Nitrogen","Potassium")
    mymat1 <- as_vector(mymat)
    # print(test)
    #get data matrix of traits 
    data2 <- dat.m %>% select(all_of(mymat1))
    # print(colnames(data2))
    dend1 <- clustplot(
      dat = data2,
      name_dat = names,
      scal = TRUE,
    )
    #load in tree 
    # tar_load(phylomaker_tree_file)
    # tree_file <- phylomaker_tree_file
    dend2 <- readRDS(tree_file)
    # dend2 <- as.dendrogram(dend2)#alternative function
    dend2 <- phylogram::as.dendrogram.phylo(dend2)
    clust_title <- paste(
      paste(c("Ca","K","N"), collapse = " & "),
      "\n Dendrogram"
    )
    #
    main1 <- paste("Cor = ", round(cor_cophenetic(dend2, dend1), 3))
    print(main1)
    ####
    #get colors
    coldat <- data.frame(Clade=sort(unique(dat.m$Clade)))
    #match in colors 
    coldat$col <- ifelse(coldat$Clade=="Poaceae","#1b9e77",
                         ifelse(coldat$Clade=="Fabaceae","#d95f02",
                                ifelse(coldat$Clade=="Asterids","#7570b3","black")))
    #get color data frame
    tmp <- left_join(dat.m,coldat) %>% 
      select(Species,Clade,col)
    #get labels
    newdata1 <- data.frame(Species=labels(dend1))
    #merge 
    newdata1 <- left_join(newdata1,tmp)
    #recolor labels
    dend1 <- dend1 %>% 
      color_labels(labels = labels(dend1),col = newdata1$col)
    #get label
    newdata2 <- data.frame(Species=labels(dend2))
    #get color labels
    newdata2 <- left_join(newdata2,tmp)
    #match colors to branches 
    dend2 <- dend2 %>% 
      color_labels(labels = labels(dend2),col = newdata2$col)
    #
    lab1 <- paste(substr(x =  labels(dend1),start = 1,stop = 3),
                  substr(x =  labels(dend1),start = 4,stop = 5),sep="_")
    labels(dend1) <- lab1
    #
    lab2 <- paste(substr(x =  labels(dend2),start = 1,stop = 3),
                  substr(x =  labels(dend2),start = 4,stop = 5),sep="_")
    labels(dend2) <- lab2
    #
    print(all.equal(dend1, dend2, use.tip.label=TRUE,
                    use.edge.length = FALSE, 
                    use.topology = FALSE))
    #get dendrogram list 
    dendlist <- dendlist(dend2,dend1)
    #set frame size 
    par(mar = c(0.2, 0.2,0.2,0.2), oma = c(0.2, 0.2,0.2,0.2)) 
    #generate tanglegram
    dendlist %>% untangle(method = "step2side") %>% 
      tanglegram(   
        # match_order_by_labels=TRUE,
        main_right = clust_title,
        main_left = "Phylogenetic \n Dendrogram",
        main = main1,
        dLeaf_right=0.05,
        dLeaf_left=-0.88,
        lab.cex = 0.94,
        type = "r",
        cex_main = 1,
        columns_width = c(5, 1, 5),
        highlight_distinct_edges = FALSE, # Turn-off dashed lines
        common_subtrees_color_lines = FALSE, # Turn-off line colors
        common_subtrees_color_branches = FALSE)
  }
  #####
  pdf(paste(fname,".pdf",sep=""), height = 3.5, width = 4.33)
  # postscript(paste(fname,".eps",sep=""), 
  #            height = 3.5, 
  #            width = 4.33,
  #            paper="special",
  #            horizontal = TRUE)
  # pdf(paste(fname,".pdf",sep=""), height = 4, width = 6)
  dendlist <- clustp(mymat = matrix1)
  par(mar = c(1, 1,1,1), oma = c(1, 1,1,1)) 
  legend("bottom", legend = c("Poaceae","Fabaceae","Asterids"),
         fill = c("#1b9e77","#d95f02","#7570b3"),
         text.col = c("#1b9e77","#d95f02","#7570b3"),
         bty="n",
         ncol=3)
  dev.off()
  # return(paste(fname,".eps",sep=""))
  return(paste(fname,".pdf",sep=""))
  
}
