###
generate_fgmean_Figure2 <- function(meantraitdat_long1,
                                    SuppTable3_file1,
                                    nametable_csv) {
  library(targets)
  library(tidyverse)
  source("Functions/00_e120plantcluster_functions.R")
  ###
  # tar_load(meantraitdat_long)
  # tar_load(SuppTable3_file)
  # tar_load(nametable_csv)
  # meantraitdat_long1 <- meantraitdat_long
  # SuppTable3_file1 <- SuppTable3_file
  # nametable_csv <- nametable_csv
  ##
  library(broom)
  library(emmeans)
  nametab <- read.csv(nametable_csv)
  tab <- read.csv(SuppTable3_file1)
  tab1 <- tab %>% filter(pval.fdr < 0.05)
  #
  colnames(tab1) <- str_replace_all(
    string = colnames(tab1),
    pattern = "Trait",
    replacement = "VarName"
  )
  tab2 <- left_join(tab1, nametab)
  ###
  dat.ml_all <- read.csv(meantraitdat_long1)
  #
  dat.ml_all <- dat.ml_all %>% group_by(VarName) %>%
    mutate(val_c = scale_this(val))
  dat.ml_short <- dat.ml_all %>%
    filter(VarName %in% tab2$VarName) #
  #
  dat.ml_short$VarName <- ifelse(dat.ml_short$VarName=="LeafDelta13Carbon",
                               "LeafDelta13C",dat.ml_short$VarName)
  #
  dat.ml_short <- dat.ml_short %>%
    group_by(VarName) %>%
    mutate(val_c = scale_this(val))
  #
  dat.ml_short <- left_join(dat.ml_short, tab2)
  #
  dat.ml_mean <- dat.ml_short %>%
    group_by(Clade, VarName) %>%
    summarise(
      se = my.stand(val_c),
      val_c = mean(val_c)
    )
  #
  dat.ml_mean <- left_join(dat.ml_mean, tab2)
  ###
  dat.ml_mean <- dat.ml_mean %>%
    arrange(desc(K))
  dat.ml_mean$VarName <- factor(dat.ml_mean$VarName,
    levels = unique(dat.ml_mean$VarName)
  )
  ###
  dat.ml_short <- dat.ml_short %>%
    arrange(desc(K))
  dat.ml_short$VarName <- factor(dat.ml_short$VarName,
    levels = unique(dat.ml_short$VarName)
  )
  ####
  library(ggthemes)
  levels(dat.ml_short$VarName)
  dat.ml_short$VarName <- factor(dat.ml_short$VarName,
                              levels(dat.ml_short$VarName)[c(2,3,6,1,4,5,7,8,9,10,11,
                                                             12)])
  dat.ml_mean$VarName <- factor(dat.ml_mean$VarName,
                                levels(dat.ml_short$VarName)[c(2,3,6,1,4,5,7,8,9,10,11,
                                                               12)])
  pvalfun2 <- function(x){
    x <- p.adjust(x,"fdr")
    out <- ifelse(x>0.05,"",
                  ifelse(0.01 < x & x < 0.05,"*",
                    ifelse(0.001 < x & x < 0.01,"**",
                          ifelse(x<0.001,"***","fail"))))
    return(out)
  }
  #
  #
  f3 <- ggplot(dat.ml_short, aes(
    x = Clade, y = val_c,
    fill = Clade, col = Clade
  )) +
    geom_bar(stat="identity", col = "black", size = 0.5,
             data=dat.ml_mean) +
    geom_errorbar(aes(ymax = val_c + se, ymin = val_c - se, width = 0.6),
                  col="darkgrey",
                  data=dat.ml_mean) +
    scale_y_continuous(
      # limits = c(-1.5, 2.5),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    facet_wrap(~ VarName, ncol = 3) +
    guides(
      col = "none",
      fill = "none"
    ) +
    theme_bw() +
    xlab("Trait") +
    ylab("Trait Value") +
    geom_hline(yintercept = 0) +
    scale_colour_brewer(
      palette = "Dark2",
      name = "Clade") +
    scale_fill_brewer(palette = "Dark2",
                      name = "Clade",
                      direction=-1) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(margin = margin(.04, 0, .04, 0, "cm"),
                                  size = 8),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
      legend.position = "bottom",
      axis.text.y = element_text(size = 12)
    )
  f3
  fname <- "Figures/e120plantcluster_Figure2_clademean.pdf"
  ggsave(
    filename = fname,
    plot = f3,
    dpi = 300,
    height = 170,
    width = 82,
    unit = "mm"
  )
  return(f3)
}
#