#
generate_e120biomass <- function(e120_biomass, choice, hypervolume_plot) {
  library(targets)
  library(tidyverse)
  library(emmeans)
  library(cluster)
  library(dendextend)
  ####
  # tar_load(hypervolume_plot)
  # tar_load(e120_biomass)
  ####
  #We used the number of clusters as a categorical variable 
  #Using Ca, N and K the clustering algorithm reproduced
  #the phylogenetic and functional groups of 
  #Grass, Legume and Forb;
  #Poaceae, Fabaceae and Asterids
  #We can therefore use the
  #experimental design of functional group plantings
  #(GLF)
  #####
  #Here is the proof
  ####
  # clustplot <- function(dat, name_dat, scal, tree) {
  #   dist.mat <- as.matrix(dat)
  #   dendo1 <- function(x, scal) {
  #     # x=dist.mat;scal=FALSE
  #     if (scal == FALSE) {
  #       mono <- x
  #     } else {
  #       mono <- scale(x)
  #     }
  #     dimnames(mono)[[1]] <- name_dat$Species
  #     d1 <- daisy(mono, metric = "euclidean")
  #     d2 <- agnes(d1, method = "ward")
  #     avg_dend_obj <- as.dendrogram(d2)
  #     return(avg_dend_obj)
  #   }
  #   dend <- dendo1(dist.mat, scal = scal)
  #   return(dend)
  # }
  # ####
  # tar_load(meantraitdat)
  # meantraitdat1 <- meantraitdat
  # ####
  # #load in trait data
  # dat.m <- read.csv(meantraitdat1)
  # #get trait names
  # names <- dat.m %>% select(Species)
  # #take vector of traits 
  # mymat <- c("Calcium","Nitrogen","Potassium")
  # mymat1 <- as_vector(mymat)
  # # print(test)
  # #get data matrix of traits 
  # data2 <- dat.m %>% select(all_of(mymat1))
  # #run cluster 
  # dend1 <- clustplot(
  #   dat = data2,
  #   name_dat = names,
  #   scal = TRUE,
  # )
  # #plot(dend1)
  # #get cuts 
  # dend_cuts <- data.frame(cuts=cutree(dend1,3),
  #                         Species=names(cutree(dend1,3)))
  # dend_cuts$cuts
  # #get metadata
  # meta <- dat.m %>% select(Species:Clade)
  # #merge
  # dend_cuts <- left_join(dend_cuts,meta)
  # #show that cuts == clade == funcgroup
  # table(dend_cuts$cuts,dend_cuts$funcgroup_noc3)
  # table(dend_cuts$cuts,dend_cuts$Clade)
  ########
  dat <- read.csv(e120_biomass)
  # make fg richness a factor
  dat$FgNum <- as.factor(dat$FgNum)
  # ols model
  mod1 <- lm(AbvBioAnnProd_20102018mean ~ log(NumSp) *
    FgNum, data = dat)
  summary(mod1)
  anova(mod1)
  #
  car::Anova(mod1, type = "III")
  # get trends
  em <- emtrends(mod1, ~FgNum, var = "log(NumSp)")
  em
  # summary different from zero
  summary(em, infer = c(TRUE, TRUE, TRUE), null = 0)
  #
  dat$residuals_mod1 <- residuals(mod1)
  dat$fit_mod1 <- fitted(mod1)
  # plot residuals
  ggplot(dat, aes(x = fit_mod1, y = residuals_mod1)) +
    geom_point(shape = 21) +
    geom_smooth() +
    geom_text(data = dat[dat$Plot == 273, ], aes(label = Plot), nudge_x = -15)
  # the high plot skews the distribution
  shapiro.test(dat$residuals_mod1)
  ggplot(dat, aes(x = residuals_mod1, y = ..density..)) +
    geom_histogram(
      bins = 10, fill = "grey", color = "Black",
      alpha = 0.5
    ) +
    geom_density()
  # qqnorm(dat$residuals_mod1)
  # qqline(dat$residuals_mod1)
  #
  ggplot(dat, aes(
    x = fit_mod1, y = AbvBioAnnProd_20102018mean,
    fill = FgNum
  )) +
    geom_point(shape = 21) +
    geom_abline(slope = 1, intercept = 0)
  #
  max(dat$AbvBioAnnProd_20102018mean)
  # remove high value plot to test
  dat2 <- dat[dat$Plot != 273, ]
  # ols reg
  mod2 <- lm(AbvBioAnnProd_20102018mean ~ log(NumSp) *
    FgNum, data = dat2)
  summary(mod2)
  anova(mod2)
  car::Anova(mod2, type = "III")
  em2 <- emtrends(mod2, ~FgNum, var = "log(NumSp)")
  em2
  summary(em2, infer = c(TRUE, TRUE, TRUE), null = 0)
  #
  dat2$residuals_mod2 <- residuals(mod2)
  dat2$fit_mod2 <- fitted(mod2)
  #
  ggplot(dat2, aes(x = fit_mod2, y = residuals_mod2)) +
    geom_point(shape = 21) +
    geom_smooth()
  # the residuals are well behaved without the high point
  shapiro.test(dat2$residuals_mod2)
  ggplot(dat2, aes(x = residuals_mod2, y = ..density..)) +
    geom_histogram(
      bins = 10, fill = "grey", color = "Black",
      alpha = 0.5
    ) +
    geom_density()
  # qqnorm(dat2$residuals_mod2)
  # qqline(dat2$residuals_mod2)
  #
  ####
  # generate plot
  # sequence of species richness
  NumSp <- seq(from = 1, to = 16, by = 0.1)
  # fgnum
  FgNum <- unique(dat$FgNum)
  # data frame
  newdat <- expand_grid(NumSp, FgNum)
  newdat$FgNum <- as.factor(newdat$FgNum)
  table(newdat)
  # predictions
  fits <- predict(mod1, se.fit = TRUE)
  #
  dat$fit <- fits$fit
  dat$se <- fits$se.fit
  ###
  # https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-rhttps://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r
  every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
    if (!inverse) {
      if (empty) {
        x[1:nth == 1] <- ""
        x
      } else {
        x[1:nth != 1]
      }
    } else {
      if (empty) {
        x[1:nth != 1] <- ""
        x
      } else {
        x[1:nth == 1]
      }
    }
  }
  ###
  source("Functions/00_e120plantcluster_functions.R")
  # get means per fgnum
  dat.m <- dat %>%
    group_by(NumSp, FgNum) %>%
    summarise(
      se = my.stand(AbvBioAnnProd_20102018mean),
      AbvBioAnnProd_20102018mean = mean(AbvBioAnnProd_20102018mean)
    )
  max(dat$AbvBioAnnProd_20102018mean)
  # generate plot
  p1 <- ggplot(dat, aes(
    x = log(NumSp),
    y = AbvBioAnnProd_20102018mean,
    fill = FgNum,
    col = FgNum,
    shape = FgNum
  )) +
    geom_hline(yintercept = 0) +
    geom_jitter(
      size = 1, alpha = 0.15,
      col = "black",
      height = 0, width = 0.05
    ) +
    geom_line(aes(y = fit)) +
    geom_line(aes(y = fit + se), linetype = 2) +
    geom_line(aes(y = fit - se), linetype = 2) +
    scale_shape_manual(values = c(21, 22, 23), name = "Number of Clusters") +
    scale_color_brewer(palette = "Set1", name = "Number of Clusters") +
    scale_fill_brewer(palette = "Set1", name = "Number of Clusters") +
    geom_errorbar(aes(
      ymax = AbvBioAnnProd_20102018mean + se,
      ymin = AbvBioAnnProd_20102018mean - se
    ),
    width = 0.1,
    data = dat.m
    ) +
    geom_point(data = dat.m, col = "black", size = 2) +
    theme_bw() +
    ylab(expression(
      paste(
        "Aboveground Biomass", " ",
        "(", "g" %.% "m"^-2, ")"
      )
    )) +
    xlab(expression(paste("log"[e], "(Number of Species)"))) +
    scale_x_continuous(
      breaks = c(log(1), log(2), log(4), log(8), log(16)),
      labels = c("1", "2", "4", "8", "16")
    ) +
    guides(fill = guide_legend(nrow=1),
           col = guide_legend(nrow=1),
           shape=guide_legend(nrow=1))+
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.001)),
      breaks = seq(from = 0, 700, by = 50),
      limits = c(0, 700),
      labels = every_nth(seq(from = 0, 700, by = 50), 2, inverse = TRUE)
    ) +
    theme(
      panel.grid.minor = element_blank(),
      title = element_text(size = 12),
      legend.position = c(0.4, 0.85),
      plot.title = element_text(margin = margin(0, 0, 0, 0)),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.margin = margin(c(0, 0, 0, 0)),
      legend.box.background = element_rect(colour = "Black"),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  p1
  library(cowplot)
  # load from other scripts
  hypervolume_plot1 <- hypervolume_plot
  p2 <- ggdraw() +
    draw_image(magick::image_read(hypervolume_plot1, 
                                  density = 1200))
    # draw_image(hypervolume_plot1,clip="on")
  p2
  ####
  p1.square <- p1+theme(aspect.ratio=1)
  ####
  library(gridExtra)
  if (choice == "plot") {
    ggsave(
      filename = "Figures/e120plantcluster_Figure3_ANPP.jpg",
      plot =       plot_grid(p1.square,p2,ncol=1,label_size = 8,
                             labels = c("(a)","(b)")),
      dpi = 2000,
      # fallback_resolution=1200,
      # device=cairo_ps,
      height = 150, width = 82, unit = "mm"
    )
    return("Figures/e120plantcluster_Figure3_ANPP.jpg")
  }
  if (choice == "table") {
    out <- as.data.frame(car::Anova(mod1, type = "III"))
    out$Coefficient <- rownames(out)
    out <- out %>% select(Coefficient, everything())
    source("Functions/00_e120plantcluster_functions.R")
    out$`Sum Sq` <- round(out$`Sum Sq`, 0)
    out$`F value` <- round(out$`F value`, 2)
    out$`Pr(>F)` <- pvalfun(out$`Pr(>F)`)
    tablefun(
      mytab = out,
      tabname = "Supplemental Table 4: Type III sum squares ANOVA",
      filename = "Tables/table_e120plantcluster_SupplementalTable4.docx"
    )
  }
  return("Tables/table_e120plantcluster_SupplementalTable4.docx")
}

