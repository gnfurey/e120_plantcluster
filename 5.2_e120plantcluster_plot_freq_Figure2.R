####
plot_freq_Figure2 <- function(list1, fname, cutoff) {
  library(targets)
  library(tidyverse)
  library(ggsci)
  ####
  # tar_load(freq_Figure2_data)
  # tar_load(cutoff_choice_0.80)
  # list1 <- freq_Figure2_data
  # fname="phylomaker"
  # cutoff=cutoff_choice_0.80
  #####
  dats <- list1[[1]]
  occurdat <- list1[[2]]
  #get sum and percent
  dats <- dats %>%
    mutate(
      sum = sum(Freq),
      per = round((Freq / sum), 3) * 100
    )
  #arrange
  dats <- dats %>% arrange(desc(per))
  #plotting
  ylab1 <- "Percentage of occurrence"
  tmpdat <- dats 
  tmpdat <- tmpdat %>%
    arrange(desc(Freq))
  tmpdat$VarName <- factor(tmpdat$VarName, levels = unique(tmpdat$VarName))
  tmpdat$Category <- as.factor(tmpdat$Category)
  tmpdat$Category <- factor(tmpdat$Category, levels(tmpdat$Category)[c(3, 2, 1)])
  library(ggsci)
  #
  textsize1 <- 8
  #
  p1 <- ggplot(tmpdat[tmpdat$per>=1,], aes(
    x = VarName, y = per,
    fill = Category
  )) +
    scale_fill_jco() +
    geom_bar(stat = "identity", col = "Black") +
    ylab(ylab1) +
    xlab("Traits used in comparing Trait Dendrogram to Phylogeny") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      # plot.margin = margin(1,1,1,1),
      axis.text.x = element_text(
        angle = 45,
        size = textsize1,
        hjust = 1,
        vjust = 0.95
      ),
      axis.text.y = element_text(size = textsize1),
      axis.title.x = element_text(size = textsize1),
      axis.title.y = element_text(size = textsize1),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = textsize1),
      legend.title = element_text(size = textsize1),
      legend.box.spacing = unit(0, "cm"),
      legend.box.margin = margin(0, 0, 0, 0, "cm")
    ) +
    guides(fill = guide_legend(
      nrow = 1,
      reverse = TRUE
    )) +
    scale_y_continuous(expand = expansion(c(0, 0)))
  p1
  #generate total per category
  dats1 <- dats %>%
    group_by(Category) %>%
    summarise(catsum = sum(Freq)) %>%
    mutate(
      sum = sum(catsum),
      prop = round((catsum / sum) * 100, 1)
    ) 
  dats1# in results 
  sum(dats1$catsum)
  #factor rearranging to match
  dats1$Category <- factor(dats1$Category, levels = unique(dats1$Category))
  dats1$Category <- factor(dats1$Category, levels(dats1$Category)[c(3, 2, 1)])
  #generate subplot
  p2 <- ggplot(dats1, aes(
    x = fct_reorder(Category, c(1, 2, 3)), prop,
    y = prop, fill = Category
  )) +
    geom_bar(
      stat = "identity", position = position_dodge(),
      col = "black"
    ) +
    scale_fill_jco() +
    scale_y_continuous(
      limits = c(NA, 100),
      breaks = c(seq(from = 0, to = 100, by = 20)),
      expand = expansion(c(0, 0))
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = textsize1,angle=45,
                                 vjust=0.85,hjust=0.70),
      axis.title.y = element_text(size = textsize1),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      legend.justification = "right"
    ) +
    ylab(ylab1)
  p2
  #####
  library(cowplot)
  #
  p1_draw <- ggdraw(p1) +
    draw_plot(p2, x = 0.35, y = 0.54, width = 0.6, height = 0.4) +
    draw_plot_label(
      label=c("(a)", "(b)"),
      x=c(0.0, 0.29),
      y=c(0.99, 0.99),
      size = 12
    )
  p1_draw
  out <- list(p1,p2,p1_draw)
  return(out)
  #####
}
