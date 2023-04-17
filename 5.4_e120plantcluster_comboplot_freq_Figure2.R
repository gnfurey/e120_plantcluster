comboplot_freq_Figure2 <- function(plot_freq_Figure2_dat, 
                                   fgmean_Figure2) {
  
library(targets)
library(cowplot)
library(tidyverse)
#
# tar_load(plot_freq_Figure2_file)
# tar_load(fgmean_Figure1)
#
p1 <- plot_grid(
  plot_freq_Figure2_dat[[3]],
  fgmean_Figure2,labels = c("","(c)"),
  label_size = 12,
  ncol=2)
p1
ggsave(filename = "Figures/e120plantcluster_Figure2_combo.eps",
       height=5,width=7,
       dpi=600,
       device=cairo_ps,
       plot = p1)
return("Figures/e120plantcluster_Figure2_combo.eps")
}

