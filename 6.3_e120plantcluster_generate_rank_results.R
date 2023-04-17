library(targets)
library(tidyverse)
tar_load(rank_SuppFig2_data)
#
newdat <- rank_SuppFig2_data
#
tmp <- newdat %>% ungroup() %>% 
  mutate(rank=1:n()) %>% 
  group_by(category) %>%
    slice_max(n = 1, order_by = cor) %>% 
  arrange(cor)
tmp
table(newdat$category)  
tmp2 <- newdat %>% filter(cor>0.8)
NROW(tmp2)#in capture figure 2
NROW(tmp2)/NROW(newdat)*100#in capture figure 2
