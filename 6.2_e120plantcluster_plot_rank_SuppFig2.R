
# helping functions
plot_rank_SuppFig2 <- function(rank_SuppFig2_data,
                        nametable_csv,
                        SuppTable3_file){
library(tidyverse)
library(targets)
library(broom)
library(emmeans)
###
# tar_load(rank_SuppFig2_data)
# tar_load(nametable_csv)
# tar_load(SuppTable3_file)
##
newdat1 <- rank_SuppFig2_data
#
tab <- read.csv(SuppTable3_file)
tab1 <- tab %>% filter(pval.fdr < 0.05)
#number of traits with P <0.05
set <- unique(tab1$Trait)
#
namelist <- str_split(string = newdat1$var, pattern = "&", simplify = FALSE)
#get traits with p<0.05
test_set <- function(x){
  all(x %in% set)==TRUE
}
namelist <- set_names(namelist,newdat1$var)
#generate set list 
dataset2 <-  map_dfr(.x = namelist,.f = test_set) %>% 
    pivot_longer(everything(),names_to = "var",values_to = "test") %>% 
  filter(test==TRUE)
dataset3 <- left_join(dataset2,newdat1)
#
dataset3$test <- NULL
dataset3$sub <- "Traits phylosignal P<0.05"
newdat1$sub <- "All 42 Traits"
#
table(dataset3$category)#in caption 
table(newdat1$category)#in caption 
#join data frame
both <- bind_rows(newdat1,dataset3) 
unique(both$sub)  
#get means 
both_m <- both %>% 
  group_by(sub,category) %>%
  summarise(sd=sd(cor),
            se=my.stand(cor),
            mean=mean(cor))
###
both_m$category <- str_replace_all(string = both_m$category,
                               pattern = "\\&",
                               replacement ="\\+")
both_m$category <- as.factor(both_m$category)
levels(both_m$category)
both_m$category <- factor(both_m$category,
                          levels(both_m$category)[
  c(1, 5, 7, 2, 4, 6, 3)
])
unique(both_m$sub)
#
p1 <- ggplot(both_m,aes(x=category,y=mean))+
  geom_bar(stat="identity",position = position_dodge(),
           fill="grey",
           col="black")+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),
                position = position_dodge())+
  scale_y_continuous(breaks=seq(from=0,to=1,by=0.1),
                     expand = expansion(mult=c(0,0.05)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,size=10),
        legend.position = "bottom")+
  facet_wrap(~sub,nrow=1)+
  ylab("Cophenetic Correlation")+
  xlab("Trait Category")
p1
ggsave(plot=p1,filename = "Figures/e120plantcluster_SupplementalFigure2.pdf",
       height=6,width=7)  
return("Figures/e120plantcluster_SupplementalFigure2.pdf")
}
