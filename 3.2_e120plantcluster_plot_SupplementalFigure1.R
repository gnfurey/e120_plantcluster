#
plot_top3way_SupplementalFigure1 <- function(top3way_SupplementalFigure1_data1){
  library(targets)
  library(tidyverse)
  # tar_load(top3way_SupplementalFigure1_data)
  # top3way_SupplementalFigure1_data1 <- top3way_SupplementalFigure1_data
  #
  top3way_SupplementalFigure1_data1$var <- str_replace_all(string = top3way_SupplementalFigure1_data1$var,
                                      pattern = "\\&",
                                      replacement ="-")
  top3way_SupplementalFigure1_data1$Category <- as.factor(top3way_SupplementalFigure1_data1$Category)
  top3way_SupplementalFigure1_data1$Category <- factor(top3way_SupplementalFigure1_data1$Category,levels(top3way_SupplementalFigure1_data1$Category)[c(3,2,1)])
  #in results
  top3way_SupplementalFigure1_data1 %>% 
    group_by(Category) %>% 
    summarise(sum=sum(total)) %>% 
    select(Category,sum) %>% 
    mutate(total=sum(sum),
           percent=(sum/total)*100)
  #
  library(viridis)
  library(ggsci)
  textsize1 <- 2
  textsize2 <- 6
  textsize3 <- 2.7
  #plot the data
  #NOTE: the dataframe has to be a bit odd to make this figure
  #the way I wanted it with each trait as a x var
  p1 <- ggplot(top3way_SupplementalFigure1_data1,aes(x=fct_reorder(var,cor),y=total,fill=Category))+
  geom_bar(stat="identity",col="black",width=1)+
  coord_flip()+
    scale_fill_jco()+
  geom_text(aes(y=-0.2,label=cor),size=textsize1)+
  annotate(geom = "text",x=15.7,y=-0.2,label="Cor",size=3)+
  scale_y_continuous(expand = expansion(mult = c(0.1,0.1)),
                     breaks = c(1,2,3))+
  scale_x_discrete(expand = expansion(mult = c(0.05,0.1)))+
  ylab("Number of Traits colored by Category")+
  theme_classic()+
  guides(fill=guide_legend(reverse=TRUE))+
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
        legend.key.height =  unit(0.1, "cm"),
        legend.box.spacing = unit(0.1,"cm"),
        legend.text=element_text(size=6),
    legend.title=element_text(size=6),
        legend.margin=margin(c(0,0,0,0)),
        legend.box.margin=margin(0,0,0,0),
        axis.text = element_text(size=textsize2))+
    geom_segment(aes(x=0.5,y=1,xend=15.5,yend=1))+
    geom_segment(aes(x=0.5,y=2,xend=15.5,yend=2))+
    #CAUTION manual annotation 
    #UPDATE IF THE SET CHANGES 
    annotate(x=seq(from=1,to=15),y=0.5,label="Chemical",geom="text",size=textsize3,col="white")+
    annotate(x=seq(from=1,to=15),y=1.5,label="Chemical",geom="text",size=textsize3,col="white")+
    #reverse order 
    annotate(x=seq(from=1,to=15),y=2.5,col="white",
             label=c("Metabolic","Morphological","Morphological",
                     "Chemical","Morphological","Morphological",
                     "Chemical","Morphological","Morphological",
                     "Chemical",
                     "Chemical","Chemical",
                     "Morphological",
                     "Chemical","Chemical"),
             geom="text",size=textsize3)
  p1
  #check row col assignment
  #build <- ggplot_build(p1)#
 # build$data[[2]]
  ggsave(filename = "Figures/e120plantcluster_SupplementalFigure1.jpg",
       dpi=300,
       plot = p1,height=3.5,width=4.33,unit="in")
  return("Figures/e120plantcluster_SupplementalFigure1.jpg")
}