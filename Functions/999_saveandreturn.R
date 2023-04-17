#
savedat <- function(x,name){
  nameout <- paste("Data/",name,".csv",sep="")
  write_csv(x = x,
            file = nameout)
  return(nameout)
}
#
my_save_plot<- function(plot1,name,height1,width1) {
  # y="tmp"
  filename0 <- paste("Figures/",name,sep="")
  filename0
  ggsave(
    filename = filename0,
    plot = plot1,
    width = width1, height = height1)
  return(filename0)
}
#
save_plot_and_return_path_multipage <- function(x,name,
                                                height1,width1) {
    # y="tmp"
    filename0 <- paste("Figures/",name,sep="")
    filename0
    ggsave(
      filename = filename0,
      plot = gridExtra::marrangeGrob(x, nrow=1, ncol=1),
      width = width1, height = height1)
    return(filename0)
}
#
save_plot_panel_flex<- function(x,name,
                                panel1,column1,
                                height1,width1) {
  # y="tmp"
  filename0 <- paste("Figures/",name,sep="")
  filename0
  plotsave <- exec(.fn = gridExtra::grid.arrange,
                   grobs=
                     x[panel1],ncol=column1)
  ggsave(
    filename = filename0,
    plot = plotsave,
    width = width1, height = height1)
  return(filename0)
}
savetable <- function(x,name){
  filename0 <- paste("Tables/",name,sep="")
  gtsave(x,filename= filename0)
  return(filename0)
}
#
save_plot_and_return_map<- function(list1,choice1,name,ncol1) {
  filename0 <- paste("Figures/",name,sep="")
  filename0
  library(gridExtra)
  list2 <- flatten(list1)
  library(tidyverse)
  library(gridExtra)
  #
  o1 <-  exec(.fn = grid.arrange,
              grobs=list2[choice1],ncol=ncol1)
  ggsave(
    filename = filename0,
    plot = o1,
    width = 7, height = 9)
  return(filename0)
}
saveRDS_return <- function(object,fname){
  saveRDS(object = object,
        file = fname)
  return(fname)
}