####
"%ni%" <- Negate("%in%")
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
my.stand <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
get_funcgroup<-function(Specid) {
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  fg <- taxdat[match(Specid, taxdat$Specid),"funcgroup"]
  fg
}
get_family<-function(Specid) {
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  taxdat[match(Specid, taxdat$Specid),"Family"]
}
get_specidfrom5<-function(Specid_5) {
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  taxdat[match(Specid_5, taxdat$Specid_5),"Specid"]
}
get_fiveID <- function(ccesr_name){
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  fg <- taxdat[match(ccesr_name, taxdat$ccesr_name),"Specid_5"]
  return(fg)
}
get_speciessplit <- function(x){
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  fg <- taxdat[match(x, taxdat$Specid_5),"name_hyphen"]
  return(fg)
}
get_fiveID_hyphen <- function(x){
  taxdat <-read.csv("Data/data_e120plantcluster_e120species_KEEP.csv")
  fg <- taxdat[match(x, taxdat$name_hyphen),"Specid_5"]
  return(fg)
}
tablefun <- function(mytab,tabname,filename){
  # library(flextable)
  # library(officer)
  #mytab dataframe
  #tabname string
  #filename string
  tmp <- flextable::flextable(mytab) %>% flextable::fontsize(size=12)
  flextable::set_table_properties(tmp, width = 1, layout = "autofit")
  tmp <- flextable::font(tmp,fontname = "Times")
  doc <- officer::read_docx() %>%
    officer::body_add_par(value = tabname, style = "Normal") %>%
    flextable::body_add_flextable(value = tmp)
  print(doc, target = filename)
}
pvalfun <- function(x){
  out <- ifelse(x>0.01,round(x,3),
          ifelse(0.001 < x & x < 0.01,"<0.01",
                ifelse(x<0.001,"<0.001",x)))
  out1 <- paste("P = ",out,sep="")
  return(out1)
}
pvalfun1 <- function(x){
  out <- ifelse(x>0.01,round(x,3),
                ifelse(0.001 < x & x < 0.01,"<0.01",
                       ifelse(x<0.001,"<0.001",x)))
  return(out)
}