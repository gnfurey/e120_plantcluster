####
try_request_fun <- function(try_trait_list) {
  tmp <- read.csv(try_trait_list)
  txt <- tmp$TraitID
  write.csv(
    x = unique(txt),
    file = "Data/Alltrytraits.txt", row.names = FALSE, eol = ","
  )
  return("Data/Alltrytraits.txt")
}
