#
generate_c3c4_FigS3 <- function(meantraitdat_long1) {
  # library(targets)
  # library(tidyverse)
  # tar_load(meantraitdat_long)
  # meantraitdat_long1 <- meantraitdat_long
  dat.m <- read.csv(meantraitdat_long1)
  #
  tmp <- dat.m %>%
    filter(VarName == "LeafDelta13Carbon") %>%
    group_by(funcgroup) %>%
    summarise(
      se = my.stand(val),
      val = mean(val)
    )
  #
  p1 <- ggplot(tmp, aes(x = funcgroup, y = val)) +
    geom_bar(stat = "identity", col = "black", fill = "grey") +
    geom_errorbar(aes(ymax = val + se, ymin = val - se)) +
    ylab(expression(paste("Leaf ", delta^{
      13
    }, "C (\u2030)"))) +
    xlab("Functional Group") +
    theme_bw()
  p1
  fname <- "Figures/e120plantcluster_SupplementalFigure3.jpg"
  ggsave(
    filename = fname,
    plot = p1,
    dpi = 300,
    height = 4,
    width = 5,
    unit = "in"
  )
  return(fname)
}
