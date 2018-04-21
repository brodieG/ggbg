
library(ggplot2)

unitizer_sect("basic", {
  dat <- data.frame(x=10:1, y=1:10)
  p0 <- ggplot(dat) + geom_waterfall()
  ggplot_build(p0)
  p1 <- ggplot(dat, aes(x=x, y=y)) + geom_waterfall()
  x <- ggplot_build(p1)
  p2 <- ggplot(dat, aes(x=x, y=y)) + geom_waterfall() +
    scale_x_log10() + scale_y_log10()
  x <- ggplot_build(p2)
})
