library(ggbg)
library(ggplot2)

unitizer_sect("Car", {
  p <- ggplot(
    geom.car.data, aes(x=x, y=y, length=length, width=width, fill=label)
  ) + geom_car()
  unclass(ggplot_build(p)$plot)
})
