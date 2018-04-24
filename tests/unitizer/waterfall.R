
library(ggplot2)

unitizer_sect("basic", {
  dat <- data.frame(x=10:1, y=1:10)
  p0 <- ggplot(dat) + geom_waterfall()
  ggplot_build(p0)
  p1 <- ggplot(dat, aes(x=x, y=y)) + geom_waterfall(position="dodge")
  x <- ggplot_build(p1)

  dat2 <- dat
  dat2[5, "x"] <- 5L

  ggplot(dat2, aes(x=x, y=y)) +
    geom_waterfall(position="dodge", color="white")

  ggplot(dat2, aes(x=x, y=y)) +
    geom_waterfall(position=position_dodge2(), color="white")

  dat3 <- dat2
  dat3[5, "y"] <- -2L

  ggplot(dat3, aes(x=x, y=y)) + 
    geom_waterfall(color="red", position="dodge")

  dat4 <- dat2
  dat4[6, "y"] <- -2L

  ggplot(dat3, aes(x=x, y=y)) + 
    geom_waterfall(color="red", position="dodge")


  # Negative values

  dat5 <- data.frame(x=1:3, y=c(1, -2, 4))
  ggplot(dat5, aes(x=x, y=y)) + geom_waterfall(position="identity")

})
unitizer_sect("corner cases", {
  # zero row data frame
  # zero col data frame
  # weird aes:
  #   * provide xmin xor xmax
  #   * width conflicts (provide in geom, position, and data), in particular
  #     geom width vs dodge width need to be controllable independently.
  #   * height
  # vjust and hjust
  # reverse order
})
