
library(ggplot2)
library(ggbg)

unitizer_sect("basic", {
  dat <- data.frame(x=3:1, y=1:3)

  gb.0 <- ggplot(dat, aes(x=x, y=y))
  p0 <- gb.0 + geom_col(position=position_waterfall())
  ggplot_build(p0)[["data"]]

  p1 <- gb.0 + geom_col(position="waterfall")
  ggplot_build(p1)[["data"]]

  p2 <- ggplot(dat, aes(x=x)) + geom_bar(position="waterfall")
  ggplot_build(p2)[["data"]]

  # add overlapping values

  dat2 <- data.frame(
    x=c(3, 2, 2, 2, 1, 1), y=1:6, grp=rep(c("A", "B", "C"), lenght.out=6)
  )
  gb.1 <- ggplot(dat2, aes(x=x, y=y, fill=grp))
  p3 <- gb.1 + geom_col(position="waterfall")
  ggplot_build(p3)[["data"]]

  p4 <- gb.1 + geom_col(position=position_waterfall(dodge=FALSE))
  ggplot_build(p4)[["data"]]

  # negative values

  dat3 <- data.frame(
    x=c(3, 2, 2, 2, 1, 1),
    y=c(-3, 1, 4, -6, -1, 10),
    grp=rep(c("A", "B", "C"), lenght.out=6)
  )
  gb.2 <- ggplot(dat3, aes(x=x, y=y, fill=grp))

  p5 <- gb.2 + geom_col(position=position_waterfall(dodge=FALSE))
  ggplot_build(p5)[["data"]]

  p6 <- gb.2 + geom_col(position="waterfall")
  ggplot_build(p6)[["data"]]

  p6a <- gb.2 + geom_col(position=position_waterfall(reverse=TRUE))
  ggplot_build(p6a)[["data"]]

  # preserve

  pw.pres.dodge <- position_waterfall(preserve="single")
  p7 <- gb.2 + geom_col(position=pw.pres.dodge)
  ggplot_build(p7)[["data"]]

  # duplicate group values

  p10 <- ggplot(rbind(dat, dat), aes(x=x, y=y)) +
    geom_col(position="waterfall", color="white")

  ggplot_build(p10)[["data"]]

})
unitizer_sect("other geoms", {
  dat <- data.frame(x=3:1, y=1:3)
  gb.0 <- ggplot(dat, aes(x=x, y=y))
  p8 <- gb.0 + geom_point(position=position_waterfall())

  ggplot_build(p8)[["data"]]
})
unitizer_sect("vjust and labels and facets", {

  dat5 <- rbind(
    cbind(dat3, facet="X"),
    cbind(transform(dat3, x=rev(x)), facet="Y")
  )
  p9 <-
    ggplot(dat5, aes(x=x, y=y, fill=grp)) +
    geom_col(position='waterfall') +
    geom_label(
      aes(label=y, group=grp),
      position='waterfall', color="gray", fill="white"
    ) +
    geom_text(
      aes(label=stat(ycum), vjust=ifelse(y < 0, 1.5, -0.5)),
      position=position_waterfall(vjust=1),
      stat="waterfall",
      size=6
    ) +
    facet_wrap(~facet)

  ggplot_build(p9)[["data"]]
})
unitizer_sect("corner cases", {
  # empty data
  p11 <- ggplot(data.frame(x=numeric(), y=numeric())) +
    geom_col(position='waterfall')
  ggplot_build(p11)[["data"]]

  p12 <- ggplot(data.frame()) + geom_col(position='waterfall')
  ggplot_build(p12)[["data"]]

  # variations on missing data
  dat6 <- data.frame(x=3:1, y=1:3)
  dat6 <- transform(dat6, xmin=x-.6, xmax=x+.6, ymin=0, ymax=y)

  p13 <- ggplot(dat6, aes(xmin=xmin, xmax=xmax, ymax=ymax, ymin=ymin)) +
    geom_rect(position='waterfall')
  ggplot_build(p13)[["data"]]

  p14 <- ggplot(dat6, aes(xmin=xmin, xmax=xmax, y=y)) +
    ggbg:::geom_null(position='waterfall')
  ggplot_build(p14)[["data"]]


  p14 <- ggplot(dat6, aes(xmin=xmin, xmax=xmax, y=y)) +
    geom_tile(position='waterfall')
  ggplot_build(p14)[["data"]]



  p14 <- ggplot(dat, aes(x=x, y=y)) + geom_col(position='waterfall', width=2)
  ggplot_build(p14)

  dat7 <- data.frame(x=4:1, y=1:4, grp=1:2)
  p15 <- ggplot(dat, aes(x=x, y=y, fill=grp)) +
    geom_col(position=position_waterfall(width=.5), width=2)

  ggplot_build(p15)[["data"]]

  # weird aes:
  #   * width conflicts (provide in geom, position, and data), in particular
  #     geom width vs dodge width need to be controllable independently.
  #   * height
  #   * provide ymin and ymax, but not y
  # vjust and hjust
  # start somewhere other than zero
  # a geom without x/y but with xmin/xmax
  # factor / character x values
})
