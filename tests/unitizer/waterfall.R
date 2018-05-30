
library(ggplot2)
library(ggbg)

unitizer_sect("basic A", {
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

  pw.pres.dodge.rev <- position_waterfall(
    preserve="single", reverse=TRUE
  )
  p7a <- gb.2 + geom_col(position=pw.pres.dodge.rev)
  ggplot_build(p7a)[["data"]]

  # duplicate group values

  p10 <- ggplot(rbind(dat, dat), aes(x=x, y=y)) +
    geom_col(position="waterfall", color="white")

  ggplot_build(p10)[["data"]]

  # non-numeric x values

  dat3a <- transform(dat3, xchr=LETTERS[x])
  p22 <- ggplot(dat3a, aes(x=xchr, y=y, fill=grp)) +
    geom_col(position='waterfall')
  ggplot_build(p22)[["data"]]
})
unitizer_sect("other geoms B ", {
  dat <- data.frame(x=3:1, y=1:3)
  gb.0 <- ggplot(dat, aes(x=x, y=y))
  p8 <- gb.0 + geom_point(position=position_waterfall())

  ggplot_build(p8)[["data"]]

  p8a <- gb.0 + geom_tile(position='waterfall')
  ggplot_build(p8a)[["data"]]

  p8b <- gb.0 + geom_tile(width=2, height=2, position='waterfall')
  ggplot_build(p8b)[["data"]]

  # geoms with multiple x/y values per group in one entity

  dat8 <- data.frame(
    x=c(1:3, 1:3), y=c(1,2,1,2,1,2), grp=rep(c("A", "B"), each=3)
  )
  p8c <- ggplot(dat8, aes(x=x, y=y, fill=grp)) +
    geom_polygon(position='waterfall')
  ggplot_build(p8c)[['data']]
})
unitizer_sect("vjust and labels and facets C", {
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
unitizer_sect("y offset D", {
  pD1 <- gb.1 + geom_col(position=position_waterfall(y.start=5))
  ggplot_build(pD1)[['data']]
  pD2 <- gb.1 + geom_col(position=position_waterfall(y.start=-5))
  ggplot_build(pD2)[['data']]
  pD3 <- ggplot(dat2, aes(x=x, y=y)) +
    geom_col(position=position_waterfall(y.start=-5)) + facet_wrap(~grp)
  ggplot_build(pD3)[['data']]
})
unitizer_sect("corner cases E", {
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
    ggbg:::geom_null(position=position_waterfall(vjust=1))
  ggplot_build(p14)[["data"]]

  p15 <- ggplot(dat6, aes(xmin=xmin, y=y)) +
    ggbg:::geom_null(position="waterfall")
  ggplot_build(p15)[["data"]]

  p16 <- ggplot(dat6, aes(xmin=xmin, x=x, y=y)) +
    ggbg:::geom_null(position="waterfall")
  ggplot_build(p16)[["data"]]

  p17 <- ggplot(dat6, aes(x=x, y=y, ymin=ymin)) +
    ggbg:::geom_null(position="waterfall")
  ggplot_build(p17)[["data"]]

  # conflicting widths

  dat7 <- data.frame(
    x=rep(2:1, 2), y=1:4, grp=rep(c('A', 'B'), each=2), width=1:4
  )
  p18 <- ggplot(dat7, aes(x=x, y=y, fill=grp)) +
    geom_col(position=position_waterfall(width=0.5)) +
    geom_point(position=position_waterfall(width=0.5))
  ggplot_build(p18)[["data"]]

  p19 <- ggplot(dat7, aes(x=x, y=y, fill=grp, width=width)) +
    geom_col(position=position_waterfall(width=0.5)) +
    geom_point(position=position_waterfall(width=0.5))
  ggplot_build(p19)[["data"]]

  p20 <- ggplot(dat7, aes(x=x, y=y, fill=grp)) +
    geom_col(position=position_waterfall(width=0.5), width=2) +
    geom_point(position=position_waterfall(width=0.5))
  ggplot_build(p20)[["data"]]

  p21 <- ggplot(dat7, aes(x=x, y=y, fill=grp)) +
    geom_col(position='waterfall', width=2)
  ggplot_build(p21)[["data"]]
})
unitizer_sect("errors F", {
  gb.0 + geom_col(position=position_waterfall(signif=-1))
})
