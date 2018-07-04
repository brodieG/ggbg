## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "ggbg - Assorted Ggplot Extensions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.



jet <- t(
  matrix(
    c(
      0.0, 0.0, 0.5,
      0.0, 0.0, 1.0,
      0.0, 1.0, 1.0,
      0.0, 1.0, 0.0,
      1.0, 1.0, 0.0,
      1.0, 0.0, 0.0,
      0.5, 0.0, 0.0
    ),
    nrow=3
  )
)

if(FALSE) {
  n <- 16
  # colour_ramp does a linear interpolation between the two nearest defined
  # colors.  It picks the nearest color simply by counting how many colors we
  # selected, and assuming the distance between colors is the same.

  jet.rgb <- colour_ramp(rgb(ggbg:::jet))((0:n)/n)
  jet.rgb.2 <- rgb(colorRamp(rgb(ggbg:::jet), space="Lab")((0:n)/n) / 255)
  jet.rgb.num <- t(col2rgb(jet.rgb)) / 255
  jet.lab <- convertColor(jet.rgb.num, "sRGB", "Lab")
  col.diff.e2000 <- ggbg:::deltaE2000_1(head(jet.lab, -1), tail(jet.lab, -1))
  col.diff.lab <-
    sqrt(rowSums((head(jet.lab, -1) - tail(jet.lab, -1)) ^ 2))
  col.diff.rgb <-
    sqrt(rowSums((head(jet.rgb.num, -1) - tail(jet.rgb.num, -1)) ^ 2))

  show_col(colour_ramp(rgb(jet))((0:99)/99), labels=FALSE)


  dat.bg <-rbind(
    # data.frame(x=seq(n + 1) - 1, y=rep(1, n+1), fill=jet.rgb, type='CIEDE'),
    # data.frame(x=seq(n + 1) - 1, y=rep(1, n+1), fill=jet.rgb, type='LAB'),
    data.frame(x=seq(n + 1) - 1, y=rep(1.05, n+1), fill=jet.rgb)
  )
  dat.fg <- data.frame(
    x=rep(seq(n) - .5, 3),
    y=c(
      col.diff.e2000/max(col.diff.e2000),
      col.diff.lab/max(col.diff.lab),
      col.diff.rgb/max(col.diff.rgb)
    ),
    type=rep(c('CIEDE2000', 'LAB - Euclidian', 'RGB - Euclidian'), each=n)
  )
  library(ggplot2)
  scale.man.vals <- c(
    CIEDE2000='white', `LAB - Euclidian`='darkgrey', `RGB - Euclidian`='black'
  )
  ggplot(data=dat.fg, aes(x=x, y=y)) +
    geom_col(data=dat.bg, fill=dat.bg$fill, width=1) +
    geom_point(aes(fill=type), size=4, shape=23) +
    # facet_grid(type ~.) +
    scale_fill_manual(values=scale.man.vals, name='Distance Metric') +
    coord_cartesian(expand=FALSE) +
    ggtitle('JET Color Ramp w/ Distances b/w Colors') +
    ylab('Distance (Normalized)') +
    theme(
      plot.background=element_rect(fill='#DDDDDD'),
      legend.background=element_rect(fill='#DDDDDD')
    ) +
    NULL

}
