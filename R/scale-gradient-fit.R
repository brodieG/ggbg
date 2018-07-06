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
  library(scales)
  library(ggplot2)
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

  dat.bg <-rbind(
    # data.frame(x=seq(n + 1) - 1, y=rep(1, n+1), fill=jet.rgb, type='CIEDE'),
    # data.frame(x=seq(n + 1) - 1, y=rep(1, n+1), fill=jet.rgb, type='LAB'),
    data.frame(x=seq(n + 1) - 1, y=rep(1.05, n+1), fill=jet.rgb, bg='RGB'),
    data.frame(x=seq(n + 1) - 1, y=rep(1.05, n+1), fill=jet.lab.rgb, bg='LAB')
  )
  dat.fg <- data.frame(
    x=rep(seq(n) - .5, 6),
    y=c(
      col.diff.e2000/max(col.diff.e2000),
      col.diff.lab/max(col.diff.lab),
      col.diff.rgb/max(col.diff.rgb),
      col.diff.e2000.2/max(col.diff.e2000.2),
      col.diff.lab.2/max(col.diff.lab.2),
      col.diff.rgb.2/max(col.diff.rgb.2)
    ),
    type=
      rep(rep(c('CIEDE2000', 'LAB - Euclidian', 'RGB - Euclidian'), each=n), 2),
    bg=rep(c('RGB', 'LAB'), each=3 * n)
  )
  library(ggplot2)
  scale.man.vals <- c(
    CIEDE2000='white', `LAB - Euclidian`='darkgrey', `RGB - Euclidian`='black'
  )
  ggplot(data=dat.fg, aes(x=x, y=y)) +
    geom_col(data=dat.bg, fill=dat.bg$fill, width=1) +
    geom_point(aes(fill=type), size=4, shape=23) +
    facet_grid(bg ~.) +
    scale_fill_manual(values=scale.man.vals, name='Distance Metric') +
    coord_cartesian(expand=FALSE) +
    ggtitle('JET Color Ramp w/ Distances b/w Colors') +
    ylab('Distance (Normalized)') +
    theme(
      plot.background=element_rect(fill='#DDDDDD'),
      legend.background=element_rect(fill='#DDDDDD')
    ) +
    NULL

  # Looking to compute equidistant breaks by changing the position of the
  # breaks.  So first step is we need to embed our 3D space (e.g. RGB, Lab,
  # etc.) into a 1D space so we can set our breakpoints along that space.
  # That's easy to do, we can just use the cumulative euclidian distance.
  #
  # Then we need to be able to get the actual 3D coordinates back for any point.
  #
  # Finally, we need to minimize the differences between distances, which will
  # have to be be an optimization?  The challenge is that we need to move around
  # n points and there is no guarantee that by moving point X below:
  #
  # a --- x -- b
  #
  # we'll preserve the total distance.  Maybe we can

  jet.lab <- convertColor(ggbg:::jet, "sRGB", "Lab")
  jet.lab.dists.euc <- sqrt(rowSums((head(jet.lab, -1) - tail(jet.lab, -1))^2))

  jet.lab.dists <- ggbg:::deltaE2000_1(head(jet.lab, -1), tail(jet.lab, -1))
  jet.lab.dists.euc.c <- c(0, cumsum(jet.lab.dists.euc))

  # we want to use euclidean lab distance first because that is less likely to
  # have problems with large color differences.

  if(n < 2) stop("'n' must be at least 2")

  # Need starting points

  # Need distance from a
  # Need distance from b
  # Difference between the two

  # Need function that given 1D value, returns Lab coords
  # For convenience, say 1D value between 0 and 1

  library(vetr)
  make_get_coords <- function(coords) {
    vetr::vetr(matrix(numeric(), ncol=3) && NUM && nrow(.) > 1)

    dists.euc <- sqrt(rowSums((head(coords, -1) - tail(coords, -1))^2))
    dists.euc.c <- c(0, cumsum(dists.euc))
    dists.euc.c.norm <- dists.euc.c / max(dists.euc.c)

    function(x) {
      vetr::vetr(NUM && all_bw(., 0, 1))
      bins <- findInterval(x, dists.euc.c.norm, rightmost.closed=TRUE)
      offset <- (x - dists.euc.c.norm[bins]) /
        (dists.euc.c.norm[bins + 1] - dists.euc.c.norm[bins])

      coords[bins, , drop=FALSE] + offset * (
        coords[bins + 1, , drop=FALSE] - coords[bins, , drop=FALSE]
      )
    }
  }
  # Figures out the distance from the origin coordinate following the path of
  # the intermediate coordinates

  make_get_dist <- function(coords) {
    vetr::vetr(matrix(numeric(), ncol=3) && NUM && nrow(.) > 1)

    dists.euc <- sqrt(rowSums((head(coords, -1) - tail(coords, -1))^2))
    dists.euc.c <- c(0, cumsum(dists.euc))
    dists.euc.c.norm <- dists.euc.c / max(dists.euc.c)

    function(x) {
      vetr::vetr(NUM && all_bw(., 0, 1))
      bins <- findInterval(x, dists.euc.c, rightmost.closed=TRUE)
      offset <- (x - dists.euc.c.norm[bins]) /
        (dists.euc.c.norm[bins + 1] - dists.euc.c.norm[bins])

      dists.euc.c[bins] + offset * (dists.euc.c[bins + 1] - dists.euc.c[bins])
    }
  }
  # Get the position in [0, 1] of our three coordinates.  Since every coordinate
  # except first and last could have been shifted along, we need to reference
  # back to the original coordinates.  Find which of the sequential pairs in the
  # inital coordinates we're in

  make_get_pos <- function(coords) {
    vetr::vetr(matrix(numeric(), ncol=3) && nrow(.) > 1)
    dists.euc <- sqrt(rowSums((head(coords, -1) - tail(coords, -1))^2))
    dists.euc.c <- c(0, cumsum(dists.euc))
    dists.euc.c.norm <- dists.euc.c / max(dists.euc.c)

    function(x) {
      vetr::vetr(matrix(numeric(), nrow=1, ncol=3))
      pos.min <- which.min(
        sqrt(colSums((t(head(coords, -1)) - c(x)) ^ 2)) +
        sqrt(colSums((t(tail(coords, -1)) - c(x)) ^ 2))
      )
      # in theory offset should be the same across the three dimensions, but we
      # just average here just in case not exactly so

      offset <- mean(
        (x - coords[pos.min, , drop=FALSE]) /
        (coords[pos.min + 1, , drop=FALSE] - coords[pos.min, , drop=FALSE])
      )
      dists.euc.c.norm[pos.min] +
        offset * (dists.euc.c.norm[pos.min + 1] - dists.euc.c.norm[pos.min])
    }
  }
  get_coords <- make_get_coords(jet.lab)
  get_dist <- make_get_dist(jet.lab)
  get_pos <- make_get_pos(jet.lab)

  jet.lab.interp <- get_coords((0:n)/n)
  jet.lab.rgb.num <- convertColor(jet.lab.interp, "Lab", "sRGB")
  jet.lab.rgb <- rgb(jet.lab.rgb.num)

  col.diff.e2000.2 <-
    ggbg:::deltaE2000_1(head(jet.lab.interp, -1), tail(jet.lab.interp, -1))
  col.diff.lab.2 <-
    sqrt(rowSums((head(jet.lab.interp, -1) - tail(jet.lab.interp, -1)) ^ 2))
  col.diff.rgb.2 <-
    sqrt(rowSums((head(jet.lab.rgb.num, -1) - tail(jet.lab.rgb.num, -1)) ^ 2))

  col.diff.orig <- c(
    0, cumsum(sqrt(rowSums((head(jet.lab, -1) - tail(jet.lab, -1)) ^ 2)))
  )
  jet.lab.rgb.orig <- rgb(convertColor(jet.lab, "Lab", "sRGB"))
  dat2 <- data.frame(
    x=seq_along(col.diff.orig), y=col.diff.orig, color=jet.lab.rgb.orig
  )
  ggplot(dat2) +
    geom_line(aes(x=x, y=y)) +
    geom_point(aes(x=x, y=y), size=3, shape=24, fill=dat2$color) +
    geom_hline(yintercept=get_dist((0:n)/n), linetype=2)

  # Given values A, x, and B, figure out where x needs to be to be in middle,
  # this is substantially complicated by the need to compute

  make_center_fun <- function(A, B) {
    a.b.coords <- get_coords(c(A, B))

    function(x) {
      vetr::vetr(NUM.1 && all_bw(., A, B))
      x.coords <- get_coords(x)
      diff(ggbg:::deltaE2000_1(rbind(x.coords, x.coords), a.b.coords)) ^ 2
    }
  }
  # Look at the differences between A && B

  jli <- jet.lab.interp

  jli.d <- ggbg:::deltaE2000_1(head(jli, -1), tail(jli, -1))
  jli.max.diff <- which.max(abs(diff(jli.d)))
  A <- jli[jli.max.diff, , drop=FALSE]
  x <- jli[jli.max.diff + 1, , drop=FALSE]
  B <- jli[jli.max.diff + 2, , drop=FALSE]

  A.pos <- get_pos(A)
  B.pos <- get_pos(B)
  x.pos <- get_pos(x)

  center_fun <- make_center_fun(A.pos, B.pos)
  x.pos.new <-
    optim(x.pos, center_fun, lower=A.pos, upper=B.pos, method='Brent')$par

  x.new <- get_coords(x.pos.new)



}
