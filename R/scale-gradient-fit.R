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
  # colour_ramp does a linear interpolation between the two nearest defined
  # colors.  It picks the nearest color simply by counting how many colors we
  # selected, and assuming the distance between colors is the same.


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

  n <- 64
  jet.rgb.num <- colorRamp(rgb(ggbg:::jet), space="Lab")((0:n)/n) / 255
  jet.rgb.lab <- convertColor(jet.rgb.num, "sRGB", "Lab")
  jet.rgb <- rgb(jet.rgb.num)
  jet.lab <- convertColor(ggbg:::jet, "sRGB", "Lab")

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
  # Get the position in [0, 1] of our three coordinates.  Since every coordinate
  # except first and last could have been shifted along, we need to reference
  # back to the original coordinates.  Find which of the sequential pairs in the
  # inital coordinates we're in

  # NOTE: shoudl check whether we have identical or close enough to coords

  make_coord_funs <- function(coords) {
    vetr::vetr(
      matrix(numeric(), ncol=3) && nrow(.) > 1 && !anyDuplicated(round(., 2))
    )
    dists.euc <- sqrt(rowSums((head(coords, -1) - tail(coords, -1))^2))
    dists.euc.c <- c(0, cumsum(dists.euc))
    dists.euc.c.norm <- dists.euc.c / max(dists.euc.c)
    t.c <- t(coords)

    list(
      # Given Lab coords, return the position in the [0,1] color ramp range

      coords_to_pos = function(x) {
        # vetr::vetr(numeric(3L) || matrix(numeric(), nrow=1, ncol=3))
        c.x <- c(x)

        # Need to find which set of coords we're between;

        if(
          length(
            coord.m <- which(round(sqrt(colSums((t.c - c.x) ^ 2)), 4) == 0)
          )
        ) {
          # start by checking whether we're at exactly one of the existing coords
          if(length(coord.m) > 1)
            stop("Internal Error: matching more than one exact coord.")

          dists.euc.c.norm[coord.m]
        } else {
          # find which pair we're in between

          a.diff <- t.c[, -ncol(t.c), drop=FALSE] - c.x
          b.diff <- c.x - t.c[, -1L, drop=FALSE]

          a.diff.n <- t(t(a.diff) / sqrt(colSums(a.diff ^ 2)))
          b.diff.n <- t(t(b.diff) / sqrt(colSums(b.diff ^ 2)))

          # or approximately in between, anyway

          coord.id <- which.min(colSums((a.diff.n - b.diff.n) ^ 2))

          dists.euc.c.norm[coord.id] +
            (dists.euc.c.norm[coord.id + 1] - dists.euc.c.norm[coord.id]) *
            (
              sqrt(sum((c.x - t.c[, coord.id, drop=FALSE]) ^ 2)) /
              sqrt(
                sum(
                  (
                    t.c[, coord.id + 1, drop=FALSE] -
                    t.c[, coord.id, drop=FALSE]
                  ) ^ 2
              ) )
            )
        }
      },
      # Given the position in the [0, 1] color ramp range, return the Lab coords
      # There is no guarantee that the coords will be equidistant in 3D if they
      # are equidistant in 1D.

      pos_to_coords = function(x) {
        # vetr::vetr(NUM && all_bw(., 0, 1))
        bins <- findInterval(x, dists.euc.c.norm, rightmost.closed=TRUE)
        offset <- (x - dists.euc.c.norm[bins]) /
          (dists.euc.c.norm[bins + 1] - dists.euc.c.norm[bins])

        coords[bins, , drop=FALSE] + offset * (
          coords[bins + 1, , drop=FALSE] - coords[bins, , drop=FALSE]
        )
      },
      # Given the position in the [0, 1] color ramp range, return cumulative Lab
      # distance through the color ramp

      pos_to_dist = function(x) {
        # vetr::vetr(NUM && all_bw(., 0, 1))
        bins <- findInterval(x, dists.euc.c.norm, rightmost.closed=TRUE)
        offset <- (x - dists.euc.c.norm[bins]) /
          (dists.euc.c.norm[bins + 1] - dists.euc.c.norm[bins])

        dists.euc.c[bins] + offset * (dists.euc.c[bins + 1] - dists.euc.c[bins])
      }
    )
  }
  f_lab <- make_coord_funs(jet.lab)
  f_rgb <- make_coord_funs(ggbg:::jet)
  pos <- (0:n) / n
  jet.lab.interp <- f_lab$pos_to_coords(pos)
  jet.lab.interp.rgb.num <- convertColor(jet.lab.interp, "Lab", "sRGB")

  # Check we can recover the positions from the coordiantes

  pos.recover <- apply(jet.lab.interp, 1, f_lab$coords_to_pos)
  stopifnot(all.equal(pos, pos.recover))

  # Given values A, x, and B, figure out where x needs to be to be in middle,
  # this is substantially complicated by the need to compute

  make_center_fun <- function(A, B, diff_fun, coord_funs) {
    fun <- coord_funs$pos_to_coords
    a.b.coords <- fun(c(A, B))

    function(x) {
      # vetr::vetr(NUM.1 && all_bw(., A, B))
      x.coords <- fun(x)
      delta <- diff_fun(rbind(x.coords, x.coords), a.b.coords)
      (delta[1] - delta[2]) ^ 2
    }
  }
  # Look at the differences between A && B

  jli <- jli.prev <- jet.lab.interp
  jli.d.init <- ggbg:::deltaE2000_1(head(jli, -1), tail(jli, -1))

  equalize_dists <- function(coords, diff_fun, coord_funs, iters=1e4) {
    coords.prev.prev <- coords.prev <- coords
    coords.rows <- nrow(coords)
    for(i in seq(iters)) {
      coords.d <- diff_fun(
        coords[-coords.rows, ,drop=FALSE], coords[-1, , drop=FALSE]
      )
      coords.max.diff <- which.max(abs(diff(coords.d)))

      if(!i %% 100)
        cat(
          sprintf(
            "\r%d: %.02f-%.02f at %d",
            i, min(coords.d), max(coords.d), coords.max.diff
          )
        )

      A <- coords[coords.max.diff, , drop=FALSE]
      x <- coords[coords.max.diff + 1, , drop=FALSE]
      B <- coords[coords.max.diff + 2, , drop=FALSE]

      A.pos <- coord_funs$coords_to_pos(A)
      B.pos <- coord_funs$coords_to_pos(B)
      x.pos <- coord_funs$coords_to_pos(x)

      center_fun <- make_center_fun(A.pos, B.pos, diff_fun, coord_funs)
      x.pos.new <-
        optim(x.pos, center_fun, lower=A.pos, upper=B.pos, method='Brent')$par

      x.new <- coord_funs$pos_to_coords(x.pos.new)
      coords.prev.prev <- coords.prev
      coords.prev <- coords
      coords[coords.max.diff + 1, ] <- x.new
    }
    coords
  }
  euc_dist <- function(x, y) sqrt(rowSums((x - y) ^ 2))
  jli.e.2000 <- equalize_dists(jli, deltaE2000_1, f_lab, iters=1e5)
  jli.e.lab <- equalize_dists(jet.lab.interp, euc_dist, f_lab, iters=1e5)
  rgb.e.rgb <- equalize_dists(
    f_rgb$pos_to_coords(pos), euc_dist, f_rgb, iters=1e5
  )
  # plot and compare

  jli.rgb.num <- convertColor(jli.e.2000, "Lab", "sRGB")
  jli.rgb <- rgb(jli.rgb.num)
  jli.e.lab.rgb <- convertColor(jli.e.lab, "Lab", "sRGB")

  rgb.e.rgb.lab <- convertColor(rgb.e.rgb, "sRGB", "Lab")

  # Plot Colors Against Distance Metrics
  #
  # Colors should be in Lab 3 column matrix.

  comp_color_dists <- function(colors, dist_funs) {
    if(!length(n <- unique(vapply(colors, nrow, 1L))) == 1)
      stop("Colors don't all have the same lenghts")

    colors.rgb.num <- lapply(colors, convertColor, 'Lab', 'sRGB')
    colors.rgb <- lapply(colors.rgb.num, rgb)

    dat.bg.list <- lapply(
      names(colors), function(x) {
        data.frame(
          x=seq(n) - 1, y=rep(1.05, n), fill=colors.rgb[[x]],
          type=rep(x, n)
    ) } )
    dat.bg <- do.call(rbind, dat.bg.list)

    dat.fg.list <- lapply(
      names(colors),
      function(x) {
        z <- do.call(
          rbind,
          lapply(
            names(dist_funs),
            function(y) {
              col.h <- head(colors[[x]], -1)
              col.t <- tail(colors[[x]], -1)
              col.dists <- dist_funs[[y]](col.h, col.t)
              data.frame(
                x=seq(n - 1) - .5,
                y=col.dists / max(col.dists),
                dist.fun=rep(y, n - 1)
        ) } ) )
        z[['type']] <- x
        z
      }
    )
    dat.fg <- do.call(rbind, dat.fg.list)

    ggplot2::ggplot(data=dat.fg, aes(x=x, y=y)) +
      geom_col(data=dat.bg, fill=dat.bg$fill, width=1) +
      # geom_point(aes(fill=NA, colour=dist.fun), size=1, shape=23) +
      facet_grid(type ~.) +
      # scale_fill_manual(values=scale.man.vals, name='Distance Metric') +
      # scale_color_grey(start=0, end=1) +
      coord_cartesian(expand=FALSE) +
      ggtitle('JET Color Ramp w/ Distances b/w Colors') +
      ylab('Distance (Normalized)') +
      theme(
        plot.background=element_rect(fill='#DDDDDD'),
        legend.background=element_rect(fill='#DDDDDD')
      ) +
      NULL
  }
  comp_color_dists(
    list(
      CIEDE=jli.e.2000, Lab=jli.e.lab, colorRamp=jet.rgb.lab, RGB=rgb.e.rgb.lab
    ),
    list(
      deltaE=deltaE2000_1,
      Lab=function(x, y) sqrt(rowSums((x - y) ^ 2)),
      RGB=function(x, y) {
        x.rgb <- convertColor(x, "Lab", "sRGB")
        y.rgb <- convertColor(y, "Lab", "sRGB")
        sqrt(rowSums((x.rgb - y.rgb) ^ 2))
  } ) )

}
