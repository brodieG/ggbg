## Generate A List of Functions for Manipulating Colors
##
## @param coords 3 column numeric matrix where each row represents a color along
##   a color ramp.  The colors need not be in any particular colorspace,
##   although it is implicitly assumed perceptual distances on any of the three
##   dimensions represented by the matrix are comparable..
## @value A list of three functions that depend on the value of `coords` (i.e.
##   the functions will produce different results with each set of `coords`):
##   * coords_to_pos: given a matrix of coordinates in the same space as the
##     `coords` parameter value used when creating these functions, return a
##     value in [0, 1] where 0 represents the first color in `coords` and 1 the
##     last color.  Conversion is done using the Euclidian distance along the
##     path of color coordinates.
##   * pos_to_coords: given a numeric vector of positions along the path defined
##     by the `coords` parameter and mapped to [0, 1] based on euclidian
##     distance, returns the coordinates along that path that correspond to
##     those positions.  This is the reverse of `coords_to_pos`.
##   * pos_to_distance: given the position values in [0, 1], return the
##     corresponding cumulative euclidian distances along the path.

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
## Plot of Color Differences
##
## @param colors a list of numeric Lab color matrices (n x 3)
## @param dist_funs a list of distance functions to compute distances with;
##   these should accept two 3 column numeric matrices as inputs and compute the
##   distance between rows in the same positions across the two matrices.
## @value a ggplot2 plot

comp_color_dists <- function(colors, dist_funs) {
  if(!length(n <- unique(vapply(colors, nrow, 1L))) == 1)
    stop("Colors don't all have the same lenghts")

  colors.rgb.num <- lapply(colors, convertColor, 'Lab', 'sRGB')
  colors.rgb <- lapply(colors.rgb.num, rgb)

  dat.bg.list <- lapply(
    names(colors), function(x) {
      data.frame(
        x=seq(n) - 1, y=rep(1.05, n), fill=colors.rgb[[x]],
        type=rep(x, n), stringsAsFactors=FALSE
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

  dat.bg <- dat.bg[order(dat.bg$type),]

  ggplot2::ggplot(data=dat.fg, aes(x=x, y=y)) +
    geom_col(data=dat.bg, fill=dat.bg$fill, width=1) +
    geom_point(aes(colour=dist.fun), size=1, shape=23) +
    facet_grid(type ~.) +
    # scale_fill_manual(values=scale.man.vals, name='Distance Metric') +
    scale_color_grey(start=0, end=1) +
    coord_cartesian(expand=FALSE) +
    ggtitle('Color Ramp w/ Distances b/w Colors') +
    ylab('Distance (Normalized)') +
    theme(
      plot.background=element_rect(fill='#DDDDDD'),
      legend.background=element_rect(fill='#DDDDDD')
    ) +
    NULL
}
## Plot Color Difference Magnitude
##
## Stack barplot showing the colors and the average CIEDE ∆E 2000 difference
## between the colors and a boxplot showing the actual ∆E values.
##
## @param colors a list of Lab numeric 3 column matrices

color_dists <- function(colors.lab, dist_fun) {
  diffs <- lapply(colors.lab, function(x) cumsum(c(0, dist_fun(x))))

  colors <- lapply(colors.lab, lab_to_color)
  colors.stack <- stack(colors)
  colors.stack[['y']] <- stack(diffs)[['values']]

  ggplot(colors.stack, aes(x=ind)) +
    geom_point(
      aes(y=y), fill=colors.stack$values, shape=23, size=4,
      colour=rep("#FFFFFF00", nrow(colors.stack))
    )
}

#' Convert Colors to L*a*b* and Back
#'
#' Colors are either standard R colors or sRGB colors encoded in hex notation
#' such as "#FF00E3".  These are simple wrappers around `grDevices` functions to
#' simplify translation from and to Lab space.
#'
#' @export
#' @importFrom grDevices convertColor col2rgb
#' @param character vector or sRGB hex color codes or standard R colors that can
#'   be interpreted by [col2rgb].

color_to_lab <- function(colors)
  grDevices::convertColor(t(col2rgb(colors) / 255), "sRGB", "Lab")

#' @export
#' @rdname color_to_lab

lab_to_color <- function(lab)
  rgb(grDevices::convertColor(lab, "Lab", "sRGB"))

