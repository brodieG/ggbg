## Run all permutations of colorspace to colorspace translation

color_to_color <- function(
  col, fun, from=names(col), to=from,
  strip.names=TRUE, time=FALSE
) {
  from.to <- expand.grid(from=from, to=to, stringsAsFactors=FALSE)
  fun_t <- function(...) {
    res <- try(if(time) system.time(fun(...))[3] else fun(...), silent=TRUE)
    if(inherits(res, 'try-error')) res <- 'error'
    if(strip.names) unname(res) else res
  }
  args <- c(
    list(fun_t, color=col[from.to$from]),
    from.to
  )
  res.raw <- do.call(Map, args)
  res <- matrix(
    if(time) as.numeric(res.raw) else res.raw,
    nrow=length(from), dimnames=list(from, to)
  )
  res
}
## Given boundaries in a series of 3D spaces, interpolate points within them
##
## @param ranges a 2 x 3 x n array of the ranges, where n is the number of
##   colorspaces we want to interpolate in, the first dimension is the start and
##   end of the range, the second dimension represents the 3 dimensions of the
##   colorspace, and the last dimension the colorspaces.

interpolate_space <- function(
  ranges, steps=16, expand=c(0.2, 1e6), na=TRUE, inf=TRUE
) {
  stopifnot(
    identical(head(dim(ranges), 2), c(2L, 3L)), length(dim(ranges)) == 3
  )
  res <-lapply(seq_len(dim(ranges)[3]),
    function(x) {
      ranges <- split(ranges[,,x], col(ranges[,,x]))
      ranges.ex <- lapply(
        ranges,
        function(y) {
          c(
            seq(from=y[1], to=y[2], length.out=steps),
            min(y) - diff(range(y)) * expand,
            max(y) + diff(range(y)) * expand,
            if(na) c(NA, NaN), if(inf) c(Inf, -Inf)
          )
        }
      )
      do.call(cbind, as.list(do.call(expand.grid, ranges.ex)))
    }
  )
  names(res) <- dimnames(ranges)[[3]]
  res
}

# Translate 'colors' to 3D colorspace

color_all <- function(col, target=names(grDevices::colorspaces)) {
  col.rgb <- t(col2rgb(col) / 255)
  setNames(
    lapply(
      target,
      grDevices::convertColor, color=col.rgb, from='sRGB'
    ),
    target
  )
}
test_conv <- function(
  col, fun, from=names(grDevices::colorspaces), to=from, strip.names=TRUE,
  time=FALSE
) {
  col.all <- color_all(col, from)
  color_to_color(col.all, fun, from, to, strip.names, time)
}

