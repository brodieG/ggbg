jet.16 <- c("#000080", "#0000B1", "#0000E4", "#414EFF", "#57A8FF", "#00FFFF", "#45FFB0", "#35FF59", "#6CFF00", "#C0FF00", "#FFFF00", "#FFB500", "#FF5F00", "#E40001", "#B10003", "#800000")

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
# Run all permutations of colorspace to colorspace translation

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
  res <- matrix(
    do.call(Map, args), nrow=length(from), dimnames=list(from, to)
  )
  res
}
test_conv <- function(
  col, fun, from=names(grDevices::colorspaces), to=from, strip.names=TRUE,
  time=FALSE
) {
  col.all <- color_all(col, from)
  color_to_color(col.all, fun, from, to, strip.names, time)
}
jet.all <- color_all(jet.16, names(grDevices::colorspaces))

## Need to make a simplified version

unitizer_sect("compare to grDevices", {
  cc1 <- test_conv(jet.16, grDevices::convertColor)
  cc2 <- test_conv(jet.16, ggbg:::convertColor)

  all.equal(cc1, cc2)

  cc3 <- test_conv(jet.16[1], grDevices:::convertColor)
  cc4 <- test_conv(jet.16[1], ggbg:::convertColor)

  all.equal(cc3, cc4)

  cc5 <- test_conv(jet.16[1], ggbg:::convertColor, strip.names=FALSE)
  cc5n <- lapply(cc5, colnames)
  dim(cc5n) <- dim(cc5)
  dimnames(cc5n) <- dimnames(cc5)

  col.all <- color_all(jet.16)
  col.all.0 <- lapply(col.all, '[', 0, , drop=FALSE)

  cc6 <- color_to_color(col.all.0, ggbg:::convertColor)
  cc7 <- color_to_color(col.all.0, grDevices::convertColor)
})

n <- 1000
v <- (0:n) / n
cr1_lab <- ggbg:::colorRamp(rgb(ggbg:::jet), space='Lab')
cr2_lab <- grDevices::colorRamp(rgb(ggbg:::jet), space='Lab')
cr1_rgb <- ggbg:::colorRamp(rgb(ggbg:::jet), space='rgb')
cr2_rgb <- grDevices::colorRamp(rgb(ggbg:::jet), space='rgb')
cr3_lab <- scales::colour_ramp(rgb(ggbg:::jet))


microbenchmark::microbenchmark(times=10,
  cr1_lab(v),
  rgb(cr1_lab(v)/255),
  cr2_lab(v),
  cr3_lab(v),
  cr1_rgb(v),
  cr2_rgb(v)
)
## Determine full possible range of colors; here we do the

rgb <- as.matrix(do.call(expand.grid, replicate(3, 0:1, simplify=FALSE)))
grDevices::convertColor(rgb, 'sRGB', 'Lab')
grDevices::convertColor(rgb, 'sRGB', 'XYZ')
grDevices::convertColor(rgb, 'sRGB', 'Luv')

XYZ <- as.matrix(do.call(expand.grid, replicate(3, 0:1, simplify=FALSE)))
grDevices::convertColor(XYZ, 'XYZ', 'Lab', clip=NA)
grDevices::convertColor(XYZ, 'XYZ', 'sRGB', clip=NA)
grDevices::convertColor(XYZ, 'XYZ', 'Luv', clip=NA)

## Ranges
##
## sRGB: [0,1]
## Lab: [0,100], [-440,440], [-168,173]  common range [-128,127] for ab
## Luv: [0,100], [-259,67], [0, 171]     common range +-100 for uv

ranges.raw <- c(
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0, 100,-128, 128,-128, 128,  # Lab
  # Luv; supposed to be +- 100, but rgb conv suggests it's wider than that
  0, 100,-180, 180,-180, 180
)
ranges <- array(
  ranges.raw, dim=c(2, 3, length(ranges.raw) / (2 * 3)),
  dimnames=list(
    range=c('lo', 'hi'), NULL,
    space=c('Apple RGB', 'sRGB', 'CIE RGB', 'XYZ', 'Lab', 'Luv')
  )
)
# For each input space, generate permutation of values in range, outside of
# range, along with NA, NaN, Inf, -Inf cases.

interpolate_space <- function(ranges, steps=16, expand=c(0.2, 1e6)) {
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
            seq(from=y[1], to=y[1], length.out=steps),
            min(y) - diff(range(y)) * expand,
            max(y) + diff(range(y)) * expand,
            NA, NaN, Inf, -Inf
          )
        }
      )
      do.call(cbind, as.list(do.call(expand.grid, ranges.ex)))
    }
  )
  names(res) <- dimnames(ranges)[[3]]
  res
}
space.input <- interpolate_space(ranges)
cc1 <- color_to_color(space.input, fun=ggbg::convertColor)

ggbg::convertColor


space.output <- lapply(
  names(space.input), function(x) {
    spc.in <- space.input[[x]]


  }


jet1k <- cr1_lab(v)

microbenchmark::microbenchmark(
  cc1 <- ggbg:::convertColor(jet1k, 'sRGB', 'Lab'),
  cc2 <- grDevices::convertColor(jet1k, 'sRGB', 'Lab'),
  cc3 <- farver::convert_colour(jet1k, 'rgb', 'lab')
)



system.time(cr1_lab(v))
treeprof::treeprof(cr1_lab(v))

cc1k <- test_conv(jet1k, grDevices::convertColor)
cc2k <- test_conv(jet1k, ggbg:::convertColor, time=T)


cc1 <- test_conv(character(), grDevices::convertColor, 'Lab', 'XYZ')
cc2 <- test_conv(character(), ggbg:::convertColor, 'Lab', 'XYZ')

jet.16.rgb <- t(col2rgb(jet.16))/255
jet.16.lab <- grDevices::convertColor(jet.16.rgb, 'sRGB', 'Lab')

ggbg:::convertColor(jet.16.lab, 'Lab', 'Apple RGB')

ggbg:::convertColor(matrix(c(1,1,1), ncol=3), 'Lab', 'Luv')
ggbg:::convertColor(matrix(numeric(), ncol=3), 'Lab', 'Luv')
grDevices:::convertColor(matrix(c(1,1,1), ncol=3), 'Lab', 'Luv')
grDevices::convertColor(matrix(numeric(), ncol=3), 'Lab', 'Luv')

unitzer_sect(

)
