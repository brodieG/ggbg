## Compare against:
##
## https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants
##
## Start with 54% of the D65 whitepoint and convert to the other reference
## whites

xyz.base <- grDevices:::c2to3(grDevices:::white.points[, 'D65']) * .54
temp.convert <- function(xyz, to.wp) {
  xyz.new <- convertColor(xyz, 'XYZ', 'XYZ', 'D65', to.wp)
}
wps <- c('A', 'B', 'C', 'D50', 'D55', 'D65', 'E')
res.xyz <- vapply(
  wps, convertColor, numeric(3L), color=xyz.base, from='XYZ', to='XYZ',
  from.ref.white='D65'
)
res.rgb <- apply(res.xyz, 2, convertColor, from='XYZ', to='sRGB')
res.rgb.hex <- rgb(t(res.rgb))

## compare to the reference values from wiki; identical except for C, which is
## basically the same (and visually almost indistinguishable; see end of script)

wiki.ref <- c(
  A='#FFB263', B='#D6BEAB', C='#C6C0CA', D50='#D1C0A8',
  D55='#CBC1B2', D65='#C2C2C2', E='#D3BEBA'
)
res.swatch <- cbind(res.rgb.hex, wiki.ref)

## Try to undo all the chromatic adaptations, and get back to
## base

res.xyz.rev <- vapply(
  wps,
  function(x)
    convertColor(
      res.xyz[, x], from='XYZ', to='XYZ', from.ref.white=x, to.ref.white='D65'
    ),
  numeric(3L)
)
all.equal(c(res.xyz.rev), rep(xyz.base, ncol(res.xyz.rev)))  ## TRUE

## These should not make any changes since default reference less color spaces
## adopt whatever reference is set

convertColor(c(54, 10, -20), 'Lab', 'Lab', to.ref.white='D50')
convertColor(c(54, 10, -20), 'Lab', 'Lab', from.ref.white='D50')

convertColor(c(54, 10, -20), 'Luv', 'Luv', from.ref.white='D50')
convertColor(c(54, 10, -20), 'Luv', 'Luv', to.ref.white='D50')

## other tests; no easy way to know whether they are correct other
## than changes seem to be in correct direction

convertColor(c(54, 10, -20), 'Lab', 'Lab', 'D65', 'A')  # less red
convertColor(c(54, 10, -20), 'Lab', 'Luv', 'D65', 'A')
convertColor(c(54, 10, -20), 'Luv', 'Luv', 'D65', 'A')
convertColor(c(54, 10, -20), 'Luv', 'Lab', 'D65', 'A')

convertColor(c(0.5, 0.5, 0.5), 'XYZ', 'sRGB')  # warning
convertColor(c(0.5, 0.5, 0.5), 'XYZ', 'sRGB', 'E', 'D65') # no warning

## Errors

try(convertColor(c(0.5, 0.5, 0.5), 'sRGB', 'sRGB', 'D50', 'D65'))
try(convertColor(c(0.5, 0.5, 0.5), 'sRGB', 'sRGB', 'D65', 'D50'))

## Tools for visual inspection -------------------------------------------------

grob_swatch <- function(colors) {
  stopifnot(is.matrix(colors), is.character(colors))
  grob.list <- vector("list", length(colors))
  len.x <- nrow(colors)
  len.y <- ncol(colors)
  z <- 0

  for(x in seq(len.x)) {
    for(y in seq(len.y)) {
      cat(
        sprintf("x: %d, y: %d %s index: %d\n", x, y, colors[x, y], x + (y - 1) * len.y)
      )
      grob.list[[(z <- z + 1)]] <- grid::rectGrob(
        y=grid::unit((x - 1) / (len.x), "npc"),
        x=grid::unit((len.y - y) / (len.y), "npc"),
        hjust=0, vjust=0,
        gp=grid::gpar(fill=colors[x, y], col=0, lty=0),
        width=grid::unit(1 / len.y, "npc"),
        height=grid::unit(1 / len.x, "npc"),
  ) } }
  res <- do.call(grid::gList, grob.list)
  res.tree <- grid::gTree(children=res)
  class(res.tree) <- c("grobSwatch", class(res.tree))
  res.tree
}
print.grobSwatch <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
grob_swatch(matrix(res.swatch, ncol=2))

## Systematically compare against 3.5.1 results --------------------------------

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
## Generate 16^3 permutations of colors in each space

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
space.input <- interpolate_space(ranges, na=FALSE, inf=FALSE)

# lapply(space.input, head); lapply(space.input, tail)

cc <- color_to_color(space.input, fun=grDevices::convertColor)

## Compare against reference created in R3.5.1; error is from CIE RGB where the
## chromatic adjustment was not carried out.  Large numbers because we use some
## very OOB numbers in testing range.

cc.ref <- readRDS('cc.RDS')
cc.err <- cc
cc.err[] <- Map(function(x, y) round(mean(abs(x-y)), 8), cc, cc.ref)
cc.err

