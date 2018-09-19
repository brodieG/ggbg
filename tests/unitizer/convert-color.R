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

space.input <- ggbg:::interpolate_space(ranges)

# because grDevices::convertColor can't handle NAs for Luv conversion, we need
# to filter them out

space.input[['Luv']] <-
  space.input[['Luv']][!is.na(rowSums(space.input[['Luv']])),,drop=FALSE]

# sapply(space.input, function(x) sum(duplicated(x, MARGIN=1)))

cc1 <- ggbg:::color_to_color(space.input, fun=ggbg:::convertColor)
cc2 <- ggbg:::color_to_color(space.input, fun=ggbg:::convertColor2)
cc3 <- ggbg:::color_to_color(space.input, fun=grDevices::convertColor)

all.equal(cc1, cc2)
all.equal(cc1, cc3)

identical(cc1, cc3)
identical(cc2, cc3)

cc2a <- cc2
cc3a <- cc3
cc2a[] <- lapply(cc2a, function(x) {x[is.nan(x)] <- NA; x})
cc3a[] <- lapply(cc3a, function(x) {x[is.nan(x)] <- NA; x})

identical(cc2a, cc3a)

id <- Map(identical, cc2, cc3)
attributes(id) <- attributes(cc3)

n <- 10
cc1t <- replicate(
  n, ggbg:::color_to_color(space.input, fun=ggbg:::convertColor, time=T)
)
cc2t <- replicate(
  n, ggbg:::color_to_color(space.input, fun=ggbg:::convertColor2, time=T)
)
cc3t <- replicate(
  n, ggbg:::color_to_color(space.input, fun=grDevices::convertColor, time=T)
)


farver <- function(x, from, to) {

}

n <- 1000
v <- (0:n) / n
cr1_lab <- ggbg:::colorRamp(rgb(ggbg:::jet), space='Lab')
jet1k <- cr1_lab(v)

microbenchmark::microbenchmark(
  {
    cc.rgb.lab.1 <- ggbg:::convertColor(jet1k, 'sRGB', 'Lab')
    ggbg:::convertColor(cc.rgb.lab.1, 'Lab', 'sRGB')
  },
  {
    cc.rgb.lab.2 <- ggbg:::convertColor2(jet1k, 'sRGB', 'Lab')
    ggbg:::convertColor2(cc.rgb.lab.2, 'Lab', 'sRGB')
  },
  {
    cc.rgb.lab.3 <- farver::convert_colour(jet1k, 'rgb', 'lab')
    farver::convert_colour(cc.rgb.lab.3,'lab', 'rgb')
  }
)
ft <- function() {
  cc.rgb.lab.1 <- ggbg:::convertColor(jet1k, 'sRGB', 'Lab')
  ggbg:::convertColor(cc.rgb.lab.1, 'Lab', 'sRGB')
}
treeprof::treeprof(ft())
ft2 <- function() {
  cc.rgb.lab.2 <- ggbg:::convertColor2(jet1k, 'sRGB', 'Lab')
  ggbg:::convertColor2(cc.rgb.lab.2, 'Lab', 'sRGB')
}
treeprof::treeprof(ft2())


x <- runif(1e5)
y <- runif(1e5)
z <- runif(1e5)

microbenchmark::microbenchmark(
  cbind(x, y, z),
  matrix(c(x, y, z), ncol=3)
)

system.time(cr1_lab(v))
treeprof::treeprof(ggbg:::convertColor2(jet1k, 'sRGB', 'Lab'))

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


x <- matrix(runif(3e5), ncol=3)

microbenchmark::microbenchmark(
  cbind(x[2,], x[2,], x[2,]),
  {
    y <- x[2,]; cbind(y, y, y)
  }
)
