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

space.input <- ggbg:::interpolate_space(ranges, na=FALSE, inf=FALSE)

# because grDevices::convertColor can't handle NAs for Luv conversion, we need
# to filter them out (and as of R 3.6 can't handle Lab either.

# space.input[['Luv']] <-
#   space.input[['Luv']][!is.na(rowSums(space.input[['Luv']])),,drop=FALSE]
# space.input[['Lab']] <-
#   space.input[['Lab']][
#     !is.na(rowSums(space.input[['Lab']])) &
#     is.finite(rowSums(space.input[['Lab']])),,drop=FALSE
#   ]

# sapply(space.input, function(x) sum(duplicated(x, MARGIN=1)))

cc1 <- ggbg:::color_to_color(space.input, fun=ggbg:::convertColor)
cc2 <- ggbg:::color_to_color(space.input, fun=ggbg:::convertColor2)
cc3 <- ggbg:::color_to_color(space.input, fun=grDevices::convertColor)

all.equal(cc1, cc2)
all.equal(cc1, cc3)

identical(cc1, cc2)
identical(cc1, cc3)
identical(cc2, cc3)

cc1a <- cc1
cc2a <- cc2
cc3a <- cc3
cc1a[] <- lapply(cc1a, function(x) {x[is.nan(x)] <- NA; x})
cc2a[] <- lapply(cc2a, function(x) {x[is.nan(x)] <- NA; x})
cc3a[] <- lapply(cc3a, function(x) {x[is.nan(x)] <- NA; x})

identical(cc1a, cc3a)
identical(cc2a, cc3a)

# Timings

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
options(digits=2)

## minimal changes to original R code
rowSums(cc3t, dims=2)/rowSums(cc1t, dims=2)

## additional Optimizations
rowSums(cc3t, dims=2)/rowSums(cc2t, dims=2)

x <- matrix(runif(3e5), ncol=3)
treeprof::treeprof(ggbg:::convertColor2(x, 'sRGB', 'CIE RGB'))

rowSums(cc1t, dims=2)/rowSums(cc2t, dims=2)

rowSums(cc2t, dims=2)
rowSums(cc3t, dims=2)


n <- 1000
v <- (0:n) / n
cr1_lab <- ggbg:::colorRamp(rgb(ggbg:::jet), space='Lab')
jet1k <- cr1_lab(v)

## 1e3 colors along Jet color ramp, sRGB -> Lab -> sRGB round trimp
##
## 1: grDevices
## 2: my patch, minimal changes
## 3: my patch, more optimization
## 4: compiled code (farver::convert_colour)

microbenchmark::microbenchmark(
  {
    cc.rgb.lab.0 <- grDevices::convertColor(jet1k, 'sRGB', 'Lab')
    grDevices::convertColor(cc.rgb.lab.0, 'Lab', 'sRGB')
  },
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

y <- runif(1e5)
z.2 <- y < .2
z.5 <- y < .5
z.which.2 <- which(z.2)
z.which.5 <- which(z.5)

microbenchmark::microbenchmark(
  v[z.2] <- y[z.2],
  v[z.5] <- y[z.5],
  v[z.which.2] <- y[z.which.2],
  v[z.which.5] <- y[z.which.5],
  which(z.2),
  which(z.5)
)

y <- runif(1e4)

microbenchmark::microbenchmark(
  ifelse(y < .5, y, -y),
  {
    x <- y
    y.else <- which(
      if(anyNA(y)) !is.na(y) & y < .5 else y < .5
    )
    x[y.else] <- -y[y.else]
    x
  },
  {
    x <- y
    y.else <- !is.na(y) & y < .5
    x[y.else] <- -y[y.else]
    x
  }
)

## convertColor2 millisecond timings:
##
##           Apple RGB sRGB CIE RGB XYZ Lab Luv
## Apple RGB        72   79      86  29  58  91
## sRGB             76   94      84  40  72  88
## CIE RGB          83  101      83  29  69  87
## XYZ              42   57      48   1  42  53
## Lab              71   87      73  34  66  87
## Luv              52   62      55  15  38  41

##     min     lq   mean median     uq    max neval
##  144528 168521 187889 186223 201163 266396   100
##    1612   1874   2047   1995   2151   3617   100
##     917   1022   1129   1072   1132   2291   100
##     297    355    385    382    394    636   100


##> rowSums(cc3t, dims=2)/rowSums(cc1t, dims=2)
##           Apple RGB sRGB CIE RGB XYZ Lab Luv
## Apple RGB        45   38      42  32  59  46
## sRGB             47   37      46  40  70  52
## CIE RGB          48   38      41  31  58  47
## XYZ              59   56      59 174 111 120
## Lab              84   71      80 101 100 111
## Luv              70   62      80 106 103  91

## > rowSums(cc3t, dims=2)/rowSums(cc2t, dims=2)
##           Apple RGB sRGB CIE RGB XYZ Lab Luv
## Apple RGB        91   94      87  80 121 106
## sRGB             87   99      74  67  99 100
## CIE RGB          91   96      91  68 116 110
## XYZ              84  116     111 Inf 151 269
## Lab             155  154     161 257 207 257
## Luv             131  129     124 211 184 203
