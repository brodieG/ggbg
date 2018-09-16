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
  cr2_lab(v),
  cr3_lab(v),
  cr1_rgb(v),
  cr2_rgb(v)
)

system.time(cr1_lab(v))
system.time(cr2_lab(v))

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
