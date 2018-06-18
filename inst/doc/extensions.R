## ----global_options, echo=FALSE------------------------------------------
options(digits=2, diffobj.pager="off", crayon.enabled=TRUE)
sgr_wrap <- function(x, options){
  browser()
  paste0(
    "hellohello<br />
    <pre class=\"r-output\"><code>",
    fansi::sgr_to_html(x = htmltools::htmlEscape(x)),
    "</code></pre>"
  )
}
knitr::opts_chunk$set(error=TRUE, comment=NA, output=sgr_wrap)
library(ggbg)

## ----eval=FALSE----------------------------------------------------------
#  print.ggplot <- function (x, ...) {
#    data <- ggplot_build(x)
#    gtable <- ggplot_gtable(data)
#    grid.draw(gtable)
#  }

## ---- eval=FALSE---------------------------------------------------------
#  dat <- data.frame(
#    loc=(1:4)^2,
#    val=(5:8)^2,
#    grp=c('A', 'B', 'B', 'C'),
#    pnl=c('P1', 'P1', 'P2', 'P2')
#  )
#  library(ggplot2)
#  p <- ggplot(dat, aes(x=loc, y=val, fill=grp)) +
#    stat_bin2d() +
#    geom_point(position="jitter", shape=21) +
#    facet_wrap(~pnl, scales="free_x") +
#    scale_x_sqrt() +
#    scale_y_sqrt()

## ---- echo=FALSE---------------------------------------------------------
input <- structure(list(loc = c(1, 4, 9, 16), val = c(25, 36, 49, 64),
    grp = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"
    ), class = "factor"), pnl = structure(c(1L, 1L, 2L, 2L), .Label = c("P1",
    "P2"), class = "factor")), .Names = c("loc", "val", "grp",
"pnl"), row.names = c(NA, -4L), class = "data.frame")
output <- structure(list(loc = c(1, 4, 9, 16), val = c(25, 36, 49, 64),
    grp = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"
    ), class = "factor"), pnl = structure(c(1L, 1L, 2L, 2L), .Label = c("P1",
    "P2"), class = "factor"), PANEL = structure(c(1L, 1L, 2L,
    2L), .Label = c("1", "2"), class = "factor")), .Names = c("loc",
"val", "grp", "pnl", "PANEL"), row.names = c(NA, 4L), class = "data.frame")
diffobj::diffPrint(input, output)

## ---- echo=FALSE---------------------------------------------------------
input <- structure(list(loc = c(1, 4, 9, 16), val = c(25, 36, 49, 64),
    grp = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"
    ), class = "factor"), pnl = structure(c(1L, 1L, 2L, 2L), .Label = c("P1",
    "P2"), class = "factor"), PANEL = structure(c(1L, 1L, 2L,
    2L), .Label = c("1", "2"), class = "factor")), .Names = c("loc",
"val", "grp", "pnl", "PANEL"), row.names = c(NA, 4L), class = "data.frame")
output <- structure(list(x = c(1, 4, 9, 16), y = c(25, 36, 49, 64), fill = structure(c(1L,
2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), .Names = c("x", "y", "fill", "PANEL",
"group"), row.names = c(NA, -4L), class = "data.frame")
diffobj::diffPrint(input, output)

## ---- echo=FALSE, results="asis"-----------------------------------------
trans.input <- structure(list(x = c(1, 4, 9, 16), y = c(25, 36, 49, 64), fill = structure(c(1L,
2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), .Names = c("x", "y", "fill", "PANEL",
"group"), row.names = c(NA, -4L), class = "data.frame")
trans.output <- structure(list(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8), fill = structure(c(1L,
2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), .Names = c("x", "y", "fill", "PANEL",
"group"), class = "data.frame", row.names = c(NA, -4L))
diffobj::diffPrint(trans.input, trans.output)

## ----echo=FALSE, results="asis"------------------------------------------
trans.output

## ----echo=FALSE----------------------------------------------------------
structure(list(x = c(1, 2), y = c(5, 6), fill = structure(1:2, .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L, 1L), .Label = c("1", "2"), class = "factor"), group = 1:2), .Names = c("x", "y", "fill", "PANEL", "group"), class = "data.frame", row.names = c(NA, -2L), vars = "PANEL")

## ----echo=FALSE----------------------------------------------------------
structure(list(x = 1, y = 5, fill = structure(1L, .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(1L, .Label = c("1", "2"), class = "factor"), group = 1L), .Names = c("x", "y", "fill", "PANEL", "group"), vars = "PANEL", row.names = 1L, class = "data.frame")

## ------------------------------------------------------------------------
structure(list(xbin = 1L, ybin = 1L, value = 1, x = 1.01666666666667, width = 0.0333333400000002, y = 5.05, height = 0.10000002, count = 1, density = 1), .Names = c("xbin", "ybin", "value", "x", "width", "y", "height", "count", "density"), row.names = 1L, class = "data.frame")

## ----echo=FALSE, results="asis"------------------------------------------
stat.output <- structure(list(xbin = c(1L, 30L, 1L, 30L), ybin = c(1L, 10L, 20L, 30L), value = c(1, 1, 1, 1), x = c(1.01666666666667, 1.98333333666667, 3.01666666666667, 3.98333333666667), width = c(0.0333333400000002, 0.033333333333333, 0.0333333399999995, 0.0333333333333337), y = c(5.05, 5.95000001, 6.95000001, 7.95000001), height = c(0.10000002, 0.0999999999999996, 0.0999999999999996, 0.100000000000001), count = c(1, 1, 1, 1), density = c(1, 1, 1, 1), fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B",
"C"), class = "factor"), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = c(1L, 2L, 2L, 3L)), .Names = c("xbin", "ybin", "value", "x", "width", "y", "height", "count", "density", "fill", "PANEL", "group"), row.names = c(NA, -4L), class = "data.frame")

## ---- echo=FALSE, results="asis"-----------------------------------------
pos.input <- structure(list(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8), fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L, 2L, 2L, 3L), n = 3L)), row.names = c(NA, -4L), class = "data.frame")
pos.output <- structure(list(x = c(0.909038454852998, 2.01686128247529, 3.11151186432689, 3.61253762301058), y = c(5.05882231164724, 6.13020793590695, 7.29328686017543, 8.22853694036603), fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L, 2L, 2L, 3L), n = 3L)), row.names = c(NA, -4L), class = "data.frame")
diffPrint(pos.input, pos.output)

## ---- echo=FALSE, results="asis"-----------------------------------------
map.scale.input <- list(structure(list(xbin = c(1L, 30L, 1L, 30L), ybin = c(1L, 10L, 20L, 30L), value = c(1, 1, 1, 1), x = c(1.01666666666667, 1.98333333666667, 3.01666666666667, 3.98333333666667), y = c(5.05, 5.95000001, 6.95000001, 7.95000001), count = c(1, 1, 1, 1), density = c(1, 1, 1, 1), fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = c(1L, 2L, 2L, 3L), xmin = c(0.999999996666667, 1.96666667, 2.99999999666667, 3.96666667), xmax = c(1.03333333666667, 2.00000000333333, 3.03333333666667, 4.00000000333333), ymin = c(4.99999999, 5.90000001, 6.90000001, 7.90000001), ymax = c(5.10000001, 6.00000001, 7.00000001, 8.00000001)), .Names = c("xbin", "ybin", "value", "x", "y", "count", "density", "fill", "PANEL", "group", "xmin", "xmax", "ymin", "ymax"), row.names = c(NA, -4L), class = "data.frame"), structure(list(x = c(0.802855186536908, 2.15149602368474, 3.06846169922501, 4.36551264487207), y = c(4.76531683262438, 6.03128328304738, 6.72641851641238, 8.20721130892634), fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L, 2L, 2L, 3L), n = 3L)), .Names = c("x", "y", "fill", "PANEL", "group"), row.names = c(NA, -4L), class = "data.frame"))
map.scale.output <- list(structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"), xbin = c(1L, 30L, 1L, 30L), ybin = c(1L, 10L, 20L, 30L), value = c(1, 1, 1, 1), x = c(1.01666666666667, 1.98333333666667, 3.01666666666667, 3.98333333666667), y = c(5.05, 5.95000001, 6.95000001, 7.95000001), count = c(1, 1, 1, 1), density = c(1, 1, 1, 1), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = c(1L, 2L, 2L, 3L), xmin = c(0.999999996666667, 1.96666667, 2.99999999666667, 3.96666667), xmax = c(1.03333333666667, 2.00000000333333, 3.03333333666667, 4.00000000333333), ymin = c(4.99999999, 5.90000001, 6.90000001, 7.90000001), ymax = c(5.10000001, 6.00000001, 7.00000001, 8.00000001)), .Names = c("fill", "xbin", "ybin", "value", "x", "y", "count", "density", "PANEL", "group", "xmin", "xmax", "ymin", "ymax"), class = "data.frame", row.names = c(NA, -4L)), structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"), x = c(0.802855186536908, 2.15149602368474, 3.06846169922501, 4.36551264487207), y = c(4.76531683262438, 6.03128328304738, 6.72641851641238, 8.20721130892634), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L, 2L, 2L, 3L), n = 3L)), .Names = c("fill", "x", "y", "PANEL", "group"), class = "data.frame", row.names = c(NA, -4L)))
diffStr(map.scale.input, map.scale.output)

## ---- echo=FALSE, results="asis"-----------------------------------------
defaults.input <- map.scale.output
defaults.ouput <- list(structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"), xbin = c(1L, 30L, 1L, 30L), ybin = c(1L, 10L, 20L, 30L), value = c(1, 1, 1, 1), x = c(1.01666666666667, 1.98333333666667, 3.01666666666667, 3.98333333666667), y = c(5.05, 5.95000001, 6.95000001, 7.95000001), count = c(1, 1, 1, 1), density = c(1, 1, 1, 1), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = c(1L, 2L, 2L, 3L), xmin = c(0.999999996666667, 1.96666667, 2.99999999666667, 3.96666667), xmax = c(1.03333333666667, 2.00000000333333, 3.03333333666667, 4.00000000333333), ymin = c(4.99999999, 5.90000001, 6.90000001, 7.90000001), ymax = c(5.10000001, 6.00000001, 7.00000001, 8.00000001), colour = c(NA, NA, NA, NA), size = c(0.1, 0.1, 0.1, 0.1), linetype = c(1, 1, 1, 1), alpha = c(NA, NA, NA, NA), width = c(NA, NA, NA, NA), height = c(NA, NA, NA, NA)), row.names = c(NA, -4L), .Names = c("fill", "xbin", "ybin", "value", "x", "y", "count", "density", "PANEL", "group", "xmin", "xmax", "ymin", "ymax", "colour", "size", "linetype", "alpha", "width", "height"), class = "data.frame"), structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"), x = c(0.802855186536908, 2.15149602368474, 3.06846169922501, 4.36551264487207), y = c(4.76531683262438, 6.03128328304738, 6.72641851641238, 8.20721130892634), PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L, 2L, 2L, 3L), n = 3L), shape = c(21, 21, 21, 21), colour = c("black", "black", "black", "black"), size = c(1.5, 1.5, 1.5, 1.5), alpha = c(NA, NA, NA, NA), stroke = c(0.5, 0.5, 0.5, 0.5)), row.names = c(NA, -4L), .Names = c("fill", "x", "y", "PANEL", "group", "shape", "colour", "size", "alpha", "stroke"), class = "data.frame"))
diffStr(defaults.input, defaults.output)

## ----eval=TRUE-----------------------------------------------------------
Example <- ggproto(
  "Example", NULL,
  method1=function(self, x) cat(sprintf("%s %s\n", x, self$secret)),
  method2=function(x) cat(sprintf("%s %s\n", x, self$secret)),
  secret="secret"
)
Example$method1("big")
Example$method2("big")

## ------------------------------------------------------------------------
GeomPoint$setup_data
environment(GeomPoint$setup_data)$f

## ------------------------------------------------------------------------
fun <- Object$method
debug(environment(fun)$f)
fun()

## ----eval=FALSE----------------------------------------------------------
#  geom_point <- function(
#    mapping = NULL, data = NULL, stat = "identity", position = "identity"
#  )
#    layer(
#      data = data, mapping = mapping, stat = stat, geom = GeomPoint,
#      position = position,
#    )

## ------------------------------------------------------------------------
args(environment(StatBin$compute_panel)$f)
args(environment(StatBin$compute_group)$f)

