## ----global_options, echo=FALSE------------------------------------------
options(
  digits=2, diffobj.pager="off", crayon.enabled=TRUE,
  # diffobj.format="ansi256", diffobj.brightness="light",
  diffobj.format="html", diffobj.disp.width=120,
  diffobj.ignore.white.space=TRUE
)
sgr_wrap <- function(x, options){
  paste0(
    "<pre class=\"r-output\"><code>",
    fansi::sgr_to_html(x = htmltools::htmlEscape(x)),
    "</code></pre>"
  )
}
# knitr::knit_hooks$set(output=sgr_wrap)
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(ggbg)

## ----eval=FALSE----------------------------------------------------------
#  print.ggplot <- function (x, ...) {
#    data <- ggplot_build(x)        # step 2: build
#    gtable <- ggplot_gtable(data)  # step 3: create grobs
#    grid.draw(gtable)              # step 4: display grobs
#  }

## ---- eval=FALSE---------------------------------------------------------
#  dat <- data.frame(
#    loc=1:4,
#    val=5:8,
#    grp=c('A', 'B', 'B', 'C'),
#    pnl=c('P_ONE', 'P_ONE', 'P_TWO', 'P_TWO')
#  )
#  library(ggplot2)
#  p <- ggplot(dat, aes(x=loc)) +
#    stat_bin(bins=1, aes(weight=val), fill="lightgray") +
#    geom_point(aes(y=val, fill=grp), position="jitter", shape=21, size=4) +
#    facet_wrap(~pnl, scales="free_x") +
#    scale_x_reverse()

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  p <- ggplot(dat, aes(x=loc)) +
#    stat_bin(bins=1, aes(weight=val), fill="lightgray") +
#    geom_point(
#      aes(y=val, fill=grp), position=position_jitter(seed=24), shape=21, size=4
#    ) +
#    facet_wrap(~pnl, scales="free_x") +
#    scale_x_reverse()

## ---- echo=FALSE, results="asis"-----------------------------------------
facet.map.in <- structure(list(loc = 1:4, val = 5:8, grp = structure(c(1L, 2L,
2L, 3L), .Label = c("A", "B", "C"), class = "factor"), pnl = structure(c(1L,
1L, 2L, 2L), .Label = c("P_ONE", "P_TWO"), class = "factor")), class = "data.frame", row.names = c(NA,
-4L))
facet.map.out <- structure(list(loc = 1:4, val = 5:8, grp = structure(c(1L, 2L,
2L, 3L), .Label = c("A", "B", "C"), class = "factor"), pnl = structure(c(1L,
1L, 2L, 2L), .Label = c("P_ONE", "P_TWO"), class = "factor"),
    PANEL = structure(c(1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor")), row.names = c(NA,
4L), class = "data.frame")

diffobj::diffPrint(facet.map.in, facet.map.out)

## ---- echo=FALSE, results="asis"-----------------------------------------
comp.aes.in <- facet.map.out
comp.aes.out <- structure(list(y = 5:8, fill = structure(c(1L, 2L, 2L, 3L), .Label = c("A",
"B", "C"), class = "factor"), x = 1:4, PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), row.names = c(NA, -4L), class = "data.frame")
diffobj::diffPrint(comp.aes.in, comp.aes.out)

## ---- echo=FALSE, results="asis"-----------------------------------------
trans.in <- comp.aes.out
trans.out <- structure(list(x = -1:-4, y = 5:8, fill = structure(c(1L, 2L,
2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), class = "data.frame", row.names = c(NA,
-4L))

diffobj::diffPrint(trans.in, trans.out)

## ------------------------------------------------------------------------
scale <- ggplot2::scale_x_continuous()
scale$aesthetics

## ---- echo=FALSE---------------------------------------------------------
stat.comp.in <- structure(list(x = c(-1, -2, -3, -4), weight = 5:8, PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = c(-1L,
-1L, -1L, -1L)), row.names = c(NA, -4L), class = "data.frame")
stat.comp.in

## ----echo=FALSE----------------------------------------------------------
stat.comp.out <- structure(list(count = c(11, 15), x = c(-1.5, -3.5), xmin = c(-2,
-4), xmax = c(-1, -3), width = c(1, 1), density = c(1, 1), ncount = c(1,
1), ndensity = c(1, 1), PANEL = structure(1:2, .Label = c("1",
"2"), class = "factor"), group = c(-1L, -1L)), class = "data.frame", row.names = c(NA,
-2L))

## ----echo=FALSE, results="asis"------------------------------------------
diffobj::diffStr(stat.comp.in, stat.comp.out)

## ---- echo=FALSE, results="asis"-----------------------------------------
stat.map.in <- stat.comp.out
stat.map.out <- structure(list(y = c(11, 15), count = c(11, 15), x = c(-1.5,
-3.5), xmin = c(-2, -4), xmax = c(-1, -3), width = c(1, 1), density = c(1,
1), ncount = c(1, 1), ndensity = c(1, 1), PANEL = structure(1:2, .Label = c("1",
"2"), class = "factor"), group = c(-1L, -1L)), class = "data.frame", row.names = c(NA,
-2L))

diffobj::diffStr(stat.map.in, stat.map.out)

## ---- echo=FALSE, results="asis"-----------------------------------------
pos.comp.in <- structure(list(x = c(-1, -2, -3, -4), y = c(5, 6, 7, 8), fill = structure(c(1L,
2L, 2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), row.names = c(NA, -4L), class = "data.frame")
pos.comp.out <- structure(list(x = c(-1.16594083365053, -2.22008708454669, -2.83662163671106,
-3.9848822819069), y = c(5.13009568769485, 6.33635502737016,
6.8237884586677, 8.21105636898428), fill = structure(c(1L, 2L,
2L, 3L), .Label = c("A", "B", "C"), class = "factor"), PANEL = structure(c(1L,
1L, 2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), row.names = c(NA, -4L), class = "data.frame")
diffobj::diffPrint(pos.comp.in, pos.comp.out)

## ---- echo=FALSE, results="asis"-----------------------------------------

map.scale.in <- pos.comp.out
map.scale.out <- structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"
), x = c(-1.16594083365053, -2.22008708454669, -2.83662163671106,
-3.9848822819069), y = c(5.13009568769485, 6.33635502737016,
6.8237884586677, 8.21105636898428), PANEL = structure(c(1L, 1L,
2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L)), class = "data.frame", row.names = c(NA,
-4L))
diffobj::diffPrint(map.scale.in, map.scale.out[names(map.scale.in)])

## ---- echo=FALSE, results="asis"-----------------------------------------
defaults.in <- map.scale.out
names(defaults.in)[names(defaults.in) == 'PANEL'] <- 'PANEL '
defaults.out <- structure(list(fill = c("#F8766D", "#00BA38", "#00BA38", "#619CFF"
), x = c(-1.16594083365053, -2.22008708454669, -2.83662163671106,
-3.9848822819069), y = c(5.13009568769485, 6.33635502737016,
6.8237884586677, 8.21105636898428), PANEL = structure(c(1L, 1L,
2L, 2L), .Label = c("1", "2"), class = "factor"), group = structure(c(1L,
2L, 2L, 3L), n = 3L), shape = c(21, 21, 21, 21), colour = c("black",
"black", "black", "black"), size = c(4, 4, 4, 4), alpha = c(NA,
NA, NA, NA), stroke = c(0.5, 0.5, 0.5, 0.5)), row.names = c(NA,
-4L), class = "data.frame")

diffobj::diffStr(defaults.in, defaults.out, mode='unified')

## ----echo=FALSE----------------------------------------------------------
defaults.out

## ---- echo=FALSE, results="asis"-----------------------------------------
coord.trans.in <- structure(list(fill = c("#F8766D", "#00BA38"), x = c(-1.16594083365053,
-2.22008708454669), y = c(5.13009568769485, 6.33635502737016),
    PANEL = structure(c(1L, 1L), .Label = c("1", "2"), class = "factor"),
    group = 1:2, shape = c(21, 21), colour = c("black", "black"
    ), size = c(4, 4), alpha = c(NA, NA), stroke = c(0.5, 0.5
    )), class = "data.frame", row.names = c(NA, -2L), vars = "PANEL")
coord.trans.out <- structure(list(fill = c("#F8766D", "#00BA38"), x = c(0.830902392317063, 
0.0454545454545454), y = c(0.35636943561787, 0.429476062264858
), PANEL = structure(c(1L, 1L), .Label = c("1", "2"), class = "factor"), 
    group = 1:2, shape = c(21, 21), colour = c("black", "black"
    ), size = c(4, 4), alpha = c(NA, NA), stroke = c(0.5, 0.5
    )), row.names = c(NA, -2L), vars = "PANEL", class = "data.frame")
diffobj::diffPrint(coord.trans.in, coord.trans.out)

## ----eval=FALSE----------------------------------------------------------
#  Object$method()

## ----eval=TRUE-----------------------------------------------------------
Object <- ggplot2::ggproto(
  "Example", NULL,
  method1=function(self, x) cat(sprintf("%s %s\n", x, self$secret)),
  method2=function(x) cat(sprintf("%s %s\n", x, self$secret)),
  method3=function(self, x) self$secret <- x,
  secret="secret"
)
Object$method1("big")  # works, `self` is provided automatically
Object$method2("big")  # Error because `self` not in signature
Object$method3("NEW SECRET")  # update `secret` member by reference
Object$method1("big")  # `secret` member updated

## ------------------------------------------------------------------------
Object$method1

## ---- eval=FALSE---------------------------------------------------------
#  debug(get("method1", Object))
#  Object$method1("big")  # this will be a debugged version of `method1`

## ----eval=FALSE----------------------------------------------------------
#  ggplot2::stat_bin(geom='point', shape=21, bins=10)

## ----}-------------------------------------------------------------------
ggplot2::GeomPoint$aesthetics()

## ------------------------------------------------------------------------
env <- new.env()
lst <- list()
env$a <- lst$a <- 1

f <- function(e, l) {
  e$a <- 2
  l$a <- 2
}
f(env, lst)

env$a
lst$a

