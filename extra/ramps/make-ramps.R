library(scales)
library(ggplot2)
library(viridisLite)
library(ggbg)
library(grid)
library(gtable)
library(gridExtra)
library(vetr)
library(pals)

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
jet.rgb.num <-
  colorRamp(rgb(ggbg:::jet), space="Lab")((0:(n - 1))/(n - 1)) / 255
jet.rgb.lab <- convertColor(jet.rgb.num, "sRGB", "Lab")
jet.rgb <- rgb(jet.rgb.num)
jet.lab <- convertColor(ggbg:::jet, "sRGB", "Lab")

# we want to use euclidean lab distance first because that is less likely to
# have problems with large color differences.

if(n < 2) stop("'n' must be at least 2")

# Need starting points

f_lab <- ggbg:::make_coord_funs(jet.lab)
f_rgb <- ggbg:::make_coord_funs(ggbg:::jet)

pos <- (0:(n - 1)) / (n - 1)
jet.lab.interp <- f_lab$pos_to_coords(pos)
jet.lab.interp.rgb.num <- convertColor(jet.lab.interp, "Lab", "sRGB")

# Check we can recover the positions from the coordiantes

pos.recover <- apply(jet.lab.interp, 1, f_lab$coords_to_pos)
stopifnot(all.equal(pos, pos.recover))

# we do `n + 1` and then drop last so we can match up better to the raw color
# map

viridis.lab <- head(color_to_lab(viridis(n)), -1)
viridis.lab.all <- color_to_lab(viridis(256))
cividis.lab <- head(color_to_lab(cividis(n)), -1)

f_vir_lab <- ggbg:::make_coord_funs(viridis.lab)

kov.lab <- color_to_lab(pals::kovesi.rainbow(n))
tol.lab <- color_to_lab(pals::tol.rainbow(n))
par.lab <- color_to_lab(pals::parula(n))

f_kov_lab <- ggbg:::make_coord_funs(kov.lab)
f_tol_lab <- ggbg:::make_coord_funs(tol.lab)
f_par_lab <- ggbg:::make_coord_funs(par.lab)

# let's get raw values straight from mapping table

vir.idx <- ((seq(n) - 1) * 4) + 1
vir.raw.lab <- convertColor(
  subset(viridisLite::viridis.map, opt=="D")[vir.idx, 1:3],
  'sRGB', 'Lab'
)
# very slow running code

if(FALSE) {
  jet.e.ciede.lab <-
    ggbg:::equalize_dists(jet.lab.interp, deltaE2000, f_lab, iters=1e5)
  jet.e.lab.lab <-
    ggbg:::equalize_dists(jet.lab.interp, euclidian, f_lab, iters=1e5)
  jet.e.rgb.rgb <-
    ggbg:::equalize_dists(f_rgb$pos_to_coords(pos), euclidian, f_rgb, iters=1e5)
  vir.e.ciede.lab <-
    ggbg:::equalize_dists(viridis.lab, deltaE2000, f_vir_lab, iters=1e5)
  kov.e.ciede.lab <-
    ggbg:::equalize_dists(kov.lab, deltaE2000, f_kov_lab, iters=1e5)
  tol.e.ciede.lab <-
    ggbg:::equalize_dists(tol.lab, deltaE2000, f_tol_lab, iters=1e5)
  par.e.ciede.lab <-
    ggbg:::equalize_dists(par.lab, deltaE2000, f_par_lab, iters=1e5)

  jet.e.ciede.lab.256 <- ggbg:::equalize_dists(
    f_lab$pos_to_coords((0:255)/255), deltaE2000, f_lab, iters=5e5
  )
  jet.e.ciede.lab.256.2 <- ggbg:::equalize_dists2(
    f_lab$pos_to_coords((0:255)/255), deltaE2000, f_lab, iters=1e4
  )
  jet.e.ciede.lab.256.3 <- ggbg:::equalize_dists2(
    f_lab$pos_to_coords((0:511)/511), deltaE2000, f_lab, iters=1e4
  )
  jet.e.ciede.lab.256 <- ggbg:::equalize_dists(
    jet.e.ciede.lab.256, deltaE2000, f_lab, iters=2e6
  )

  saveRDS(jet.e.ciede.lab, 'extra/ramps/jet-64-ciede-lab.RDS')
  saveRDS(jet.e.ciede.lab.256, 'extra/ramps/jet-256-ciede-lab.RDS')
  saveRDS(jet.e.lab.lab, 'extra/ramps/jet-64-lab-lab.RDS')
  saveRDS(jet.e.rgb.rgb, 'extra/ramps/jet-64-rgb-rgb.RDS')
  saveRDS(vir.e.ciede.lab, 'extra/ramps/vir-64-ciede-lab.RDS')
  saveRDS(kov.e.ciede.lab, 'extra/ramps/kov-64-ciede-lab.RDS')
  saveRDS(tol.e.ciede.lab, 'extra/ramps/tol-64-ciede-lab.RDS')
  saveRDS(par.e.ciede.lab, 'extra/ramps/par-64-ciede-lab.RDS')
} else {
  jet.e.ciede.lab <- readRDS('extra/ramps/jet-64-ciede-lab.RDS')
  jet.e.ciede.lab.256 <- readRDS('extra/ramps/jet-256-ciede-lab.RDS')
  jet.e.lab.lab <- readRDS('extra/ramps/jet-64-lab-lab.RDS')
  jet.e.rgb.rgb <- readRDS('extra/ramps/jet-64-rgb-rgb.RDS')
  vir.e.ciede.lab <- readRDS('extra/ramps/vir-64-ciede-lab.RDS')
  kov.e.ciede.lab <- readRDS('extra/ramps/kov-64-ciede-lab.RDS')
  tol.e.ciede.lab <- readRDS('extra/ramps/tol-64-ciede-lab.RDS')
  par.e.ciede.lab <- readRDS('extra/ramps/par-64-ciede-lab.RDS')
}
# plot and compare

rgb.e.rgb.lab <- convertColor(jet.e.rgb.rgb, "sRGB", "Lab")

# Plot Colors Against Distance Metrics
#
# Colors should be in Lab 3 column matrix.

delta_funs <- list(
    `CIE ∆E 2000`=deltaE2000,
    `Lab Euc.`=function(x, y) sqrt(rowSums((x - y) ^ 2)),
    `sRGB Euc.`=function(x, y) {
      x.rgb <- convertColor(x, "Lab", "sRGB")
      y.rgb <- convertColor(y, "Lab", "sRGB")
      sqrt(rowSums((x.rgb - y.rgb) ^ 2))
} )

ggbg:::comp_color_dists(
  list(
    `Tol ∆E 2000`=tol.e.ciede.lab,
    `Tol`=color_to_lab(pals::tol.rainbow(n)),
    `Kov ∆E 2000`=kov.e.ciede.lab,
    Kov=color_to_lab(pals::kovesi.rainbow(n))
  ),
  delta_funs
)

# Try A 10 color version of tol.rainbow and kov.rainbow

n2 <- 12

tol.lab.n2 <- color_to_lab(pals::tol.rainbow(n2))
f_tol_n2 <- ggbg:::make_coord_funs(tol.lab.n2)
# tol.e.lab.n2 <-
#   ggbg:::equalize_dists(tol.lab.n2, deltaE2000, f_tol_n2, iters=1e3)

kov.lab.n2 <- color_to_lab(pals::kovesi.rainbow(n2))
f_kov_n2 <- ggbg:::make_coord_funs(kov.lab.n2)
# kov.e.lab.n2 <-
#   ggbg:::equalize_dists(kov.lab.n2, deltaE2000, f_kov_n2, iters=1e3)

gg_hue <- scales::hue_pal(
  h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1
)
gg.lab.n2 <- color_to_lab(gg_hue(n2))
f_gg_n2 <- ggbg:::make_coord_funs(gg.lab.n2)
gg.e.lab.n2 <-
  ggbg:::equalize_dists(gg.lab.n2, deltaE2000, f_gg_n2, iters=1e3)

jet.lab.n2 <- f_lab$pos_to_coords((0:(n2 - 1))/(n2 - 1))
jet.e.lab.n2 <-
  ggbg:::equalize_dists(jet.lab.n2, deltaE2000, f_lab, iters=1e3)
jet.e.lab.n2 <- ggbg:::equalize_dists2(jet.lab.n2, deltaE2000, f_lab, iters=100)

stop('done with auto-run portion')

# Make plots showing the deltas

pal.list <- list(
  `Jet\nCIE∆E2000`=jet.e.lab.n2,
  `jet`=color_to_lab(pals::jet(n2)),
  `tol\nrainbow`=tol.lab.n2,
  `kovesi\nrainbow`=kov.lab.n2,
  parula=color_to_lab(pals::parula(n2)),
  viridis=color_to_lab(viridisLite::viridis(n2)),
  cividis=color_to_lab(viridisLite::cividis(n2)),
  gg_hue=gg.lab.n2,
  `gg_hue\nCIE∆E2000`=gg.e.lab.n2,
  `kovesi\ngray`=color_to_lab(kovesi.linear_grey_0_100_c0(n2))
)
ggbg:::color_dists(
  pal.list,
  dist.funs=list(
    `CIE∆E 2000`=deltaE2000_adj#, `Lab Euclidian`=euclidian_adj
  )
)
ggbg:::comp_color_dists(
  list(
    gg_hue=rbind(gg.lab.n2, head(gg.lab.n2, 1)),
    `gg_hue\nCIE∆E2000`=rbind(gg.e.lab.n2, head(gg.e.lab.n2, 1))
  ),
  delta_funs
)
# Some random walk plots

set.seed(1)
DF <- as.data.frame(
  setNames(
    replicate(n2,  pmax(rnorm(10, 1), 0.2), simplify=FALSE),
    paste0("V", seq_len(n2))
  )
)
DF[['x']] <- seq_len(nrow(DF))

g.base <- ggplot(reshape2::melt(DF, id.vars='x')) +
  geom_col(aes(x=x, y=value, fill=variable))

g1 <- g.base + ggtitle("Default Palette")
g2 <- g.base + ggtitle("CIE∆E 2000 Palette") +
  scale_fill_manual(values=lab_to_color(gg.e.lab.n2))
grid.arrange(arrangeGrob(grobs=list(g1, g2), ncol=2))

# Some volano action

volcano.df <- reshape2::melt(volcano)
thm <- list(
  theme(
    text=element_text(size=8), plot.margin=unit(c(0,-2,0,0), "pt"),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.grid=element_blank(),
    plot.background=element_rect(fill='#DDDDDD', color='#DDDDDD'),
    panel.background=element_rect(fill='#DDDDDD'),
    legend.background=element_rect(fill='#DDDDDD'),
    legend.margin=margin(.5,2,.5,-6),
    legend.box.margin=margin(.5,2,.5,-6)
  ),
  xlab(NULL), ylab(NULL)
)
plot.base  <- ggplot(volcano.df, aes(Var1, Var2, fill=value)) +
    geom_raster() + thm

pl <- list(
  plot.base + scale_fill_gradientn(colors=lab_to_color(jet.e.lab.lab)),
  plot.base + scale_fill_gradientn(colors=tol.rainbow(256)),
  plot.base + scale_fill_gradientn(colors=kovesi.rainbow(256)),
  plot.base + scale_fill_gradientn(colors=viridis(256)),
  plot.base + scale_fill_gradientn(colors=parula(256)),
  plot.base + scale_fill_grey(start=0, end=1)
)
grid.arrange(arrangeGrob(grobs=pl, nrow=2, padding=unit(0, "line")))

gl <- lapply(seq(4), grid.rect, gp=gpar(fill="#DDDDDD", col=0))
gl <- list(
  grid.rect(gp=gpar(fill="#DDDDDD", col=0)),
  grid.rect(gp=gpar(fill="#DDDDDD", col=0)),
  grid.rect(gp=gpar(fill="#DDDDDD", col=0)),
  grid.rect(gp=gpar(fill="#DDDDDD", col=0))
)
grid.arrange(arrangeGrob(grobs=gl, nrow=2, ncol=2, padding=unit(0, "line")))

# Z rects

f_jlie <- ggbg:::make_coord_funs(jet.e.ciede.lab.256)
jlie.256 <- f_jlie$pos_to_coords((0:255)/255)
jlie.128 <- f_jlie$pos_to_coords((0:127)/127)
jlie.16 <- f_jlie$pos_to_coords((0:15)/15)

ggbg:::grob_palette(lab_to_color(jet.e.ciede.lab.256.3))

pal.list <- list(
  ggbg:::grob_palette(
    lab_to_color(jet.e.ciede.lab.256), name='Jet CIE∆E 2000'
  ),
  ggbg:::grob_palette(tol.rainbow(256)),
  ggbg:::grob_palette(kovesi.rainbow(256)),
  ggbg:::grob_palette(parula(256)),
  ggbg:::grob_palette(viridis(256)),
  ggbg:::grob_palette(cividis(256))
)
g.rows <- 2
g.cols <- ceiling(length(pal.list) / g.rows)

g.t <- gtable(
  widths=unit(rep(1 / g.cols, g.cols), "npc"),
  heights=unit(rep(1 / g.rows, g.rows), "npc")
)
for(i in seq_along(pal.list)) {
  g.t <- gtable_add_grob(
    g.t, pal.list[[i]], ((i - 1) %/% g.cols) + 1, ((i - 1) %% g.cols) + 1
  )
}
# dev.new(width=6, height=4)
grid.newpage(); grid.draw(g.t)

pal.list <- list(
  ggbg:::grob_palette(
    lab_to_color(jet.e.ciede.lab.256), name='Jet CIE∆E 2000'
  ),
  ggbg:::grob_palette(jet(256))
)
g.rows <- 1
g.cols <- ceiling(length(pal.list) / g.rows)

g.t <- gtable(
  widths=unit(rep(1 / g.cols, g.cols), "npc"),
  heights=unit(rep(1 / g.rows, g.rows), "npc")
)
for(i in seq_along(pal.list)) {
  cat(sprintf("row %d col %d\n", ((i - 1) %/% g.cols) + 1, ((i - 1) %% g.cols) + 1))
  g.t <- gtable_add_grob(
    g.t, pal.list[[i]], ((i - 1) %/% g.cols) + 1, ((i - 1) %% g.cols) + 1
  )
}
dev.new(width=6, height=3)
grid.newpage(); grid.draw(g.t)

# test out color mapping

n <- 1024
jet1k <- colorRamp(rgb(ggbg:::jet), space="Lab")((0:(n - 1))/(n - 1)) / 255
from.ref.white <- grDevices:::c2to3(grDevices:::white.points[, "D65"])
microbenchmark::microbenchmark(
  a <- colorspaces$sRGB$toXYZ(jet1k, from.ref.white),
  b <- convertColor(jet1k, "sRGB", "XYZ")
)

microbenchmark::microbenchmark(times=10,
  grDevices::convertColor(jet1k, "sRGB", "Lab"),
  ggbg:::convertColor(jet1k, "sRGB", "Lab")
)

