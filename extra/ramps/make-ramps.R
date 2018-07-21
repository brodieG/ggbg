library(scales)
library(ggplot2)
library(viridisLite)
library(ggbg)
library(vetr)

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
euc_dist <- function(x, y) sqrt(rowSums((x - y) ^ 2))

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

stop()

if(FALSE) {
  jet.e.ceide.lab <-
    ggbg:::equalize_dists(jet.lab.interp, deltaE2000, f_lab, iters=1e5)
  jet.e.lab.lab <-
    ggbg:::equalize_dists(jet.lab.interp, euc_dist, f_lab, iters=1e5)
  jet.e.rgb.rgb <-
    ggbg:::equalize_dists(f_rgb$pos_to_coords(pos), euc_dist, f_rgb, iters=1e5)
  vir.e.ceide.lab <-
    ggbg:::equalize_dists(viridis.lab, deltaE2000, f_vir_lab, iters=1e5)
  kov.e.ceide.lab <-
    ggbg:::equalize_dists(kov.lab, deltaE2000, f_kov_lab, iters=1e5)
  tol.e.ceide.lab <-
    ggbg:::equalize_dists(tol.lab, deltaE2000, f_tol_lab, iters=1e5)
  par.e.ceide.lab <-
    ggbg:::equalize_dists(par.lab, deltaE2000, f_par_lab, iters=1e5)

  saveRDS(jet.e.ciede.lab, 'extra/ramps/jet-64-ciede-lab.RDS')
  saveRDS(jet.e.lab.lab, 'extra/ramps/jet-64-lab-lab.RDS')
  saveRDS(rgb.e.rgb, 'extra/ramps/jet-64-rgb-rgb.RDS')
  saveRDS(vir.e.ciede.lab, 'extra/ramps/vir-64-ciede-lab.RDS')
  saveRDS(kov.e.ciede.lab, 'extra/ramps/kov-64-ciede-lab.RDS')
  saveRDS(tol.e.ciede.lab, 'extra/ramps/tol-64-ciede-lab.RDS')
  saveRDS(par.e.ciede.lab, 'extra/ramps/par-64-ciede-lab.RDS')
} else {
  jet.e.ciede.lab <- readRDS('extra/ramps/jet-64-ciede-lab.RDS')
  jet.e.lab.lab <- readRDS('extra/ramps/jet-64-lab-lab.RDS')
  rgb.e.rgb <- readRDS('extra/ramps/jet-64-rgb-rgb.RDS')
  vir.e.ciede.lab <- readRDS('extra/ramps/vir-64-ciede-lab.RDS')
  kov.e.ciede.lab <- readRDS('extra/ramps/kov-64-ciede-lab.RDS')
  tol.e.ciede.lab <- readRDS('extra/ramps/tol-64-ciede-lab.RDS')
  par.e.ciede.lab <- readRDS('extra/ramps/par-64-ciede-lab.RDS')
}
# plot and compare

rgb.e.rgb.lab <- convertColor(rgb.e.rgb, "sRGB", "Lab")

# Plot Colors Against Distance Metrics
#
# Colors should be in Lab 3 column matrix.

ggbg:::comp_color_dists(
  list(
    Tol.e=tol.e.ciede.lab, Tol=color_to_lab(pals::tol.rainbow(n)),
    Kov.e=kov.e.ciede.lab, Kov=color_to_lab(pals::kovesi.rainbow(n)),
    Par.e=par.e.ceide.lab, Par=color_to_lab(pals::parula(n))
    # CIEDE2000=jli.e.2000, 
    # Kovesi=kov.e.lab, Parula=par.e.lab,
    # Tol=tol.e.lab, Viridis=vir.e.lab
    # `viridis()`=vir.lab.all,
    # `viridis CIEDE2000`=vir.e.lab,
    # `viridis.map[opt='D']`=vir.raw.lab
    #, civ=cividis.lab
  ),
  list(
    CIEDE2000=deltaE2000,
    Lab=function(x, y) sqrt(rowSums((x - y) ^ 2)),
    RGB=function(x, y) {
      x.rgb <- convertColor(x, "Lab", "sRGB")
      y.rgb <- convertColor(y, "Lab", "sRGB")
      sqrt(rowSums((x.rgb - y.rgb) ^ 2))
} ) )

