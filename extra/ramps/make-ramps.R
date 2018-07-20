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

viridis.lab <-
  head(convertColor(t(col2rgb(viridis(n + 1))) / 255, "sRGB", "Lab"), -1)
viridis.lab.all <- convertColor(t(col2rgb(viridis(256))) / 255, "sRGB", "Lab")
cividis.lab <-
  head(convertColor(t(col2rgb(cividis(n + 1))) / 255, "sRGB", "Lab"), -1)
euc_dist <- function(x, y) sqrt(rowSums((x - y) ^ 2))

f_vir_lab <- ggbg:::make_coord_funs(viridis.lab)

# let's get raw values straight from mapping table

vir.idx <- ((seq(n) - 1) * 4) + 1
vir.raw.lab <- convertColor(
  subset(viridisLite::viridis.map, opt=="D")[vir.idx, 1:3],
  'sRGB', 'Lab'
)
vir.lab.all <- viridis.lab.all[vir.idx, ]

vir.ramp.dat <- subset(viridisLite::viridis.map, opt=="D")[, 1:3]
vir.ramp.dat.hex <- rgb(vir.ramp.dat)
vir.lin.lab <- convertColor(
  colorRamp(vir.ramp.dat.hex, space='Lab')((0:255)/255)/255,
  'sRGB', 'Lab'
)[vir.idx, ]

# very slow running code

stop()

if(FALSE) {
  jli.e.2000 <-
    ggbg:::equalize_dists(jet.lab.interp, deltaE2000_1, f_lab, iters=1e5)
  jli.e.lab <-
    ggbg:::equalize_dists(jet.lab.interp, euc_dist, f_lab, iters=1e5)
  rgb.e.rgb <-
    ggbg:::equalize_dists(
    f_rgb$pos_to_coords(pos), euc_dist, f_rgb, iters=1e5
  )
  vir.e.lab <-
    ggbg:::equalize_dists(viridis.lab, deltaE2000_1, f_vir_lab, iters=1e5)

  saveRDS(jli.e.2000, 'extra/ramps/jet-65-ceide-lab.RDS')
  saveRDS(jli.e.lab, 'extra/ramps/jet-65-lab-lab.RDS')
  saveRDS(rgb.e.rgb, 'extra/ramps/jet-65-rgb-rgb.RDS')
  saveRDS(vir.e.lab, 'extra/ramps/viridis-65-ceide-lab.RDS')
} else {
  jli.e.2000 <- readRDS('extra/ramps/jet-65-ceide-lab.RDS')
  jli.e.lab <- readRDS('extra/ramps/jet-65-lab-lab.RDS')
  rgb.e.rgb <- readRDS('extra/ramps/jet-65-rgb-rgb.RDS')
  vir.e.lab <- readRDS('extra/ramps/viridis-65-ceide.lab.RDS')
}
# plot and compare

jli.rgb.num <- convertColor(jli.e.2000, "Lab", "sRGB")
jli.rgb <- rgb(jli.rgb.num)
jli.e.lab.rgb <- convertColor(jli.e.lab, "Lab", "sRGB")
rgb.e.rgb.lab <- convertColor(rgb.e.rgb, "sRGB", "Lab")

# Plot Colors Against Distance Metrics
#
# Colors should be in Lab 3 column matrix.

ggbg:::comp_color_dists(
  list(
    CIEDE2000=jli.e.2000, Lab=jli.e.lab, colorRamp=jet.rgb.lab,
    RGB=rgb.e.rgb.lab
    # `viridis()`=vir.lab.all,
    # `viridis CIEDE2000`=vir.e.lab,
    # `viridis.map[opt='D']`=vir.raw.lab
    #, civ=cividis.lab
  ),
  list(
    CIEDE2000=deltaE2000_1,
    Lab=function(x, y) sqrt(rowSums((x - y) ^ 2)),
    RGB=function(x, y) {
      x.rgb <- convertColor(x, "Lab", "sRGB")
      y.rgb <- convertColor(y, "Lab", "sRGB")
      sqrt(rowSums((x.rgb - y.rgb) ^ 2))
} ) )

