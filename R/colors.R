##
structure(
  list(toXYZ = function (Lab, white)
  {
      stopifnot(ncol(Lab) == 3L)
      epsilon <- 216/24389
      kappa <- 24389/27
      yr <- ifelse(Lab[1L] < kappa * epsilon, Lab[1L]/kappa, ((Lab[1L] +
          16)/116)^3)
      fy <- ifelse(yr <= epsilon, (kappa * yr + 16)/116, (Lab[1L] +
          16)/116)
      fx <- Lab[2L]/500 + fy
      fz <- fy - Lab[3L]/200
      zr <- ifelse(fz^3 <= epsilon, (116 * fz - 16)/kappa, fz^3)
      xr <- ifelse(fx^3 <= epsilon, (116 * fx - 16)/kappa, fx^3)
      c(X = xr, Y = yr, Z = zr) * white
  },
  fromXYZ = function (XYZ, white)
  {
      stopifnot(ncol(XYZ) == 3L)
      epsilon <- 216/24389
      kappa <- 24389/27
      xyzr <- XYZ/white
      fxyz <- xyzr
      fxyz[] <- ifelse(
        xyzr <= epsilon, (kappa * xyzr + 16)/116, xyzr^(1/3)
      )
      cbind(L = 116 * fxyz[,2L] - 16, a = 500 * (fxyz[,1L] - fxyz[,2L]),
          b = 200 * (fxyz[, 2L] - fxyz[, 3L]))
  }, name = "Lab", white = NULL), class = "colorConverter"
)
