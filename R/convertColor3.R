#  File src/library/grDevices/R/convertColor.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## http://www.brucelindbloom.com/index.html?Equations.html

make.rgb3 <-
    function(red, green, blue, name = NULL, white = "D65", gamma = 2.2)
{
    whitexyz <- c2to3(white.points[, white])
    rgb <- rbind(c2to3(red),
                 c2to3(green),
                 c2to3(blue))
    S <- drop(whitexyz %*% solve(rgb))
    M <- S * rgb

    if (is.numeric(gamma) && length(gamma) == 1) {
        dogamma <- function(x) x %^2% gamma
        ungamma <- function(x) x %^2% (1/gamma)
    } else if (gamma == "sRGB") {
        dogamma <- function(x) .ifelse(x < 0.04045,
                                      x/12.92,
                                      ((x+0.055)/1.055)^2.4)
        ungamma <- function(x) .ifelse(x <= 0.0031308,
                                      12.92*x,
                                      1.055*x %^2% (1/2.4)-0.055)
    } else stop("'gamma' must be a scalar or 'sRGB'")

    toXYZ <- function(rgb,...) { dogamma(rgb) %*% M }
    toRGB <- function(xyz,...) {
      res <- ungamma(xyz %*% solve(M))
      # for backward compatibily, return vector if input is vector
      if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
    }
    if (is.null(name)) name <- deparse(sys.call())[1L]
    rval <- list(toXYZ = toXYZ, fromXYZ = toRGB, gamma = gamma,
                 reference.white = white, name = name)
    class(rval) <- c("RGBcolorConverter", "colorConverter")
    rval
}

print.colorConverter <- function(x,...) {
    cat(gettextf("Color space converter: %s", x$name), "\n", sep = "")
    if (!is.null(x$reference.white))
        cat(gettextf("Reference white: %s", x$reference.white), "\n", sep = "")
    invisible(x)
}

print.RGBcolorConverter <- function(x,...) {
    print.colorConverter(x, ...)
    if (!is.null(x$gamma))
        cat(gettextf("display gamma = %s", format(x$gamma)), "\n", sep = "")
    invisible(x)
}

chromaticAdaptation <- function(xyz, from, to) {
    ## bradford scaling algorithm
    Ma <- matrix(c( 0.40024, -0.22630, 0.,
                    0.70760,  1.16532, 0.,
                   -0.08081,  0.04570, 0.91822), nrow = 3L, byrow = TRUE)
    nWhite <- colnames(white.points)
    from <- c2to3(white.points[, match.arg(from, nWhite)])
    to   <- c2to3(white.points[, match.arg(to,   nWhite)])
    from.cone <- drop(from %*% Ma)
    to.cone   <- drop(to %*% Ma)
    ## M <- Ma %*% diag(to.cone/from.cone) %*% solve(Ma)
    M <- (Ma * rep(to.cone/from.cone, each=3)) %*% solve(Ma)
    xyz %*% M
}


colorConverter <- function(toXYZ, fromXYZ, name, white = NULL) {
    rval <- list(toXYZ = toXYZ, fromXYZ = fromXYZ,
                 name = name, white = white)
    class(rval) <- "colorConverter"
    rval
}
# Each colorspace should define an fromXYZ and a toXYZ function.  Return values
# should be a 3 long vector or a 3 column matrix  Functions should expect
# either a 3 long vector or a 3 column matrix as an input, where each row
# represents a color in 3D coordinates in the corresponding color space.
#
# The original code assumed 3 long vectors only, so there are some
# idiosyncracies now to produce the same resuts whether 3 long vectors or three
# column matrices are provided.

colorspaces3 <-
    list("XYZ" =
         colorConverter(toXYZ = function(x,w) x,
                        fromXYZ = function(x,w) x,
                        white = NULL,name = "XYZ"),

         "Apple RGB" =
         make.rgb3(red = c(0.6250,0.3400),
                  green = c(0.2800,0.5950),
                  blue = c(0.1550,0.0700),gamma = 1.8,
                  white = "D65", name = "Apple RGB"),

         "sRGB" =
         make.rgb3(red = c(0.6400, 0.3300),
                  green = c(0.3000,0.6000),
                  blue = c(0.1500,0.0600), gamma = "sRGB",
                  white = "D65", name = "sRGB"),

         "CIE RGB" =
         make.rgb3(red = c(0.7350,0.2650),
                  green = c(0.2740,0.7170),
                  blue = c(0.1670,0.0090), gamma = 2.2,
                  white = "E", name = "CIE RGB"),

         "Lab" =
         colorConverter(fromXYZ = function(XYZ, white) {
             stopifnot(length(XYZ) == 3 | ncol(XYZ) == 3L)
             white <- rep(white, length.out=3L)
             if (is.null(nrow(XYZ)))
               XYZ <- matrix(XYZ, nrow = 1L)

             epsilon <- 216/24389
             kappa <- 24389/27

             # Alternate is `%*% diag(1/white)`, but produces different results
             # if there are non-finite values
             xyzr <- cbind(
               XYZ[,1L] / white[1L],
               XYZ[,2L] / white[2L],
               XYZ[,3L] / white[3L]
             )
             fxyz <- .ifelse(xyzr <= epsilon, (kappa*xyzr+16)/116, xyzr^(1/3))

             res <- cbind(L = 116*fxyz[,2L]-16,
               a = 500*(fxyz[,1L]-fxyz[,2L]),
               b = 200*(fxyz[,2L]-fxyz[,3L]))
             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         },
         toXYZ = function(Lab, white) {
             stopifnot(ncol(Lab) == 3L | length(Lab)==3)
             white <- rep(white, length.out=3L)
             if (is.null(nrow(Lab)))
               Lab <- matrix(Lab, nrow = 1L)

             epsilon <- 216/24389
             kappa <- 24389/27

             L <- Lab[,1L]

             yr <- .ifelse(
               L < kappa*epsilon, L/kappa, pow3((L+16)/116)
             )
             fy <- (.ifelse(yr <= epsilon, kappa*yr, L) + 16)/116
             fx <- Lab[,2L]/500+fy
             fz <- fy-Lab[,3L]/200

             fz3 <- pow3(fz)
             fx3 <- pow3(fx)
             zr <- .ifelse(fz3 <= epsilon, (116*fz-16)/kappa, fz3)
             xr <- .ifelse(fx3 <= epsilon, (116*fx-16)/kappa, fx3)

             res <- cbind(X = xr*white[1], Y = yr*white[2], Z = zr*white[3])

             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, name = "Lab", white = NULL),

         "Luv" =
         colorConverter(fromXYZ = function(XYZ, white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             yr <- XYZ[,2L]/white[2L]

             denom  <- rowSums(cbind(XYZ[,1L], XYZ[,2L] * 15, XYZ[,3L] * 3))
             wdenom <- sum(white*c(1,15,3))

             one <- rep_len(1, length(denom))
             u1 <- .ifelse(denom == 0, one, 4*XYZ[,1L]/denom)
             v1 <- .ifelse(denom == 0, one, 9*XYZ[,2L]/denom)
             ur <- 4*white[1L]/wdenom
             vr <- 9*white[2L]/wdenom

             L <- .ifelse(yr <= epsilon, kappa*yr, 116*(yr^(1/3))-16)
             res <- cbind(L = L, u = 13*L*(u1-ur), v = 13*L*(v1-vr))
             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, toXYZ = function(Luv,white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             u0 <- 4*white[1L]/(white[1L]+15*white[2L]+3*white[3L])
             v0 <- 9*white[2L]/(white[1L]+15*white[2L]+3*white[3L])

             L <- Luv[,1L]
             Y <- .ifelse(L <= kappa*epsilon,
                     L/kappa, pow3((L+16)/116))
             a <- (52*L/(Luv[,2L]+13*L*u0)-1)/3
             b <- -5*Y
             c <- -1/3
             d <- Y*(39*L/(Luv[,3L]+13*L*v0)-5)

             X <- (d-b)/(a-c)
             Z <- X*a+b

             res <- cbind(X = X,Y = Y,Z = Z)

             res[!is.na(L) & L == 0L] <- c(0,0,0)
             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, name = "Luv", white = NULL)

         ) # colorspaces


convertColor3 <-
    function(color, from, to,
             from.ref.white = NULL, to.ref.white = NULL,
             scale.in = 1, scale.out = 1, clip = TRUE)
{
  if (is.character(from))
      from <- colorspaces3[[match.arg(from, names(colorspaces3))]]
  if (!inherits(from,"colorConverter"))
      stop("'from' must be a \"colorConverter\" object or a string")
  if (is.character(to))
      to <- colorspaces3[[match.arg(to, names(colorspaces3))]]
  if (!inherits(to,"colorConverter"))
      stop("'to' must be a \"colorConverter\" object or a string")

  ## Need a reference white. If both the definition and the argument
  ## specify one they must agree.

  if (is.null(from.ref.white))
      from.ref.white <- from$white
  else if (!is.null(from$white) && from.ref.white != from$white)
      stop(gettextf("'from.ref.white' disagrees with definition of %s",
                    from$name), domain = NA)

  if (is.null(to.ref.white))
      to.ref.white <- to$white
  else if (!is.null(to$white) && to.ref.white != to$white)
      stop(gettextf("'to.ref.white' disagrees with definition of %s",
                    to$name), domain = NA)

  if (is.null(to.ref.white) && is.null(from.ref.white))
      to.ref.white <- from.ref.white <- "D65"

  if (is.null(to.ref.white))
      to.ref.white <- from.ref.white
  if (is.null(from.ref.white))
      from.ref.white <- to.ref.white

  from.ref.white <- c2to3(white.points[, from.ref.white])
  to.ref.white   <- c2to3(white.points[, to.ref.white])

  if (is.null(nrow(color)))
    color <- matrix(color, nrow = 1L)

  if (!is.null(scale.in))
      color <- color/scale.in

  trim <- function(rgb) {
      rgb <- round(rgb,5)
      if (is.na(clip))
          rgb[rgb < 0 | rgb > 1] <- NaN
      else if(clip) {
          rgb[rgb < 0] <- 0
          rgb[rgb > 1] <- 1
      }
      rgb
  }

  xyz <- from$toXYZ(color, from.ref.white)

  if (is.null(nrow(xyz)))
    xyz <- matrix(xyz, nrow = 1L, dimnames=list(NULL, names(xyz)))

  if (!isTRUE(all.equal(from.ref.white, to.ref.white))) {
      mc <- match.call()
      if (is.null(mc$from.ref.white) || is.null(mc$to.ref.white))
          warning("color spaces use different reference whites")
      xyz <- chromaticAdaptation(xyz, from.ref.white, to.ref.white)
  }

  rval <- to$fromXYZ(xyz, to.ref.white)

  if (inherits(to,"RGBcolorConverter"))
      rval <- trim(rval)

  if(!is.matrix(rval)) rval <- t(rval)

  if (is.null(scale.out))
      rval
  else
      rval*scale.out
}
## Simplified version of `ifelse` with constraints:
##
## * test, yes, and no must be the same length
## * test must be logical
## * yes and no must be numeric
## * if test is NA, then `no` is returned

.ifelse <- function(test, yes, no) {
  test.w <- if(anyNA(test)) which(!is.na(test) & test) else which(test)
  no[test.w] <- yes[test.w]
  no
}
## Benchmarks show x ^ 3 is much slower than x * x * x

pow3 <- function(x) x * x * x
