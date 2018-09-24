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

make.rgb2 <-
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
        dogamma <- function(x) {
          x.else <- which(
              if(anyNA(x)) !is.na(x) & x >= 0.04045 else x >= 0.04045
          )
          res <- x/12.92
          res[x.else] <- ((x[x.else]+0.055)/1.055)^2.4
          res
        }
        ungamma <- function(x) {
          x.else <- which(
              if(anyNA(x)) !is.na(x) & x <= .0031308 else x <= .0031308
          )
          res <- (1.055 * x %^2% (1/2.4)) - 0.055
          res[x.else] <- 12.92 * x[x.else]
          res
        }
    } else stop("'gamma' must be a scalar or 'sRGB'")

    M.s <- solve(M)

    toXYZ <- function(rgb,...) { dogamma(rgb) %*% M }
    toRGB <- function(xyz,...) {
      res <- ungamma(xyz %*% M.s)
      # for backward compatibily, return vector if input is vector
      if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
    }
    if (is.null(name)) name <- deparse(sys.call())[1L]
    rval <- list(toXYZ = toXYZ, fromXYZ = toRGB, gamma = gamma,
                 reference.white = white, name = name)
    class(rval) <- c("RGBcolorConverter", "colorConverter")
    rval
}

# Each colorspace should define an fromXYZ and a toXYZ function.  Return values
# should be a 3 long vector or a 3 column vector.  Functions should expect
# either a thre long vector or a 3 column vector as an input, where each row
# represents a color in 3D coordinates in the corresponding color space.

colorspaces2 <-
    list("XYZ" =
         colorConverter(toXYZ = function(x,w) x,
                        fromXYZ = function(x,w) x,
                        white = NULL,name = "XYZ"),

         "Apple RGB" =
         make.rgb2(red = c(0.6250,0.3400),
                  green = c(0.2800,0.5950),
                  blue = c(0.1550,0.0700),gamma = 1.8,
                  white = "D65", name = "Apple RGB"),

         "sRGB" =
         make.rgb2(red = c(0.6400, 0.3300),
                  green = c(0.3000,0.6000),
                  blue = c(0.1500,0.0600), gamma = "sRGB",
                  white = "D65", name = "sRGB"),

         "CIE RGB" =
         make.rgb2(red = c(0.7350,0.2650),
                  green = c(0.2740,0.7170),
                  blue = c(0.1670,0.0090), gamma = 2.2,
                  white = "E", name = "CIE RGB"),

         "Lab" =
         colorConverter(fromXYZ = function(XYZ, white) {
             if(!(length(XYZ) == 3 | ncol(XYZ) == 3L))
               stop("Input must be a length 3 vector or a 3 column matrix")
             white <- rep(white, length.out=3L)
             if (is.null(nrow(XYZ)))
               XYZ <- matrix(XYZ, nrow = 1L)
             epsilon <- 216/24389
             kappa <- 24389/27

             ## Matrix multiply with diag(1/white) would be faster
             ## but produce different results with Inf

             X <- XYZ[,1L]
             Y <- XYZ[,2L]
             Z <- XYZ[,3L]

             xyzr <- cbind(X / white[1L], Y / white[2L], Z / white[3L])
             xyzr.else <- which(
                 if(anyNA(xyzr)) xyzr > epsilon & !is.na(xyzr)
                 else xyzr > epsilon
             )

             fxyz <- (kappa*xyzr+16)/116
             fxyz[xyzr.else] <- xyzr[xyzr.else]^(1/3)

             fxyz.2 <- fxyz[,2L]
             res <- cbind(
                 L = 116*fxyz.2-16, a = 500*(fxyz[,1L]-fxyz.2),
                 b = 200*(fxyz.2-fxyz[,3L])
             )
             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         },
         toXYZ = function(Lab, white) {
             if(!(length(Lab) == 3 | ncol(Lab) == 3L))
               stop("Input must be a length 3 vector or a 3 column matrix")
             white <- rep(white, length.out=3L)
             if (is.null(nrow(Lab)))
               Lab <- matrix(Lab, nrow = 1L)

             epsilon <- 216/24389
             kappa <- 24389/27

             L <- Lab[, 1L]
             L.nona <- if(L.anyNA <- anyNA(L)) !is.na(L)

             yr.raw <- ((L+16)/116)
             yr <- yr.raw * yr.raw * yr.raw
             yr.else <- which(
                 if(L.anyNA) L < kappa*epsilon & L.nona
                 else L < kappa*epsilon
             )
             yr[yr.else] <- L[yr.else]/kappa

             fy.pre <- L
             fy.pre.else <- which(
                 if(L.anyNA) yr <= epsilon & L.nona
                 else yr <= epsilon
             )
             fy.pre[fy.pre.else] <- kappa*yr[fy.pre.else]
             fy <- (fy.pre + 16) / 116
             fx <- Lab[,2L]/500+fy
             fz <- fy-Lab[,3L]/200

             fz.3 <- fz * fz * fz
             fx.3 <- fx * fx * fx

             zr <- fz.3
             zr.else <- which(
                 if(anyNA(fz.3)) !is.na(fz.3) & fz.3 <= epsilon
                 else fz.3 <= epsilon
             )
             zr[zr.else] <- (116*fz[zr.else]-16)/kappa

             xr <- fx.3
             xr.else <- which(
                 if(anyNA(fx.3)) !is.na(fx.3) & fx.3 <= epsilon
                 else fx.3 <= epsilon
             )
             xr[xr.else] <- (116*fx[xr.else]-16)/kappa

             res <- cbind(X = xr*white[1], Y = yr*white[2], Z = zr*white[3])

             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, name = "Lab", white = NULL),

         "Luv" =
         colorConverter(fromXYZ = function(XYZ, white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             X <- XYZ[,1L]
             Y <- XYZ[,2L]
             Z <- XYZ[,3L]

             yr <- Y/white[2L]

             denom  <- rowSums(cbind(X, Y * 15, Z * 3))
             wdenom <- sum(white*c(1,15,3))
             denom.not.0 <- which(
                 if(anyNA(denom)) !is.na(denom) & denom != 0
                 else denom != 0
             )

             u1 <- v1 <- rep.int(1, length(X))
             u1[denom.not.0] <- 4*X[denom.not.0]/denom[denom.not.0]
             v1[denom.not.0] <- 9*Y[denom.not.0]/denom[denom.not.0]

             ur <- 4*white[1L]/wdenom
             vr <- 9*white[2L]/wdenom

             yr.else <- which(
                 if(anyNA(yr)) !is.na(yr) & yr > epsilon
                 else yr > epsilon
             )
             L <- kappa * yr
             L[yr.else] <- 116*(yr[yr.else]^(1/3))-16
             res <- cbind(L = L, u = 13*L*(u1-ur), v = 13*L*(v1-vr))
             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, toXYZ = function(Luv,white) {
             epsilon <- 216/24389
             kappa <- 24389/27

             u0 <- 4*white[1L]/(white[1L]+15*white[2L]+3*white[3L])
             v0 <- 9*white[2L]/(white[1L]+15*white[2L]+3*white[3L])

             L <- Luv[,1L]
             L.nona <- if(L.anyNA <- anyNA(L)) !is.na(L.nona)

             L.else <-  which(
               if(L.anyNA) L.nona & L <= kappa*epsilon else L <= kappa*epsilon
             )
             Y.raw <- ((L+16)/116)
             Y <- Y.raw * Y.raw * Y.raw
             Y[L.else] <- L[L.else]/kappa
             a <- (52*L/(Luv[,2L]+13*L*u0)-1)/3
             b <- -5*Y
             c <- -1/3
             d <- Y*(39*L/(Luv[,3L]+13*L*v0)-5)

             X <- (d-b)/(a-c)
             Z <- X*a+b

             res <- cbind(X = X,Y = Y,Z = Z)

             if(L.anyNA) res[L.nona & L == 0L] <- c(0,0,0)
             else res[L == 0L] <- c(0,0,0)

             if(nrow(res) == 1L) res[1L, ,drop=TRUE] else res
         }, name = "Luv", white = NULL)

         ) # colorspaces

convertColor2 <-
    function(color, from, to,
             from.ref.white = NULL, to.ref.white = NULL,
             scale.in = 1, scale.out = 1, clip = TRUE)
{
  if (is.character(from))
      from <- colorspaces2[[match.arg(from, names(colorspaces))]]
  if (!inherits(from,"colorConverter"))
      stop("'from' must be a \"colorConverter\" object or a string")
  if (is.character(to))
      to <- colorspaces2[[match.arg(to, names(colorspaces))]]
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

  from.ref.white.chr <- from.ref.white
  to.ref.white.chr <- to.ref.white
  from.ref.white <- c2to3(white.points[, from.ref.white])
  to.ref.white   <- c2to3(white.points[, to.ref.white])

  if (is.null(nrow(color)))
    color <- matrix(color, nrow = 1L)

  if (!is.null(scale.in))
      color <- color/scale.in

  trim <- function(rgb) {
      rgb <- round(rgb,5)
      rgb.lo <- rgb < 0
      rgb.hi <- rgb > 1
      any.lo <- any(rgb.lo)
      any.hi <- any(rgb.hi)

      if(any.lo || any.hi) {
        if (is.na(clip))
            rgb[rgb.lo | rgb.hi] <- NaN
        else if(clip) {
            if(any.lo) rgb[rgb.lo] <- 0
            if(any.hi) rgb[rgb.hi] <- 1
        }
      }
      rgb
  }

  xyz <- from$toXYZ(color, from.ref.white)

  if (is.null(nrow(xyz)))
    xyz <- matrix(xyz, nrow = 1L, dimnames=list(NULL, names(xyz)))

  if (!isTRUE(from.ref.white.chr == to.ref.white.chr)) {
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

`%^2%` <- function(a,b) sign(a) * (abs(a) ^ b)

# faster, less flexible, and more dangerous version of ifelse
#
# Behavior is only "defined" iff:
#
# * `test` is logical
# * `yes` and `no` are numeric
# * `test`, `yes`, and `no` are the same length
#
# Additionally, unlike `ifelse`, `.ifelse` will return one of `yes` or `no` if
# `test` is NA.
#
# The intended use case for this function is when `test`, `yes`, and `no` are
# all expressions based on a single vector such as:
#
# .ifelse(x > 0, x * 2, abs(x) * 2)
#
# such that the result will be the same as `ifelse` except for the survival of
# `NaN` values (`ifelse` turns those to NA).

# .ifelse(test, yes, no) {
#   if(anyNA(test)) no[!is.na(test) & test] <- yes
#   else no[test] <- yes
#   no
# }
