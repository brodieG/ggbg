## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "ggbg - Assorted Ggplot Extensions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

# nocov start
.onLoad <- function(libname, pkgname) {

  .default.opts <- list(
    ggbg.signif=11L,
    ggbg.vjust.mode="end",
    ggbg.vjust=0.5
  )
  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
}
# nocov end
