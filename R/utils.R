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

## Ensure that data contains required aesthetics

check_req_aes <- function (self, data) {
  # nocov start
  if(!is.ggproto(self) || !length(class(self)))
    stop(
      "Internal Error: checking required aes on non-ggproto object; ",
      "contact maintainer."
    )
  if(!is.data.frame(data))
    stop(
      "Internal Error: checking required aes against non-dataframe; ",
      "contact maintainer."
    )
  if(!inherits(self, "Geom"))
    stop(
      "Internal Error: checking required aes against non-Geom object; ",
      "contat maintainer."
    )
  # nocov end

  if(!all(self$required_aes %in% names(data)))
    stop(
      class(self)[1], " requires the following missing aesthetics: ",
      paste0(setdiff(self$required_aes, names(data)), collapse=", ")
    )
  TRUE
}
