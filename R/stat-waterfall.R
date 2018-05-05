#' Computes the `ycum` Aesthetic
#'
#' This stat is intended only to be used to add the computed `ycum` aesthetic to
#' the plot data.  It does not exist as a stand alone layer.
#'
#' @section Computed variables:
#' \describe{
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' }
#' @export
#' @rdname position_waterfall
#' @export

StatWaterfall <- ggproto("StatWaterfall", Stat,
  required_aes = c("x", "y"),
  reverse = FALSE,

  compute_panel = function(data, scales, reverse=FALSE) {
    if("ycum" %in% names(data)) {
      warning(
        "`ycum` aesthetic aesthetic already exists, `stat_waterfall` ",
        "will not recompute it."
      )
    } else {
      dat.ord <-
        order(data[["x"]], data[["group"]] * if(reverse) -1 else 1)

      data.o <- data[dat.ord, , drop=FALSE]
      data.o[["ycum"]] <- cumsum(data.o[["y"]])
      data <- data.o[rank(dat.ord), , drop=FALSE]
    }
    data
  }
)
