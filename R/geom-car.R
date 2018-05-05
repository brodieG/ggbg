
library(png)
library(ggplot2)

# Generate a car 'grob' using a baseline PNG

car.raster <- png::readPNG("~/Downloads/car2.png")

carGrob <- function(x, y, length, width) {
  grid::rasterGrob(
    car.raster, x=x, y=y,
    hjust=1, height=width, width=length
  )
  rectGrob(
    x, y,
    default.units = "native",
    just = c("left", "top"),
    gp = 
  )
}
# And a 'ggproto' object with the required

GeomCar <- ggplot2::ggproto("GeomCar", ggplot2::Geom,
  draw_panel=function(self, data, panel_params, coords) {
    with(
      coords$transform(data, panel_params),
      carGrob(
        x, y, xmax-xmin, ymax-ymin, gp=gpar(
        col = coords$colour,
        fill = alpha(coords$fill, coords$alpha),
        lwd = coords$size * .pt,
        lty = coords$linetype,
        lineend = "butt"
      )
    )
  },
  # draw_legend=function(self, data, ...) {
  #   with(data, ggname(self$my_name(), fieldGrob(1, 0.5, carGrob)))
  # },
  setup_data=function(self, data, params) {
    transform(data,
      xmin = x - length / 2, xmax = x + length / 2, length = NULL,
      ymin = y - width / 2, ymax = y + width / 2, width = NULL
    )
  },
  required_aes=c("x", "y")
)
geom_car <- function(mapping=NULL, data=NULL, ...) {
  layer(
    data=data, mapping=mapping,
    geom=GeomCar,
    position="identity",
    stat="identity",
    params=list(...)
  )
}
