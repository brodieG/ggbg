
library(png)
library(ggplot2)

# Generate a car 'grob' using a baseline PNG

car.raster <- png::readPNG("~/Downloads/car2.png")

# The `grid` grob actually responsible for rendering our car

carGrob <- function(x, y, length, width, gp) {
  grid::grobTree(
    grid::rectGrob(
      x, y, hjust=.5, height=width, width=length,
      gp = gp
    ),
    grid::rasterGrob(
      car.raster, x=x, y=y, hjust=.5, height=width, width=length
) ) }
# The `ggproto` object that maps our data to the `grid` grobs

GeomCar <- ggplot2::ggproto("GeomCar", ggplot2::Geom,
  # Generate grobs from the data, we have to reconvert length/width so
  # that the transformations persist

  draw_panel=function(self, data, panel_params, coords) {
    with(
      coords$transform(data, panel_params),
      carGrob(
        x, y, length=xmax-xmin, width=ymax-ymin,
        gp=grid::gpar(
          col = colour, fill = alpha(fill, alpha),
          lwd = size * .pt, lty = linetype, lineend = "butt"
  ) ) ) },
  # Convert data to coordinates that will get transformed (length/width don't
  # normally).

  setup_data=function(self, data, params) {
    transform(data,
      xmin = x - length / 2, xmax = x + length / 2,
      ymin = y - width / 2, ymax = y + width / 2
  ) },
  # Required and default aesthetics

  required_aes=c("x", "y", "length", "width"),
  default_aes = aes(
    colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
  ),
  # Use the car grob in the legend

  draw_key = function(data, params, size) {
    with(
      data,
      carGrob(
        0.5, 0.5, length=.75, width=.5,
        gp = grid::gpar(
          col = colour, fill = alpha(fill, alpha),
          lwd = size * .pt, lty = linetype, lineend = "butt"
  ) ) ) }
)
# External interface

geom_car <- function(
  mapping=NULL, data=NULL, ..., inherit.aes=TRUE, show.legend=NA
) {
  layer(
    data=data, mapping=mapping, geom=GeomCar, position="identity",
    stat="identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params=list(...)
  )
}
