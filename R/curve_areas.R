# plot_curve_areas_gg ----------------------------------------------------------
#' Plot Filled Areas below Curve Lines using ggplot
#' 
#' @param x x positions of each curve given in \code{y_list}
#' @param y_list list of vectors of y positions each of which must be as long as
#'   \code{x}
#' @param col vector of the same length as \code{y_list} giving the colours of
#'   the areas to be filled
#' @param stack if \code{TRUE} (the default if \code{FALSE}) the areas are
#'   stacked on top of each other otherwise they are overlayed.
#' @param legend if \code{TRUE} (default) the standard legend is shown, else it 
#'   is hidden
#' @param line_colour colour of the curve lines
#' 
#' @export
#' 
#' @examples 
#' 
#' x <- 1:10 
#' y_list <- list(rep(10, 10), 10*sin(x/pi), 5*cos(x/pi))
#' 
#' # Basic plot 
#' plot_curve_areas_gg(x, y_list) 
#' 
#' # Set the colours (must be as many as vectors in y_list)
#' plot_curve_areas_gg(x, y_list, col = c("black", "white", "red"))
#' 
#' # Hide the legend
#' plot_curve_areas_gg(x, y_list, legend = FALSE)
#' 
#' # Stack the values instead of overlaying them
#' plot_curve_areas_gg(x, y_list, stack = TRUE)
#' 
plot_curve_areas_gg <- function
(
  x = seq_along(y_list[[1]]), y_list, col = NULL, stack = FALSE, legend = TRUE, 
  line_colour = "black"
)
{
  stopifnot(is.null(col) || length(y_list) == length(col))
  
  data_frames <- lapply(y_list, function(y) data.frame(x = x, y = y))
  
  data <- kwb.utils::rbindAll(data_frames, "index", namesAsFactor = FALSE)
  
  data$index <- kwb.utils::toFactor(data$index)
  
  position <- if (stack) "stack" else ggplot2::position_dodge(width = 0)
  
  gg <- ggplot2::ggplot(data, ggplot2::aes_string("x", "y", fill = "index")) +
    ggplot2::geom_area(position = position, colour = line_colour)
  
  # Set colours if colours are given
  if (! is.null(col)) {
    gg <- gg + ggplot2::scale_fill_manual(values = col) 
  }
  
  # Hide the legend if legend is FALSE
  if (! legend) {
    gg <- gg + ggplot2::guides(fill = FALSE)
  }
  
  gg
}

# plot_curve_area --------------------------------------------------------------

#' Plot Filled Area Below a Curve Line
#' 
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param y.base y coordinate of horizontal line that closes the area
#' @param col colour of area to be plotted. Default: NA
#' @param \dots further arguments given to polygon such as \code{border} (colour of the
#'   border of the polygon)
#' 
#' @export
#' 
#' @examples 
#'   x <- seq(-pi, pi, pi/8)
#'   y <- sin(x)
#'   
#'   plot(x, y)
#'   plot_curve_area(x, y, 0, col = "red")
#'   
#'   plot(x, y)
#'   plot_curve_area(x, y, -1, col = "red")
#'   plot_curve_area(x, y, 1, col = "green")
#'   
plot_curve_area <- function(x, y, y.base = 0, col = NA, ...)
{
  stopifnot(length(x) == length(y))
  
  # duplicate first and last x
  polygon.x <- c(x[1], x, x[length(x)])
  polygon.y <- c(y.base, y, y.base)
  
  graphics::polygon(polygon.x, polygon.y, col = col, ...)
}
