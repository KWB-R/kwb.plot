% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curve_areas.R
\name{plot_curve_area}
\alias{plot_curve_area}
\title{Plot Filled Area Below a Curve Line}
\usage{
plot_curve_area(x, y, y.base = 0, col = NA, ...)
}
\arguments{
\item{x}{vector of x coordinates}

\item{y}{vector of y coordinates}

\item{y.base}{y coordinate of horizontal line that closes the area}

\item{col}{colour of area to be plotted. Default: NA}

\item{\dots}{further arguments given to polygon such as \code{border} (colour of the
border of the polygon)}
}
\description{
Plot Filled Area Below a Curve Line
}
\examples{
  x <- seq(-pi, pi, pi/8)
  y <- sin(x)
  
  plot(x, y)
  plot_curve_area(x, y, 0, col = "red")
  
  plot(x, y)
  plot_curve_area(x, y, -1, col = "red")
  plot_curve_area(x, y, 1, col = "green")
  
}
