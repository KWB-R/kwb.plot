# getCurrentLimits -------------------------------------------------------------

#' Get Current xlim and ylim
#' 
#' get the values of xlim and/or ylim that created the last plot
#' 
#' @param type one of "x", "y" or "xy", see section "Value"
#' @param xaxs value of argument \emph{xaxs} when calling a plot function last, see
#'   \code{par}
#' @param yaxs value of argument \emph{yaxs} when calling a plot function last, see
#'   \code{par}
#' 
#' @return \emph{type} = "x" or "y": vector of two elements corresponding to xlim or
#'   ylim, respectively. \emph{type} = "xy": list with elements \emph{xlim} and
#'   \emph{ylim} each of which is a vector of two elements.
#' 
getCurrentLimits <- function(
  type = "xy", xaxs = graphics::par("xaxs"), yaxs = graphics::par("yaxs")
)
{ 
  stopifnot(type %in% c("x", "y", "xy"))
  
  box <- kwb.plot::getPlotRegionSizeInUserCoords()
  
  factor.4 <- 4/108
  
  factor.x <- ifelse(xaxs == "r", factor.4, 0)
  factor.y <- ifelse(yaxs == "r", factor.4, 0)
  
  dx <- factor.x * box$width
  dy <- factor.y * box$height
  
  xlim <- c(box$left + dx, box$right - dx)
  ylim <- c(box$bottom + dy, box$top - dy)
  
  if (type == "x") {
    
    xlim
  } else if (type == "y") {
    
    ylim
  } else {
    
    list(xlim = xlim, ylim = ylim)
  }  
}

# inLimits ---------------------------------------------------------------------

#' are values within limits (e.g. xlim, ylim)?
#' 
#' are values within limits (e.g. xlim, ylim)?
#' 
#' @param x vector of values
#' @param limits two-element vector of (lower and upper) limits
#' 
#' @return vetor of logical. Each element corresponds to an element in \emph{x} and
#'   is TRUE if the element is within the limits or FALSE if the element is
#'   out of the limits.
#' 
inLimits <- function(x, limits)
{
  kwb.utils::inRange(x, limits[1], limits[2])
}

# appropriateLimits ------------------------------------------------------------

#' default limits for plotting values
#' 
#' default limits for plotting values
#' 
#' @param x vector of numeric or POSIXt (min, max must be able to be applied).
#' @param limits vector of two elements. Default: NULL.
#' @param default vector of two elements. Default: c(0, 1)
#' 
#' @return returns \emph{limits} if \emph{limits} is a vector of two non-NA values. 
#'   If the first element of \emph{limits} is NA it is replaced with the 
#'   minimum of \emph{x} (or with \emph{default[1]} if the minimum is NA). If
#'   the second element of \emph{limits} is NA it is replaced with the maximum
#'   of \emph{x} (or with \emph{default[2]} if the maximum is NA).
#' 
appropriateLimits <- function(x, limits = NULL, default = c(0, 1))
{
  if (is.null(limits)) {
    limits <- range(x)
  }
  
  if (is.na(limits[1])) {
    limits[1] <- kwb.utils::getFunctionValueOrDefault(
      x, FUN = min, default = default[1]
    )
  }
  
  if (is.na(limits[2])) {
    limits[2] <- kwb.utils::getFunctionValueOrDefault(
      x, FUN = max, default = default[2]
    )
  }
  
  return (limits)
}

# userCoordinatesToLimits ------------------------------------------------------

#' userCoordinatesToLimits
#' 
#' back-calculate xlim and ylim from user coordinates of plot region
#' 
#' @param userCoordinates list with elements 
#'   \code{width, height, left, bottom, right, top}, as returned by 
#'   \code{\link{getPlotRegionSizeInUserCoords}}
#' 
#' @return list with elements \emph{xlim} and \emph{ylim}, each of which is a numeric
#'   vector of length two.
#' 
userCoordinatesToLimits <- function(userCoordinates) 
{
  dx <- userCoordinates$width / 1.08 * 0.04
  dy <- userCoordinates$height / 1.08 * 0.04
  
  list(
    xlim = c(userCoordinates$left + dx, userCoordinates$right - dx),
    ylim = c(userCoordinates$bottom + dy, userCoordinates$top - dy)
  )  
}
