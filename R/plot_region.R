# getPlotRegionRatio -----------------------------------------------------------

#' Current plot region's ratio 
#' 
#' Current plot region's height/width ratio 
#' 
#' @return numeric of length one representing the width/height ratio of the current
#'   plot region
#' 
getPlotRegionRatio <- function() 
{
  region <- getPlotRegionSizeInCm()
  
  with(region, height / width)
}

# getPlotRegionSizeInUserCoords ------------------------------------------------

#' Size of Current Plot Region in User Coordinates
#' 
#' @return list with elements \emph{width}, \emph{height}, \emph{left}, \emph{bottom}, 
#'   \emph{right}, \emph{top}
#' 
getPlotRegionSizeInUserCoords <- function() 
{
  usr <- graphics::par()$usr
  
  list(
    width = diff(usr[1:2]), 
    height = diff(usr[3:4]),
    left = usr[1], 
    bottom = usr[3], 
    right = usr[2], 
    top = usr[4]
  )
}

# getPlotRegionSizeInPixels ----------------------------------------------------

#' Size of Current Plot Region in Pixels
#' 
getPlotRegionSizeInPixels <- function() 
{  
  dpi <- grDevices::dev.size(units = "px") / grDevices::dev.size(units = "in")
  size.inches <- getPlotRegionSizeInInches()
  
  list(
    width = round(size.inches$width * dpi[1]),
    height = round(size.inches$height * dpi[2])
  )
}

# getPlotRegionSizeInInches ----------------------------------------------------

#' Size of Current Plot Region in Inches
#' 
getPlotRegionSizeInInches <- function() 
{
  mai <- graphics::par()$mai
  size <- grDevices::dev.size(units = "in")
  
  list(
    width  = size[1] - mai[2] - mai[4], 
    height = size[2] - mai[1] - mai[3]
  )
}

# getPlotRegionSizeInCm --------------------------------------------------------

#' Size of Current Plot Region in Centimetres
#' 
getPlotRegionSizeInCm <- function() 
{
  size.inches <- getPlotRegionSizeInInches()
  cm.per.inch <- 2.54
  
  list(
    width = size.inches$width * cm.per.inch,
    height = size.inches$height * cm.per.inch
  )
}
