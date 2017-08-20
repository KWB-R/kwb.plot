# inchesToUserWidthAndHeight ---------------------------------------------------

#' Convert Length in Inches to Lengths in User Coordinates
#' 
#' @param inches length in inches
#' 
inchesToUserWidthAndHeight <- function(inches)
{
  sizeInches <- getPlotRegionSizeInInches()
  sizeUserCoords <- getPlotRegionSizeInUserCoords()
  list(width  = inches * sizeUserCoords$width / sizeInches$width,
       height = inches * sizeUserCoords$height / sizeInches$height) 
}

# cmToUserWidthAndHeight -------------------------------------------------------

#' Convert Length in Centimetres to Lengths in User Coordinates
#' 
#' @param cm length in centimetres
#' 
cmToUserWidthAndHeight <- function(cm)
{
  inches.per.cm <- 1/2.54
  inchesToUserWidthAndHeight(cm * inches.per.cm)
}

# userWidthAndHeightToCm -------------------------------------------------------

#' Convert Lenghts in User Coordinates to Lengths in Centimetres
#' 
#' @param width.user width in user coordinates
#' @param height.user height in user coordinates 
userWidthAndHeightToCm <- function(width.user, height.user)
{
  size.cm <- getPlotRegionSizeInCm()
  size.user <- getPlotRegionSizeInUserCoords()
  
  list(
    width = width.user * size.cm$width / size.user$width,
    height = height.user * size.cm$height / size.user$height
  )
}
