# arrange_in_png ---------------------------------------------------------------

#' Arrange Plots in PNG file
#' 
#' Arrange ggplot objects with gridExtra::grid.arrange and write the result
#'   to a PNG file
#' 
#' @param plots list of ggplot objects
#' @param filename full path to the PNG file to be written. The extension ".png" will be 
#'   automatically appended if it is omitted
#' @param \dots arguments passed to \code{\link[gridExtra]{grid.arrange}}
#' 
#' @export
#' 
arrange_in_png <- function(plots, filename, ...)
{
  output_to_png(
    FUN = gridExtra::grid.arrange, args = c(plots, list(...)),
    filename = filename
  )
}

# output_to_png ----------------------------------------------------------------

#' Redirect Output of Plot Function to PNG file
#' 
#' Redirect the output of a plot function to a PNG file
#' 
#' @param FUN plot function to be called
#' @param args list of arguments given to the plot function \code{FUN}
#' @param filename full path to the PNG file to be written. The extension ".png" will be 
#'   automatically appended if it is omitted
#' @param size vector containing the width (first element) and height (second element)
#'   of the plot, as passed to \code{\link[grDevices]{png}}. By default 
#'   the dimensions of a DIN A4 page are used.
#' @param units units as passed to \code{\link[grDevices]{png}}
#' @param res resolution as passed to \code{\link[grDevices]{png}}
#' @param \dots further arguments passed to \code{\link[grDevices]{png}}
#' @param dbg if \code{TRUE} a message "Plotting to <filename>..." is shown
#' 
#' @export
#' 
output_to_png <- function(
  FUN, args, filename, size = unlist(kwb.utils::DIN.A4()), units = "cm", 
  res = 300, ..., dbg = TRUE
)
{
  extension <- ".png"
  
  if (! endsWith(tolower(filename), extension)) {
    
    filename <- paste0(filename, extension)
  }
  
  kwb.utils::catIf(dbg, "Plotting to", filename, "... ")
  
  grDevices::png(
    filename, width = size[1], height = size[2], units = units, res = res, ...
  )
  
  print(do.call(FUN, args))
  
  grDevices::dev.off()
  
  kwb.utils::catIf(dbg, "ok.\n")
}
