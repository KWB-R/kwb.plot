# arrange_in_pdf ---------------------------------------------------------------

#' Arrange Plots in PDF file (DIN A4)
#' 
#' Arrange ggplot objects with gridExtra::grid.arrange and write the result
#'   to a PDF file in DIN A4 format
#' 
#' @param plots list of ggplot objects
#' @param landscape passed to \code{\link[kwb.utils]{preparePdf}}
#' @param \dots arguments passed to \code{\link[gridExtra:arrangeGrob]{grid.arrange}}
#' 
#' @export
#' 
arrange_in_pdf <- function(plots, landscape = TRUE, ...)
{
  pdf_file <- kwb.utils::preparePdf(landscape = landscape)
  do.call(gridExtra::grid.arrange, c(plots, list(...)))
  kwb.utils::finishAndShowPdf(pdf_file)
}

# plot_all_to_pdf --------------------------------------------------------------

#' Print all ggplot Objects to a PDF file
#' 
#' @param plots list of ggplot objects
#' @param landscape passed to \code{\link[kwb.utils]{preparePdf}}
#' @param \dots further arguments passed to \code{\link[kwb.utils]{preparePdf}}
#' 
#' @export
#' 
plot_all_to_pdf <- function(plots, landscape = TRUE, ...)
{
  pdf_file <- kwb.utils::preparePdf(landscape = landscape, ...)
  print(plots)
  kwb.utils::finishAndShowPdf(pdf_file)
}
