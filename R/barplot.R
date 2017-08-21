# mybarplot --------------------------------------------------------------------

#' barplot with more flexibility on labels and axis
#' 
#' barplot function allowing to set label and axis arguments, e.g. rotation of
#'   labels, giving a unit to the axis values
#' 
#' @param height as in \code{\link{barplot}}
#' @param names.arg as in \code{\link{barplot}}
#' @param cex.axis character expansion factor of axis labels
#' @param cex.names character expansion factor of bar group labels
#' @param \dots additional arguments to barplot
#' @param args.text list of arguments that are given to \code{text} being called to plot the
#'   bar group labels. Use \code{\link{args_text}} to create such a list.
#' @param args.yaxis list of arguments that are given to \code{axis} being called to plot the
#'   y axis. Use \code{\link{args_yaxis}} to create such a list.
#' 
#' @return Returns (invisibly) what \code{barplot} returns: the x positions of the
#'   bars that have been plotted
#' 
#' @examples 
#'   height1 <- structure(1:10, names = paste("Category", 1:10))
#'   
#'   height2 <- matrix(
#'     1:12, nrow = 3, dimnames = list(
#'       c("A", "B", "C"), paste("Long cateogry name", 1:4))
#'   )
#'   
#'   graphics::par(mfrow = c(1, 2))
#'   
#'   # In the simplest form, mybarplot does what barplot does...
#'   x1 <- graphics::barplot(height1, main = "barplot")
#'   x2 <- mybarplot(height1, main = "mybarplot")
#'   
#'   # ... and gives the same result
#'   identical(x1, x2)
#'   
#'   # We cannot distinguish the labels. With mybarplot we have finer access to the
#'   # labels, such as rotation, yposition and adjustment  
#'   graphics::par(mfrow = c(1, 3))
#'   
#'   graphics::barplot(height1, las = 2, main = "barplot")  
#'   
#'   mybarplot(height1, las = 2, main = "mybarplot", 
#'             args.text = args_text(srt = 45, y.abs = -1, col = "blue"))
#'   
#'   mybarplot(height1, las = 2, main = "mybarplot", 
#'             args.text = args_text(srt = 90, y.abs = 10, col = "red"))
#'   
#'   # Concerning the y-axis, you may e.g. give a unit to the values or modify the
#'   # positions at which to draw ticks
#'   graphics::par(mfrow = c(1, 1))
#'   
#'   mybarplot(height1, las = 2, cex.names = 0.8,,
#'             args.text = args_text(srt = 90, y.abs = -0.5), 
#'             args.yaxis = args_yaxis(at = 0:10, unit = " %", col.axis = "blue"))
#'   
#'   graphics::par(mfrow = c(1, 2))
#'   
#'   graphics::barplot(height2, main = "barplot", cex.names = 0.8)
#'   
#'   mybarplot(height2, main = "mybarplot", cex.names = 0.8, 
#'             args.text = args_text(srt = 30))
#'   
#'   graphics::barplot(height2, las = 1, main = "barplot",   beside = TRUE, 
#'                     cex.names = 0.8)
#'   
#'   mybarplot(height2, las = 1, main = "mybarplot", beside = TRUE, 
#'             cex.names = 0.8, args.text = args_text(srt = 30))
#'   
mybarplot <- function(
  height, names.arg = NULL, cex.axis = graphics::par("cex.axis"),
  cex.names = graphics::par("cex.axis"), ..., args.text = NULL, 
  args.yaxis = NULL
)
{
  # Prepare a list of arguments to barplot
  args.barplot <- list(height = height, names.arg = names.arg, 
                       cex.axis = cex.axis, cex.names = cex.names, ...)
  
  if (isTRUE(kwb.utils::defaultIfNULL(args.barplot$horiz, FALSE))) {
    
    warning("mybarplot does not support horiz = TRUE. ",
            "Using barplot() instead...")
    
    return (do.call(graphics::barplot, args.barplot))
  } 
  
  # If arguments for specifying the y-axis are given, tell barplot not 
  # to plot the y axis
  if (! is.null(args.yaxis)) {
    args.barplot <- c(args.barplot, yaxt = "n")
  }
  
  # If arguments for specifying the labels are given, tell barplot not 
  # to plot the labels
  if (! is.null(args.text)) {
    args.barplot <- c(args.barplot, axisnames = FALSE)
  }
    
  # Call barplot giving the prepared and all additional arguments
  x <- do.call(graphics::barplot, args.barplot)
  
  # If arguments for specifying the labels are given, get the labels
  # and plot them with text()
  if (! is.null(args.text)) {
    
    if (is.null(names.arg)) {
      names.arg <- if (is.matrix(height)) {
        colnames(height) 
      } else {
        names(height)
      }    
    }
    
    # If height is a matrix and beside = TRUE, use the x position of the middle
    # bar
    if (is.matrix(x) && ncol(x) > 1) {
      x <- apply(x, 2, stats::median)
    }

    # If y.abs is not given, calculate it from y.rel
    if (is.null(args.text$y.fix)) {
      y.rel <- kwb.utils::defaultIfNULL(args.text$y.rel, args_text()$y.rel)
      y.abs <- y.rel * kwb.plot::getPlotRegionSizeInUserCoords()$height
    }
    
    args.text <- c(
      list(x = x, 
           y = y.abs, 
           labels = names.arg, 
           cex = cex.names, xpd = TRUE), 
      args.text[setdiff(names(args.text), c("y.abs", "y.rel"))]
    )
    
    do.call(graphics::text, args.text)
  }
  
  # If arguments for specifying the y axis are given, plot the y axis
  # with axis()
  if (! is.null(args.yaxis)) {
    args.yaxis <- c(list(cex.axis = cex.axis), args.yaxis)
    do.call(graphics::axis, args.yaxis)
  }
  
  invisible(x)
}

# args_mybarplot ---------------------------------------------------------------

#' Create argument list for mybarplot
#' 
#' Create argument list for mybarplot
#' 
#' @param cex.axis character expansion factor of axis labels
#' @param cex.names character expansion factor of bar group labels
#' @param \dots additional arguments to barplot
#' @param args.text list of arguments that are given to \code{text} being called to plot the
#'   bar group labels. Use \code{\link{args_text}} to create such a list.
#' @param args.yaxis list of arguments that are given to \code{axis} being called to plot the
#'   y axis. Use \code{\link{args_yaxis}} to create such a list.
#' 
args_mybarplot <- function(
  cex.axis = graphics::par("cex.axis"), cex.names = graphics::par("cex.axis"),
  ..., args.text = NULL, args.yaxis = NULL
)
{
  list(cex.axis = cex.axis, cex.names = cex.names, ..., 
       args.text = args.text, args.yaxis = args.yaxis)
}

# args_text --------------------------------------------------------------------

#' Create argument list for text
#' 
#' Create argument list for text
#'
#' @param y.abs absolute y positions
#' @param y.rel relative y positions
#' @param srt see \code{\link{text}}
#' @param adj see \code{\link{text}}
#' @param \dots further arguments to \code{\link{text}}
#' 
#' @return list of arguments that may be used as \code{args.text} in
#'   \code{\link{mybarplot}}
#' 
args_text <- function(
  y.abs = NULL, y.rel = -0.01, srt = 90, adj = c(1, 0.5), ...
)
{
  list(y.abs = y.abs, y.rel = y.rel, srt = srt, adj = adj, ...)
}

# args_yaxis -------------------------------------------------------------------

#' Create argument list for axis
#' 
#' Create argument list for axis
#' 
#' @param side 1: bottom, 2: left, 3: top, 4: right
#' @param at where to put the ticks/labels
#' @param labels vector of character labels
#' @param unit text appended to the labels with \code{\link{paste0}}
#' @param las numeric in {0,1,2,3}; the style of axis labels 
#'   (see \code{\link{par}})
#' @param \dots additional parameters passed to \code{\link{axis}}
#' 
#' @return list of arguments that may be used as \code{args.yasis} in
#'   \code{\link{mybarplot}} 
#' 
args_yaxis <- function(
  side = 2, at = graphics::axTicks(side), labels = paste0(at, unit), unit = "", 
  las = 1, ...
)
{
  list(side = side, at = at, labels = labels, las = las, ...)
}
