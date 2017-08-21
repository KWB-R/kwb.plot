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

# niceLabels -------------------------------------------------------------------

#' Nice Labels 
#' 
#' Generates a nice vector of labels by suppressing labels at certain
#'   positions. If a \emph{labelStep} is given, only every \emph{labelStep}-th 
#'   label is considered. If a vector \emph{labpos} of label positions and
#'   a minimum distance \emph{mindist} of labels is given, it is guaranteed
#'   that the distance between labels to be shown is at least \emph{mindist}.
#' 
#' @param label vector of labels
#' @param labelstep step width in which labels are to be printed, e.g. labelStep = 2 is used
#'   for plotting every second label.
#' @param labelpos label positions
#' @param mindist minimum distance between labels
#' @param offset offset by which the index of the first remaining label (default: 1) is 
#'   shifted. Default: 0, i.e. the labels at indices \emph{1}, \emph{1 + 
#'   1*labelstep}, \emph{1 + 2*labelstep}, etc. remain. Offset = 1: the labels 
#'   at indices \emph{2}, \emph{2 + 1*labelstep}, \emph{2 + 2*labelstep}, etc.
#'   remain.
#'   
#' @export
#' 
#' @examples 
#'   
#'   x <- matrix(1:12, nrow = 3)
#'   
#'   names.arg <- rep(1:4, each = 3)
#'   
#'   mybarplot <- function(x, ...) {
#'     barplot(x, horiz = TRUE, las = 1, beside = TRUE, ...)
#'   }
#'     
#'   mybarplot(x, names.arg = names.arg)
#'   mybarplot(x, names.arg = niceLabels(names.arg, labelstep = 3))
#'   mybarplot(x, names.arg = niceLabels(names.arg, labelstep = 3, offset = 1))
#'   mybarplot(x, names.arg = niceLabels(names.arg, labelstep = 3, offset = 2))
#'   
niceLabels <- function(
  label, labelstep = NULL, labelpos = NULL, mindist = 1, offset = 0
)
{
  if (is.null(labelstep) && is.null(labelpos))
    stop("Either labelstep or labelpos must be given.")
  
  if (is.null(labelstep) && length(labelpos) != length(label))
    stop("label and labelpos must be of same length.")
  
  # convert labels to character
  label <- as.character(label)
  
  if (! is.null(labelstep)) {
    idx <- seq(1, by = 1, along.with = label)
    visible <- ((idx - 1 - offset) %% labelstep == 0)
  }
  else {
    
    # first label is always visible, set first position as reference
    visible <- TRUE
    ref <- labelpos[1]
    
    # loop through remaining label positions
    for (pos in labelpos[-1]) {
      
      # the label is visible if the distance to the reference 
      # (last visible) label is at least mindist
      vis <- (pos - ref >= mindist)
      visible <- c(visible, vis)
      
      # update reference label position if label at current position is visible
      if (vis) {
        ref <- pos
      }
    }
  }
  
  # set invisible labels to NA
  label[!visible] <- ""
  
  # Return labels
  label
}

# drawBoxplot ------------------------------------------------------------------

#' Draw Boxplot Icon
#' 
#' draws a symmetric boxplot icon around a centre
#' 
#' @param centre.x x coordinate in user coordinates around which the box is to be drawn
#' @param centre.y y coordinate in user coordinates around which the box is to be drawn  
#' @param boxwidth.cm width of the box in cm. Default: 1
#' @param boxheight.cm height of the box in cm. Default: \emph{boxwidth.cm}
#' @param whisker.cm length of the whiskers in cm. Default: \emph{boxheight.cm}
#' 
#' @export
#' 
#' @examples 
#'   
#'   ### prepare a simple plot area
#'   plot(1:5)
#'   
#'   ### draw a box around the centre at (2, 2) with default proportions
#'   drawBoxplot(2, 2, boxwidth.cm = 1)
#'   
#'   ### draw a box around the centre at (3, 3) with differing width and height
#'   drawBoxplot(3, 3, boxwidth.cm = 2, boxheight.cm = 1)
#'   
#'   ### draw a box around the centre at (4, 4) with modified whisker lengths
#'   drawBoxplot(4, 4, boxwidth.cm = 0.5, boxheight.cm = 1.5, whisker.cm = 0.5)
#'   
drawBoxplot <- function(
  centre.x, centre.y, boxwidth.cm = 1, boxheight.cm = boxwidth.cm, 
  whisker.cm = boxheight.cm
)
{
  area <- kwb.plot::getPlotRegionSizeInUserCoords()
  
  cm <- cmToUserWidthAndHeight(1)
  
  dx <- cm$width
  dy <- cm$height  
  
  # Box
  graphics::rect(
    xleft   = centre.x - boxwidth.cm / 2 * dx, 
    xright  = centre.x + boxwidth.cm / 2 * dx, 
    ybottom = centre.y - boxheight.cm / 2 * dy,
    ytop    = centre.y + boxheight.cm / 2 * dy
  )
  
  # Median line
  graphics::segments(
    x0 = centre.x - boxwidth.cm/2*dx,
    x1 = centre.x + boxwidth.cm/2*dx,
    y0 = centre.y,
    y1 = centre.y,
    lwd = 2
  )
  
  # whisker lines
  graphics::segments(
    x0 = centre.x,
    x1 = centre.x,
    y0 = centre.y + c(boxheight.cm / 2 + whisker.cm / 2, - boxheight.cm / 2) * dy,
    y1 = centre.y + c(boxheight.cm / 2, - boxheight.cm / 2 - whisker.cm / 2) * dy, 
    lty = "dashed"
  )
  
  # whisker ends
  graphics::segments(
    x0 = centre.x + c(+ boxwidth.cm / 4) * dx,
    x1 = centre.x + c(- boxwidth.cm / 4) * dx,
    y0 = centre.y + c(boxheight.cm / 2 + whisker.cm / 2, - boxheight.cm / 2 - whisker.cm / 2) * dy, 
    y1 = centre.y + c(boxheight.cm / 2 + whisker.cm / 2, - boxheight.cm / 2 - whisker.cm / 2) * dy
  )
}

# getPlotCharacterConstants ----------------------------------------------------

#' List of Named Constants for Graphical Parameter pch
#' 
#' @return list of plot character constants with each element being named according
#'   to the appearence of the plot character, e.g. "CIRCLE", "TRIANGLE", ...
#' 
#' @keywords internal
#' 
getPlotCharacterConstants <- function()
{
  list(
    CIRCLE = 1,
    TRIANGLE = 2,
    FILLED_CIRCLE = 16,
    FILLED_TRIANGLE = 17
  )
}

# addGridIfTrue ----------------------------------------------------------------

#' Add a Grid if the First Argument is TRUE
#' 
#' @param plot.grid logical. if TRUE the grid is plotted, else not.
#' @param xPositions x positions of the vertical grid lines
#' @param yPositions y positions of the horizontal grid lines
#' @param col colour of the grid lines, passed to \code{\link{abline}}
#' @param lty line type of the grid lines, passed to \code{\link{abline}}
#' @param \dots additional arguments passed to \code{\link{abline}}
#' 
#' @keywords internal
#' 
addGridIfTrue <- function(
  plot.grid, xPositions, yPositions = graphics::axTicks(2), col = "grey", 
  lty = 3, ...
)
{
  if (plot.grid) {      
    graphics::abline(v = xPositions, h = yPositions, col = col, lty = lty, ...)
  }  
}

# addLabels --------------------------------------------------------------------

#' Add Labels
#' 
#' add labels at given x-positions to plot (with alternating y positions
#'   to avoid overlapping labels)
#' 
#' @param x x positions of the labels
#' @param labels vector of character containing the labels
#' @param y0 base y position of the labels
#' @param bandheight height of band "around" (alternating == FALSE) or above (alternating ==
#'   TRUE) y0 as a fraction of the plot region height (e.g. 0.1 for 10
#'   percent). Default: 0.1
#' @param col colour of the labels
#' @param group.size passed to \code{\link{labelPositionY}}
#' @param alternating passed to \code{\link{labelPositionY}}
#' @param col.line colour of the lines to the labels
#' @param lty type of the lines to the labels (as defined in \code{\link{par}})
#' @param lty.horiz.line type of the horizontal line (as defined in
#'   \code{\link{par}})
#' @param adj passed to \code{\link{text}}
#' @param cex passed to \code{\link{text}}
#' 
#' @export
#' 
addLabels <- function(
  x, labels = as.character(x), y0 = 0, bandheight = 0.1, col = "black", 
  group.size = 3, alternating = FALSE, col.line = "black", lty = 1, 
  lty.horiz.line = 0, adj = -0.1, cex = 0.7
)
{
  y <- labelPositionY(
    n = length(labels), 
    y0 = y0, 
    bandheight = bandheight,
    group.size = group.size, 
    alternating = alternating
  )
  
  graphics::segments(x0 = x, y0 = y0, x1 = x, y1 = y, col = col.line, lty = lty)

  if (lty.horiz.line != 0) {
    graphics::abline(h = y0, col = col.line, lty = lty.horiz.line)    
  }
  
  graphics::text(
    x, y, labels = labels, col = col, srt = 0, cex = cex, adj = adj
  )
}

# labelPositionY ---------------------------------------------------------------

#' y-positions for labels
#' 
#' alternating y-positions to be used for label placement
#' 
#' @param n number of y-positions to be generated
#' @param y0 y-positions around which the generated values will "alternate"  
#' @param bandheight height of band "around" (alternating == FALSE) or above (alternating ==
#'   TRUE) y0 as a fraction of the plot region height (e.g. 0.1 for 10
#'   percent). Default: 0.1
#' @param group.size number of labels that are to be placed in on "group" in
#'   which label positions are modified along the y axis
#' @param alternating if \code{TRUE} (default) the label positions are 
#'   alternating between positive and negative values
#'   
#' @keywords internal
#' 
labelPositionY <- function(
  n, y0 = 0, bandheight = 0.1, group.size = 3, alternating = TRUE
)
{
  y <- alternatingPositions(n, group.size = group.size, alternating = alternating)  
  y0 + y/max(abs(y)) * bandheight * getPlotRegionSizeInUserCoords()$height
}

# alternatingPositions ---------------------------------------------------------

#' Alternating Positions
#' 
#' @param n number of positions to be generated
#' @param group.size number of positions to be grouped. Within each group the
#'   positions are modified. 
#' @param alternating if \code{TRUE} the positions are alternating between
#'   negative and positive
#'   
#' @keywords internal
#' 
alternatingPositions <- function(n = 10, group.size = 3, alternating = TRUE)
{
  x <- 1:n
  
  x <- if (alternating) {
    as.integer((x - 1) / 2) %% group.size + 1
  } else {
    ((x - 1) %% group.size) + 1
  }
  
  x <- (group.size + 1) - x
  
  if (alternating) {
    x <- (-1)^(1:n) * x
  }
  
  x
}

# addTimeAxis ------------------------------------------------------------------

#' Add Time Axis
#' 
#' @param myDateTime vector of POSIXct timestamps
#' @param xlim lower and upper limits of range of timestamps to be shown
#' @param n number of timestamps to be shown, passed to \code{\link{pretty}}
#' @param time.format time format string such as "%H:%M:%S", see 
#'   \code{\link{format.POSIXct}}
#' @param add.grid if \code{TRUE} vertical lines are added at the positions of 
#'   the time tickmarks
#' @param padj passed to \code{\link{axis}}
#' 
#' @export
#' 
addTimeAxis <- function(
  myDateTime, xlim = range(myDateTime), n = 20, time.format = NULL, 
  add.grid = FALSE, padj = 0.5
)
{
  # Set default time format (inlinedocs does not like it above)
  if (is.null(time.format)) {
    time.format <- "\n%H:%M\n%d.%m."
  }
  
  in.range <- which(kwb.utils::inRange(myDateTime, xlim[1], xlim[2]))
  
  if (kwb.utils::isNullOrEmpty(in.range)) {
    cat("\nNo timestamps within xlim!\n")  
    return()
  }
  
  intervalLength.s <- diff(as.integer(range(myDateTime[in.range])))
  
  pretty.times <- if (intervalLength.s < n) {
    
    myDateTime[in.range]
    
  } else {
    
    pretty(myDateTime[in.range], n)
  }
  
  graphics::axis(
    side = 1, 
    at = pretty.times, 
    labels = format(pretty.times, format = time.format), 
    padj = padj
  )  
  
  if (add.grid) {    
    graphics::abline(v = pretty.times, col = "grey", lty = 3)
  }
}

# bestRowColumnSetting ---------------------------------------------------------

#' best nrow/ncol setting for n plots
#' 
#' Number of rows and columns in an optimal plot grid for n plots
#' 
#' @param n number of plots to be placed in a matrix of equally sized plot cells
#' @param target.ratio desired height/width ratio within each plot (ignoring margins). Default: 1
#' 
#' @return named vector of two elements with the first element (\code{nrow})
#'   representing the number of rows and the second element (\code{ncol})
#'   representing the number of columns for an optimal plot grid to be used for
#'   \code{n} plots in the current plot region
#' 
#' @export
#' 
#' @examples 
#'   # save current graphical parameter setting
#'   old.par <- graphics::par(no.readonly = TRUE)
#'   
#'   for (i in 2:5) {
#'     
#'     graphics::par(mfrow = kwb.plot::bestRowColumnSetting(i))
#'     
#'     for (j in 1:i) {
#'       plot(seq_len(j), main = paste0("j = ", j, "/", i))
#'     }      
#'   }
#'   
#'   # restore graphical parameter setting
#'   graphics::par(old.par)  
#' 
bestRowColumnSetting <- function(n, target.ratio = 1)
{
  rowsColumns <- lapply(seq_len(n), function(n.rows) {
    n.cols = ceiling(n / n.rows)
    c(n.rows, n.cols)
  })  
  
  ratio <- getPlotRegionRatio()
  
  ratios <- sapply(rowsColumns, function(rowColumn) {
    n.rows <- rowColumn[1]
    n.cols <- rowColumn[2]  
    ratio * n.cols/n.rows
  })
  
  structure(
    rowsColumns[[which.min(abs(ratios - target.ratio))]],
    names = c("nrow", "ncol")
  )
}

# setMargins -------------------------------------------------------------------

#' Set the Plot Margins
#' 
#' @param bottom bottom margin as used in \code{\link{par}}("mar")
#' @param left left margin as used in \code{\link{par}}("mar")
#' @param top top margin as used in \code{\link{par}}("mar")
#' @param right right margin as used in \code{\link{par}}("mar")
#' 
#' @export
#' 
setMargins <- function(bottom = NA, left = NA, top = NA, right = NA)
{
  values <- c(bottom, left, top, right)
  selected <- !is.na(values)
  
  margins <- graphics::par("mar")
  margins[selected] <- values[selected]
  
  graphics::par(mar = margins)
}
