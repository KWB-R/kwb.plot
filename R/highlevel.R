# plotRain ---------------------------------------------------------------------

#' Plot Rain Data as Bars
#' 
#' @param timestamps timestamps 
#' @param values precipitation values in mm
#' @param gaugeName Name of rain gauge to appear in the y label
#' @param xlim x limits
#' @param ylim y limits
#' @param signal.width width of time intervals represented by timestamps, in seconds. If NA 
#'   (default) this value is deduced from the time differences in 
#'   \emph{timestamps}
#' @param shift.to.begin value in seconds by which the timstamps need to be shifted in order to get
#'   the begin of the time intervals that the timestamps represent. Default: 0
#'   (meaning that the timestamps represent the beginnings of the time 
#'   intervals). A value of \emph{signal.width} would mean that the timestamps
#'   represent the ends of the time intervals and a value of 
#'   \emph{signal.width/2} would mean that the timestamps represent the 
#'   midpoints of the time intervals that they represent. 
#' @param col fill colour of bars. Default: "blue"
#' @param pch plot character to be used for plotting data points (at the given
#'   timestamps), additionally to plotting bars. Set to NA in order to suppress
#'   plotting these points
#' @param reverse.ylim if TRUE (default), the y axis is reversed, i.e. the bars are heading from
#'   top to bottom
#' @param las numeric in {0,1,2,3}; the style of axis labels. See help for par.
#' @param innerMargins passed to \code{\link{plot_variable}} 
#' @param add.time.axis passed to \code{\link{plot_variable}} 
#' @param \dots further arguments passed to \code{\link{points}} 
#' 
#' @export
#' 
plotRain <- function(
  timestamps, values, gaugeName = "", xlim = NULL, ylim = NULL, 
  signal.width = NA, shift.to.begin = 0, col = "blue", pch = 16, 
  reverse.ylim = TRUE, las = 1, innerMargins = c(0, 0, 0, 0), 
  add.time.axis = TRUE, ...
)
{
  n <- length(timestamps)
  
  if (n == 0) {
    
    warning("Nothing to plot...")
    
  } else {
    
    if (is.na(signal.width)) {
      signal.width <- kwb.datetime::getTimestepInSeconds(timestamps, default = 60)
    }
    
    interval.begin <- timestamps - shift.to.begin
    interval.end <- interval.begin + signal.width
    
    #if (is.null(xlim)) {
    #  xlim <- c(min(interval.begin), max(interval.end))
    #}
    
    xlim <- appropriateLimits(
      x = c(interval.begin, interval.end), 
      limits = xlim)
    
    #     ylim <- appropriateLimits(
    #       x = values, #[inLimits(timestamps, xlim)], 
    #       limits = c(0, NA), 
    #       default = c(0, 1))

    if (is.null(ylim)) {
      ylim <- appropriateLimits(
        x = values[inLimits(timestamps, xlim)], 
        limits = c(0, NA), 
        default = c(0, 1))      
    }
    
    #     if (is.null(ylim)) {
    #       
    #       t.in.xlim <- which(inLimits(timestamps, xlim))
    #       
    #       if (kwb.utils::isNullOrEmpty(t.in.xlim)) {
    #         warning("No rain data available for time interval ", 
    #                 xlim[1], ":", xlim[2], " -> ymax = 1")
    #         
    #         ymax <- 1
    #       }
    #       else {
    #         
    #         ymax <- max(values[t.in.xlim], na.rm=TRUE)
    #         
    #         if (ymax == 0) {
    #           # warning("No rain within time interval ", xlim[1], ":", xlim[2], " -> ymax = 1")
    #           ymax <- 1
    #         }      
    #       }
    #       
    #       ylim <- c(0, ymax)
    #     }
    
  }
  
  plot_variable(
    data.frame(timestamps, values),
    type = "n", 
    xlim = xlim, 
    ylim = ylim, 
    reverse.ylim = reverse.ylim,
    innerMargins = innerMargins, 
    ylab = sprintf("N %s mm/(%dmin)", gaugeName, round(signal.width/60)),
    add.time.axis = add.time.axis)
  
  graphics::rect(
    xleft = interval.begin, 
    xright = interval.end, 
    ybottom = 0, ytop = values, 
    col = col)
  
  if (!is.na(pch)) {
    graphics::points(timestamps, values, pch = pch, ...)
  }
}

# plot_variable ----------------------------------------------------------------

#' Plot a Variable of a Data Frame
#' 
#' @param hydraulicData data frame with at least two columns. First column is expected to contain
#'   the timestamps
#' @param variableName default: name of second column
#' @param type plot type, passed to \code{\link{points}} 
#' @param col colour, passed to \code{\link{points}} 
#' @param ylab y label, passed to \code{\link{plot}}
#' @param pch plot character, passed to \code{\link{points}} 
#' @param xlim x limits, passed to \code{\link{plot}} 
#' @param ylim y limits, passed to \code{\link{plot}} 
#' @param add if \code{TRUE} points are added to an existing plot
#' @param plot.grid if \code{TRUE} a grid is drawn
#' @param innerMargins margins (bottom, left, top, right) within the plot area
#' @param absolute if \code{TRUE}, innerMargins are to be interpreted as absolute values instead of 
#'   fractions of the plot region. Default: FALSE
#' @param reverse.ylim if \code{TRUE} (default), the y axis is reversed, i.e. the bars are heading from
#'   top to bottom  
#' @param add.time.axis if \code{TRUE} a time axis is drawn
#' @param time.axis.in.margins if \code{TRUE} the time axis is drawn only within
#'   the plot area excluding the margins
#' @param \dots additional arguments passed to \code{\link{plot}}
#' 
#' @export
#' 
plot_variable <- function(
  hydraulicData, variableName = names(hydraulicData)[[2]],
  type = .defaultPlotParameter("type", variableName), 
  col = .defaultPlotParameter("col", variableName), 
  ylab = .defaultPlotParameter("ylab", variableName), 
  pch = .defaultPlotParameter("pch", variableName), 
  xlim = NULL, ylim = NULL, add = FALSE, plot.grid = TRUE, 
  innerMargins = c(0, 0, 0, 0), absolute = FALSE,
  reverse.ylim = FALSE, add.time.axis = TRUE, time.axis.in.margins = FALSE, ...
)
{
  stopifnot(is.data.frame(hydraulicData) || ncol(hydraulicData) < 2)
  
  if (! kwb.utils::checkForMissingColumns(hydraulicData, variableName, 
                                          do.stop = FALSE)) {
    return()
  }
  
  x <- hydraulicData[[1]]
  y <- hydraulicData[[variableName]]
  
  # TODO: check if we can use plotNewOrAdd...
  if (add) {
    
    graphics::points(x, y, type = type, col = col, pch = pch)
    
  } else {
    
    xlim <- appropriateLimits(x, xlim, default=c(0, 1))
    xlim.extended <- kwb.utils::extendLimits(
      xlim, innerMargins[2], innerMargins[4], absolute = absolute
    )
    
    in.xlim <- inLimits(x, xlim)
    in.xlim.extended <- inLimits(x, xlim.extended)
    
    ylim <- appropriateLimits(y[in.xlim.extended], ylim, default = c(0, 1))
    
    in.ylim <- inLimits(y, ylim)
    
    # extend limits by inner margins, 
    ylim.extended <- kwb.utils::extendLimits(
      ylim, innerMargins[1], innerMargins[3], absolute = absolute
    )
    
    if (reverse.ylim) {
      ylim.extended <- rev(ylim.extended)
    }
    
    #cat("ylim:\n")
    #print(ylim)
    
    #cat("ylim.extended:\n")
    #print(ylim.extended)    
    
    graphics::plot(
      x, y, type = type, col = col, pch = pch, xlim = xlim.extended, 
      ylim = ylim.extended,xaxt = "n", 
      #yaxt = "n", 
      xlab = "", ylab = ylab, ...
    )
    
    # label the axes manually
    xValues <- if (!time.axis.in.margins) {
      
      x[which(in.xlim)]
      
    } else {
      
      x[which(in.xlim.extended)]
    }
    
    xPositions <- if (length(xValues) > 1) {
      
      pretty(xValues, n = 10)
      
    } else {
      
      x
    }
    
    if (add.time.axis) {
      
      addTimeAxis(xValues, time.format = "%H:%M\n%d.%m.%y")
    }
    
    addGridIfTrue(plot.grid, xPositions)
    
    # indicate the original limits
    graphics::abline(v=xlim, h=ylim, col="blue", lty=3)    
  }
}

# .defaultPlotParameter --------------------------------------------------------

#' Default Plot Parameters for Hydraulic Variables
#' 
#' @param parameterName one of c("type", "col", "ylab", "pch")
#' @param variableName variable name matching
#'   "(Q|H|v)[.](raw|signal|interpol|pred)"
#' 
#' @export
#' 
.defaultPlotParameter <- function(parameterName, variableName)
{
  default <- list(
    type = "p",
    col  = list(Q = "blue", H = "red", v = "green", other = "black"),
    ylab = list(Q = "Q (L/s)", H = "H (m)", v = "v (m/s)", other = "y (unit)"),
    pch  = list(raw = 1, signal = 3, interpol = 4, pred = 4, other = 1)
  )
  
  defaultValue <- default[[parameterName]]
  
  variableAcronym <- if (parameterName == "pch") {
    
    strsplit(variableName, "\\.")[[1]][2]
    
  } else {
    
    substr(variableName, 1, 1)
  }
  
  if (is.list(defaultValue)) {
    
    if (variableAcronym %in% names(defaultValue)) {
      
      defaultValue[[variableAcronym]]
      
    } else {
      
      defaultValue$other
    }
  } else {
    
    defaultValue
  }    
}
