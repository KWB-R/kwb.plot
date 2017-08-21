# stackplot --------------------------------------------------------------------

#' Plots on Top of Each Other
#' 
#' @param functionCalls list of expressions each of which is expected to create a plot when
#'   being evaluated with eval
#' @param heights.cm heights of plots in cm
#' @param margins.top.cm top margins of plots in cm
#' @param margins.bottom.cm bottom margins of plots in cm
#' @param margins.left.cm left margins of plots in cm
#' @param margins.right.cm right margins of plots in cm
#' @param envir environment in which the expressions given in functionCalls are to be
#'   evaluated. Default: parent.frame()
#' 
#' @export
#' 
stackplot <- function(
  functionCalls, heights.cm = 5, margins.top.cm = 0, margins.bottom.cm = 0,
  margins.left.cm = 3, margins.right.cm = 1, envir = parent.frame()
)
{
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(graphicalParameters))
  
  n <- length(functionCalls)
 
  heights.cm        <- kwb.utils::recycle(heights.cm, n)
  
  margins.top.cm    <- kwb.utils::recycle(margins.top.cm, n)
  margins.bottom.cm <- kwb.utils::recycle(margins.bottom.cm, n)
  margins.left.cm   <- kwb.utils::recycle(margins.left.cm, n)
  margins.right.cm  <- kwb.utils::recycle(margins.right.cm, n)
  
  stackplotLayout(heights.cm, margins.top.cm, margins.bottom.cm)
  
  for (i in 1:n) {

    cat(sprintf("\n*** Plotting plot %d/%d: %s... ", 
                i, n, as.character(functionCalls[[i]])))
    
    graphics::par(mai = c(
      kwb.utils::toInches(margins.bottom.cm[i]),
      kwb.utils::toInches(margins.left.cm[i]),
      kwb.utils::toInches(margins.top.cm[i]),
      kwb.utils::toInches(margins.right.cm[i])
    ))
    
    eval(functionCalls[[i]], envir = envir)
    
    cat("ok.\n")
  }
}

# stackplotLayout --------------------------------------------------------------

#' Create Layout for "stackplot"
#' 
#' @param heights.cm heights of plots in cm
#' @param margins.top.cm top margins of plots in cm
#' @param margins.bottom.cm bottom margins of plots in cm
#' @param show if \code{TRUE} the layout is shown in the form of a plot with
#'   the borders between the different plot sections being indicated

stackplotLayout <- function(
  heights.cm, margins.top.cm, margins.bottom.cm, show = FALSE
)
{
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphicalParameters)
  
  n <- length(heights.cm)
  
  rowheights.cm <- sapply(1:n, function(i) {
    heights.cm[i] + margins.top.cm[i] + margins.bottom.cm[i]
  })
  
  graphics::layout(matrix(1:n), heights = graphics::lcm(rowheights.cm))
  
  if (show) {
    
    graphics::layout.show(n)    
  }
}
