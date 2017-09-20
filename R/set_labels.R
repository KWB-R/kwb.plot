# set_subtitles ----------------------------------------------------------------

#' Set the Subtitles in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param subtitle subtitle (character) to be given to each plot or to the plots
#'   selected by their \code{indices}
#' @param indices indices of the plots to which the subtitle is to be given. By
#'   default the subtitle is given to all plots
#' @param \dots additional arguments to \code{\link{set_labels}}, such as 
#'   \code{action}
#'   
#' @export
#' 
set_subtitles <- function(plots, subtitle, indices = seq_along(plots), ...)
{
  set_labels(plots, subtitle = subtitle, indices = indices)
}

# set_titles -------------------------------------------------------------------

#' Set the Titles in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param title title (character) to be given to each plot or to the plots
#'   selected by their \code{indices}
#' @param indices indices of the plots to which the title is to be given. By
#'   default the title is given to all plots
#' @param \dots additional arguments to \code{\link{set_labels}}, such as 
#'   \code{action}
#'   
#' @export
#' 
set_titles <- function(plots, title, indices = seq_along(plots), ...)
{
  set_labels(plots, title = title, indices = indices)
}

# set_xlabs --------------------------------------------------------------------

#' Set the x Axis Label in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param xlab x axis label (character) to be given to each plot or to the plots
#'   selected by their \code{indices}
#' @param indices indices of the plots to which the x axis label is to be given.
#'   By default the x axis label is given to all plots
#' @param \dots additional arguments to \code{\link{set_labels}}, such as 
#'   \code{action}
#'   
#' @export
#' 
set_xlabs <- function(plots, xlab, indices = seq_along(plots), ...)
{
  set_labels(plots, x = xlab, indices = indices)
}

# set_labels -------------------------------------------------------------------

#' Set the Labels in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param \dots name-value pairs as given to \code{\link[ggplot2]{labs}}.
#'   Possible names are e.g. "x", "title", "subtitle", "caption". The values are
#'   vectors of character that are recycled to the length of \code{indices}.
#'   They are used as labels given to each plot or to the plots selected by
#'   their \code{indices}.
#' @param indices indices of the plots to which the label is to be given. By
#'   default the label is given to all plots
#' @param action one of \code{"replace"} (replace the existing label), 
#'   \code{"append"} (append to the existing label), \code{"prepend"} (prepend
#'   to the existing label).
#' @param sep separator to be used when \code{append} is one of 
#' \code{"append", "prepend"}.
#' 
#' @export
#' @examples
#' p <- example_plot_2()
#' 
#' plots <- list(p, p, p, p)
#' 
#' plots_1 <- set_labels(
#'   plots, title = c("Title A", "Title B", "Title C", "Title D"),
#'   subtitle = "same subtitle", x = c("x label one", "x label two")
#' )
#' 
#' plots_2 <- set_labels(
#'   plots, title = c("(A)", "(B)", "(C)", "(D)"),
#'   subtitle = "(always the same)", x = c("(one)", "(two)"),
#'   action = "append"
#' )
#' 
#' do.call(gridExtra::grid.arrange, plots_1)
#' do.call(gridExtra::grid.arrange, plots_2)
#' 
set_labels <- function(
  plots, ..., indices = seq_along(plots), 
  action = c("replace", "append", "prepend")[1], sep = " "
)
{
  expected <- c("replace", "append", "prepend")
  
  if (! action %in% expected) {
    
    stop("action must be one of ", kwb.utils::stringList(expected))
  }
  
  args <- list(...)

  if (length(args)) {

    # Recycle all given label vectors to the length of indices
    labels <- lapply(args, kwb.utils::recycle, length(indices))

    plots[indices] <- lapply(seq_along(indices), function(i) {
      
      # Select the plot
      p <- plots[[indices[i]]]
      
      # Get the new labels for this plot
      new_labels <- lapply(labels, "[", i)
      
      # Extend the existing plot labels, if requested
      if (action != "replace") {
        
        new_labels <- lapply(names(new_labels), function(name) {
          
          old_label <- ggplot2::labs(p)$labels[[name]]
          
          if (action == "append") {
            paste(old_label, new_labels[[name]], sep = sep)
          } else {
            paste(new_labels[[name]], old_label, sep = sep)
          }
        })
        
        # Reset the element names
        names(new_labels) <- names(labels)
      }
      
      # Set the new label
      p + do.call(ggplot2::labs, new_labels)
    })
  }

  plots
}
