# set_subtitles ----------------------------------------------------------------

#' Set the Subtitles in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param subtitle subtitle (character) to be given to each plot or to the plots selected
#'   by their \code{indices}
#' @param indices indices of the plots to which the subtitle is to be given. By default
#'   the subtitle is given to all plots
#' 
set_subtitles <- function(plots, subtitle, indices = seq_along(plots))
{
  set_labels(plots, subtitle = subtitle, indices)
}

# set_titles -------------------------------------------------------------------

#' Set the Titles in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param title title (character) to be given to each plot or to the plots selected
#'   by their \code{indices}
#' @param indices indices of the plots to which the title is to be given. By default
#'   the title is given to all plots
#' 
set_titles <- function(plots, title, indices = seq_along(plots))
{
  set_labels(plots, title = title, indices)
}

# set_xlabs --------------------------------------------------------------------

#' Set the x Axis Label in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param xlab x axis label (character) to be given to each plot or to the plots selected
#'   by their \code{indices}
#' @param indices indices of the plots to which the x axis label is to be given. By default
#'   the x axis label is given to all plots
#' 
set_xlabs <- function(plots, xlab, indices = seq_along(plots))
{
  set_labels(plots, x = xlab, indices)
}

# set_labels -------------------------------------------------------------------

#' Set the Labels in a List of ggplot Objects
#' 
#' @param plots list of ggplot objects as returned by \code{ggplot}
#' @param \dots name-value pairs as given to \code{\link[ggplot2]{labs}}. Possible names
#'   are e.g. "x", "title", "subtitle", "caption". The values are vectors
#'   of character that are recycled to the length of \code{indices}. They are
#'   used as labels given to each plot or to the plots selected by their
#'   \code{indices}.
#' @param indices indices of the plots to which the label is to be given. By default the
#'   label is given to all plots
#' 
set_labels <- function(plots, ..., indices = seq_along(plots))
{
  args <- list(...)

  if (length(args)) {

    # Recycle all given label vectors to the length of indices
    labels <- lapply(args, kwb.utils::recycle, length(indices))

    plots[indices] <- lapply(seq_along(indices), function(i) {
      plots[[indices[i]]] + do.call(ggplot2::labs, lapply(labels, "[", i))
    })
  }

  plots
}
