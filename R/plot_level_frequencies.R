# plotLevelFrequencies ---------------------------------------------------------

#' Plot Frequencies of Level Combinations
#' 
#' For combinations of columns \code{c1} and \code{c2} in \code{data},
#' ggplot-objects are generated each of which shows the number (\code{relative =
#' FALSE}) or percentage (\code{relative = TRUE}) of the different possible
#' combinations of values in \code{c1}, and \code{c2}. The combinations of
#' column names are created from the permutation of column names given in
#' \code{vars_1} and \code{vars_2}, respectively.
#' 
#' @param data data frame
#' @param vars_1 first vector of column names
#' @param vars_2 second vector of column names
#' @param relative if \code{TRUE} (default) 
#' 
#' @return list of objects of class "ggplot"
#' 
#' @export
#' 
#' @examples 
#' data <- data.frame(
#'   fruit = c("apple", "cherry", "apple", "banana", "cherry"),
#'   colour = c("green", "red", "red", "yellow", "green"),
#'   size = c("small", "small", "big", "small", "big")
#' )
#' 
#' plotLevelFrequencies(data)
#' 
plotLevelFrequencies <- function(
  data, vars_1 = NULL, vars_2 = NULL, relative = TRUE
)
{
  factors <- names(which(sapply(data, is.factor)))
  
  vars_1 <- kwb.utils::defaultIfNULL(vars_1, factors)
  vars_2 <- kwb.utils::defaultIfNULL(vars_2, factors)
  
  vars <- sort(unique(c(vars_1, vars_2)))
  
  data <- kwb.utils::selectColumns(data, vars, drop = FALSE, do.stop = FALSE)
  
  # All variables must be factors (convert as necessary)
  data <- as.data.frame(lapply(data, kwb.utils::toFactor))
  
  # Get all combinations of variable names
  combis <- kwb.utils::expandGrid(
    var_1 = intersect(vars_1, names(data)), 
    var_2 = intersect(vars_2, names(data))
  )
  
  # Exclude combinations where the first and second variables are the same
  combis <- combis[combis$var_1 != combis$var_2, , drop = FALSE]
  
  # Calculate the frequencies for each variable combination
  plots <- lapply(seq_len(nrow(combis)), function(i) {
    
    frequencies <- table(data[, as.character(combis[i, ])], useNA = "always")
    
    # Convert absolute values to relative values if required
    if (relative) {
      
      # Calculate the "rowwise" percentage
      frequencies <- kwb.utils::rowwisePercentage(frequencies)
    }
    
    # Create the plot title
    main <- sprintf(
      "%s vs %s (%s)", 
      combis[i, 1], combis[i, 2], ifelse(relative, "relative", "absolute")
    )
    
    # Plot the frequencies
    plotFrequencyData(data = as.data.frame.table(frequencies), main = main)
  })
  
  # Name the plots according to the combination of variables
  stats::setNames(plots, sprintf(
    "%s__%s_vs_%s", ifelse(relative, "rel", "abs"), combis[, 1], combis[, 2]
  ))
}

# plotFrequencyData ------------------------------------------------------------
plotFrequencyData <- function(data, dim_names = NULL, main = NULL)
{
  dim_names <- kwb.utils::defaultIfNULL(dim_names, names(data)[1:2])
  
  main <- kwb.utils::defaultIfNULL(main, sprintf(
    "%s vs %s", dim_names[1], dim_names[2]
  ))
  
  ggplot2::ggplot(data, ggplot2::aes_string(
    x = dim_names[1], y = "Freq", fill = dim_names[2])
  ) + 
    ggplot2::geom_bar(stat = "identity") + 
    ggplot2::ggtitle(main) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.ontop = TRUE, 
      panel.background = ggplot2::element_blank(), 
      panel.grid.major.x = ggplot2::element_blank()
    )
}
