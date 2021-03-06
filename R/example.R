# example_plot -----------------------------------------------------------------

#' Simple ggplot Barplot
#' 
#' Simple ggplot example barplot
#' 
#' @export
#' 
example_plot <- function() 
{
  data <- data.frame(x = 1:5, y = 1:5)
  ggplot2::ggplot(data, ggplot2::aes_string("x", "y", fill = "y")) + 
    ggplot2::geom_col()
}

# example_plot_2 ---------------------------------------------------------------

#' Simple ggplot Scatter Plot
#' 
#' @param n number of points
#' 
#' @export
#' 
example_plot_2 <- function(n = 7)
{
  data <- data.frame(
    index = factor(i <- seq_len(n)), 
    value = (y <- round(10 * stats::rnorm(length(i), sd = 0.2), (digit <- 1))), 
    digit_2 = (digit_2 <- substr(sprintf("%0.2f", abs(y)), (p <- 2 + digit), p)),
    group = c("odd", "even")[(as.integer(digit_2) %% 2 == 0) + 1],
    sign = c("pos", "neg")[(y < 0) + 1]
  )
  
  ggplot2::ggplot(data, ggplot2::aes_string("index", "value", col = "sign")) + 
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c("grey40", "grey70")) +
    ggplot2::facet_wrap("group") + 
    ggplot2::labs(title = "title", subtitle = "subtitle", caption = "caption") +
    ggplot2::theme_minimal()
}
