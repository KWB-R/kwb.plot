# ggplot_text_lines ------------------------------------------------------------

#' Plot Text Only
#' 
#' @param x vector of character representing the rows of text to be plotted from
#'   top to bottom
#' @param max_rows number of rows to be prepared in the plot area. 
#'   Increase/decrease this number to decrease/increase the line spacing
#' @param size text size. Default: 3
#' @param max_chars maximum number of characters to be used in each row. 
#'   Longer lines that shortened to this number using 
#'   \code{\link[kwb.utils]{shorten}}
#' @param margins_cm vector of four numerics giving the bottom, left, top and
#'   top margins in cm, respectively. Default: \code{c(0, 0, 0, 0)}
#' @importFrom kwb.utils shorten
#' @importFrom ggplot2 ggplot aes_string geom_text xlab ylab
#' @importFrom ggplot2 scale_x_continuous scale_y_reverse theme_void theme
#' @importFrom ggplot2 element_blank unit 
#' @export
#' @examples
#' ggplot_text_lines(paste(c("first", "second", "third"), "row"))
#' 
ggplot_text_lines <- function(
  x, max_rows = max(10, length(x)), size = 3, max_chars = 170,
  margins_cm = c(0, 0, 0, 0)
)
{
  x <- kwb.utils::shorten(x, max_chars, delimiter = "[...]")
  plot_data <- data.frame(x = 0, y = seq_along(x), label = x)
  
  ggplot2::ggplot(plot_data, ggplot2::aes_string("x", "y")) +
    ggplot2::geom_text(
      ggplot2::aes_string(label = "label"), hjust = 0, size = size
    ) +
    ggplot2::xlab("") + 
    ggplot2::ylab("") +
    ggplot2::scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(max_rows, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}
