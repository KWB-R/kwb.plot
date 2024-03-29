# demo.adj ---------------------------------------------------------------------

#' Demo of Graphical Parameter "adj"
#' 
#' Demonstration of the effect of the graphical parameter "adj" (horizontal and
#'   vertical adjustment)
#' 
#' @param text text to be plotted
#' @param srt The string rotation in degrees (see \code{par})
#' @param cex character expansion factor
#' @param \dots further arguments passed to \code{text}
#' @param to.pdf if \code{TRUE} the output goes into a PDF file
#' 
#' @export
#' 
#' @examples 
#' kwb.plot::demo.adj()
#' kwb.plot::demo.adj("Exampletext", srt = 30)
#' #kwb.plot::demo.adj(srt = c(0, 45, 90), to.pdf = TRUE)  
#' 
demo.adj <- function(
  text = "Text", srt = c(0, 90), cex = 1, ..., to.pdf = FALSE
)
{
  adjs <- list(
    c(0.0, 0.0),
    c(0.0, 0.5), 
    c(0.0, 1.0),
    c(0.5, 0.0),
    c(0.5, 0.5),
    c(0.5, 1.0),
    c(1.0, 0.0),
    c(1.0, 0.5), 
    c(1.0, 1.0)
  )
  
  names.adj <- names(adjs)
  
  pdfFile <- kwb.utils::preparePdfIf(to.pdf)
  
  graphics::par(
    mfrow = kwb.plot::bestRowColumnSetting(length(adjs)), mar = c(2, 2, 2, 2)
  )
  
  for (rotation in srt) {
    
    for (i in seq_along(adjs)) {
      
      graphics::plot(
        0, 0, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "", ylab = "",  
        axes = FALSE, main = sprintf(
          "srt = %s\nadj = c(%s, %s)", rotation, adjs[[i]][1], adjs[[i]][2]
        )
      )
      
      graphics::abline(h = 0, v = 0, col = "grey")
      
      graphics::text(
        x = 0, 0, labels = text, adj = adjs[[i]], cex = cex, srt = rotation, ...
      )
    }    
  }
  
  kwb.utils::finishAndShowPdfIf(to.pdf, pdfFile)
}

# preview_themes ---------------------------------------------------------------

#' Preview the Effects of Themes on a Plot
#' 
#' Preview the effects of ggplot2-themes on a given plot
#' 
#' @param x ggplot object
#' @param themes list of ggplot-themes as returned by
#'   \code{\link[ggplot2]{theme}}
#' @param to_pdf if \code{TRUE} (default) the output goes to a PDF file
#' @param landscape if \code{TRUE} (default) the output to a PDF file will be in
#'   DIN A4 landscape format, else in DIN A4 portrait format
#' @param \dots arguments passed to \code{\link[gridExtra:arrangeGrob]{grid.arrange}}
#' 
#' @export
#' 
preview_themes <- function(
  x = example_plot(), themes = ggplot_themes(), to_pdf = TRUE, 
  landscape = TRUE, ...
)
{
  plots <- lapply(themes, function(theme) x + theme)
  
  plots <- set_titles(plots, names(themes))
  
  if (to_pdf) {
    arrange_in_pdf(plots, landscape = landscape, ...)
  } else {
    do.call(gridExtra::grid.arrange, c(plots, list(...)))
  }
}

# ggplot_themes ----------------------------------------------------------------

#' List of available ggplot2-Themes
#' 
#' Return a list of predefined themes from the ggplot2-package
#' 
ggplot_themes <- function()
{
  envir <- asNamespace("ggplot2")
  
  # Find functions returning complete themes
  #theme_names <- grep("^theme_", ls(envir = envir), value = TRUE)
  #cat(kwb.utils::stringList(theme_names, qchar = '"'))
  
  theme_names <- c(
    "theme_bw",
    "theme_classic",
    "theme_dark",
    "theme_gray",
    "theme_grey", 
    "theme_light",
    "theme_linedraw",
    "theme_minimal",
    "theme_void"
  )

  lapply(stats::setNames(nm = theme_names), function(name) {
    do.call(get(name, envir = envir), list())
  })
}

# apply_elements_text ----------------------------------------------------------

#' Apply themes with given text properties to a plot
#' 
#' @param x ggplot object
#' @param elements objects as created by function 
#' \code{\link[ggplot2:element]{element_text}}
apply_elements_text <- function(x, elements)
{
  plots <- lapply(elements, function(element) {
    x + ggplot2::theme(text = element)
  })
  
  kwb.plot::set_titles(plots, names(elements))
}

# demo_theme_properties --------------------------------------------------------

#' Plots Demonstrating Theme Properties
#' 
#' @param x ggplot object on which to demonstrate the theme properties
#' @param to_pdf if \code{TRUE} (default) the plots are written to a pdf file
#' 
#' @export
#' 
demo_theme_properties <- function(x = example_plot_2(), to_pdf = TRUE)
{
  pdf_file <- file.path(tempdir(), "demo_theme_properties.pdf")
  kwb.utils::preparePdfIf(to_pdf, pdf_file)
  
  print(preview_themes(x, demo_themes_text(), to_pdf = FALSE))
  print(preview_themes(x, demo_themes_rect(), to_pdf = FALSE))
  print(preview_themes(x, demo_themes_line(), to_pdf = FALSE))
  
  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)
}

# demo_themes_text -------------------------------------------------------------

#' demo themes text
#' 
demo_themes_text <- function()
{
  style_text <- ggplot2::element_text(colour = "red", face = "bold")
  to_element_themes(element_types()$text, style_text)
}

# demo_themes_rect -------------------------------------------------------------

#' demo themes rect
#' 
demo_themes_rect <- function()
{
  style_rect <- ggplot2::element_rect(
    colour = "red", size = 1, fill = "lightpink"
  )
  to_element_themes(element_types()$rect, style_rect)
}

# demo_themes_line -------------------------------------------------------------

#' demo themes line
#' 
demo_themes_line <- function()
{
  style_line <- ggplot2::element_line(
    colour = "red", size = 1, linetype = "solid"
  )
  
  to_element_themes(element_types()$line, style_line)
}

# element_types ----------------------------------------------------------------

#' element types
#' 
#' @param secondary if \code{TRUE} all parameters are returned otherwise those
#' parameters related to the x axis on top, the y axis on the right or the 
#' distict x an y parameters related to the "stip.text" parameter are omitted
element_types <- function(secondary = FALSE)
{
  list(
    
    line = c(
      "line", "axis.ticks", "axis.ticks.x", "axis.ticks.y", "axis.line", 
      "axis.line.x", "axis.line.y", "panel.grid", "panel.grid.major", 
      "panel.grid.minor", "panel.grid.major.x", "panel.grid.major.y", 
      "panel.grid.minor.x", "panel.grid.minor.y"
    ),
    
    rect = c(
      "rect", "legend.background", "legend.key", "legend.box.background",
      "panel.background", "panel.border", "plot.background", "strip.background"
    ),
    
    text = c(
      "text", "title", "axis.title", "axis.title.x", 
      if (secondary) "axis.title.x.top" else NULL, 
      "axis.title.y", 
      if (secondary) "axis.title.y.right" else NULL, 
      "axis.text", "axis.text.x", 
      if (secondary) "axis.text.x.top" else NULL, 
      "axis.text.y", 
      if (secondary) "axis.text.y.right" else NULL, 
      "legend.text", "legend.title", "plot.title", "plot.subtitle", 
      "plot.caption", "strip.text",
      if (secondary) "strip.text.x" else NULL,
      if (secondary) "strip.text.y" else NULL
    ),
    
    unit = c(
      "axis.ticks.length", "legend.spacing", "legend.spacing.x", 
      "legend.spacing.y", "legend.key.size", "legend.key.height", 
      "legend.key.width", "legend.box.spacing", "panel.spacing", 
      "panel.spacing.x", "panel.spacing.y", "plot.margin", 
      "strip.switch.pad.grid", "strip.switch.pad.wrap"
    ),
    
    margin = c(
      "legend.margin", "legend.box.margin"
    ),
    
    numeric = c("aspect.ratio", "legend.text.align", "legend.title.align"),
    
    categorical = c(
      "legend.position", "legend.direction", "legend.box", "legend.box.just",
      "legend.justification", "strip.placement"
    ),
    
    logical = c(
      "panel.ontop"
    )
  )
}

# to_element_themes ------------------------------------------------------------

#' to element themes
#' 
#' @param names name of the argument given to \code{\link[ggplot2]{theme}}
#' @param element value of the argument given to \code{\link[ggplot2]{theme}}
to_element_themes <- function(names, element)
{
  lapply(kwb.utils::toNamedList(names), function(name) {
    do.call(ggplot2::theme, structure(list(element), names = name))
  })
}
