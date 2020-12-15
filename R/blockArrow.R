if (FALSE)
{
  plot(-5:5, -5:5)
  blockArrow("left")
  blockArrow("right", 2, 1)
  blockArrow("left", 2, 1, length = 4)
  blockArrow("up")
  blockArrow("down", width = 0.5, length = 3)
}

# blockArrow -------------------------------------------------------------------
blockArrow <- function(
  direction, x = 0, y = 0, length = 2, width = 1, tip = 0.1 * length, 
  col = 1, ...
)
{
  directions <- list(
    horizontal = c("right", "left"), 
    vertical = c("up", "down")
  )
  
  direction <- match.arg(direction, unlist(directions))
  
  is_inverted <- direction %in% sapply(directions, "[", 2)
  is_horizontal <- direction %in% directions$horizontal
  
  signum <- ifelse(is_inverted, -1, 1)
  
  xrel <- c(0, signum * length, signum * (length + tip), signum * length, 0)
  yrel <- c(0, 0, width/2, width, width)
  
  polygon(
    x = x + if (is_horizontal) xrel else yrel,
    y = y + if (is_horizontal) yrel else xrel,
    col = col,
    ...
  )
}
