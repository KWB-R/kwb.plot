# barplotFlows -----------------------------------------------------------------
barplotFlows <- function(
  flows, cols = seq_len(nrow(flows)), bar_width = 1, xspace = 4 * bar_width, 
  xlim = NULL, ylim = NULL, xstart = 0, add = FALSE, arrow_length = 1, ...
)
{
  stopifnot(is.matrix(flows), nrow(flows) == ncol(flows))
  stopifnot(is.atomic(cols), length(cols) == nrow(flows))
  
  heights <- colSums(flows)
  
  # Clear diagonal elements
  flows[which(diag(nrow(flows)) == 1)] <- NA
  
  n <- length(heights)
  ymax <- max(heights)

  if (! add) {
    
    initPlot(
      xlim = kwb.utils::defaultIfNULL(xlim, c(-xspace, (n + 1) * xspace)), 
      ylim = kwb.utils::defaultIfNULL(ylim, c(0, ymax))
    )
  }
  
  indices <- seq_along(heights)
  xpos <- xstart + (indices - 1L) * xspace
  
  for (i in indices) {
    #i <- 1
    x <- xpos[i]
    y <- heights[i]
    drawBar(y, cols[i], x = x, w = bar_width)
    plotInOut(
      x = x, 
      y = y, 
      out = flows[, i], 
      inp = flows[i, ], 
      cols, 
      dx = bar_width / 2,
      length = arrow_length,
      ...
    )
  }
  
  invisible(xpos)
}

# initPlot ---------------------------------------------------------------------
initPlot <- function(xlim = c(-r, r), ylim = c(-r, r), r = 10, asp = NA)
{
  graphics::plot(
    NA, NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = ""
    , asp = asp
  )
  
  graphics::abline(h = 0)
}

# drawBar ----------------------------------------------------------------------
drawBar <- function(h, col, w = 4, x = 0)
{
  graphics::rect(
    xleft = x - w/2, ybottom = 0, xright = x + w/2, ytop = h, col = col
  )
}

# plotInOut --------------------------------------------------------------------
plotInOut <- function(
  y, 
  out, 
  inp, 
  cols = seq_along(out), 
  x = 0, 
  dx = 2, 
  arrow_type = "straight",
  ...
)
{
  arrow_fun = kwb.utils::createAccessor(arrowFunctions(arrow_type, dx = dx))
  
  i <- which(is.na(out))
  stopifnot(length(i) == 1L)
  n <- length(inp) - 1L
  
  r1 <- y - cumsum(out[-i])
  r2 <- c(y, r1[-n])
  
  mapply(arrow_fun("output"), r1, r2, cols[-i], x, MoreArgs = list(...))
  
  y <- r1[n]
  
  r2 <- y + cumsum(rev(inp[-i]))
  r1 <- c(y, r2[-n])
  
  mapply(arrow_fun("input"), r1, r2, rev(cols[-i]), x, MoreArgs = list(...))
}

# arrowFunctions ---------------------------------------------------------------
arrowFunctions <- function(key, dx = 2)
{
  kwb.utils::selectElements(elements = key, list(
    straight = list(
      output = function(r1, r2, col, x, length, ...) blockArrow(
        direction = "right", 
        x = x + dx, 
        y = r1,
        length = length,
        width = r2 - r1, 
        col = col, 
        ...
      ), 
      input = function(r1, r2, col, x, length, ...) blockArrow(
        direction = "right", 
        x = x - dx - length, 
        y = r1,
        length = length,
        width = r2 - r1, 
        col = col, 
        ...
      )
    ),
    bent = list(
      out = function(r1, r2, col, x) drawCircle(
        r1, r2, col = col, x = x + dx
      ),
      inp = function(r1, r2, col, x) drawCircle(
        r1, r2, 0, -90, col = col, x = x - dx
      )
    ) 
  ))
}

# drawCircle ------------------------------------------------------------------
drawCircle <- function(
  r1 = 1, r2 = r1 + 1, to = 90, from = 0, col = "skyblue", 
  pch = NA, tip_offset = 10, x = 0
)
{
  #r1 = 1; r2 = r1 + 1; to = 90; from = 0; pch = NA
  
  xy_1 <- degreeRangeToXy(from, to)
  
  xy <- rbind(
    r1 * xy_1, 
    (r1 + 0.5 * (r2 - r1)) * phiToXy(degreeToPhi(
      to + ifelse(from <= to, tip_offset, - tip_offset)
    )),
    r2 * xy_1[rev(seq_len(nrow(xy_1))), ]
  )
  
  xy[, 1] <- xy[, 1] + x
  
  if (! is.na(pch)) {
    graphics::points(xy, pch = pch)
  } 
  
  graphics::polygon(xy, col = col)
}

# degreeRangeToXy --------------------------------------------------------------
degreeRangeToXy <- function(
  from = 0, to = 90, by = ifelse(from <= to, 1, -1)
)
{
  phiToXy(degreeRangeToPhi(from, to, by))
}

# degreeRangeToPhi -------------------------------------------------------------
degreeRangeToPhi <- function(from = 0, to = 90, by = 10)
{
  degreeToPhi(seq(from, to, by = by))
}

# degreeToPhi ------------------------------------------------------------------
degreeToPhi <- function(x) (90 - x) / 180 * pi

# phiToXy ----------------------------------------------------------------------
phiToXy <- function(phi)
{
  cbind(x = cos(phi), y = sin(phi))
}