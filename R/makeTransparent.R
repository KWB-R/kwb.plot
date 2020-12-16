# makeTransparent --------------------------------------------------------------
makeTransparent <- function(colour_strings, alpha = 0.5)
{
  do.call(mapply, args = c(
    list(FUN = grDevices::rgb, MoreArgs = list(alpha = alpha)), 
    lapply(
      kwb.utils:::extractSubstring(
        pattern = "#(..)(..)(..)", 
        x = colour_strings, 
        index = c(red = 1, green = 2, blue = 3)
      ),
      function(x) strtoi(paste0("0x", x)) / 255
    )
  ))
}
