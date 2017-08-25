#' Create progress bar for \code{foreach} function.
#' @param max  Numeric value (finite) for the extreme of the progress bar.
#' @param style The style of the bar. See Details.
#' @return A list which could pass to \code{\link[foreach]{foreach}}
#' through \code{.options.snow} argument.
#' @details \code{style = 1} and \code{style = 2} just shows a line of char.
#' They differ in that \code{style = 2} redraws the line each time,
#' which is useful if other code might be writing to the R console.
#' \code{style = 3} marks the end of the range by | and gives a percentage
#' to the right of the bar.
#' @seealso \code{\link[utils]{txtProgressBar}} \code{\link[utils]{setTxtProgressBar}}
set_foreach_pb <- function(max, style = 3){
  pb <- utils::txtProgressBar(max = max, style = style)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  return(opts)
}
