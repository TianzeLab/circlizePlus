#' Object generator for S4 class ccPar
#'
#' @slot params params list. A **named** list that stores the parameters of the function [circos.par] called by the backend.
#'
#' @export
#'
#' @examples
#' NULL
setClass(
  "ccPar",
  slots = c(
    params = "list"
  )
)

#' Object generator for S4 class ccPar
#'
#' Object [ccPar-class] will call the function [circos.par] while drawing.
#'
#' @inheritDotParams circlize::circos.par
#'
#' @return Object [ccPar-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' set.seed(999)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),x = rnorm(n), y = runif(n))
#' par1 = ccPar("track.height" = 0.1)
#' cc = ccPlot(initMode = "initialize", sectors = df$sectors, x = df$x)
#' track1 = ccTrack(df$sectors, y = df$y)
#' col = rep(c("#FF0000", "#00FF00"), 4)
#' tPoint1 = ccTrackPoints(df$sectors, df$x, df$y, col = col, pch = 16, cex = 0.5)
#' cc + par1 + (track1 + tPoint1)
ccPar = function(...){
  new("ccPar", params = list(...))
}
