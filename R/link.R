#' S4 class ccLink
#'
#' @slot func character. Normally it is "circos.link".
#' @slot params list. A **named** list that stores the parameters of the function [circos.link] called by the backend.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccLink",
         slots = c(func="character", params = "list"))


#' S4 class ccHeatmapLink
#'
#' @slot func character. Normally it is "circos.heatmap.link".
#' @slot params list. A **named** list that stores the parameters of the function [circos.heatmap.link] called by the backend.
#' @export
#'
#' @examples
#' NULL
setClass("ccHeatmapLink", contains = c('ccLink'))

#' S4 class ccGenomicLink
#'
#' @slot func character. Normally it is "circos.genomicLink".
#' @slot params list. A **named** list that stores the parameters of the function [circos.genomicLink] called by the backend.
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicLink", contains = c('ccLink'))

#' Object generator for S4 class ccLink
#'
#' Object [ccLink-class] will call the function [circos.link] while drawing.
#'
#' @inheritDotParams circlize::circos.link
#'
#' @return Object [ccLink-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' set.seed(999)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),x = rnorm(n), y = runif(n))
#' cc = ccPlot(initMode = "initialize", sectors = df$sectors, x = df$x)
#' track1 = ccTrack(df$sectors, y = df$y)
#' col = rep(c("#FF0000", "#00FF00"), 4)
#' tPoint1 = ccTrackPoints(df$sectors, df$x, df$y, col = col, pch = 16, cex = 0.5)
#' link1 = ccLink("a", 0, "b", 0, h = 0.4)
#' link2 = ccLink("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",border = "blue", h = 0.2)
#' link3 = ccLink("e", 0, "g", c(-1,1), col = "green", border = "black", lwd = 2, lty = 2)
#' cc + (track1 + tPoint1) + link1 + link2 + link3
ccLink = function(...) {
  new("ccLink",
      func = 'circos.link',
      params = list(...))
}

#' Object generator for S4 class ccHeatmapLink
#'
#' Object [ccHeatmapLink-class] will call the function [circos.heatmap.link] while drawing.
#'
#' @inheritDotParams circlize::circos.heatmap.link
#'
#' @return Object [ccHeatmapLink-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' set.seed(123)
#' mat = matrix(rnorm(100*10), nrow = 100)
#' rownames(mat) = paste0("R", 1:100)
#' col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#' cc = ccHeatmap(mat, col = col_fun, rownames.side = "outside")
#' link1 = ccHeatmapLink(10, 60)
#' cc + link1
ccHeatmapLink = function(...) {
  new("ccHeatmapLink",
      func = 'circos.heatmap.link',
      params = list(...))
}

#' Object generator for S4 class ccGenomicLink
#'
#' Object [ccGenomicLink-class] will call the function [circos.genomicLink] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicLink
#'
#' @return Object [ccGenomicLink-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' set.seed(123)
#'
#' bed1 = generateRandomBed(nr = 100)
#' bed1 = bed1[sample(nrow(bed1), 20), ]
#' bed2 = generateRandomBed(nr = 100)
#' bed2 = bed2[sample(nrow(bed2), 20), ]
#' par1 = ccPar("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
#' cc = ccPlot(initMode="initializeWithIdeogram")
#'
#' link1 = ccGenomicLink(bed1, bed2, col = sample(1:5, 20, replace = TRUE), border = NA)
#' cc + par1 + link1
ccGenomicLink = function(...) {
  new("ccGenomicLink",
      func = 'circos.genomicLink',
      params = list(...))
}
