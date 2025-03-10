#' S4 class ccLink
#'
#' @slot func character. Normally it is "circos.link".
#' @slot params list. A **named** list that stores the parameters of the function [circlize::circos.link] called by the backend.
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
#' @slot params list. A **named** list that stores the parameters of the function [circlize::circos.heatmap.link] called by the backend.
#' @export
#'
#' @examples
#' NULL
setClass("ccHeatmapLink", contains = c('ccLink'))

#' S4 class ccGenomicLink
#'
#' @slot func character. Normally it is "circos.genomicLink".
#' @slot params list. A **named** list that stores the parameters of the function [circlize::circos.genomicLink] called by the backend.
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicLink", contains = c('ccLink'))

#' Add a link
#'
#' Object [ccLink-class] will call the function [circlize::circos.link] while drawing.
#'
#' @inheritParams circlize::circos.link
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
ccLink = function( sector.index1,
                   point1,
                   sector.index2,
                   point2,
                   rou = get_most_inside_radius(),
                   rou1 = rou,
                   rou2 = rou,
                   h = NULL,
                   h.ratio = 0.5,
                   w = 1,
                   h2 = h,
                   w2 = w,
                   inverse = FALSE,
                   col = "black",
                   lwd = par("lwd"),
                   lty = par("lty"),
                   border = col,
                   directional = 0,
                   arr.length = ifelse(arr.type == "big.arrow", 0.02, 0.4),
                   arr.width = arr.length/2,
                   arr.type = "triangle",
                   arr.lty = lty,
                   arr.lwd = lwd,
                   arr.col = col,
                   reduce_to_mid_line = FALSE) {
  name_args=list(
    sector.index1=sector.index1,
    point1=point1,
    sector.index2=sector.index2,
    point2=point2,
    rou = rou,
    rou1 = rou1,
    rou2 = rou2,
    h = h,
    h.ratio = h.ratio,
    w = w,
    h2 = h2,
    w2 = w2,
    inverse = inverse,
    col = col,
    lwd = lwd,
    lty = lty,
    border = border,
    directional = directional,
    arr.length = arr.length,
    arr.width = arr.width,
    arr.type = arr.type,
    arr.lty = arr.lty,
    arr.lwd = arr.lwd,
    arr.col = arr.col,
    reduce_to_mid_line = reduce_to_mid_line
  )
  new("ccLink",
      func = 'circos.link',
      params = c(name_args))
}

#' Draw a link between two matrix rows in the circular heatmap
#'
#' Object [ccHeatmapLink-class] will call the function [circlize::circos.heatmap.link] while drawing.
#'
#' @inheritParams circlize::circos.heatmap.link
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
ccHeatmapLink = function(row_from, row_to, ...) {
  name_args=list(row_from=row_from, row_to=row_to)
  new("ccHeatmapLink",
      func = 'circos.heatmap.link',
      params = c(name_args,list(...)))
}

#' Add links between two sets of genomic positions
#'
#' Object [ccGenomicLink-class] will call the function [circlize::circos.genomicLink] while drawing.
#'
#' @inheritParams circlize::circos.genomicLink
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
ccGenomicLink = function(region1,
                         region2,
                         rou = get_most_inside_radius(),
                         rou1 = rou,
                         rou2 = rou,
                         col = "black",
                         lwd = par("lwd"),
                         lty = par("lty"),
                         border = col,
                         ...) {
  name_args=list(region1=region1,
                 region2=region2,
                 rou = rou,
                 rou1 = rou1,
                 rou2 = rou2,
                 col = col,
                 lwd = lwd,
                 lty = lty,
                 border = border
  )
  new("ccGenomicLink",
      func = 'circos.genomicLink',
      params = c(name_args,list(...)))
}
