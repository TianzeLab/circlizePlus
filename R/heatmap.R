#' S4 class ccHeatmap
#'
#' ccHeatmap is a special class. It can be used not only as a single track but also as the result of adding a heatmap track to a ccPlot
#'
#' @slot func character. Normally it is "circos.heatmap".
#' @slot params list. A **named** list that stores the parameters of the function [circlize::circos.heatmap] called by the backend.
#' @slot trackGeoms list. A list where [ccTrackGeom-class] are stored.
#' @slot cells list. A list where [ccCell-class] are stored.
#' @slot tracks list. A list where [ccTrack-class] or [ccGenomicTrack-class] or [ccHeatmap-class] are stored.
#' @slot links list. A list where [ccLink-class] or [ccGenomicLink-class] or [ccHeatmapLink-class] are stored.
#' @slot pars list. A list where [ccPar-class] are stored.
#' @slot clear logical. Whether to call [circlize::circos.clear] before drawing.
#' @export
#' @include track.R
setClass(
  "ccHeatmap",
  slots = c(
    tracks = "list",
    links = "list",
    pars = "list",
    clear = "logical"
  ),
  contains = c("ccTrack")
)

#' Object generator for S4 class ccHeatmap
#'
#' Object [ccHeatmap-class] will call the function [circlize::circos.heatmap] while drawing.
#'
#' @inheritParams circlize::circos.heatmap
#' @param clear Whether to call [circlize::circos.clear] before drawing.
#' @return Object [ccHeatmap-class]
#' @importFrom graphics par
#' @importFrom methods new
#' @importFrom stats reorder
#' @export
#' @examples
#' \donttest{
#' library(circlizePlus)
#' set.seed(123)
#' mat1 <- rbind(
#'   cbind(
#'     matrix(rnorm(50 * 5, mean = 1), nr = 50),
#'     matrix(rnorm(50 * 5, mean = -1), nr = 50)
#'   ),
#'   cbind(
#'     matrix(rnorm(50 * 5, mean = -1), nr = 50),
#'     matrix(rnorm(50 * 5, mean = 1), nr = 50)
#'   )
#' )
#' rownames(mat1) <- paste0("R", 1:100)
#' colnames(mat1) <- paste0("C", 1:10)
#' mat1 <- mat1[sample(100, 100), ] # randomly permute rows
#' split <- sample(letters[1:5], 100, replace = TRUE)
#' split <- factor(split, levels = letters[1:5])
#' col_fun1 <- colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#' ccHeatmap(mat = mat1, split = split, col = col_fun1)
#' }
ccHeatmap <- function(mat, split = NULL, col, na.col = "grey",
                      cell.border = NA, cell.lty = 1, cell.lwd = 1,
                      bg.border = NA, bg.lty = par("lty"), bg.lwd = par("lwd"),
                      ignore.white = is.na(cell.border),
                      cluster = TRUE, clustering.method = "complete", distance.method = "euclidean",
                      dend.callback = function(dend, m, si) reorder(dend, rowMeans(m)),
                      dend.side = c("none", "outside", "inside"), dend.track.height = 0.1,
                      rownames.side = c("none", "outside", "inside"), rownames.cex = 0.5,
                      rownames.font = par("font"), rownames.col = "black",
                      show.sector.labels = FALSE, cell_width = rep(1, nrow(mat)), clear = TRUE, ...) {
  name_args <- list(
    mat = mat, split = split, col = col, na.col = na.col,
    cell.border = cell.border, cell.lty = cell.lty, cell.lwd = cell.lwd,
    bg.border = bg.border, bg.lty = bg.lty, bg.lwd = bg.lwd,
    ignore.white = ignore.white,
    cluster = cluster, clustering.method = clustering.method, distance.method = distance.method,
    dend.callback = dend.callback,
    dend.side = dend.side, dend.track.height = dend.track.height,
    rownames.side = rownames.side, rownames.cex = rownames.cex,
    rownames.font = rownames.font, rownames.col = rownames.col,
    show.sector.labels = show.sector.labels, cell_width = cell_width
  )
  new("ccHeatmap",
    func = "circos.heatmap",
    params = c(name_args, list(...)),
    tracks = list(),
    links = list(),
    pars = list(),
    trackGeoms = list(),
    cells = list(),
    clear = clear
  )
}

#' Draw the figures described by ccHeatmap
#'
#' @param object Object of [ccHeatmap-class]
#' @return No return information
#' @importMethodsFrom methods show
#' @exportMethod show
#' @examples
#' \donttest{
#' library(circlizePlus)
#' set.seed(123)
#' mat1 <- rbind(
#'   cbind(
#'     matrix(rnorm(50 * 5, mean = 1), nr = 50),
#'     matrix(rnorm(50 * 5, mean = -1), nr = 50)
#'   ),
#'   cbind(
#'     matrix(rnorm(50 * 5, mean = -1), nr = 50),
#'     matrix(rnorm(50 * 5, mean = 1), nr = 50)
#'   )
#' )
#' rownames(mat1) <- paste0("R", 1:100)
#' colnames(mat1) <- paste0("C", 1:10)
#' mat1 <- mat1[sample(100, 100), ] # randomly permute rows
#' split <- sample(letters[1:5], 100, replace = TRUE)
#' split <- factor(split, levels = letters[1:5])
#' col_fun1 <- colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#' show(ccHeatmap(mat = mat1, split = split, col = col_fun1))
#' }
setMethod("show", signature="ccHeatmap", definition= function(object) {
  if (object@clear)
    circos.clear()

  if (length(object@pars) > 0) {
    do.call(circos.par, object@pars)
  }
  do.call(circos.heatmap, object@params)

  if (length(object@tracks) > 0) {
    for (i in 1:length(object@tracks)) {
      do.call(object@tracks[[i]]@func, object@tracks[[i]]@params)
      if (length(object@tracks[[i]]@trackGeoms) > 0) {
        for (j in 1:length(object@tracks[[i]]@trackGeoms)) {
          do.call(
            object@tracks[[i]]@trackGeoms[[j]]@func,
            object@tracks[[i]]@trackGeoms[[j]]@params
          )
        }
      }
      if (length(object@tracks[[i]]@cells) > 0) {
        for (j in object@tracks[[i]]@cells) {
          for (k in 1:length(j@geoms)) {
            j@geoms[[k]]@params["sector.index"] <- j@sector.index
            do.call(j@geoms[[k]]@func, j@geoms[[k]]@params)
          }
        }
      }
    }
  }

  if (length(object@links) > 0) {
    for (l in object@links) {
      do.call(l@func, l@params)
    }
  }
  return(invisible(NULL))
})
