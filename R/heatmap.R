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
#' @export
#' @include track.R
setClass(
  "ccHeatmap",
  slots = c(
    tracks = "list",
    links = "list",
    pars = "list"
  ),
  contains = c("ccTrack")
)

#' Object generator for S4 class ccHeatmap
#'
#' Object [ccHeatmap-class] will call the function [circlize::circos.heatmap] while drawing.
#'
#' @inheritDotParams circlize::circos.heatmap
#' @return Object [ccHeatmap-class]
#' @export
#' @examples
#' library(circlizePlus)
#' set.seed(123)
#' mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50),
#'                    matrix(rnorm(50*5, mean = -1), nr = 50)),
#'              cbind(matrix(rnorm(50*5, mean = -1), nr = 50),
#'                    matrix(rnorm(50*5, mean = 1), nr = 50))
#'             )
#' rownames(mat1) = paste0("R", 1:100)
#' colnames(mat1) = paste0("C", 1:10)
#' mat1 = mat1[sample(100, 100), ] # randomly permute rows
#' split = sample(letters[1:5], 100, replace = TRUE)
#' spilt = factor(split, levels = letters[1:5])
#' col_fun1 = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#' ccHeatmap(mat=mat1, split = split, col = col_fun1)
ccHeatmap = function(...){
  new("ccHeatmap",
      func = 'circos.heatmap',
      params = list(...),
      tracks = list(),
      links = list(),
      pars = list(),
      trackGeoms = list(),
      cells = list())
}

#' A generic function of show ccHeatmap
#'
#' @param object Object of [ccHeatmap-class]
#' @usage show(object)
#' @export
#' @examples
#' library(circlizePlus)
#' set.seed(123)
#' mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50),
#'                    matrix(rnorm(50*5, mean = -1), nr = 50)),
#'              cbind(matrix(rnorm(50*5, mean = -1), nr = 50),
#'                    matrix(rnorm(50*5, mean = 1), nr = 50))
#'             )
#' rownames(mat1) = paste0("R", 1:100)
#' colnames(mat1) = paste0("C", 1:10)
#' mat1 = mat1[sample(100, 100), ] # randomly permute rows
#' split = sample(letters[1:5], 100, replace = TRUE)
#' spilt = factor(split, levels = letters[1:5])
#' col_fun1 = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#' show(ccHeatmap(mat=mat1, split = split, col = col_fun1))
show.ccHeatmap = function(object) {
  circos.clear()

  if (length(object@pars) > 0) {
    do.call(circos.par, object@pars)
  }
  do.call(circos.heatmap, object@params)

  if (length(object@tracks) > 0) {
    for (i in 1:length(object@tracks)) {
      do.call(object@tracks[[i]]@func, object@tracks[[i]]@params)
      if (length(object@tracks[[i]]@trackGeoms) > 0) {
        for (j in 1:length(object@tracks[[i]]@trackGeoms))
          do.call(object@tracks[[i]]@trackGeoms[[j]]@func,
                  object@tracks[[i]]@trackGeoms[[j]]@params)
      }
      if (length(object@tracks[[i]]@cells) > 0) {
        for (j in object@tracks[[i]]@cells) {
          for (k in 1:length(j@geoms)) {
            j@geoms[[k]]@params['sector.index'] = j@sector.index
            do.call(j@geoms[[k]]@func, j@geoms[[k]]@params)
          }
        }
      }
    }
  }

  if (length(object@links) > 0) {
    for (l in object@links)
      do.call(l@func, l@params)
  }
}

#' @export
setMethod('show', 'ccHeatmap', show.ccHeatmap)
