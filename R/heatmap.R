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

#' @export
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

#' @export
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
