#' @export
setClass(
  "ccPlot",
  slots = c(
    initMode = "character",
    initParams = "list",
    tracks = "list",
    links = "list",
    pars = "list",
    clear = "logical"
  )
)

#' @export
ccPlot = function(initMode = 'initialize',
                  clear = TRUE,
                  ...) {
  new(
    "ccPlot",
    initMode = initMode,
    initParams = list(...),
    links = list(),
    tracks = list(),
    pars = list(),
    clear = clear
  )
}

#' @export
#' @include utils.R
show.ccPlot = function(object) {
  if (object@clear)
    circos.clear()

  if (length(object@pars) > 0) {
    do.call(circos.par, removeNullParam(object@pars))
  }

  if (object@initMode == 'initialize') {
    do.call(circos.initialize, removeNullParam(object@initParams))
  } else if (object@initMode == 'initializeWithIdeogram') {
    do.call(circos.initializeWithIdeogram,
            removeNullParam(object@initParams))
  } else if (object@initMode == 'heatmap.initialize') {
    do.call(circos.heatmap.initialize,
            removeNullParam(object@initParams))
  } else if (object@initMode == 'genomicInitialize') {
    do.call(circos.genomicInitialize,
            removeNullParam(object@initParams))
  }

  if (length(object@tracks) > 0) {
    for (i in 1:length(object@tracks)) {
      do.call(object@tracks[[i]]@func,
              removeNullParam(object@tracks[[i]]@params))
      if (length(object@tracks[[i]]@trackGeoms) > 0) {
        for (j in 1:length(object@tracks[[i]]@trackGeoms))
          do.call(
            object@tracks[[i]]@trackGeoms[[j]]@func,
            removeNullParam(object@tracks[[i]]@trackGeoms[[j]]@params)
          )
      }
      if (length(object@tracks[[i]]@cells) > 0) {
        for (j in object@tracks[[i]]@cells) {
          for (k in 1:length(j@geoms)) {
            j@geoms[[k]]@params['sector.index'] = j@sector.index
            do.call(j@geoms[[k]]@func,
                    removeNullParam(j@geoms[[k]]@params))
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
setMethod('show', 'ccPlot', show.ccPlot)
