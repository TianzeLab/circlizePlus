#' @export
setClass(
  "ccPlot",
  slots = c(
    initFunc = "character",
    initFuncParams = "list",
    tracks = "list",
    params = "list"
  )
)

#' @export
ccPlot = function(initFunc = 'initialize', ...) {
  new(
    "ccPlot",
    initFunc = initFunc,
    initFuncParams = list(...),
    tracks = list(),
    params = list()
  )
}

#' @export
show.ccPlot = function(object) {
  circos.clear()

  if (length(object@params) > 0) {
    do.call(circos.par, object@params)
  }

  if (object@initFunc == 'initialize') {
    do.call(circos.initialize, object@initFuncParams)
  } else if (object@initFunc == 'initializeWithIdeogram') {
    do.call(circos.initializeWithIdeogram, object@initFuncParams)
  } else if (object@initFunc == 'heatmap.initialize') {
    do.call(circos.heatmap.initialize, object@initFuncParams)
  } else if (object@initFunc == 'genomicInitialize') {
    do.call(circos.genomicInitialize, object@initFuncParams)
  }

  if (length(object@tracks) > 0) {
    for (i in 1:length(object@tracks)) {
      do.call(circos.trackPlotRegion, (object@tracks[[i]])@params)
      if (length(object@tracks[[i]]@trackGeoms) > 0) {
        for (j in 1:length(object@tracks[[i]]@trackGeoms))
          do.call(object@tracks[[i]]@trackGeoms[[j]]@func,
                  object@tracks[[i]]@trackGeoms[[j]]@params)
      }
      if (length(object@tracks[[i]]@cells) > 0) {
        for (j in 1:length(object@tracks[[i]]@cells)){

        }
      }
    }
  }

}

#' @export
setMethod('show', 'ccPlot', show.ccPlot)
