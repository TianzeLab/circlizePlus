#' @export
setClass(
  "ccPlot",
  slots = c(
    initFunc = "character",
    initFuncParams = "list"
  )
)

#' @export
ccPlot = function(initFunc='initialize', ...){
  new("ccPlot", initFunc=initFunc, initFuncParams=list(...))
}

#' @export
#' @importFrom circlize circos.genomicInitialize
#' @importFrom circlize circos.heatmap.initialize
#' @importFrom circlize circos.initialize
#' @importFrom circlize circos.initializeWithIdeogram
show.ccPlot = function(object){
  if (object@initFunc == 'initialize') {
    do.call(circos.initialize, object@initFuncParams)
  } else if (object@initFunc == 'initializeWithIdeogram') {
    do.call(circos.initializeWithIdeogram, object@initFuncParams)
  } else if  (object@initFunc == 'heatmap.initialize') {
    do.call(circos.heatmap.initialize, object@initFuncParams)
  } else if  (object@initFunc == 'genomicInitialize') {
    do.call(circos.genomicInitialize, object@initFuncParams)
  }

}

#' @export
setMethod('show', 'ccPlot', show.ccPlot)
