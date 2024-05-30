#' @export
setClass(
  "ccPlot",
  slots = c(
    initFunc = "character",
    initFuncParams = "list",
    params = "list"
  )
)

#' @export
ccPlot = function(initFunc='initialize', ...){
  new("ccPlot", initFunc=initFunc, initFuncParams=list(...), params = list())
}

#' @export
#' @importFrom circlize circos.genomicInitialize
#' @importFrom circlize circos.heatmap.initialize
#' @importFrom circlize circos.initialize
#' @importFrom circlize circos.initializeWithIdeogram
#' @importFrom circlize circos.par
show.ccPlot = function(object){
  if(length(object@params) >0){
    do.call(circos.par, object@params)
  }

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
