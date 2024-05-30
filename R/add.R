#' @export
#' @include initialize.R
#' @include param.R
setMethod("+",
          signature = c(e1 = "ccPlot", e2 = "ccPar"),
          definition = function(e1,e2){
            e1@params = c(e1@params, e2@params)
            e1
            })
