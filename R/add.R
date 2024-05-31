#' @export
#' @include initialize.R
#' @include param.R
setMethod(
  "+",
  signature = c(e1 = "ccPlot", e2 = "ccPar"),
  definition = function(e1, e2) {
    e1@params = c(e1@params, e2@params)
    e1
  }
)

#' @export
#' @include initialize.R
#' @include track.R
setMethod(
  "+",
  signature = c(e1 = "ccPlot", e2 = "ccTrack"),
  definition = function(e1, e2) {
    e1@tracks = c(e1@tracks, e2)
    e1
  }
)

#' @export
#' @include track.R
#' @include trackGeom.R
setMethod(
  "+",
  signature = c(e1 = "ccTrack", e2 = "ccTrackGeom"),
  definition = function(e1, e2) {
    e1@trackGeoms = c(e1@trackGeoms, e2)
    e1
  }
)
