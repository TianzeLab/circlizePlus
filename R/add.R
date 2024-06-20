#' @export
#' @include initialize.R
#' @include param.R
setMethod(
  "+",
  signature = c(e1 = "ccPlot", e2 = "ccPar"),
  definition = function(e1, e2) {
    e1@pars = c(e1@pars, e2@params)
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
#' @include initialize.R
#' @include link.R
setMethod(
  "+",
  signature = c(e1 = "ccPlot", e2 = "ccLink"),
  definition = function(e1, e2) {
    e1@links = c(e1@links, e2)
    e1
  }
)

#' @export
#' @include track.R
#' @include track-geom.R
setMethod(
  "+",
  signature = c(e1 = "ccTrack", e2 = "ccTrackGeom"),
  definition = function(e1, e2) {
    e1@trackGeoms = c(e1@trackGeoms, e2)
    e1
  }
)

#' @export
#' @include track.R
#' @include cell-geom.R
setMethod(
  "+",
  signature = c(e1 = "ccTrack", e2 = "ccCell"),
  definition = function(e1, e2) {
    e1@cells = c(e1@cells, e2)
    e1
  }
)

#' @export
#' @include cell-geom.R
setMethod(
  "+",
  signature = c(e1 = "ccCell", e2 = "ccCellGeom"),
  definition = function(e1, e2) {
    e1@geoms = c(e1@geoms, e2)
    e1
  }
)

#' @export
#' @include heatmap.R
#' @include param.R
setMethod(
  "+",
  signature = c(e1 = "ccHeatmap", e2 = "ccPar"),
  definition = function(e1, e2) {
    e1@pars = c(e1@pars, e2@params)
    e1
  }
)

#' @export
#' @include heatmap.R
#' @include track.R
setMethod(
  "+",
  signature = c(e1 = "ccHeatmap", e2 = "ccTrack"),
  definition = function(e1, e2) {
    e1@tracks = c(e1@tracks, e2)
    e1
  }
)

#' @export
#' @include heatmap.R
#' @include link.R
setMethod(
  "+",
  signature = c(e1 = "ccHeatmap", e2 = "ccLink"),
  definition = function(e1, e2) {
    e1@links = c(e1@links, e2)
    e1
  }
)


