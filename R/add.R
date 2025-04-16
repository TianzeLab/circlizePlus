
#' Addition rules in circlizePlus
#'
#' ccPlot(contain n ccTracks)+ccTrack=ccPlot(contain n+1 ccTracks),n>=0 <br>
#' ccPlot(contain n ccLinks)+ccLink=ccPlot(contain n+1 ccLinks),n>=0 <br>
#' ccTrak(contain n ccTrakGeoms)+ccTrackGeom=ccTrack(contain n+1 ccTrackGeoms),n>=0 <br>
#' ccTrack(contain n ccCells)+ccCell=ccTrack(contain n+1 ccCells),n>=0 <br>
#' ccCell(contain n ccCellGeoms)+ccCellGeom=ccCell(contain n+1 ccCellGeoms),n>=0 <br>
#'
#' @param e1 A object defined in circlizePlus
#' @param e2 A object defined in circlizePlus
#'
#' @return A object defined in circlizePlus
#' @examples
#' NULL
#'
#' @name addition-rules
NULL

#' @rdname addition-rules
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

#' @rdname addition-rules
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

#' @rdname addition-rules
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

#' @rdname addition-rules
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

#' @rdname addition-rules
#' @export
#' @include track.R
#' @include cell-geom.R
setMethod(
  "+",
  signature = c(e1 = "ccTrack", e2 = "ccCells"),
  definition = function(e1, e2) {
    for (scell in e2) {
      e1@cells = c(e1@cells, scell)
    }
    e1
  }
)

#' @rdname addition-rules
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

#' @rdname addition-rules
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

#' @rdname addition-rules
#' @export
#' @include cell-geom.R
setMethod(
  "+",
  signature = c(e1 = "ccCells", e2 = "ccCellGeom"),
  definition = function(e1, e2) {
    for (i in 1:length(e1)) {
      e1[[i]]@geoms = c(e1[[i]]@geoms, e2)
    }
    e1
  }
)

#' @rdname addition-rules
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

#' @rdname addition-rules
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

#' @rdname addition-rules
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


