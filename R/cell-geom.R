#' @export
setClass("ccCell",
         slots = c(sector.index = "character", geoms = "list"))

#' @export
ccCell = function(sector.index = get.all.sector.index()[[1]], ...) {
  new("ccCell",sector.index = sector.index, geoms = list(...))
}

#' @export
setClass("ccCellGeom",
         slots = c(func = "character", params = "list"))

#' @export
ccText = function(...) {
  new("ccCellGeom",func = 'circos.text',params = list(...))
}

#' @export
ccPoints = function(...) {
  new("ccCellGeom",func = 'circos.points',params = list(...))
}

#' @export
ccLines = function(...) {
  new("ccCellGeom",func = 'circos.lines',params = list(...))
}

#' @export
ccSegments = function(...) {
  new("ccCellGeom",func = 'circos.segments',params = list(...))
}

#' @export
ccRect = function(...) {
  new("ccCellGeom",func = 'circos.rect',params = list(...))
}

#' @export
ccPolygon = function(...) {
  new("ccCellGeom",func = 'circos.polygon',params = list(...))
}

#' @export
ccXaxis = function(...) {
  new("ccCellGeom",func = 'circos.xaxis',params = list(...))
}

#' @export
ccYaxis = function(...) {
  new("ccCellGeom",func = 'circos.yaxis',params = list(...))
}

#' @export
ccBarplo = function(...) {
  new("ccCellGeom",func = 'circos.barplot',params = list(...))
}

#' @export
ccBoxplot = function(...) {
  new("ccCellGeom",func = 'circos.boxplot',params = list(...))
}

#' @export
ccViolin = function(...) {
  new("ccCellGeom",func = 'circos.violin',params = list(...))
}

#' @export
ccArrow = function(...) {
  new("ccCellGeom",func = 'circos.arrow',params = list(...))
}

#' @export
ccRaster = function(...) {
  new("ccCellGeom",func = 'circos.raster',params = list(...))
}
