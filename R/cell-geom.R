#' @export
setClass("ccCell",
         slots = c(sector.index = "character", geoms = "list"))

#' @export
setClass("ccCells", contains = c("list"))

#' @export
setClass("ccCellGeom",
         slots = c(func = "character", params = "list"))

#' @export
setClass("ccGenomicCellGeom", contains = c("ccCellGeom"))

#' @export
ccCell = function(sector.index = NULL) {
  new("ccCell",sector.index = sector.index, geoms = list())
}

#' @export
ccCells = function(sector.indexes = list()) {
  if (length(sector.indexes) == 0) {
    stop("'sector.indexes' can't be an empty list.")
  }
  cells = new("ccCells")
  for (si in sector.indexes) {
    cells[[si]] = ccCell(sector.index = si)
  }
  cells
}

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
ccBarplot = function(...) {
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

#' @export
ccDendrogram = function(...){
  new("ccCellGeom",func = 'circos.dendrogram',params = list(...))
}

#' @export
ccGenomicPoints = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicPoints',params = list(...))
}

#' @export
ccGenomicLines = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicLines',params = list(...))
}

#' @export
ccGenomicRect = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicRect',params = list(...))
}

#' @export
ccGenomicText = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicText',params = list(...))
}

#' @export
ccGenomicAxis = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicAxis',params = list(...))
}
