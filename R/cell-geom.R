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

