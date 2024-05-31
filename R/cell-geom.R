#' @export
setClass("ccCell",
         slots = c(geoms = "list"))

#' @export
ccCell = function(...) {
  new("ccCell",geoms = list(...))
}

#' @export
setClass("ccCellGeom",
         slots = c(func = "character", params = "list"))

#' @export
ccText = function(...) {
  new("ccCellGeom",func = 'circos.text',params = list(...))
}

