#' @export
setClass("ccTrackGeom",
         slots = c(func = "character", params = "list"))

#' @export
ccTrackPoints = function(...) {
  new("ccTrackGeom",func = 'circos.trackPoints',params = list(...))
}

