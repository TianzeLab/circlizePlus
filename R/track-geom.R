#' @export
setClass("ccTrackGeom",
         slots = c(func = "character", params = "list"))
#' @export
ccTrackHist = function(...) {
  new("ccTrackGeom",func = 'circos.trackHist', params = list(...))
}

#' @export
ccTrackLines = function(...) {
  new("ccTrackGeom",func = 'circos.trackLines', params = list(...))
}

#' @export
ccTrackPoints = function(...) {
  new("ccTrackGeom",func = 'circos.trackPoints', params = list(...))
}

#' @export
ccTrackText = function(...) {
  new("ccTrackGeom",func = 'circos.trackText', params = list(...))
}
