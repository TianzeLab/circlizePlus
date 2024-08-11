#' S4 class ccTrackGeom
#'
#' @slot func character.
#' @slot params list.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccTrackGeom",
         slots = c(func = "character", params = "list"))

#' Object generator for S4 class ccTrackGeom
#'
#'Object [ccTrackGeom-class] will call the function [circos.trackLines] while drawing.
#'
#'@inheritDotParams circlize::circos.trackLines
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackLines = function(...) {
  new("ccTrackGeom",func = 'circos.trackLines', params = list(...))
}

#' Object generator for S4 class ccTrackGeom
#'
#'Object [ccTrackGeom-class] will call the function [circos.trackPoints] while drawing.
#'
#'@inheritDotParams circlize::circos.trackPoints
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackPoints = function(...) {
  new("ccTrackGeom",func = 'circos.trackPoints', params = list(...))
}

#' Object generator for S4 class ccTrackGeom
#'
#'Object [ccTrackGeom-class] will call the function [circos.trackText] while drawing.
#'
#'@inheritDotParams circlize::circos.trackText
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackText = function(...) {
  new("ccTrackGeom",func = 'circos.trackText', params = list(...))
}
