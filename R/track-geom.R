#' S4 class ccTrackGeom
#'
#' Objectified representation of the R package circlize's plotting functions and corresponding parameters at the track level.
#'
#' @slot func character. The name of the plot function in the R package circlize.
#' @slot params list. A **named** list that stores the parameters of the function called by the backend.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccTrackGeom",
         slots = c(func = "character", params = "list"))

#' Add lines on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackLines] while drawing.
#'
#' @inheritDotParams circlize::circos.trackLines
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackLines = function(...) {
  new("ccTrackGeom",func = 'circos.trackLines', params = list(...))
}

#' Add points on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackPoints] while drawing.
#'
#' @inheritDotParams circlize::circos.trackPoints
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackPoints = function(...) {
  new("ccTrackGeom",func = 'circos.trackPoints', params = list(...))
}

#' Add texts on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackText] while drawing.
#'
#' @inheritDotParams circlize::circos.trackText
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackText = function(...) {
  new("ccTrackGeom",func = 'circos.trackText', params = list(...))
}
