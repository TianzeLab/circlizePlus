#' S4 class ccLink
#'
#' @slot func character.
#' @slot params list.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccLink",
         slots = c(func="character", params = "list"))


#' S4 class ccHeatmapLink
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccHeatmapLink", contains = c('ccLink'))

#' S4 class ccGenomicLink
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicLink", contains = c('ccLink'))

#' Object generator for S4 class ccLink
#'
#'Object [ccLink-class] will call the function [circos.link] while drawing.
#'
#'@inheritDotParams circlize::circos.link
#'
#' @return Object [ccLink-class]
#' @export
#'
#' @examples
#' NULL
ccLink = function(...) {
  new("ccLink",
      func = 'circos.link',
      params = list(...))
}

#' Object generator for S4 class ccHeatmapLink
#'
#'Object [ccHeatmapLink-class] will call the function [circos.heatmap.link] while drawing.
#'
#'@inheritDotParams circlize::circos.heatmap.link
#'
#' @return Object [ccHeatmapLink-class]
#' @export
#'
#' @examples
#' NULL
ccHeatmapLink = function(...) {
  new("ccHeatmapLink",
      func = 'circos.heatmap.link',
      params = list(...))
}

#' Object generator for S4 class ccGenomicLink
#'
#'Object [ccGenomicLink-class] will call the function [circos.genomicLink] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicLink
#'
#' @return Object [ccGenomicLink-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicLink = function(...) {
  new("ccGenomicLink",
      func = 'circos.genomicLink',
      params = list(...))
}
