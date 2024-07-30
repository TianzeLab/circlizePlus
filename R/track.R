#' S4 class ccTrack
#'
#' @slot func character.
#' @slot params list.
#' @slot trackGeoms list.
#' @slot cells list.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccTrack",
         slots = c(func="character", params = "list", trackGeoms = "list",cells = "list"))


#' @export
setClass("ccGenomicTrack", contains = c("ccTrack"))


#' Object generator for S4 class ccTrack
#'
#' Object [ccTrack-class] will call the function [circos.trackPlotRegion] while drawing.
#'
#' @inheritParams circlize::circos.trackPlotRegion
#'
#' @return Object [ccTrack-class]
#' @export
#'
#' @examples
#' NULL
ccTrack = function(sectors = NULL,
                   x = NULL,
                   ylim = NULL,
                   force.ylim = NULL,
                   track.index = NULL,
                   track.height = NULL,
                   track.margin = NULL,
                   cell.padding = NULL,
                   bg.col = NULL,
                   bg.border = NULL,
                   bg.lty = NULL,
                   bg.lwd = NULL,
                   panel.fun = NULL,
                   factors = NULL) {
  new(
    "ccTrack",
    func = 'circos.track',
    params = list(x = x, y = y),
    trackGeoms = list(),
    cells = list()
  )
}

#' @export
ccTrackHist = function(...) {
  new("ccTrack",
      func = 'circos.trackHist',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicTrack = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicTrack',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicIdeogram = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicIdeogram',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicHeatmap = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicHeatmap',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicLabels = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicLabels',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}


#' @export
ccGenomicRainfall = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicRainfall',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicDensity = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicDensity',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}
