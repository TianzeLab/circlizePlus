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



#' S4 class ccGenomicTrack
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicTrack", contains = c("ccTrack"))


#' Object generator for S4 class ccTrack
#'
#' Object [ccTrack-class] will call the function [circos.trackPlotRegion] while drawing.
#'
#' @inheritDotParams circlize::circos.trackPlotRegion
#'
#' @return Object [ccTrack-class]
#' @export
#'
#' @examples
#' NULL
ccTrack = function(...) {
  new(
    "ccTrack",
    func = 'circos.track',
    params = list(...),
    trackGeoms = list(),
    cells = list()
  )
}

#' Object generator for S4 class ccTrack
#'
#'Object [ccTrack-class] will call the function [circos.trackHist] while drawing.
#'
#'@inheritDotParams circlize::circos.trackHist
#'
#' @return Object [ccTrack-class]
#' @export
#'
#' @examples
#' NULL
ccTrackHist = function(...) {
  new("ccTrack",
      func = 'circos.trackHist',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}


#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicTrackPlotRegion] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicTrackPlotRegion
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicTrack = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicTrack',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicIdeogram] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicIdeogram
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicIdeogram = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicIdeogram',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicHeatmap] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicHeatmap
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicHeatmap = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicHeatmap',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicLabels] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicLabels
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicLabels = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicLabels',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}


#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicRainfall] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicRainfall
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicRainfall = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicRainfall',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#'Object [ccGenomicTrack-class] will call the function [circos.genomicDensity] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicDensity
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicDensity = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicDensity',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}
