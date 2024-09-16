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
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter, 
#'                                CELL_META$cell.ylim[2] + mm_y(5), 
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cc=cc+track1
#' cc
setClass("ccTrack",
         slots = c(func="character", params = "list", trackGeoms = "list",cells = "list"))



#' S4 class ccGenomicTrack
#'
#' @export
#'
#' @examples
#' library(cilclizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' bed = generateRandomBed(nr = 300)
#' t1 = ccGenomicTrack(bed, panel.fun = function(region, value, ...) {
#'   circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
#' })
#' cc+_t1
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
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter, 
#'                                CELL_META$cell.ylim[2] + mm_y(5), 
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cc=cc+track1
#' cc
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
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1;bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
#' track2 = ccTrackHist(df$sectors, df$x, bin.size = 0.2, bg.col = bgcol, col = NA)
#' cc=cc+track2
#' cc
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
#' library(cilclizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' bed = generateRandomBed(nr = 300)
#' t1 = ccGenomicTrack(bed, panel.fun = function(region, value, ...) {
#'   circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
#' })
#' cc+_t1
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



