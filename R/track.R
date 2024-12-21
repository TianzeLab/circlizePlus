#' S4 class ccTrack
#'
#' @slot func character. Normally it is "circos.track" or "circos.trackHist".
#' @slot params list. A **named** list that stores the parameters of the function called by the backend.
#' @slot trackGeoms list. A list where [ccTrackGeom-class] are stored.
#' @slot cells list. A list where [ccCell-class] are stored.
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
#' circos.clear()
setClass("ccTrack",
         slots = c(func="character", params = "list", trackGeoms = "list",cells = "list"))



#' S4 class ccGenomicTrack
#'
#' @export
#'
#' @slot func character. Normally it is "circos.genomicTrack" or "circos.genomicIdeogram" or "circos.genomicHeatmap" or "circos.genomicLabels" or"circos.genomicRainfall" or "circos.genomicDensity".
#' @slot params list. A **named** list that stores the parameters of the function called by the backend.
#' @slot trackGeoms list. A list where [ccTrackGeom-class] are stored.
#' @slot cells list. A list where [ccCell-class] are stored.
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' bed = generateRandomBed(nr = 300)
#' t1 = ccGenomicTrack(bed, panel.fun = function(region, value, ...) {
#'   circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
#' })
#' cc+t1
#' circos.clear()
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
#' library(circlizePlus)
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
#' circos.clear()
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
#' Object [ccTrack-class] will call the function [circos.trackHist] while drawing.
#'
#' @inheritDotParams circlize::circos.trackHist
#'
#' @return Object [ccTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1;bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
#' track2 = ccTrackHist(df$sectors, df$x, bin.size = 0.2, bg.col = bgcol, col = NA)
#' cc=cc+track2
#' cc
#' circos.clear()
ccTrackHist = function(...) {
  new("ccTrack",
      func = 'circos.trackHist',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}


#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicTrackPlotRegion] while drawing.
#'
#'@inheritDotParams circlize::circos.genomicTrackPlotRegion
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' bed = generateRandomBed(nr = 300)
#' t1 = ccGenomicTrack(bed, panel.fun = function(region, value, ...) {
#'   circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
#' })
#' cc+t1
#' circos.clear()
ccGenomicTrack = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicTrack',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicIdeogram] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicIdeogram
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' human_cytoband = read.cytoband(species = "hg19")$df
#' t2=ccGenomicIdeogram(human_cytoband)
#' cc+t2
#' circos.clear()
ccGenomicIdeogram = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicIdeogram',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicHeatmap] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicHeatmap
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram")
#' bed = generateRandomBed(nr = 100, nc = 4)
#' col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
#' t1 = ccGenomicHeatmap(bed, col = col_fun, side = "inside", border = "white")
#' cc + t1
#' circos.clear()
ccGenomicHeatmap = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicHeatmap',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicLabels] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicLabels
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' bed = generateRandomBed(nr = 50, fun = function(k) sample(letters, k, replace = TRUE))
#' bed[1, 4] = "aaaaa"
#' cc = ccPlot(initMode = "initializeWithIdeogram", plotType = NULL)
#' t1 = ccGenomicLabels(bed, labels.column = 4, side = "outside",
#'                      col = as.numeric(factor(bed[[1]])), line_col = as.numeric(factor(bed[[1]])))
#' cc + t1
#' circos.clear()
ccGenomicLabels = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicLabels',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}


#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicRainfall] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicRainfall
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' load(system.file(package = "circlize", "extdata", "DMR.RData"))
#' cc = ccPlot(initMode="initializeWithIdeogram", chromosome.index = paste0("chr", 1:22))
#' bed_list = list(DMR_hyper, DMR_hypo)
#' t1 = ccGenomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))
#' cc + t1
#' circos.clear()
ccGenomicRainfall = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicRainfall',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' Object generator for S4 class ccGenomicTrack
#'
#' Object [ccGenomicTrack-class] will call the function [circos.genomicDensity] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicDensity
#'
#' @return Object [ccGenomicTrack-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' load(system.file(package = "circlize", "extdata", "DMR.RData"))
#' cc = ccPlot(initMode="initializeWithIdeogram", chromosome.index = paste0("chr", 1:22))
#' t2 = ccGenomicDensity(DMR_hyper, col = c("#FF000080"), track.height = 0.1)
#' t3 = ccGenomicDensity(DMR_hypo, col = c("#0000FF80"), track.height = 0.1)
#' cc + t2 + t3
#' circos.clear()
ccGenomicDensity = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicDensity',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}



