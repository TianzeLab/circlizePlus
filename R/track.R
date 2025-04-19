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


#' Define a generic track
#'
#' Object [ccTrack-class] will call the function [circlize::circos.trackPlotRegion] while drawing.
#'
#' @inheritParams circlize::circos.trackPlotRegion
#'
#' @return Object [ccTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
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
ccTrack = function(sectors = NULL,
                   x = NULL, y = NULL,
                   ylim = NULL,
                   force.ylim = TRUE,
                   track.index = NULL,
                   track.height = circos.par("track.height"),
                   track.margin = circos.par("track.margin"),
                   cell.padding = circos.par("cell.padding"),
                   bg.col = NA,
                   bg.border = "black",
                   bg.lty = par("lty"),
                   bg.lwd = par("lwd"),
                   panel.fun = function(x, y) {NULL},
                   factors = sectors) {name_args=list(
                     sectors = sectors,
                     x = x, y = y,
                     ylim = ylim,
                     force.ylim = force.ylim,
                     track.index = track.index,
                     track.height = track.height,
                     track.margin = track.margin,
                     cell.padding = cell.padding,
                     bg.col = bg.col,
                     bg.border = bg.border,
                     bg.lty = bg.lty,
                     bg.lwd = bg.lwd,
                     panel.fun = panel.fun,
                     factors = factors
                   )
  new(
    "ccTrack",
    func = 'circos.track',
    params = c(name_args),
    trackGeoms = list(),
    cells = list()
  )
}

#' Define a track of histograms
#'
#' Object [ccTrack-class] will call the function [circlize::circos.trackHist] while drawing.
#'
#' @inheritParams circlize::circos.trackHist
#'
#' @return Object [ccTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
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
ccTrackHist = function(sectors,
                       x,
                       track.height = circos.par("track.height"),
                       track.index = NULL,
                       ylim = NULL,
                       force.ylim = TRUE,
                       col = ifelse(draw.density, "black", NA),
                       border = "black",
                       lty = par("lty"),
                       lwd = par("lwd"),
                       bg.col = NA,
                       bg.border = "black",
                       bg.lty = par("lty"),
                       bg.lwd = par("lwd"),
                       breaks = "Sturges",
                       include.lowest = TRUE,
                       right = TRUE,
                       draw.density = FALSE,
                       bin.size = NULL,
                       area = FALSE,
                       factors = sectors) {name_args=list(
                         sectors=sectors,
                         x=x,
                         track.height = track.height,
                         track.index = track.index,
                         ylim = ylim,
                         force.ylim = force.ylim,
                         col = col,
                         border = border,
                         lty = lty,
                         lwd = lwd,
                         bg.col = bg.col,
                         bg.border = bg.border,
                         bg.lty = bg.lty,
                         bg.lwd = bg.lwd,
                         breaks = breaks,
                         include.lowest = include.lowest,
                         right = right,
                         draw.density = draw.density,
                         bin.size = bin.size,
                         area = area,
                         factors = factors
                       )
  new("ccTrack",
      func = 'circos.trackHist',
      params = c(name_args),
      trackGeoms = list(),
      cells = list())
}


#' Define a track for genomic data visualization
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicTrackPlotRegion] while drawing.
#'
#'@inheritParams circlize::circos.genomicTrackPlotRegion
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
ccGenomicTrack = function(data = NULL,
                          ylim = NULL,
                          stack = FALSE,
                          numeric.column = NULL,
                          jitter = 0,
                          panel.fun = function(region, value, ...) {NULL},
                          ...) {name_args=list(
                            data = data,
                            ylim = ylim,
                            stack = stack,
                            numeric.column = numeric.column,
                            jitter = jitter,
                            panel.fun = panel.fun
                          )
  new("ccGenomicTrack",
      func = 'circos.genomicTrack',
      params = c(name_args,list(...)),
      trackGeoms = list(),
      cells = list())
}

#' Define an ideograms track for genomic graph
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicIdeogram] while drawing.
#'
#' @inheritParams circlize::circos.genomicIdeogram
#'
#' @return Object [ccGenomicTrack-class]
#' @importFrom methods new
#' @export
#'
#' @examples
#' \donttest{
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram",chromosome.index = "chr1", plotType = NULL)
#' human_cytoband = read.cytoband(species = "hg19")$df
#' t2=ccGenomicIdeogram(human_cytoband)
#' cc+t2
#' circos.clear()
#' }
ccGenomicIdeogram = function( cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),
                              species = NULL,
                              track.height = mm_h(2),
                              track.margin = circos.par("track.margin")) {name_args=list(
                                cytoband = cytoband,
                                species = species,
                                track.height = track.height,
                                track.margin = track.margin
                              )
  new("ccGenomicTrack",
      func = 'circos.genomicIdeogram',
      params = c(name_args),
      trackGeoms = list(),
      cells = list())
}

#' Define a heatmap track for genomic graph
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicHeatmap] while drawing.
#'
#' @inheritParams circlize::circos.genomicHeatmap
#'
#' @return Object [ccGenomicTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
#' @export
#'
#' @examples
#' \donttest{
#' library(circlizePlus)
#' cc = ccPlot(initMode = "initializeWithIdeogram")
#' bed = generateRandomBed(nr = 100, nc = 4)
#' col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
#' t1 = ccGenomicHeatmap(bed, col = col_fun, side = "inside", border = "white")
#' cc + t1
#' circos.clear()
#' }
ccGenomicHeatmap = function(bed,
                            col,
                            na_col = "grey",
                            numeric.column = NULL,
                            border = NA,
                            border_lwd = par("lwd"),
                            border_lty = par("lty"),
                            connection_height = mm_h(5),
                            line_col = par("col"),
                            line_lwd = par("lwd"),
                            line_lty = par("lty"),
                            heatmap_height = 0.15,
                            side = c("inside", "outside"),
                            track.margin = circos.par("track.margin")) {name_args=list(
                              bed=bed,
                              col=col,
                              na_col = na_col,
                              numeric.column = numeric.column,
                              border = border,
                              border_lwd = border_lwd,
                              border_lty = border_lty,
                              connection_height = connection_height,
                              line_col = line_col,
                              line_lwd = line_lwd,
                              line_lty = line_lty,
                              heatmap_height = heatmap_height,
                              side = side,
                              track.margin = track.margin
                            )
  new("ccGenomicTrack",
      func = 'circos.genomicHeatmap',
      params = c(name_args),
      trackGeoms = list(),
      cells = list())
}

#' Add labels to specific genomic track
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicLabels] while drawing.
#'
#' @inheritParams circlize::circos.genomicLabels
#'
#' @return Object [ccGenomicTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
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
ccGenomicLabels = function(bed,
                           labels = NULL,
                           labels.column = NULL,
                           facing = "clockwise",
                           niceFacing = TRUE,
                           col = par("col"),
                           cex = 0.8,
                           font = par("font"),
                           padding = 0.4,
                           connection_height = mm_h(5),
                           line_col = par("col"),
                           line_lwd = par("lwd"),
                           line_lty = par("lty"),
                           labels_height = NULL,
                           side = c("inside", "outside"),
                           labels.side = side,
                           track.margin = circos.par("track.margin")) {
  name_args=list(
    bed=bed,
    labels = labels,
    labels.column = labels.column,
    facing = facing,
    niceFacing = niceFacing,
    col = col,
    cex = cex,
    font = font,
    padding = padding,
    connection_height = connection_height,
    line_col = line_col,
    line_lwd = line_lwd,
    line_lty = line_lty,
    labels_height = labels_height,
    side = side,
    labels.side = labels.side,
    track.margin = track.margin
  )
  new("ccGenomicTrack",
      func = 'circos.genomicLabels',
      params = c(name_args),
      trackGeoms = list(),
      cells = list())
}


#' Create a rainfall plot
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicRainfall] while drawing.
#'
#' @inheritParams circlize::circos.genomicRainfall
#'
#' @return Object [ccGenomicTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
#' @export
#'
#' @examples
#' \donttest{
#' library(circlizePlus)
#' load(system.file(package = "circlize", "extdata", "DMR.RData"))
#' cc = ccPlot(initMode="initializeWithIdeogram", chromosome.index = paste0("chr", 1:22))
#' bed_list = list(DMR_hyper, DMR_hypo)
#' t1 = ccGenomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))
#' cc + t1
#' circos.clear()
#' }
ccGenomicRainfall = function(data,
                             mode = "min",
                             ylim = NULL,
                             col = "black",
                             pch = par("pch"),
                             cex = par("cex"),
                             normalize_to_width = FALSE,
                             ...) {name_args=list(
                               data=data,
                               mode = mode,
                               ylim = ylim,
                               col = col,
                               pch = pch,
                               cex = cex,
                               normalize_to_width = normalize_to_width
                             )
  new("ccGenomicTrack",
      func = 'circos.genomicRainfall',
      params = c(name_args,list(...)),
      trackGeoms = list(),
      cells = list())
}

#' Create a track of density plot
#'
#' Object [ccGenomicTrack-class] will call the function [circlize::circos.genomicDensity] while drawing.
#'
#' @inheritParams circlize::circos.genomicDensity
#'
#' @return Object [ccGenomicTrack-class]
#' @importFrom graphics par
#' @importFrom methods new
#' @export
#'
#' @examples
#' \donttest{
#' library(circlizePlus)
#' load(system.file(package = "circlize", "extdata", "DMR.RData"))
#' cc = ccPlot(initMode="initializeWithIdeogram", chromosome.index = paste0("chr", 1:22))
#' t2 = ccGenomicDensity(DMR_hyper, col = c("#FF000080"), track.height = 0.1)
#' t3 = ccGenomicDensity(DMR_hypo, col = c("#0000FF80"), track.height = 0.1)
#' cc + t2 + t3
#' circos.clear()
#' }
ccGenomicDensity = function(data,
                            ylim.force = FALSE,
                            window.size = NULL,
                            overlap = TRUE,
                            count_by = c("percent", "number"),
                            col = ifelse(area, "grey", "black"),
                            lwd = par("lwd"),
                            lty = par("lty"),
                            type = "l",
                            area = TRUE,
                            area.baseline = NULL,
                            baseline = 0,
                            border = NA,
                            ...) {name_args=list(
                              data=data,
                              ylim.force = ylim.force,
                              window.size = window.size,
                              overlap = overlap,
                              count_by = count_by,
                              col = col,
                              lwd = lwd,
                              lty = lty,
                              type = type,
                              area = area,
                              area.baseline = area.baseline,
                              baseline = baseline,
                              border = border
                            )
  new("ccGenomicTrack",
      func = 'circos.genomicDensity',
      params = c(name_args,list(...)),
      trackGeoms = list(),
      cells = list())
}



