
#' S4 class ccCell
#'
#' @slot sector.index character.
#' @slot geoms list.
#'
#' @export
#'
#' @examples
#' library(circlizePlus)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter,
#'                                CELL_META$cell.ylim[2] + mm_y(5),
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cell1 = ccCell(sector.index = 'a')
#' track1 = track1 + cell1
#' cc = cc + track1
#' cc
setClass("ccCell",
         slots = c(sector.index = "character", geoms = "list"))

#' S4 class ccCells
#'
#' @export
#'
#' @examples
#' library(circlizePlus)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter,
#'                                CELL_META$cell.ylim[2] + mm_y(5),
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cell1 = ccCells(sector.index = 'a')
#' track1 = track1 + cell1
#' cc = cc + track1
#' cc
setClass("ccCells", contains = c("list"))

#' S4 class ccCellGeom
#'
#' @slot func character.
#' @slot params list.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccCellGeom",
         slots = c(func = "character", params = "list"))

#' S4 class ccGenomicCellGeom
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicCellGeom", contains = c("ccCellGeom"))


#' @export
ccCell = function(sector.index = NULL) {
  new("ccCell",sector.index = sector.index, geoms = list())
}

#' @export
ccCells = function(sector.indexes = list()) {
  if (length(sector.indexes) == 0) {
    stop("'sector.indexes' can't be an empty list.")
  }
  cells = new("ccCells")
  for (si in sector.indexes) {
    cells[[si]] = ccCell(sector.index = si)
  }
  cells
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.text] while drawing.
#'
#' @inheritParams circlize::circos.text
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter,
#'                                CELL_META$cell.ylim[2] + mm_y(5),
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cell1 = ccCell(sector.index = 'a') + ccText(-1, 0.5, "text")
#' track1 = track1 + cell1
#' cc = cc + track1
#' cc
ccText = function(x,
                  y,
                  labels,
                  direction = NULL,
                  facing = c(
                    "inside",
                    "outside",
                    "reverse.clockwise",
                    "clockwise",
                    "downward",
                    "bending",
                    "bending.inside",
                    "bending.outside"
                  ),
                  niceFacing = FALSE,
                  adj = par("adj"),
                  cex = 1,
                  col = par("col"),
                  font = par("font"),
                  ...) {
  name_args = list(
    x = x,
    y = y,
    labels = labels,
    direction = direction ,
    facing = facing ,
    niceFacing = niceFacing ,
    adj = adj ,
    cex = cex ,
    col = col ,
    font = font
  )
  new("ccCellGeom",
      func = 'circos.text',
      params = c(name_args, list(...)))
}



#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.points] while drawing.
#'
#' @inheritDotParams circlize::circos.points
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track1 = ccTrack(ylim = c(0, 1), panel.fun = function(x, y) {
#'                                   circos.points(runif(10), runif(10))
#'                                   })
#' cells=ccCell(sector.index = 'a')+ccPoints(x=runif(10), y=runif(10),
#'                                          sector.index = "c", pch = 16, col = "red")
#' track1 = track1+cells
#' cc+track1
ccPoints = function(...) {
  new("ccCellGeom",func = 'circos.points',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.lines] while drawing.
#'
#' @inheritDotParams circlize::circos.lines
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' sectors = letters[1:9]
#' par=ccPar(points.overflow.warning = FALSE)
#' cc=ccPlot(sectors = sectors, xlim = c(0, 10))
#' cc=cc+par
#' track=ccTrack(sectors=sectors, ylim = c(0, 10), track.height = 0.5)
#' cells=ccCell(sector.index = 'a')+ccLines(sort(x=runif(10)*10), y=runif(10)*8, sector.index = "a", type = "h", baseline = 5)
#' track=track+cells
#' cc+track
ccLines = function(...) {
  new("ccCellGeom",func = 'circos.lines',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.segments] while drawing.
#'
#' @inheritDotParams circlize::circos.segments
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track=ccTrack(ylim = c(0, 1), track.height = 0.3)
#' cell=ccCell(sector.index = 'a')+ccSegments(x0=0.7,y0=0.1,x1=0.7,y1=0.9)
#' track=track+cell
#' cc+track
ccSegments = function(...) {
  new("ccCellGeom",func = 'circos.segments',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.rect] while drawing.
#'
#' @inheritDotParams circlize::circos.rect
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track=ccTrack(ylim = c(0, 1), track.height = 0.3)
#' cell=ccCell(sector.index = 'a')+ccRect(xleft=0.7,ybottom=0.1,xright=0.7,ytop=0.9)
#' track=track+cell
#' cc+track
ccRect = function(...) {
  new("ccCellGeom",func = 'circos.rect',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.polygon] while drawing.
#'
#' @inheritDotParams circlize::circos.polygon
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track=ccTrack(ylim = c(0, 10))
#' cell=ccCell(sector.index = 'a')+ccPolygon(x=c(2,8),y=c(2,8))
#' track=track+cell
#' cc+track
ccPolygon = function(...) {
  new("ccCellGeom",func = 'circos.polygon',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.axis] while drawing.
#'
#' @inheritDotParams circlize::circos.axis
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track=ccTrack(ylim = c(0, 10))
#' cell=ccCell(sector.index = 'a')+ccXaxis(sector.index = "a")
#' track=track+cell
#' cc+track
ccXaxis = function(...) {
  new("ccCellGeom",func = 'circos.xaxis',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.yaxis] while drawing.
#'
#' @inheritDotParams circlize::circos.yaxis
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:8], xlim = c(0, 1))
#' track=ccTrack(ylim = c(0, 10))
#' cell=ccCell(sector.index = 'a')+ccYaxis(side = "left",sector.index = 2)
#' track=track+cell
#' cc+track
ccYaxis = function(...) {
  new("ccCellGeom",func = 'circos.yaxis',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.barplot] while drawing.
#'
#' @inheritDotParams circlize::circos.barplot
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccBarplot(value=runif(10), 1:10 - 0.5, col = 1:10)
#' track=track+cell
#' cc+track
ccBarplot = function(...) {
  new("ccCellGeom",func = 'circos.barplot',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.boxplot] while drawing.
#'
#' @inheritDotParams circlize::circos.boxplot
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccBoxplot(value=runif(10), 1:10 - 0.5, col = 1:10)
#' track=track+cell
#' cc+track
ccBoxplot = function(...) {
  new("ccCellGeom",func = 'circos.boxplot',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.violin] while drawing.
#'
#' @inheritDotParams circlize::circos.violin
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccViolin(value=runif(10), 0.5)
#' track=track+cell
#' cc+track
ccViolin = function(...) {
  new("ccCellGeom",func = 'circos.violin',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.arrow] while drawing.
#'
#' @inheritDotParams circlize::circos.arrow
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccArrow(x1=1,x2=9)
#' track=track+cell
#' cc+track
ccArrow = function(...) {
  new("ccCellGeom",func = 'circos.arrow',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.raster] while drawing.
#'
#' @inheritDotParams circlize::circos.raster
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' require(png)
#' image = system.file("extdata", "Rlogo.png", package = "circlize")
#' image = as.raster(readPNG(image))
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccRaster(image=image, x=5,y=0.5,width = "2cm",facing = "inside", niceFacing = TRUE)
#' track=track+cell
#' cc+track
ccRaster = function(...) {
  new("ccCellGeom",func = 'circos.raster',params = list(...))
}

#' Object generator for S4 class ccCellGeom
#'
#' Object [ccCellGeom-class] will call the function [circos.dendrogram] while drawing.
#'
#' @inheritDotParams circlize::circos.dendrogram
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(ape)
#' suppressPackageStartupMessages(library(dendextend))
#' library(circlizePlus)
#' data(bird.orders)
#' hc = as.hclust(bird.orders)
#' labels = hc$labels
#' ct = cutree(hc, 6)
#' n = length(labels)
#' dend = as.dendrogram(hc)
#' par1 = ccPar(cell.padding = c(0, 0, 0, 0))
#' cc = ccPlot(sectors = "a", xlim = c(0, n)) # only one sector
#' dend = color_branches(dend, k = 6, col = 1:6)
#' dend_height = attr(dend, "height")
#' t1 = ccTrack(ylim = c(0, dend_height), bg.border = NA,track.height = 0.4)
#' cell1 = ccCell(sector.index = "a") + ccDendrogram(dend = dend)
#' cc + par1 + (t1 + cell1)
ccDendrogram = function(...){
  new("ccCellGeom",func = 'circos.dendrogram',params = list(...))
}

#' Object generator for S4 class ccGenomicCellGeom
#'
#' Object [ccGenomicCellGeom-class] will call the function [circos.genomicPoints] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicPoints
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' load(system.file(package = "circlize", "extdata", "bird.orders.RData"))
#' dend = as.dendrogram(hc)
#' dend = color_branches(dend, k = 6, col = 1:6)
#' require(dendextend)
#' library(circlizePlus)
#' cc=ccPlot(sectors=letters[1:4], xlim = c(0, 10))
#' track=ccTrack(ylim = c(0, 1))
#' cell=ccCell(sector.index = 'a')+ccDendrogram(dend=dend, max_height = attr(dend, "height"))
#' track=track+cell
#' cc+track
ccGenomicPoints = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicPoints',params = list(...))
}

#' Object generator for S4 class ccGenomicCellGeom
#'
#' Object [ccGenomicCellGeom-class] will call the function [circos.genomicLines] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicLines
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicLines = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicLines',params = list(...))
}

#' Object generator for S4 class ccGenomicCellGeom
#'
#' Object [ccGenomicCellGeom-class] will call the function [circos.genomicRect] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicRect
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' NULL
ccGenomicRect = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicRect',params = list(...))
}

#' Object generator for S4 class ccGenomicCellGeom
#'
#' Object [ccGenomicCellGeom-class] will call the function [circos.genomicText] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicText
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode='initializeWithIdeogram',chromosome.index = paste0("chr", 1:4), plotType = NULL)
#' track=ccTrack(ylim=c(0,1))
#' cell=ccCell(sector.index = 'chr1')+ccGenomicAxis()
#' e=track+cell
#' cc+e
ccGenomicText = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicText',params = list(...))
}

#' Object generator for S4 class ccGenomicCellGeom
#'
#' Object [ccGenomicCellGeom-class] will call the function [circos.genomicAxis] while drawing.
#'
#' @inheritDotParams circlize::circos.genomicAxis
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc = ccPlot(initMode='initializeWithIdeogram',chromosome.index = paste0("chr", 1:4), plotType = NULL)
#' track=ccTrack(ylim=c(0,1))
#' cell=ccCell(sector.index = 'chr1')+ccGenomicAxis()
#' e=track+cell
#' cc+e
ccGenomicAxis = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicAxis',params = list(...))
}
