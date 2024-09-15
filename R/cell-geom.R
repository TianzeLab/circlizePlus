
#' S4 class ccCell
#'
#' @slot sector.index character.
#' @slot geoms list.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccCell",
         slots = c(sector.index = "character", geoms = "list"))

#' S4 class ccCells
#'
#' @export
#'
#' @examples
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
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
#' NULL
ccGenomicAxis = function(...){
  new("ccGenomicCellGeom",func = 'circos.genomicAxis',params = list(...))
}
