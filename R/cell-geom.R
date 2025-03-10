#' S4 class ccCell
#'
#' A cell container that belongs to a particular sector.
#'
#' @slot sector.index character. It is the index that corresponds to the sector.
#' @slot geoms list. The elements in the list should all be of type [ccCellGeom-class] or [ccGenomicCellGeom-class].
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccCell",
  slots = c(sector.index = "character", geoms = "list")
)

#' S4 class ccCells
#'
#' A list of multiple ccCell. Any ccCellGeom and ccCells are added together as if they were added to each ccCell contained in the ccCells.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccCells", contains = c("list"))

#' S4 class ccCellGeom
#'
#' Objectified representation of the R package circlize's plotting functions and corresponding parameters at the cell level.
#'
#' @slot func character. The name of the plot function in the R package circlize.
#' @slot params list. When the function corresponding to the parameter param is called, it represents the argument of this function.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccCellGeom",
  slots = c(func = "character", params = "list")
)

#' S4 class ccGenomicCellGeom
#'
#' It is a subclass of ccCellGeom. It only works if the plotted data is genomic data. Objectified representation of the R package circlize's plotting functions and corresponding parameters at the cell level.
#'
#' @slot func character. The name of the plot function in the R package circlize.
#' @slot params list. When the function corresponding to the parameter param is called, it represents the argument of this function.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccGenomicCellGeom", contains = c("ccCellGeom"))



#' Generate a cell container that belongs to a particular sector
#'
#' @param sector.index character. It is the index that corresponds to the sector.
#'
#' @return Object [ccCell-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' sectors <- c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c", "d", "d", "d", "d")
#' x1 <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
#' y1 <- c(1, 2, 3, 4, 4, 3, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2)
#' cc <- ccPlot(initMode = "initialize", sectors = sectors, x = x1)
#' track1 <- ccTrack(sectors = sectors, x = x1, y = y1)
#' cell_single <- ccCell(sector.index = letters[3]) + ccPoints(y = \(x, y){
#'   y
#' })
#' track1 <- track1 + cell_single
#' cc + track1
ccCell <- function(sector.index = NULL) {
  new("ccCell", sector.index = sector.index, geoms = list())
}


#' Generate a list of multiple object ccCell-class
#'
#' @param sector.indexes list. A list of indexs that corresponds to the sectors.
#'
#' @return Object [ccCells-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' sectors <- c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c", "d", "d", "d", "d")
#' x1 <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
#' y1 <- c(1, 2, 3, 4, 4, 3, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2)
#' cc <- ccPlot(initMode = "initialize", sectors = sectors, x = x1)
#' cells <- ccCells(sector.indexes = letters[1:4])
#' cc_point <- ccPoints()
#' cells <- cells + cc_point + ccLines()
#' track1 <- ccTrack(sectors = sectors, x = x1, y = y1)
#' track1 <- track1 + cells
#' cc + track1
ccCells <- function(sector.indexes = list()) {
  if (length(sector.indexes) == 0) {
    stop("'sector.indexes' can't be an empty list.")
  }
  cells <- new("ccCells")
  for (si in sector.indexes) {
    cells[[si]] <- ccCell(sector.index = si)
  }
  cells
}

#' Draw text in a cell
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.text] while drawing.
#'
#' @inheritParams circlize::circos.text
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' n <- 1000
#' df <- data.frame(
#'   sectors = sample(letters[1:8], n, replace = TRUE),
#'   x = rnorm(n), y = runif(n)
#' )
#' par1 <- ccPar("track.height" = 0.1)
#' cc <- ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 <- ccTrack(
#'   sectors = df$sectors, y = df$y,
#'   panel.fun = function(x, y) {
#'     circos.text(
#'       CELL_META$xcenter,
#'       CELL_META$cell.ylim[2] + mm_y(5),
#'       CELL_META$sector.index
#'     )
#'     circos.axis(labels.cex = 0.6)
#'   }
#' )
#' cell1 <- ccCell(sector.index = "a") + ccText(-1, 0.5, "text")
#' track1 <- track1 + cell1
#' cc <- cc + track1
#' cc
ccText <- function(x = NULL,
                   y = NULL,
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
  name_args <- list(
    x = x,
    y = y,
    labels = labels,
    direction = direction,
    facing = facing,
    niceFacing = niceFacing,
    adj = adj,
    cex = cex,
    col = col,
    font = font
  )
  new("ccCellGeom",
    func = "circos.text",
    params = c(name_args, list(...))
  )
}



#' Draw points in a region
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.points] while drawing.
#'
#' @inheritParams circlize::circos.points
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track1 <- ccTrack(ylim = c(0, 1), panel.fun = function(x, y) {
#'   circos.points(runif(10), runif(10))
#' })
#' cells <- ccCell(sector.index = "a") + ccPoints(
#'   x = runif(10), y = runif(10),
#'    pch = 16, col = "red"
#' )
#' track1 <- track1 + cells
#' cc + track1
ccPoints <- function(x = NULL, y = NULL,
                     pch = par("pch"),
                     col = par("col"),
                     cex = par("cex"),
                     bg = par("bg")) {
  name_args <- list(
    x = x,
    y = y,
    pch = pch,
    col = col,
    cex = cex,
    bg = bg
  )
  new("ccCellGeom", func = "circos.points", params = c(name_args))
}

#' Draw lines in a region
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.lines] while drawing.
#'
#' @inheritParams circlize::circos.lines
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' sectors <- letters[1:9]
#' par <- ccPar(points.overflow.warning = FALSE)
#' cc <- ccPlot(sectors = sectors, xlim = c(0, 10))
#' cc <- cc + par
#' track <- ccTrack(sectors = sectors, ylim = c(0, 10), track.height = 0.5)
#' cells <- ccCell(sector.index = "a") + ccLines(sort(x = runif(10) * 10), y = runif(10) * 10)
#' track <- track + cells
#' cc + track
ccLines <- function(x = NULL, y = NULL,
                    col = ifelse(area, "grey", par("col")),
                    lwd = par("lwd"),
                    lty = par("lty"),
                    type = "l",
                    straight = FALSE,
                    area = FALSE,
                    area.baseline = NULL,
                    border = "black",
                    baseline = "bottom",
                    pt.col = par("col"),
                    cex = par("cex"),
                    pch = par("pch")) {
  name_args <- list(
    x = x, y = y,
    col = col,
    lwd = lwd,
    lty = lty,
    type = type,
    straight = straight,
    area = area,
    area.baseline = area.baseline,
    border = border,
    baseline = baseline,
    pt.col = pt.col,
    cex = cex,
    pch = pch
  )
  new("ccCellGeom", func = "circos.lines", params = c(name_args))
}

#' Draw segments connecting points in a region
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.segments] while drawing.
#'
#' @inheritParams circlize::circos.segments
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track <- ccTrack(ylim = c(0, 1), track.height = 0.3)
#' cell <- ccCell(sector.index = "a") + ccSegments(x0 = 0.7, y0 = 0.1, x1 = 0.7, y1 = 0.9)
#' track <- track + cell
#' cc + track
ccSegments <- function(x0 = NULL, y0 = NULL, x1 = NULL, y1 = NULL,
                       straight = FALSE,
                       col = par("col"),
                       lwd = par("lwd"),
                       lty = par("lty"),
                       ...) {
  name_args <- list(
    x0 = x0, y0 = y0, x1 = x1, y1 = y1,
    straight = straight,
    col = col,
    lwd = lwd,
    lty = lty
  )
  new("ccCellGeom", func = "circos.segments", params = c(name_args, list(...)))
}

#' Draw rectangle in a region
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.rect] while drawing.
#'
#' @inheritParams circlize::circos.rect
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track <- ccTrack(ylim = c(0, 1), track.height = 0.3)
#' cell <- ccCell(sector.index = "a") + ccRect(xleft = 0.7, ybottom = 0.1, xright = 0.8, ytop = 0.9)
#' track <- track + cell
#' cc + track
ccRect <- function(xleft = NULL, ybottom = NULL, xright = NULL, ytop = NULL,
                   rot = 0,
                   ...) {
  name_args <- list(
    xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop,
    rot = rot
  )
  new("ccCellGeom", func = "circos.rect", params = c(name_args, list(...)))
}

#' Draw polygon
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.polygon] while drawing.
#'
#' @inheritParams circlize::circos.polygon
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track <- ccTrack(ylim = c(0, 10))
#' cell <- ccCell(sector.index = "a") + ccPolygon(x = c(0.5, 0.7, 1), y = c(2, 6, 8))
#' track <- track + cell
#' cc + track
ccPolygon <- function(x = NULL, y = NULL,
                      ...) {
  name_args <- list(x = x, y = y)
  new("ccCellGeom", func = "circos.polygon", params = c(name_args, list(...)))
}

#' Draw x-axis
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.axis] while drawing.
#'
#' @inheritParams circlize::circos.axis
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track <- ccTrack(ylim = c(0, 10))
#' cell <- ccCell(sector.index = "a") + ccXaxis()
#' track <- track + cell
#' cc + track
ccXaxis <- function(h = "top",
                    major.at = NULL,
                    labels = TRUE,
                    major.tick = TRUE,
                    labels.font = par("font"),
                    labels.cex = par("cex"),
                    labels.facing = "inside",
                    labels.direction = NULL,
                    labels.niceFacing = TRUE,
                    direction = c("outside", "inside"),
                    minor.ticks = 4,
                    major.tick.length = mm_y(1),
                    lwd = par("lwd"),
                    col = par("col"),
                    labels.col = par("col"),
                    labels.pos.adjust = TRUE) {
  name_args <- list(
    h = h,
    major.at = major.at,
    labels = labels,
    major.tick = major.tick,
    labels.font = labels.font,
    labels.cex = labels.cex,
    labels.facing = labels.facing,
    labels.direction = labels.direction,
    labels.niceFacing = labels.niceFacing,
    direction = direction,
    minor.ticks = minor.ticks,
    major.tick.length = major.tick.length,
    lwd = lwd,
    col = col,
    labels.col = labels.col,
    labels.pos.adjust = labels.pos.adjust
  )
  new("ccCellGeom", func = "circos.xaxis", params = c(name_args))
}

#' Draw y-axis
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.yaxis] while drawing.
#'
#' @inheritParams circlize::circos.yaxis
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:8], xlim = c(0, 1))
#' track <- ccTrack(ylim = c(0, 10))
#' cell <- ccCell(sector.index = "a") + ccYaxis(side = "left")
#' track <- track + cell
#' cc + track
ccYaxis <- function(side = c("left", "right"),
                    at = NULL,
                    labels = TRUE,
                    tick = TRUE,
                    labels.font = par("font"),
                    labels.cex = par("cex"),
                    labels.niceFacing = TRUE,
                    lwd = par("lwd"),
                    col = par("col"),
                    labels.col = par("col")) {
  name_args <- list(
    side = side,
    at = at,
    labels = labels,
    tick = tick,
    labels.font = labels.font,
    labels.cex = labels.cex,
    labels.niceFacing = labels.niceFacing,
    lwd = lwd,
    col = col,
    labels.col = labels.col
  )
  new("ccCellGeom", func = "circos.yaxis", params = c(name_args))
}

#' Draw barplots
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.barplot] while drawing.
#'
#' @inheritParams circlize::circos.barplot
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:4], xlim = c(0, 10))
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "a") + ccBarplot(value = runif(10), pos = 1:10 - 0.5, col = 1:10)
#' track <- track + cell
#' cc + track
ccBarplot <- function(value, pos, bar_width = 0.6,
                      col = NA, border = "black", lwd = par("lwd"), lty = par("lty")) {
  name_args <- list(
    value = value, pos = pos, bar_width = bar_width,
    col = col, border = border, lwd = lwd, lty = lty
  )
  new("ccCellGeom", func = "circos.barplot", params = c(name_args))
}

#' Draw boxplots
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.boxplot] while drawing.
#'
#' @inheritParams circlize::circos.boxplot
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:4], xlim = c(0, 10))
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "a") + ccBoxplot(value = replicate(runif(10), n = 10, simplify = FALSE), pos = 1:10 - 0.5, col = 1:10)
#' track <- track + cell
#' cc + track
ccBoxplot <- function(value, pos, outline = TRUE, box_width = 0.6,
                      col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
                      cex = par("cex"), pch = 1, pt.col = par("col")) {
  name_args <- list(
    value = value, pos = pos, outline = outline, box_width = box_width,
    col = col, border = border, lwd = lwd, lty = lty,
    cex = cex, pch = pch, pt.col = pt.col
  )
  new("ccCellGeom", func = "circos.boxplot", params = c(name_args))
}

#' Draw violin plots
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.violin] while drawing.
#'
#' @inheritParams circlize::circos.violin
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:4], xlim = c(0, 10))
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "a") + ccViolin(value = replicate(runif(10), n = 10, simplify = FALSE), pos = 1:10 - 0.5, col = 1:10)
#' track <- track + cell
#' cc + track
ccViolin <- function(value, pos, violin_width = 0.8,
                     col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
                     show_quantile = TRUE, pt.col = par("col"), cex = par("cex"), pch = 16,
                     max_density = NULL) {
  name_args <- list(
    value = value, pos = pos, violin_width = violin_width,
    col = col, border = border, lwd = lwd, lty = lty,
    show_quantile = show_quantile, pt.col = pt.col, cex = cex, pch = pch,
    max_density = max_density
  )
  new("ccCellGeom", func = "circos.violin", params = c(name_args))
}

#' Draw an arrow
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.arrow] while drawing.
#'
#' @inheritParams circlize::circos.arrow
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:4], xlim = c(0, 10))
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "a") + ccArrow(x1 = 1, x2 = 9, y=0.5, width=0.5)
#' track <- track + cell
#' cc + track
ccArrow <- function(x1,
                    x2,
                    y ,
                    width,
                    arrow.head.length = NULL,
                    arrow.head.width = width * 2,
                    arrow.position = c("end", "start"),
                    tail = c("normal", "point"),
                    border = "black",
                    col = "#FFCCCC",
                    lty = par("lty"),
                    ...) {
  name_args <- list(
    x1 = x1,
    x2 = x2,
    y = y,
    width = width,
    arrow.head.length = arrow.head.length,
    arrow.head.width = arrow.head.width,
    arrow.position = arrow.position,
    tail = tail,
    border = border,
    col = col,
    lty = lty
  )
  new("ccCellGeom", func = "circos.arrow", params = c(name_args, list(...)))
}

#' Add raster image
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.raster] while drawing.
#'
#' @inheritParams circlize::circos.raster
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' library(png)
#' image <- system.file("extdata", "Rlogo.png", package = "circlize")
#' image <- as.raster(readPNG(image))
#' library(circlizePlus)
#' cc <- ccPlot(sectors = letters[1:4], xlim = c(0, 10))
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "a") + ccRaster(image = image, x = 5, y = 0.5, width = "2cm", height = "2cm", facing = "inside", niceFacing = TRUE)
#' track <- track + cell
#' cc + track
ccRaster <- function(image, x, y,
                     width, height,
                     facing = c(
                       "inside", "outside", "reverse.clockwise", "clockwise",
                       "downward", "bending.inside", "bending.outside"
                     ),
                     niceFacing = FALSE,
                     scaling = 1) {
  name_args <- list(
    image = image, x = x, y = y,
    width = width, height = height,
    facing = facing,
    niceFacing = niceFacing,
    scaling = scaling
  )
  new("ccCellGeom", func = "circos.raster", params = c(name_args))
}

#' Draw dendrogram plots in a track
#'
#' Object [ccCellGeom-class] will call the function [circlize::circos.dendrogram] while drawing.
#'
#' @inheritParams circlize::circos.dendrogram
#'
#' @return Object [ccCellGeom-class]
#' @export
#'
#' @examples
#' library(ape)
#' suppressPackageStartupMessages(library(dendextend))
#' library(circlizePlus)
#' data(bird.orders)
#' hc <- as.hclust(bird.orders)
#' labels <- hc$labels
#' ct <- cutree(hc, 6)
#' n <- length(labels)
#' dend <- as.dendrogram(hc)
#' par1 <- ccPar(cell.padding = c(0, 0, 0, 0))
#' cc <- ccPlot(sectors = "a", xlim = c(0, n)) # only one sector
#' dend <- color_branches(dend, k = 6, col = 1:6)
#' dend_height <- attr(dend, "height")
#' t1 <- ccTrack(ylim = c(0, dend_height), bg.border = NA, track.height = 0.4)
#' cell1 <- ccCell(sector.index = "a") + ccDendrogram(dend = dend)
#' cc + par1 + (t1 + cell1)
ccDendrogram <- function(dend,
                         facing = c("outside", "inside"),
                         max_height = NULL,
                         use_x_attr = FALSE) {
  name_args <- list(
    dend = dend,
    facing = facing,
    max_height = max_height,
    use_x_attr = use_x_attr
  )
  new("ccCellGeom", func = "circos.dendrogram", params = c(name_args))
}

#' Add points for genomic data visualization
#'
#' Object [ccGenomicCellGeom-class] will call the function [circlize::circos.genomicPoints] while drawing.
#'
#' @inheritParams circlize::circos.genomicPoints
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' data <- generateRandomBed(nr = 30, nc = 2)
#' all_chr <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrX", "chrY")
#' cc <- ccPlot(initMode = "initializeWithIdeogram", plotType = NULL)
#' t1 <- ccGenomicTrack(data = data, numeric.column = 4)
#' cells1 <- ccCells(sector.indexes = all_chr) + ccGenomicPoints(region = \(region, value){
#'   region
#' }, value = \(region, value){
#'   value
#' }, numeric.column = 2)
#' t1 <- t1 + cells1
#' show(cc + t1)
ccGenomicPoints <- function(region = NULL,
                            value = NULL,
                            numeric.column = NULL,
                            posTransform = NULL,
                            pch = par("pch"),
                            col = par("col"),
                            cex = par("cex"),
                            bg = par("bg"),
                            ...) {
  name_args <- list(
    region = region,
    value = value,
    numeric.column = numeric.column,
    posTransform = posTransform,
    pch = pch,
    col = col,
    cex = cex,
    bg = bg
  )
  new("ccGenomicCellGeom", func = "circos.genomicPoints", params = c(name_args, list(...)))
}

#' Add lines for genomic data visualization
#'
#' Object [ccGenomicCellGeom-class] will call the function [circlize::circos.genomicLines] while drawing.
#'
#' @inheritParams circlize::circos.genomicLines
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' data <- generateRandomBed(nr = 30, nc = 2)
#' all_chr <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrX", "chrY")
#' cc <- ccPlot(initMode = "initializeWithIdeogram", plotType = NULL)
#' t1 <- ccGenomicTrack(data = data, numeric.column = 4)
#' cells1 <- ccCells(sector.indexes = all_chr) + ccGenomicLines(numeric.column = 2)
#' t1 <- t1 + cells1
#' show(cc + t1)
ccGenomicLines <- function(region = NULL,
                           value = NULL,
                           numeric.column = NULL,
                           posTransform = NULL,
                           col = ifelse(area, "grey", "black"),
                           lwd = par("lwd"),
                           lty = par("lty"),
                           type = "l",
                           area = FALSE,
                           area.baseline = NULL,
                           border = "black",
                           baseline = "bottom",
                           pt.col = par("col"),
                           cex = par("cex"),
                           pch = par("pch"),
                           ...) {
  name_args <- list(
    region = region,
    value = value,
    numeric.column = numeric.column,
    posTransform = posTransform,
    col = col,
    lwd = lwd,
    lty = lty,
    type = type,
    area = area,
    area.baseline = area.baseline,
    border = border,
    baseline = baseline,
    pt.col = pt.col,
    cex = cex,
    pch = pch
  )
  new("ccGenomicCellGeom", func = "circos.genomicLines", params = c(name_args, list(...)))
}

#' Draw rectangle for genomic data visualization
#'
#' Object [ccGenomicCellGeom-class] will call the function [circlize::circos.genomicRect] while drawing.
#'
#' @inheritParams circlize::circos.genomicRect
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' par1 <- ccPar("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
#' cc <- ccPlot(initMode = "initializeWithIdeogram", plotType = NULL)
#' bed1 <- generateRandomBed(nr = 100)
#' bed2 <- generateRandomBed(nr = 100)
#' bed_list <- list(bed1, bed2)
#' f <- colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
#' track1 <- ccGenomicTrack(data = bed_list, stack = TRUE)
#' all_chr <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrX", "chrY")
#' rect1 <- ccGenomicRect(col = 1, border = NA)
#' cells1 <- ccCells(sector.indexes = all_chr) + rect1
#' cc + par1 + (track1 + cells1)
ccGenomicRect <- function(region = NULL,
                          value = NULL,
                          ytop = NULL,
                          ybottom = NULL,
                          ytop.column = NULL,
                          ybottom.column = NULL,
                          posTransform = NULL,
                          col = NA,
                          border = "black",
                          lty = par("lty"),
                          ...) {
  name_args <- list(
    region = region,
    value = value,
    ytop = ytop,
    ybottom = ybottom,
    ytop.column = ytop.column,
    ybottom.column = ybottom.column,
    posTransform = posTransform,
    col = col,
    border = border,
    lty = lty
  )
  new("ccGenomicCellGeom", func = "circos.genomicRect", params = c(name_args, list(...)))
}

#' Add text for genomic data visualization
#'
#' Object [ccGenomicCellGeom-class] will call the function [circlize::circos.genomicText] while drawing.
#'
#' @inheritParams circlize::circos.genomicText
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(initMode = "initializeWithIdeogram", plotType = NULL)
#' bed <- generateRandomBed(nr = 20)
#' track1 <- ccGenomicTrack(data = bed, ylim = c(0, 1))
#' all_chr <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", "chr22", "chrX", "chrY")
#' text1 <- ccGenomicText(y = 0.5, labels = "text")
#' cells1 <- ccCells(sector.indexes = all_chr) + text1
#' cc + (track1 + cells1)
ccGenomicText <- function(region = NULL,
                          value = NULL,
                          y = NULL,
                          labels = NULL,
                          labels.column = NULL,
                          numeric.column = NULL,
                          posTransform = NULL,
                          direction = NULL,
                          facing = "inside",
                          niceFacing = FALSE,
                          adj = par("adj"),
                          cex = 1,
                          col = "black",
                          font = par("font"),
                          padding = 0,
                          extend = 0,
                          align_to = "region",
                          ...) {
  name_args <- list(
    region = region,
    value = value,
    y = y,
    labels = labels,
    labels.column = labels.column,
    numeric.column = numeric.column,
    posTransform = posTransform,
    direction = direction,
    facing = facing,
    niceFacing = niceFacing,
    adj = adj,
    cex = cex,
    col = col,
    font = font,
    padding = padding,
    extend = extend,
    align_to = align_to
  )
  new("ccGenomicCellGeom", func = "circos.genomicText", params = c(name_args, list(...)))
}

#' Add genomic axes
#'
#' Object [ccGenomicCellGeom-class] will call the function [circlize::circos.genomicAxis] while drawing.
#'
#' @inheritParams circlize::circos.genomicAxis
#'
#' @return Object [ccGenomicCellGeom-class]
#' @export
#'
#' @examples
#' library(circlizePlus)
#' cc <- ccPlot(initMode = "initializeWithIdeogram", chromosome.index = paste0("chr", 1:4), plotType = NULL)
#' track <- ccTrack(ylim = c(0, 1))
#' cell <- ccCell(sector.index = "chr1") + ccGenomicAxis()
#' e <- track + cell
#' cc + e
ccGenomicAxis <- function(h = "top",
                          major.at = NULL,
                          labels = NULL,
                          major.by = NULL,
                          tickLabelsStartFromZero = TRUE,
                          labels.cex = 0.4 * par("cex"),
                          ...) {
  name_args <- list(
    h = h,
    major.at = major.at,
    labels = labels,
    major.by = major.by,
    tickLabelsStartFromZero = tickLabelsStartFromZero,
    labels.cex = labels.cex
  )
  new("ccGenomicCellGeom", func = "circos.genomicAxis", params = c(name_args, list(...)))
}
