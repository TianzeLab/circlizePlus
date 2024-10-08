% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell-geom.R
\name{ccRaster}
\alias{ccRaster}
\title{Object generator for S4 class ccCellGeom}
\usage{
ccRaster(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[circlize:circos.raster]{circlize::circos.raster}}
  \describe{
    \item{\code{image}}{A \code{raster} object, or an object that can be converted by \code{\link[grDevices]{as.raster}}.}
    \item{\code{x}}{Position of the center of the raster image, measued in the data coordinate in the cell.}
    \item{\code{y}}{Position of the center of the raster image, measued in the data coordinate in the cell.}
    \item{\code{width}}{Width of the raster image. When \code{facing} is one of "inside", "outside", "clockwise" and "reverse.clockwise", the image should have absolute size where the value of \code{width} should be specified like \code{20mm}, \code{1cm} or \code{0.5inche}. When \code{facing} is one of \code{bending.inside} and \code{bending.outside}, the value of \code{width} is measured in the data coordinate in the cell.}
    \item{\code{height}}{Height of the raster image. Same format as \code{width}. If the value of \code{height} is omit,  default height is calculated by taking the aspect ratio of the original image. But when \code{facing} is one of \code{bending.inside} and \code{bending.outside}, \code{height} is mandatory to set.}
    \item{\code{facing}}{Facing of the raster image.}
    \item{\code{niceFacing}}{Facing of text. Please refer to vignette for different settings.}
    \item{\code{sector.index}}{Index for the sector.}
    \item{\code{track.index}}{Index for the track.}
    \item{\code{scaling}}{Scaling factor to resize the raster image.}
  }}
}
\value{
Object \linkS4class{ccCellGeom}
}
\description{
Object \linkS4class{ccCellGeom} will call the function \link{circos.raster} while drawing.
}
\examples{
NULL
}
