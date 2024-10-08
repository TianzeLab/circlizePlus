% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell-geom.R
\name{ccSegments}
\alias{ccSegments}
\title{Object generator for S4 class ccCellGeom}
\usage{
ccSegments(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[circlize:circos.segments]{circlize::circos.segments}}
  \describe{
    \item{\code{x0}}{x coordinates for starting points.}
    \item{\code{y0}}{y coordinates for ending points.}
    \item{\code{x1}}{x coordinates for starting points.}
    \item{\code{y1}}{y coordinates for ending points.}
    \item{\code{sector.index}}{Index for the sector.}
    \item{\code{track.index}}{Index for the track.}
    \item{\code{straight}}{Whether the segment is a straight line.}
    \item{\code{col}}{Color of the segments.}
    \item{\code{lwd}}{Line width of the segments.}
    \item{\code{lty}}{Line type of the segments.}
  }}
}
\value{
Object \linkS4class{ccCellGeom}
}
\description{
Object \linkS4class{ccCellGeom} will call the function \link{circos.segments} while drawing.
}
\examples{
NULL
}
