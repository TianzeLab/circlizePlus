% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell-geom.R
\name{ccBarplot}
\alias{ccBarplot}
\title{Object generator for S4 class ccCellGeom}
\usage{
ccBarplot(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[circlize:circos.barplot]{circlize::circos.barplot}}
  \describe{
    \item{\code{value}}{A numeric vector or a matrix. If it is a matrix, columns correspond to the height of bars.}
    \item{\code{pos}}{Positions of the bars.}
    \item{\code{bar_width}}{Width of bars. It assumes the bars locating at \code{x = 1, 2, ...}.}
    \item{\code{col}}{Filled color of bars.}
    \item{\code{border}}{Color for the border.}
    \item{\code{lwd}}{Line width.}
    \item{\code{lty}}{Line style.}
    \item{\code{sector.index}}{Index of sector.}
    \item{\code{track.index}}{Index of track.}
  }}
}
\value{
Object \linkS4class{ccCellGeom}
}
\description{
Object \linkS4class{ccCellGeom} will call the function \link{circos.barplot} while drawing.
}
\examples{
NULL
}
