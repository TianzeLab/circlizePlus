% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell-geom.R
\name{ccYaxis}
\alias{ccYaxis}
\title{Object generator for S4 class ccCellGeom}
\usage{
ccYaxis(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[circlize:circos.yaxis]{circlize::circos.yaxis}}
  \describe{
    \item{\code{side}}{add the y-axis on the left or right of the cell}
    \item{\code{at}}{If it is numeric vector, it identifies the positions of the ticks. It can exceed \code{ylim} value and the exceeding part would be trimmed automatically.}
    \item{\code{labels}}{labels of the ticks. The exceeding part would be trimmed automatically. The value can also be logical (either an atomic value or a vector) which represents which labels to show.}
    \item{\code{tick}}{Whether to draw ticks.}
    \item{\code{sector.index}}{Index for the sector}
    \item{\code{track.index}}{Index for the track}
    \item{\code{labels.font}}{font style for the axis labels}
    \item{\code{labels.cex}}{font size for the axis labels}
    \item{\code{labels.niceFacing}}{Should facing of axis labels be human-easy}
    \item{\code{tick.length}}{length of the tick}
    \item{\code{lwd}}{line width for ticks}
    \item{\code{col}}{color for the axes}
    \item{\code{labels.col}}{color for the labels}
  }}
}
\value{
Object \linkS4class{ccCellGeom}
}
\description{
Object \linkS4class{ccCellGeom} will call the function \link{circos.yaxis} while drawing.
}
\examples{
NULL
}
