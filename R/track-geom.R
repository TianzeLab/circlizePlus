#' S4 class ccTrackGeom
#'
#' Objectified representation of the R package circlize's plotting functions and corresponding parameters at the track level.
#'
#' @slot func character. The name of the plot function in the R package circlize.
#' @slot params list. A **named** list that stores the parameters of the function called by the backend.
#'
#' @export
#'
#' @examples
#' NULL
setClass("ccTrackGeom",
         slots = c(func = "character", params = "list"))

#' Add lines on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackLines] while drawing.
#'
#' @inheritParams circlize::circos.trackLines
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackLines = function(sectors,
                        x, y,
                        col = par("col"),
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
                        pch = par("pch"),
                        factors = sectors) {name_args=list(
                          sectors=sectors,
                          x=x, y=y,
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
                          pch = pch,
                          factors = factors
                        )
  new("ccTrackGeom",func = 'circos.trackLines', params = c(name_args))
}

#' Add points on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackPoints] while drawing.
#'
#' @inheritParams circlize::circos.trackPoints
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackPoints = function( sectors,
                          x, y,
                          pch = par("pch"),
                          col = par("col"),
                          cex = par("cex"),
                          bg = par("bg"),
                          factors = sectors) {name_args=list(
                            sectors=sectors,
                            x=x, y=y,
                            pch = pch,
                            col = col,
                            cex = cex,
                            bg = bg,
                            factors = factors
                          )
  new("ccTrackGeom",func = 'circos.trackPoints', params = c(name_args))
}

#' Add texts on all sections of a single track.
#'
#' Object [ccTrackGeom-class] will call the function [circlize::circos.trackText] while drawing.
#'
#' @inheritParams circlize::circos.trackText
#'
#' @return Object [ccTrackGeom-class]
#' @export
#'
#' @examples
#' NULL
ccTrackText = function(sectors,
                       x, y,
                       labels,
                       direction = NULL,
                       facing = c("inside", "outside", "reverse.clockwise", "clockwise",
                                  "downward", "bending", "bending.inside", "bending.outside"),
                       niceFacing = FALSE,
                       adj = par("adj"),
                       cex = 1,
                       col = par("col"),
                       font = par("font"),
                       factors = sectors) {name_args=list(
                         sectors=sectors,
                         x=x, y=y,
                         labels=labels,
                         direction = direction,
                         facing = facing,
                         niceFacing = niceFacing,
                         adj = adj,
                         cex = cex,
                         col = col,
                         font = font,
                         factors = factors
                       )
  new("ccTrackGeom",func = 'circos.trackText', params = c(name_args))
}
