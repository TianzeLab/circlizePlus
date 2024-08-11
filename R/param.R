#' Object generator for S4 class ccPar
#'
#' @slot params list.
#'
#' @export
#'
#' @examples
#' NULL
setClass(
  "ccPar",
  slots = c(
    params = "list"
  )
)

#' Object generator for S4 class ccPar
#'
#'Object [ccPar-class] will call the function [circos.par] while drawing.
#'
#'@inheritDotParams circlize::circos.par
#'
#' @return Object [ccPar-class]
#' @export
#'
#' @examples
#' NULL
ccPar = function(...){
  new("ccPar", params = list(...))
}
