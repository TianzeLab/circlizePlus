#' @export
setClass("ccTrack",
         slots = c(params = "list", trackGeoms = "list",cells = "list"))

#' @export
ccTrack = function(...) {
  new("ccTrack",
    params = list(...),
    trackGeoms = list(),
    cells = list())
}
