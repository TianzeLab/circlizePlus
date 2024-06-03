#' @export
setClass("ccTrack",
         slots = c(func="character", params = "list", trackGeoms = "list",cells = "list"))

#' @export
ccTrack = function(...) {
  new("ccTrack",
    func = 'circos.track',
    params = list(...),
    trackGeoms = list(),
    cells = list())
}

#' @export
ccTrackHist = function(...) {
  new("ccTrack",
      func = 'circos.trackHist',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}
