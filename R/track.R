#' @export
setClass("ccTrack",
         slots = c(func="character", params = "list", trackGeoms = "list",cells = "list"))

#' @export
setClass("ccGenomicTrack", contains = c("ccTrack"))

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

#' @export
ccGenomicTrack = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicTrack',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}

#' @export
ccGenomicIdeogram = function(...) {
  new("ccGenomicTrack",
      func = 'circos.genomicIdeogram',
      params = list(...),
      trackGeoms = list(),
      cells = list())
}
