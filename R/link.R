#' @export
setClass("ccLink",
         slots = c(func="character", params = "list"))

#' @export
setClass("ccHeatmapLink", contains = c('ccLink'))

#' @export
setClass("ccGenomicLink", contains = c('ccLink'))

#' @export
ccLink = function(...) {
  new("ccLink",
      func = 'circos.link',
      params = list(...))
}

#' @export
ccHeatmapLink = function(...) {
  new("ccHeatmapLink",
      func = 'circos.heatmap.link',
      params = list(...))
}

#' @export
ccGenomicLink = function(...) {
  new("ccGenomicLink",
      func = 'circos.genomicLink',
      params = list(...))
}
