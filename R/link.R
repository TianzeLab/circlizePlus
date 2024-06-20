#' @export
setClass("ccLink",
         slots = c(func="character", params = "list"))

#' @export
ccLink = function(...) {
  new("ccLink",
      func = 'circos.link',
      params = list(...))
}

#' @export
ccHeatmapLink = function(...) {
  new("ccLink",
      func = 'circos.heatmap.link',
      params = list(...))
}
