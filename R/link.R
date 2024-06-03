#' @export
setClass("ccLink",
         slots = c(func="character", params = "list"))

#' @export
ccLink = function(...) {
  new("ccLink",
      func = 'circos.link',
      params = list(...))
}
