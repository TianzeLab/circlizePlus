#' @export
setClass(
  "ccPar",
  slots = c(
    params = "list"
  )
)

#' @export
ccPar = function(...){
  new("ccPar", params = list(...))
}
