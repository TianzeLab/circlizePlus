#' S4 class ccPlot
#'
#' @slot initMode character. It can only be the following values: "initialize", "genomicInitialize", "initializeWithIdeogram", "heatmap.initialize".
#' @slot initParams list. A **named** list that stores the parameters of the function called by the backend. Based on the value of initMode, the backend function will be one of the following four:[circlize::circos.initialize], [circlize::circos.genomicInitialize], [circlize::circos.initializeWithIdeogram], [circlize::circos.heatmap.initialize].
#' @slot tracks list. A list where [ccTrack-class] or [ccGenomicTrack-class] or [ccHeatmap-class] are stored.
#' @slot links list. A list where [ccLink-class] or [ccGenomicLink-class] or [ccHeatmapLink-class] are stored.
#' @slot pars list. A list where [ccPar-class] are stored.
#' @slot clear logical. Whether to call [circlize::circos.clear] before drawing.
#'
#' @export
#'
#' @examples
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' cc=ccPlot(initMode = 'initialize', sectors = df$sectors, x = df$x)
setClass(
  "ccPlot",
  slots = c(
    initMode = "character",
    initParams = "list",
    tracks = "list",
    links = "list",
    pars = "list",
    clear = "logical"
  )
)

#' Object generator for S4 class ccPlot
#'
#' Object [ccPlot-class] calls one of the following functions based on the value of initMode: [circlize::circos.initialize], [circlize::circos.genomicInitialize], [circlize::circos.initializeWithIdeogram], [circlize::circos.heatmap.initialize]. <br>
#' The correct way to call it is as follows: <br>
#' `ccPlot(initMode = 'initialize',clear = TRUE,sectors = NULL,x = NULL,xlim = NULL,sector.width = NULL,factors = sectors,ring = FALSE)` <br> <br>
#' `ccPlot(initMode = 'genomicInitialize',clear = TRUE,data=NULL,sector.names = NULL,major.by = NULL,plotType = c("axis", "labels"),tickLabelsStartFromZero = TRUE,axis.labels.cex = 0.4*par("cex"),labels.cex = 0.8*par("cex"),track.height = NULL,...)` <br> <br>
#' `ccPlot(initMode = 'initializeWithIdeogram',clear = TRUE,cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),species = NULL,sort.chr = TRUE,chromosome.index = usable_chromosomes(species),major.by = NULL,plotType = c("ideogram", "axis", "labels"),track.height = NULL,ideogram.height = convert_height(2, "mm"),...)` <br> <br>
#' `ccPlot(initMode = 'heatmap.initialize',clear = TRUE,mat=NULL, split = NULL, cluster = TRUE,clustering.method = "complete", distance.method = "euclidean",dend.callback = function(dend, m, si) reorder(dend, rowMeans(m)),cell_width = rep(1, nrow(mat)))` <br>
#'
#' @param initMode It can only be the following values: "initialize", "genomicInitialize", "initializeWithIdeogram", "heatmap.initialize".
#' @param clear Whether to call [circlize::circos.clear] before drawing.
#' @inheritDotParams circlize::circos.initialize
#' @inheritDotParams circlize::circos.genomicInitialize
#' @inheritDotParams circlize::circos.initializeWithIdeogram
#' @inheritDotParams circlize::circos.heatmap.initialize
#' @return Object [ccPlot-class]
#'
#' @importFrom methods new
#' @export
#' @examples
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' cc=ccPlot(initMode = 'initialize', sectors = df$sectors, x = df$x)
ccPlot = function(initMode = 'initialize',
                  clear = TRUE,
                  ...) {
  new(
    "ccPlot",
    initMode = initMode,
    initParams = list(...),
    links = list(),
    tracks = list(),
    pars = list(),
    clear = clear
  )
}

#' Draw the figures described by ccPlot
#'
#' @param object Object of [ccPlot-class]
#' @return No return information
#'
#' @importMethodsFrom methods show
#' @importFrom graphics strwidth
#'
#' @exportMethod show
#' @include utils.R
#' @examples
#' library(circlizePlus)
#' n = 1000
#' df = data.frame(sectors = sample(letters[1:8], n, replace = TRUE),
#'                 x = rnorm(n), y = runif(n))
#' library(circlizePlus)
#' par1=ccPar("track.height" = 0.1)
#' cc=ccPlot(sectors = df$sectors, x = df$x) + par1
#' track1 = ccTrack(sectors = df$sectors, y = df$y,
#'                  panel.fun = function(x, y) {
#'                    circos.text(CELL_META$xcenter,
#'                                CELL_META$cell.ylim[2] + mm_y(5),
#'                                CELL_META$sector.index)
#'                    circos.axis(labels.cex = 0.6)
#'                  })
#' cc=cc+track1
#' show(cc)
setMethod('show', signature='ccPlot', definition = function(object) {
  if (object@clear)
    circos.clear()

  if (length(object@pars) > 0) {
    do.call(circos.par, object@pars)
  }

  if (object@initMode == 'initialize') {
    do.call(circos.initialize, object@initParams)
  } else if (object@initMode == 'initializeWithIdeogram') {
    do.call(circos.initializeWithIdeogram, object@initParams)
  } else if (object@initMode == 'heatmap.initialize') {
    do.call(circos.heatmap.initialize, object@initParams)
  } else if (object@initMode == 'genomicInitialize') {
    do.call(circos.genomicInitialize, object@initParams)
  }

  for (current_track in object@tracks) {
    remain_geom_call = list()
    panel_fun_cell_call = list()
    #Start: make data share cell func in track panel.fun
    for (current_cell in current_track@cells) {
      panel_fun_geom_call = list()
      for (current_geom in current_cell@geoms) {
        current_geom@params[['sector.index']] = current_cell@sector.index
        current_geom@params = current_geom@params
        if ((
          current_track@func == "circos.genomicTrack" &&
          current_geom@func %in% list(
            "circos.genomicPoints",
            "circos.genomicLines",
            "circos.genomicRect",
            "circos.genomicText"
          )
        ) || (
          current_track@func == "circos.track" &&
          current_geom@func %in% list(
            "circos.lines",
            "circos.points",
            "circos.polygon",
            "circos.rect",
            "circos.segments",
            "circos.text"
          )
        )
        ) {
          need_check_params = list()
          how_fill_params = list()
          for (check_i in 1:length(current_geom@params)) {
            current_check_param = current_geom@params[[check_i]]
            if (is.function(current_check_param)) {
              need_check_params = c(need_check_params, names(current_geom@params[check_i]))
              how_fill_params = c(how_fill_params, current_check_param)
            }
          }
          if (length(need_check_params)) {
            panel_fun_geom_call = c(panel_fun_geom_call, list(
              list(
                check_params = need_check_params,
                fill_params = how_fill_params,
                geom = current_geom
              )
            ))
            next
          }


        }
        remain_geom_call = c(remain_geom_call, current_geom)
      }
      panel_fun_geom_call = c(panel_fun_geom_call, panel_fun_cell_call[[current_cell@sector.index]])
      panel_fun_cell_call[[current_cell@sector.index]] = panel_fun_geom_call
    }

    if (length(panel_fun_cell_call) > 0) {
      if (current_track@func == "circos.track") {
        if (is.null(current_track@params[["panel.fun"]])) {
          current_track@params[["panel.fun"]] = function(x, y) {
            NULL
          }
        }
        old_track_fun = current_track@params[["panel.fun"]]
        current_track@params[["panel.fun"]] = function(x, y) {
          do.call(old_track_fun, list(x = x, y = y))
          current_cell_calls = panel_fun_cell_call[[get.current.sector.index()]]

          for (geom_call in current_cell_calls) {
            if (length(geom_call$check_params) > 0) {
              for (check_param_i in 1:length(geom_call$check_params)) {
                if (is.function(geom_call$fill_params[[check_param_i]])) {
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = geom_call$fill_params[[check_param_i]](x =
                                                                                                                              x, y = y)
                }
              }
            }

            do.call(geom_call$geom@func, geom_call$geom@params)
          }
        }
      }
      if (current_track@func == "circos.genomicTrack") {
        if (is.null(current_track@params[["panel.fun"]])) {
          current_track@params[["panel.fun"]] = function(region, value, ...) {
            NULL
          }
        }
        old_track_fun = current_track@params[["panel.fun"]]
        current_track@params[["panel.fun"]] = function(region, value, ...) {
          do.call(old_track_fun, c(list(
            region = region, value = value
          ), list(...)))
          current_cell_calls = panel_fun_cell_call[[get.current.sector.index()]]

          for (geom_call in current_cell_calls) {
            for (check_param_i in 1:length(geom_call$check_params)) {
              if (is.function(geom_call$fill_params[[check_param_i]]))
                geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = geom_call$fill_params[[check_param_i]](region = region, value = value)
            }
            do.call(geom_call$geom@func, c(geom_call$geom@params, list(...)))
          }
        }
      }
    }
    #End: make data share cell func in track panel.fun
    #Bugfix: circos.genomicLabels and parameter 'labels_height'
    if(current_track@func == "circos.genomicLabels" && is.null(current_track@params$labels_height)){
      if(is.null(current_track@params$labels)){
        current_track@params$labels = current_track@params$bed[[current_track@params$labels.column]]
      }
      current_track@params$labels_height = min(c(cm_h(1.5), max(strwidth(s = current_track@params$labels, cex = current_track@params$cex, font = current_track@params$font))))
    }
    do.call(current_track@func, current_track@params)
    for (current_track_geom in current_track@trackGeoms)
      do.call(current_track_geom@func, current_track_geom@params)

    for (current_geom in remain_geom_call) {
      #Bugfix: circos.arrow and parameter 'arrow.head.length'
      if(current_geom@func == "circos.arrow" && is.null(current_geom@params$arrow.head.length)){
        current_geom@params$arrow.head.length = mm_x(5)
      }
      if(current_geom@func == "circos.xaxis" && is.null(current_geom@params$major.tick.length)){
        current_geom@params$major.tick.length = mm_y(1)
      }
      do.call(current_geom@func, current_geom@params)
    }
  }

  for (current_link in object@links)
    do.call(current_link@func, current_link@params)
  return(invisible(NULL))
})
