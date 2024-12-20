#' S4 class ccPlot
#'
#' @slot initMode character. It can only be the following values: "initialize", "genomicInitialize", "initializeWithIdeogram", "heatmap.initialize".
#' @slot initParams list. A **named** list that stores the parameters of the function called by the backend. Based on the value of initMode, the backend function will be one of the following four:[circos.initialize], [circos.genomicInitialize], [circos.initializeWithIdeogram], [circos.heatmap.initialize].
#' @slot tracks list. A list where [ccTrack-class] or [ccGenomicTrack-class] or [ccHeatmap-class] are stored.
#' @slot links list. A list where [ccLink-class] or [ccGenomicLink-class] or [ccHeatmapLink-class] are stored.
#' @slot pars list. A list where [ccPar-class] are stored.
#' @slot clear logical. Whether to call [circos.clear] after drawing.
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
#' Object [ccPlot-class] calls one of the following functions based on the value of initMode: [circos.initialize], [circos.genomicInitialize], [circos.initializeWithIdeogram], [circos.heatmap.initialize].
#'
#' @param initMode It can only be the following values: "initialize", "genomicInitialize", "initializeWithIdeogram", "heatmap.initialize".
#' @param clear Whether to call [circos.clear] after drawing.
#' @param ... Parameters passed to the function in the package circlize. The function is one of the following four:[circos.initialize], [circos.genomicInitialize], [circos.initializeWithIdeogram], [circos.heatmap.initialize].
#'
#' @usage ccPlot(initMode = 'initialize',clear = TRUE,sectors = NULL,x = NULL,xlim = NULL,sector.width = NULL,factors = sectors,ring = FALSE)
#' @usage ccPlot(initMode = 'genomicInitialize',clear = TRUE,data=NULL,sector.names = NULL,major.by = NULL,plotType = c("axis", "labels"),tickLabelsStartFromZero = TRUE,axis.labels.cex = 0.4*par("cex"),labels.cex = 0.8*par("cex"),track.height = NULL,...)
#' @usage ccPlot(initMode = 'initializeWithIdeogram',clear = TRUE,cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),species = NULL,sort.chr = TRUE,chromosome.index = usable_chromosomes(species),major.by = NULL,plotType = c("ideogram", "axis", "labels"),track.height = NULL,ideogram.height = convert_height(2, "mm"),...)
#' @usage ccPlot(initMode = 'heatmap.initialize',clear = TRUE,mat=NULL, split = NULL, cluster = TRUE,clustering.method = "complete", distance.method = "euclidean",dend.callback = function(dend, m, si) reorder(dend, rowMeans(m)),cell_width = rep(1, nrow(mat)))
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

#' A generic function of show ccPlot
#'
#' @param object Object of [ccPlot-class]
#'
#' @export
#' @include utils.R
#' @usage show(object)
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
show.ccPlot = function(object) {
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
        if (current_track@func == "circos.genomicTrack" &&
            current_geom@func %in% list(
              "circos.genomicPoints",
              "circos.genomicLines",
              "circos.genomicRect",
              "circos.genomicText"
            )) {
          check_params = list('region', 'value')
          fill_params = list('region', 'value')
          need_check_params = list()
          how_fill_params = list()
          for (check_i in 1:length(check_params)) {
            current_check_param = check_params[[check_i]]
            if (is.function(current_geom@params[[current_check_param]])) {
              need_check_params = c(need_check_params, current_check_param)
              how_fill_params = c(how_fill_params, current_geom@params[[current_check_param]])
              next
            }
            if (is.null(current_geom@params[[current_check_param]])) {
              need_check_params = c(need_check_params, current_check_param)
              how_fill_params = c(how_fill_params, fill_params[[check_i]])
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
        if (current_track@func == "circos.track" &&
            current_geom@func %in% list(
              "circos.lines",
              "circos.points",
              "circos.polygon",
              "circos.rect",
              "circos.segments",
              "circos.text"
            )) {
          check_calls = list(
            circos.lines = list(
              check_params = list('x', 'y'),
              fill_params = list('x', 'y')
            ),
            circos.points = list(
              check_params = list('x', 'y'),
              fill_params = list('x', 'y')
            ),
            circos.polygon = list(
              check_params = list('x', 'y'),
              fill_params = list('x', 'y')
            ),
            circos.rect = list(
              check_params = list('xleft', 'ybottom', 'xright', 'ytop'),
              fill_params = list('x', 'y', 'x', 'y')
            ),
            circos.segments = list(
              check_params = list('x0', 'y0', 'x1', 'y1'),
              fill_params = list('x', 'y', 'x', 'y')
            ),
            circos.text = list(
              check_params = list('x', 'y'),
              fill_params = list('x', 'y')
            )
          )
          current_check_call = check_calls[[current_geom@func]]
          need_check_params = list()
          how_fill_params = list()
          for (check_i in 1:length(current_check_call$check_params)) {
            current_check_param = current_check_call$check_params[[check_i]]
            if (is.function(current_geom@params[[current_check_param]])) {
              need_check_params = c(need_check_params, current_check_param)
              how_fill_params = c(how_fill_params, current_geom@params[[current_check_param]])
              next
            }
            if (is.null(current_geom@params[[current_check_param]])) {
              need_check_params = c(need_check_params, current_check_param)
              how_fill_params = c(how_fill_params,
                                  current_check_call$fill_params[[check_i]])
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
                } else{
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = get(x =
                                                                                           geom_call$fill_params[[check_param_i]])
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
            if (length(geom_call$check_params) > 0) {
              for (check_param_i in 1:length(geom_call$check_params)) {
                if (is.function(geom_call$fill_params[[check_param_i]])) {
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = geom_call$fill_params[[check_param_i]](region = region, value = value)
                } else{
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = get(x =
                                                                                           geom_call$fill_params[[check_param_i]])
                }
              }
            }
            do.call(geom_call$geom@func,
                    c(geom_call$geom@params, list(...)))
          }
        }
      }
    }
    #End: make data share cell func in track panel.fun
    do.call(current_track@func, current_track@params)
    for (current_track_geom in current_track@trackGeoms)
      do.call(current_track_geom@func, current_track_geom@params)

    for (current_geom in remain_geom_call) {
      do.call(current_geom@func, current_geom@params)
    }
  }

  for (current_link in object@links)
    do.call(current_link@func, current_link@params)


}

#' @export
setMethod('show', 'ccPlot', show.ccPlot)
