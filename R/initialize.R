#' @export
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

#' @export
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

#' @export
#' @include utils.R
show.ccPlot = function(object) {
  if (object@clear)
    circos.clear()

  if (length(object@pars) > 0) {
    do.call(circos.par, object@pars)
  }

  if (object@initMode == 'initialize') {
    do.call(circos.initialize, removeNullParam(object@initParams))
  } else if (object@initMode == 'initializeWithIdeogram') {
    do.call(circos.initializeWithIdeogram,
            removeNullParam(object@initParams))
  } else if (object@initMode == 'heatmap.initialize') {
    do.call(circos.heatmap.initialize,
            removeNullParam(object@initParams))
  } else if (object@initMode == 'genomicInitialize') {
    do.call(circos.genomicInitialize,
            removeNullParam(object@initParams))
  }

  for (current_track in object@tracks) {
    remain_geom_call = list()
    panel_fun_cell_call = list()
    #Start: make data share cell func in track panel.fun
    for (current_cell in current_track@cells) {
      panel_fun_geom_call = list()
      for (current_geom in current_cell@geoms) {
        current_geom@params[['sector.index']] = current_cell@sector.index
        current_geom@params = removeNullParam(current_geom@params)
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
          panel_fun_geom_call = c(panel_fun_geom_call, list(list(check_params = need_check_params, fill_params=how_fill_params, geom=current_geom)))
          next

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
              how_fill_params = c(how_fill_params, current_check_call$fill_params[[check_i]])
            }

          }

          panel_fun_geom_call = c(panel_fun_geom_call, list(list(check_params = need_check_params, fill_params=how_fill_params, geom=current_geom)))
          next


        }
        remain_geom_call = c(remain_geom_call, current_geom)
      }
      panel_fun_geom_call = c(panel_fun_geom_call, panel_fun_cell_call[[current_cell@sector.index]])
      panel_fun_cell_call[[current_cell@sector.index]] = panel_fun_geom_call
    }

    if(length(panel_fun_cell_call) > 0){
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
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = geom_call$fill_params[[check_param_i]](x=x, y=y)
                }else{
                  geom_call$geom@params[[geom_call$check_params[[check_param_i]]]] = get(x=geom_call$fill_params[[check_param_i]])
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
          do.call(old_track_fun, c(list(region = region, value = value), list(...)))
          current_cell_calls = panel_fun_cell_call[[get.current.sector.index()]]

          for (geom_call in current_cell_calls) {
            if (length(geom_call$check_params) > 0) {
              for (check_param_i in 1:length(geom_call$check_params)) {
                if (is.function()) {
                  geom_call@geom@params[[geom_call$check_params[[check_param_i]]]] = geom_call$fill_params[[check_param_i]](region = region, value = value)
                }
                geom_call@geom@params[[geom_call$check_params[[check_param_i]]]] = get(x=geom_call$fill_params[[check_param_i]])
              }
            }
            do.call(geom_call@geom@func, c(geom_call@geom@params, list(...)))
          }
        }
      }
    }
    #End: make data share cell func in track panel.fun
    do.call(current_track@func,
            removeNullParam(current_track@params))
    for (current_track_geom in current_track@trackGeoms)
      do.call(current_track_geom@func,
              removeNullParam(current_track_geom@params))

    for (current_geom in remain_geom_call) {
        do.call(current_geom@func, geom_params)
    }
  }

  for (current_link in object@links)
    do.call(current_link@func, removeNullParam(current_link@params))


}

#' @export
setMethod('show', 'ccPlot', show.ccPlot)
