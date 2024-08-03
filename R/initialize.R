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
    #Start: make data share cell func in track panel.fun
    if (current_track@func == 'circos.track') {
      miss_data_cells = list()
      for (current_cell in current_track@cells) {
        miss_data_calls = list()
        for (current_geom in current_cell@geoms) {
          if (current_geom@func %in% list(
            "circos.lines",
            "circos.points",
            "circos.polygon",
            "circos.raster",
            "circos.polygon"
          )) {
            miss_data = list()
            if (!'x' %in% names(removeNullParam(current_geom@params))) {
              miss_data = c(miss_data, 'x')
            }
            if (!'y' %in% names(removeNullParam(current_geom@params))) {
              miss_data = c(miss_data, 'y')
            }
            if (length(miss_data) > 0) {
              miss_data_calls = c(
                miss_data_calls,
                list(list(
                  miss_data = miss_data,
                  func = current_geom@func,
                  params = removeNullParam((current_geom@params))
                ))
              )

            }

          }
        }
        if (length(miss_data_calls) > 0) {
          miss_data_cells = c(
            miss_data_cells,
            list(list(
              sector.index = current_cell@sector.index,
              miss_data_calls = miss_data_calls
            ))
          )
        }
      }
      if (length(miss_data_cells) > 0) {
        if (is.null(current_track@params[['panel.fun']])) {
          current_track@params[['panel.fun']] = function(x, y) {
            NULL
          }
        }
        old_track_fun = current_track@params[['panel.fun']]
        current_track@params[['panel.fun']] = function(x, y) {
          do.call(old_track_fun, list(x = x, y = y))
          for (current_miss_data_cell in miss_data_cells) {
            if (current_miss_data_cell$sector.index == get.current.sector.index()) {
              for (current_miss_data_call in miss_data_calls) {
                for (miss_axis in current_miss_data_call$miss_data) {
                  current_miss_data_call$params[[miss_axis]] = str2lang(miss_axis)
                }
                do.call(current_miss_data_call$func,
                        current_miss_data_call$params)
              }
            }
          }

        }
      }


    }
    if (current_track@func == 'circos.genomicTrack') {

    }
    #End: make data share cell func in track panel.fun
    do.call(current_track@func,
            removeNullParam(current_track@params))
    for (current_track_geom in current_track@trackGeoms)
      do.call(current_track_geom@func,
              removeNullParam(current_track_geom@params))

    for (current_cell in current_track@cells) {
      for (current_geom in current_cell@geoms) {
        geom_params = removeNullParam(current_geom@params)
        if (current_track@func == 'circos.track' &&
            current_geom@func %in% list(
              "circos.lines",
              "circos.points",
              "circos.polygon",
              "circos.raster",
              "circos.polygon"
            )) {
          if (!('x' %in% names(geom_params) && 'y' %in% names(geom_params))) {
            next
          }
        }

        if (current_track@func == 'circos.genomicTrack' &&
            current_geom@func %in% list("circos.genomicLines",
                                        "circos.genomicPoints",
                                        "circos.genomicText")) {
          if (!('data' %in% names(geom_params))) {
            next
          }
        }
        geom_params[['sector.index']] = current_cell@sector.index
        do.call(current_geom@func, geom_params)
      }

    }
  }

  for (current_link in object@links)
    do.call(current_link@func, removeNullParam(current_link@params))


}

#' @export
setMethod('show', 'ccPlot', show.ccPlot)
