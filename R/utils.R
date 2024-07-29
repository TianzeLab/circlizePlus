removeNullParam = function(params){
  nparams = list()
  for (i in names(params)) {
    if(!is.null(params[[i]])){
      nparams[[i]] = params[[i]]
    }
  }
  nparams
}
