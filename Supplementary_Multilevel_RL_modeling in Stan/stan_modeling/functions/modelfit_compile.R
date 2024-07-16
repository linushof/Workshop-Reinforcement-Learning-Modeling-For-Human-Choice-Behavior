modelfit_compile <-function(path,format){
  set_cmdstan_path(path = NULL)
  if(format){ #changing to stan in newer version
    my_compiledmodel <- cmdstan_model(paste0(path$model,'.stan'),compile=F)
    formatted_model=my_compiledmodel$format(canonicalize = list("deprecations"),overwrite_file = TRUE)
    my_compiledmodel$compile()
  }
  else{
    my_compiledmodel <- cmdstan_model(paste0(path$model,'.stan'))
  }
  save(my_compiledmodel, file=paste0(path$data,'/modelfit_compile.rdata'))
  cat(paste0('[stan_modeling]:  "modelfit_compile.Rdata" was saved at "',path$data,'"'))
}