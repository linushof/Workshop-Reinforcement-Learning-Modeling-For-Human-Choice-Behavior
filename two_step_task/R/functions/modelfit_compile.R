modelfit_compile <-function(path,format){
  set_cmdstan_path(path = NULL)
  if(format){ #changing to stan in newer version
    my_compiledmodel <- cmdstan_model(path,compile=F)
    formatted_model=my_compiledmodel$format(canonicalize = list("deprecations"),overwrite_file = TRUE)
    my_compiledmodel$compile()
  }
  else{
    my_compiledmodel <- cmdstan_model(path)
  }
  save(my_compiledmodel, file='two_step_task/stan/data/modelfit_compile.rdata')
  cat('[stan_modeling]:  "modelfit_compile.Rdata" was saved at "two_step_task/stan/data"')
}