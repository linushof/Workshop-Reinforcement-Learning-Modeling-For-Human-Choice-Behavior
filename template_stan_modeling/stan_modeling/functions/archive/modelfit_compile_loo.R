
modelfit_compile_loo <-function(path){
  
  set_cmdstan_path(path = NULL)
  
  my_compiledmodel <- cmdstan_model(paste0(path$model,'_loo.stan'))
  
  save(my_compiledmodel, file=paste0(path$data,'/modelfit_loo_compile.rdata'))
  cat(paste0('[stan_modeling]:  "modelfit_loo_compile.Rdata" was saved at "',path$data,'"'))
  
}
