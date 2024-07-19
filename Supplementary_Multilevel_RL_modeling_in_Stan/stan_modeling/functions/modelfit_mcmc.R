
modelfit_mcmc <-function(path, mymcmc){
  

  # load model
  load(paste0(path$data,'/modelfit_compile.rdata'))

  # load data
  load(paste0(path$data,'/artificial_standata.Rdata'))

  #sample
  fit<- my_compiledmodel$sample(
    data            = data_for_stan, 
    iter_sampling   = mymcmc$samples,
    iter_warmup     = mymcmc$warmup,
    chains          = mymcmc$chains,
    parallel_chains = mymcmc$cores)  


  #save
  if (mymcmc$datatype=='empirical'){fit$save_object(paste0(path$data,'/modelfit_empirical.rds'))
    cat(paste0('[stan_modeling]:  "modelfit_empirical.rds" was saved at "',path$data,'"'))
  }
  if (mymcmc$datatype=='artificial'){fit$save_object(paste0(path$data,'/modelfit_recovery.rds'))
    cat(paste0('[stan_modeling]:  "modelfit_recovery.rds" was saved at "',path$data,'"'))
    }
}