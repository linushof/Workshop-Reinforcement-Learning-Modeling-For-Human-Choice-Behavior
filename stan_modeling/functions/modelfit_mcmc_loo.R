
modelfit_mcmc_loo <-function(path,mymcmc){
  
  
  #load model
  load(paste0(path$data,'/modelfit_loo_compile.rdata'))
  
  # load standata
 
  if (mymcmc$datatype=='empirical') {print('using empirical data')
    load('./data/empirical_data/standata.Rdata')}
  if (mymcmc$datatype=='artificial'){print('using artificial data')
    current_model=set_data()
    load(paste0(current_model,'/artificial_standata.Rdata'))}

  #sample
  like=
    lapply(1:max(data_for_stan$Nblocks), function(mytestfold) {
    print(Sys.time())
    print(mytestfold)
    data_for_stan$testfold=mytestfold
  fit<- my_compiledmodel$sample(
    data            = data_for_stan, 
    iter_sampling   = mymcmc$samples,
    iter_warmup     = mymcmc$warmup,
    chains          = mymcmc$chains,
    parallel_chains = mymcmc$cores)  
  fit$draws(variables ='log_lik',format='draws_matrix')
    })
  
  #aggregate across all blocks
  like=Reduce("+",like) 
  #save mean predicted probability per trial (across samples)
  like   =t(sapply(1:dim(like)[1], function(i){x=c(t(like[i,]))
  x[x==0]<-NA
  x=na.omit(x)}))
  #save

  if (mymcmc$datatype=='empirical'){
    save(like, file=paste0(path$data,'/modelfit_like_per_trial_empirical.rdata'))
    cat(paste0('[stan_modeling]:  "modelfit_like_per_trial_empirical.rdata" was saved at "',path$data,'"'))
  }
  if (mymcmc$datatype=='artificial'){
    save(like, file=paste0(path$data,'/modelfit_like_per_trial_recovery.rdata'))
    cat(paste0('[stan_modeling]:  "modelfit_like_per_trial_recovery.rdata" was saved at "',path$data,'"'))
  }
}