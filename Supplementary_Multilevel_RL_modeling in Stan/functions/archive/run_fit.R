run_fit = function(include_loo,data_for_stan,iter_sampling=1000,iter_warmup=1000,chains=4){
  if(include_loo==1){
    data_for_stan$include_loo=1
    like=
      lapply(1:data_for_stan$Nblocks, function(mytestfold) {
        data_for_stan$testfold=mytestfold
        #fit the model for each test fold in every function iteration
        fit = my_compiledmodel$sample(
          data = data_for_stan,
          iter_sampling = iter_sampling,
          iter_warmup = iter_warmup,
          chains = chains,
          parallel_chains = chains)
        like = t(as_draws_df(fit$draws(variables="null_model")))}) #transpose to get a nicer matrix 
    return (like)
  }
  else{
    data_for_stan$include_loo=0
    data_for_stan$testfold=0
    fit<- my_compiledmodel$sample(
      data = data_for_stan, 
      iter_sampling = iter_sampling,
      iter_warmup = iter_warmup,
      chains =chains,
      parallel_chains = chains) 
    
    return (fit)
  }
  
}