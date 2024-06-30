

#####compare models--------------------

modelfit_compile_loo(path)

modelfit_mcmc_loo(path,
                  
                  mymcmc = list(
                    datatype = set_datatype() ,
                    samples  = 500,
                    warmup  = 500,
                    chains  = 8,
                    cores   = 8
                  ))
compare = compare_models(path)