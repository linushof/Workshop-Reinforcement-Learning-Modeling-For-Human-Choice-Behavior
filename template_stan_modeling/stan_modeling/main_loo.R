rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()
#####compare models--------------------

modelfit_compile_loo(path)

modelfit_mcmc_loo(path,
                  
                  mymcmc = list(
                    datatype = set_datatype() ,
                    samples  = 1000,
                    warmup  = 1000,
                    chains  = 4,
                    cores   = 4
                  ))
compare = compare_models(path)
