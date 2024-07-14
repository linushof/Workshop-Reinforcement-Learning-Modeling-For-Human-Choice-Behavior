rm(list = ls())

source('two_step_task/R/functions/my_starter.R')
path = set_workingmodel()
#####sample posterior--------------------

modelfit_compile(path='two_step_task/stan/two_step_task.stan', format = F)

modelfit_compile(path='two_step_task/stan/two_step_task_hierarchical.stan', format = F)

modelfit_mcmc(
  path,
  data_path = paste0('two_step_task/R/data/stan_ready_data_files/artificial_standata_', path$name, '.Rdata'),
  
  mymcmc = list(
    datatype = 'artificial' ,
    samples  = 1000,
    warmup  = 1000,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("population_locations_transformed[1]",
           "population_locations_transformed[2]")

examine_mcmc(path, mypars, datatype = 'artificial')

