#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()

#####Load stan data--------------------
data_path = set_standata_file()

#####sample posterior--------------------

modelfit_compile(path, format = F)

modelfit_mcmc(
  path,
  data_path = data_path,
  
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 100,
    warmup  = 200,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("population_scales[1]",
           "population_scales[2]")

examine_mcmc(path, mypars, datatype = 'empirical')

examine_population_parameters_recovery(path, datatype = 'empirical')


####examine model
#load parameters
fit   = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
Qdiff = fit$draws(variables = 'Qdiff_external', format = 'draws_matrix')
Qval1 = fit$draws(variables = 'Qval1_external', format = 'draws_matrix')
Qval2 = fit$draws(variables = 'Qval2_external', format = 'draws_matrix')
Qval3 = fit$draws(variables = 'Qval3_external', format = 'draws_matrix')
Qval4 = fit$draws(variables = 'Qval4_external', format = 'draws_matrix')

PE    = fit$draws(variables = 'PE_external', format = 'draws_matrix')


