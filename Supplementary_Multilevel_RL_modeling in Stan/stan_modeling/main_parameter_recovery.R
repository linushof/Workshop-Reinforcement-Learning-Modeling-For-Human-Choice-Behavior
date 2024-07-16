#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()


#cfg for MAB
#cfg for sequence

#cfg for two_step_task
cfg = list(
  Nsubjects        = 20,
  Nblocks          = 4,
  Ntrials          = 50,
  Nstates          = 2, 
  Narms            = 2, #number of arms in the task
  Nraffle          = 2 #number of arms offered for selection each trial
)
rndwlk=randomwalk(Narms=cfg$Narms,Ntrials=cfg$Ntrials,save_text=F,save_csv=F) #for two_step_task
cfg$rndwlk=rndwlk

#####Simulate data--------------------
generate_artificial_data(cfg = cfg) #update the variables saved for the stan matrix inside this function.
load(paste0(path$data,'/artificial_data.Rdata'))
#####sample posterior--------------------

modelfit_compile(path, format = F)

modelfit_mcmc(
  path,
  data_path = paste0('data/stan_ready_data_files/artificial_standata_', path$name, '.Rdata'),
  
  mymcmc = list(
    datatype = 'artificial' ,
    samples  = 500,
    warmup  = 500,
    chains  = 4,
    cores   = 4,
    refresh = 10
  )
)

#####examine results--------------------
mypars = c("population_locations[1]",
           "population_locations_transformed[1]")

examine_mcmc(path, mypars, datatype = 'artificial')

examine_population_parameters_recovery(path, datatype = 'artificial',ncolumns = 2,format="beta")

examine_individual_parameters_recovery(path,ncolumns=2)


####examine model
#load parameters
fit   = readRDS(paste0(path$data, '/modelfit_recovery.rds'))
fit   = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
Qdiff = fit$draws(variables = 'Qdiff_external', format = 'draws_matrix')
Qval1 = fit$draws(variables = 'Qval1_external', format = 'draws_matrix')
Qval2 = fit$draws(variables = 'Qval2_external', format = 'draws_matrix')
Qval3 = fit$draws(variables = 'Qval3_external', format = 'draws_matrix')
Qval4 = fit$draws(variables = 'Qval4_external', format = 'draws_matrix')

PE    = fit$draws(variables = 'PE_external', format = 'draws_matrix')
