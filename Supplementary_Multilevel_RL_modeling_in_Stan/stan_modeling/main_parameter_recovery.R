#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()


#####Simulate data--------------------

#task variables
cfg = list(
  Nsubjects        = 20,
  Nblocks          = 4,
  Ntrials          = 50,
  rndwlk           = randomwalk(Narms = 8, Ntrials = 50 , save_text=F , save_csv=F) 
)


#generate parameters and behavior
generate_artificial_data(cfg = cfg) 

#load to examine df
load(paste0(path$data,'/artificial_data.Rdata'))

#data in stan format for two_step_task
simulate_convert_to_standata(path,cfg,
                             
                             var_toinclude  = c(
                               'first_trial_in_block',
                               'trial',
                               #'choice',
                               'choice1',
                               'choice2',
                               'choice3',
                               #'state',
                               'state1',
                               'state2',
                               'state3',
                               'reward',
                               'fold')
)

#load to examine df
load(paste0(path$data,'/artificial_standata.Rdata'))



#####sample posterior--------------------

modelfit_compile(path, format = F)

modelfit_mcmc(path,  mymcmc = list(
    datatype = 'artificial' ,
    samples  = 500,
    warmup  = 500,
    chains  = 4,
    cores   = 4,
    refresh = 10
  )
)


##### examine results ----
fit   = readRDS(paste0(path$data, '/modelfit_recovery.rds'))
print(fit)

examine_mcmc(path, mypars = c("population_locations[1]"), datatype = 'artificial')

examine_population_parameters_recovery(path, datatype = 'artificial',ncolumns = 2,format="beta")

examine_individual_parameters_recovery(path,ncolumns=2)


