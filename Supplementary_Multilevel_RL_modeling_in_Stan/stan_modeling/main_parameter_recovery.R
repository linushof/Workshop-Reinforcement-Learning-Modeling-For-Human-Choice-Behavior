#####Setup--------------------
rm(list = ls())
#setwd('./Supplementary_Multilevel_RL_modeling_in_Stan/')
source('./functions/my_starter.R')

path = set_workingmodel()


#cfg for MAB
#cfg for sequence
cfg = list(Nsubjects       =20, #dim(model_parameters$artificial_individual_parameters)[1],
           Nstages         = 3,
           Nblocks         =2,
           Ntrials_perblock=100,
           Narms           =2,  #number of arms in the task 
           Nstates         =4,
           Nstages         =3,
           rndwlk          =read.csv('./stan_modeling/models/sequence_learning/rndwlk_depth3_100trials.csv',header=F))


#cfg for two_step_task
cfg = list(
  Nsubjects        = 20,
  Nblocks          = 4,
  Ntrials          = 50,
  Nstates          = 2, 
  Narms            = 2, #number of arms in the task
  Nstages          = 1,
  Nraffle          = 2 #number of arms offered for selection each trial
)
rndwlk=randomwalk(Narms=cfg$Narms,Ntrials=cfg$Ntrials,save_text=F,save_csv=F) #for two_step_task
cfg$rndwlk=rndwlk

#####Simulate data--------------------
generate_artificial_data(cfg = cfg) #update the variables saved for the stan matrix inside this function.

#data in stan format for sequence learning
simulate_convert_to_standata(path,cfg,
                             
                             var_toinclude  = c(
                               'first_trial_in_block',
                               'trial',
                               'choice',
                               'reward',
                               'state1',
                               'state2',
                               'state3',
                               'choice1',
                               'choice2',
                               'choice3',
                               'choice1_oneback')
)


#data in stan format for two_step_task
simulate_convert_to_standata(path,cfg,
                             
                             var_toinclude  = c(
                               'first_trial_in_block',
                               'trial',
                               'choice',
                               'reward',
                               'fold')
)




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
mypars = c("population_locations[1]")

#add the variables you need, for example: "population_locations_transformed[1]"

examine_mcmc(path, mypars, datatype = 'artificial')

#change to logit/beta/none
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
