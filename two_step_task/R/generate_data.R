#####Setup--------------------
rm(list = ls())

source('two_step_task/R/functions/my_starter.R')
path = set_workingmodel()

rndwlk=randomwalk(Narms=4,Ntrials=1000,save_text=F,save_csv=F) #for two_step_task

cfg = list(
  Nsubjects        = 10,
  Nblocks          = 4,
  Ntrials          = 20,
  Nstates          = 2,
  Narms            = 2, #number of arms in the task
  Nraffle          = 2, #number of arms offered for selection each trial
  rndwlk           = rndwlk
)


#####Simulate data--------------------
generate_artificial_data(cfg = cfg)
load(paste0(path$data,'/artificial_data.Rdata'))