#####Setup--------------------
rm(list = ls())

source('./functions/my_starter.R')
path = set_workingmodel()

rndwlk=randomwalk(Narms=4,Ntrials=500,save_text=F,save_csv=F) #for two_step_task

#cfg for MAB
#cfg for sequence
#cfg for two_step_task
cfg = list(
  Nsubjects        = 1,
  Nblocks          = 20,
  Ntrials          = 100,
  Nstates          = 2,
  Narms            = 2, #number of arms in the task
  Nraffle          = 2, #number of arms offered for selection each trial
  rndwlk           = rndwlk
)


#####Simulate data--------------------
generate_artificial_data(cfg = cfg)
load(paste0(path$data,'/artificial_data.Rdata'))
#####sample posterior--------------------

modelfit_compile(path, format = F)

modelfit_mcmc(
  path,
  data_path = paste0('data/stan_ready_data_files/artificial_standata_', path$name, '.Rdata'),
  
  mymcmc = list(
    datatype = 'artificial' ,
    samples  = 1000,
    warmup  = 1000,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("beta1[1]",
           "beta2[1]")

examine_mcmc(path, mypars, datatype = 'artificial')

