source('two_step_task/R/functions/check_packages.r')


#load project packages
library(cmdstanr)
library(posterior)
library(doParallel)
library(foreach)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(svDialogs)
library(gridExtra)
library(ggpubr)
library(grid)
library(jsonlite)

source('two_step_task/R/functions/simulate_parameters.R')
source('two_step_task/R/functions/simulate_create_artifical_data.R')
source('two_step_task/R/functions/simulate_convert_to_stan_format.R')
source('two_step_task/R/functions/modelfit_compile.R')
source('two_step_task/R/functions/modelfit_mcmc.R')
source('two_step_task/R/functions/examine_mcmc.R')
source('two_step_task/R/functions/examine_population_parameters_recovery.R')
source('two_step_task/R/functions/examine_individual_parameters_recovery.R')
source('two_step_task/R/functions/generate_individual_parameters.r')
source('two_step_task/R/functions/generate_artificial_data.r')

source('two_step_task/R/functions/randomwalk.R')
source('two_step_task/R/functions/run_fit.r')
source('two_step_task/R/functions/plot_artificial_parameters.r')
source('two_step_task/R/functions/set_datatype.r')
source('two_step_task/R/functions/set_data.r')
source('two_step_task/R/functions/set_standata_file.R')
source('two_step_task/R/functions/update_standata_files.R')
source('two_step_task/R/functions/set_workingmodel.R')
update_standata_files()






