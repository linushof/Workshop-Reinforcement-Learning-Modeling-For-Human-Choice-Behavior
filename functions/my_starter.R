source('./functions/check_packages.r')


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

source('./functions/simulate_parameters.R')
source('./functions/simulate_create_artifical_data.R')
source('./functions/simulate_convert_to_stan_format.R')
source('./functions/modelfit_compile.R')
source('./functions/modelfit_mcmc.R')
source('./functions/examine_mcmc.R')
source('./functions/examine_population_parameters_recovery.R')
source('./functions/examine_individual_parameters_recovery.R')
source('./functions/generate_individual_parameters.r')
source('./functions/generate_artificial_data.r')

source('./functions/set_workingmodel.r')
source('./functions/randomwalk.R')
source('./functions/remove_workingmodel.r')
source('./functions/run_fit.r')
source('./functions/plot_artificial_parameters.r')
source('./functions/set_datatype.r')
source('./functions/set_data.r')
source('./functions/add_standata_file.r')
source('./functions/set_standata_file.R')
source('./functions/update_standata_files.R')
update_standata_files()






