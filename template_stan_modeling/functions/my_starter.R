source('./functions/check_packages.r')


#load project packages
library(cmdstanr)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(svDialogs)
library(ggdist)
library(ggpubr)
library(ramify)
library(patchwork)
library(brms)
library(jsonlite)

source('./stan_modeling/functions/simulate_parameters.R')
source('./stan_modeling/functions/simulate_create_artifical_data.R')
source('./stan_modeling/functions/simulate_convert_to_stan_format.R')
source('./stan_modeling/functions/modelfit_compile.R')
source('./stan_modeling/functions/modelfit_mcmc.R')
source('./stan_modeling/functions/examine_mcmc.R')
source('./stan_modeling/functions/examine_population_parameters_recovery.R')
source('./stan_modeling/functions/examine_individual_parameters_recovery.R')
source('./stan_modeling/functions/generate_individual_parameters.r')
source('./stan_modeling/functions/generate_artificial_data.r')


source('./functions/set_workingmodel.r')
source('./functions/add_workingmodel.r')
source('./functions/my_xyplot.R')
source('./functions/remove_workingmodel.r')
source('./functions/plot_artificial_parameters.r')
source('./functions/add_standata_file.r')
source('./functions/set_standata_file.R')
source('./functions/update_standata_files.R')
source('./functions/randomwalk.R')
update_standata_files()






