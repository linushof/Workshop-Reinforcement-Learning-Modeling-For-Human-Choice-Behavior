#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')
mydatatype=set_datatype()


#--------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(bayestestR)
library(stringr)


#load recovered parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

#load artificial parameters
load(paste0(path$data,'/model_parameters.Rdata'))


#--------------------------------------------------------------------------------------------------------

#population level parameters
my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[1]',
                                               format='draws_matrix')),
                     myxlim  = c(0,1),
                     my_vline= model_parameters$artificial_population_location[1], 
                     myxlab  = expression(alpha['location']),
                     mycolor = "pink")


my_posteriorplot(x       = fit$draws(variables ='population_locations[2]',
                                        format='draws_matrix'),
                     myxlim  = c(0.5,5),
                     my_vline= model_parameters$artificial_population_location[2], 
                     myxlab  = expression(beta['location']),
                     mycolor = "pink")

#--------------------------------------------------------------------------------------------------------

# individual level parameters

apply(fit$draws(variables ='alpha',format='draws_matrix'), 2, mean)
apply(fit$draws(variables ='beta' ,format='draws_matrix'), 2, mean)

