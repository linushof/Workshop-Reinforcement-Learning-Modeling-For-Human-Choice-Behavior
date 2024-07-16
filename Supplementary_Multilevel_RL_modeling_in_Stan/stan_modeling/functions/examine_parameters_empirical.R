#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')
mydatatype=set_datatype()


#--------------------------------------------------------------------------------------------------------
#load recovered parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

#load artificial parameters
load(paste0(path$data,'/model_parameters.Rdata'))


#--------------------------------------------------------------------------------------------------------

#population level parameters
my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[1]',
                                               format='draws_matrix')),
                     myxlim  = c(0,1),
                     my_vline= 0, 
                     myxlab  = expression(alpha['location']),
                     mycolor = "pink")

my_posteriorplot(x       = (fit$draws(variables ='population_locations[2]',
                                            format='draws_matrix')),
                 myxlim  = c(0,10),
                 my_vline= 0, 
                 myxlab  = expression(beta['location']),
                 mycolor = "pink")

my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[3]',
                                            format='draws_matrix')),
                 myxlim  = c(0,1),
                 my_vline= 0, 
                 myxlab  = expression(rho['location']),
                 mycolor = "pink")

my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[4]',
                                            format='draws_matrix')),
                 myxlim  = c(0,1),
                 my_vline= 0, 
                 myxlab  = expression(alpha['location']),
                 mycolor = "pink")

my_posteriorplot(x       = fit$draws(variables ='population_locations[5]',
                                            format='draws_matrix'),
                 myxlim  = c(0,10),
                 my_vline= 0, 
                 myxlab  = expression(alpha['location']),
                 mycolor = "pink")
#--------------------------------------------------------------------------------------------------------
mean(plogis(fit$draws(variables ='population_locations[1]',
                     format='draws_matrix')))
hdi(plogis(fit$draws(variables ='population_locations[1]',
                     format='draws_matrix')))
mean((fit$draws(variables ='population_locations[2]',
                        format='draws_matrix')))
mean((fit$draws(variables ='population_scales[2]',
                      format='draws_matrix')))
hdi(plogis(fit$draws(variables ='population_locations[2]',
                     format='draws_matrix')))
median((fit$draws(variables ='population_locations[3]',
                        format='draws_matrix')))
hdi((fit$draws(variables ='population_locations[3]',
                     format='draws_matrix')))

# individual level parameters

mean(fit$draws(variables ='population_locations[1]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[1]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[1]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[2]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[2]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[2]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[3]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[3]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[3]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[4]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[4]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[4]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[5]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[5]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[5]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[6]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[6]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[6]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[7]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[7]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[7]',format='draws_matrix'))

mean(fit$draws(variables ='population_locations[8]',format='draws_matrix'))
sd(fit$draws(variables ='population_locations[8]',format='draws_matrix'))
hdi(fit$draws(variables ='population_locations[8]',format='draws_matrix'))

