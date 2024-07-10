# the idea is that stan will always estimate unbounded parameters.
# if we want to have the true parameters bounded in any way, we need to define a link function (transformation)
# here, you can define the mean and sd of the true parameters
# type of transformation:
# none 
# exp - you will get parameters in the mean and sd you define below, but bounded to be positive
# logit - you will get parameters in the mean and sd you define below, but bounded between 0 and 1. You should keep the mean between 0 and 1, 
#          and also make sure your sd is not to high, since the transformation generates Inf when reaching one or zero

model_parameters=list()
model_parameters$names                             =c('alpha','lambda','beta','omega')
model_parameters$transformation                    =c('beta','beta','none','beta')
model_parameters$artificial_population_location    =c(0.3,0.5,3,0.7)
model_parameters$artificial_population_scale       =c(0.001,0.001,0.001,0.001)
