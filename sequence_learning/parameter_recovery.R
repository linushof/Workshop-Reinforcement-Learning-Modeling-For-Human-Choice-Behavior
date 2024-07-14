#aim: compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)
rm(list=ls())
library(rstan) 
library(cmdstanr)

set_cmdstan_path(path = NULL)

my_compiledmodel <- cmdstan_model('sequence_learning/model/alpha_lambda_beta_model.stan')
save(my_compiledmodel,file = 'sequence_learning/model/alpha_lambda_beta_modelfit_compile.rdata')


#--------------------------------------------------------------------------------------------------------

#aim: Hierarchical fit Stan 
#load model
load('sequence_learning/model/alpha_lambda_beta_modelfit_compile.rdata')

# load data
load('sequence_learning/data/data_stan_format.Rdata')

#sample
fit<- my_compiledmodel$sample(
  data = data_for_stan, 
  iter_sampling = 1000,
  iter_warmup = 2000,
  chains           = 4,
  parallel_chains  = 4)  


#save
fit$save_object('sequence_learning/data/modelfit_data.rds')

library(posterior)
pars <- as_draws_df(fit$draws())
save(pars, file = 'sequence_learning/data/modelfit_data.Rdata')

#--------------------------------------------------------------------------------------------------------
#This code plot recovered parameters against the true parameters
library(bayestestR)

my_posteriorplot<-function(x_data,myxlabel,mycolor,myxlim,myylim,my_vline){
  
  
  ggplot(data.frame(x=x_data),aes(x=x_data))+geom_density(alpha = .5,fill=mycolor)+ 
    geom_vline(xintercept = my_vline, linetype="dotted",color = "blue", size=1.5)+
    geom_segment(aes(x = (hdi(x_data, ci = 0.95))$CI_low, y = 0, xend = (hdi(x_data, ci = 0.95))$CI_high, yend = 0),
                 color="darkgray",size=2,show.legend = F)+
    xlim(myxlim[1],myxlim[2])+ ylim(myylim[1],myylim[2]) + 
    xlab(myxlabel) + theme_classic()
  
}

p1 = my_posteriorplot(x_data = plogis(pars$`population_locations[1]`),
                      myxlim = c(0,1),
                      myylim = c(0,17),
                      my_vline =0.5, 
                      myxlabel = expression(alpha['location']),
                      mycolor = "#6699EE")

p1

p4 = my_posteriorplot(x_data       = pars$`population_locations[2]`,
                      myxlim  = c(0,6),
                      myylim =c(0,3),
                      my_vline= 3, 
                      myxlabel  = expression(beta['location']),
                      mycolor = "#FF8C42")

p4


p7 = my_posteriorplot(x_data       = plogis(pars$`population_locations[3]`),
                      myxlim  = c(0,1),
                      myylim =c(0,13),
                      my_vline= 0.3, 
                      myxlabel  = expression(lambda['location']),
                      mycolor = "purple")

p7

my_xyplot<-function(x,y,myxlab,myylab,mycolor){
  p =
    ggplot(data.frame(x =x, y =y), aes(x=x,y=y))+
    
    geom_point(col=mycolor,alpha=0.7)+
    
    ggtitle('',subtitle = paste('r=',round(cor(x,y),3)))+
    
    xlab(myxlab)+ylab(myylab)+ 
    
    #xlim(0,1)+ylim(0,1)+
    theme_classic()
  
  return(p)
  
}
pr1=my_xyplot(0.5,apply(fit$draws(variables ='alpha',format='draws_matrix'), 2, mean),'alpha true','alpha recovered','navy')#+geom_smooth(method = "lm", se = FALSE)
pr1

pr2=my_xyplot(3,apply(fit$draws(variables ='beta',format='draws_matrix'), 2, mean),'beta true','beta recovered','navy')#+geom_smooth(method = "lm", se = FALSE)
pr2

pr3=my_xyplot(0.3,apply(fit$draws(variables ='lambda',format='draws_matrix'), 2, mean),'lambda true','lambda recovered','navy')#+geom_smooth(method = "lm", se = FALSE)
pr3

