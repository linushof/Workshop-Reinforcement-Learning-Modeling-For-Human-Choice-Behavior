#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')

# samples x observations

#--------------------------------------------------------------------------------------------------------
library(loo)

mymodel='null'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
null=elpd(like)$pointwise[,1]

mymodel='double_updating'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
double_updating=elpd(like)$pointwise[,1]

mymodel='approach_avoid'
load(paste0('data/model_',mymodel,'/modelfit_like_per_trial.rdata'))
approach_avoid=elpd(like)$pointwise[,1]

#compare model trial-by-trial-----------------------------------------
library(dplyr)
library(tidyr)
load('./data/empirical_data/df.rdata')
df=cbind(df,null=(null),double_updating=(double_updating),approach_avoid=(approach_avoid))
names(df)
df%>%mutate(elpd_diff_winning_against_second_best=approach_avoid-double_updating,
            elpd_diff_winning_against_null=approach_avoid-null)%>%
     group_by(reoffer_ch,reoffer_unch)%>%
     summarise(mean(elpd_diff_winning_against_null),
               mean(elpd_diff_winning_against_second_best))

t.test(df$null[!df$reoffer_ch&df$reoffer_unch],df$approach_avoid[!df$reoffer_ch&df$reoffer_unch],paired=T)
t.test(df$null[!df$reoffer_unch],df$approach_avoid[!df$reoffer_unch],paired=T)

t.test(df$null[df$reoffer_ch&df$reoffer_unch],df$double_updating[df$reoffer_ch&df$reoffer_unch],paired=T)
t.test(df$null[!df$reoffer_ch&df$reoffer_unch],df$double_updating[!df$reoffer_ch&df$reoffer_unch],paired=T)



