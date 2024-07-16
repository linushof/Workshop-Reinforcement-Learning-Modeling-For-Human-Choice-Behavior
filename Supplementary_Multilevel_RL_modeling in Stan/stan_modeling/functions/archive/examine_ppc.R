#This code plot posterior predictive checks
#y_rep should be a matrix with samples x observations 
#(note - that samples = Niter*Nchains and observations = Nsubjects * Ntrials_per_subject)

rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')


#--------------------------------------------------------------------------------------------------------

#load stan object
fit=readRDS(paste0(path$data,'/modelfit_recovery.rds'))
load(paste0(path$data,'/artificial_data.Rdata'))

#ppc for stay effect
df=df%>%mutate(stay_card=(choice==lag(choice))*1,reward_oneback=lag(reward),reoffer_chosen = if_else(lag(choice)==offer1|lag(choice)==offer2,1,0))
df_reoffer_chosen = df%>%filter(reoffer_chosen==1 &trial!=1) #filter trials in which the previously chosen card was reoffered and trial number is not 1.
stay_rep=fit$draws(variables ='stay_rep',format="draws_matrix")
reoffer_index=(df$reoffer_chosen==1 & df$trial!=1)
df_stay_rep_reoffer_chosen = (df_reoffer_chosen$stay_card)*1 #simulated data
stay_rep_reoffer_chosen = stay_rep[,reoffer_index] #model data
group_vec=as.factor(df_reoffer_chosen$reward_oneback==1) #simulated group vector
ppc_stat_grouped(df_stay_rep_reoffer_chosen,stay_rep_reoffer_chosen,group_vec)

