#This code plot recovered parameters against the true parameters

compare_models <-function(path){
#--------------------------------------------------------------------------------------------------------
data_type=set_datatype()
load('functions/working_model.rdata')
model1=dlg_list(mymodels_list, multiple = F,title="Pick model 1")$res
if (data_type=="artificial"){
load(paste0('data/stanmodel_',model1,'/modelfit_like_per_trial_recovery.rdata'))
}
if (data_type=="empirical"){
  load(paste0('data/stanmodel_',model1,'/modelfit_like_per_trial_empirical.rdata'))
}
dim(like)
model1=loo(like)

model2=dlg_list(mymodels_list, multiple = F,title="Pick model 2")$res
if (data_type=="artificial"){
  load(paste0('data/stanmodel_',model2,'/modelfit_like_per_trial_recovery.rdata'))
}
if (data_type=="empirical"){
  load(paste0('data/stanmodel_',model2,'/modelfit_like_per_trial_empirical.rdata'))
}
dim(like)
model2=loo(like)

print(loo_compare(model1,model2))
loo_compare(model1,model2)

}