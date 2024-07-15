#### this function opens a dialog allowing the enter the name of a new model
#### the new models is then updated in three locations: the "working_model.txt" files, models and data directories.

add_workingmodel<-function(){
load('functions/working_model.rdata')
new_modelname=dlg_input(message = "Enter name for your new model:")

mymodels_list=c(mymodels_list,new_modelname$res)

dir.create(paste0('data/stanmodel_',new_modelname$res))
dir.create(paste0('stan_modeling/models/',new_modelname$res))


save(mymodels_list,file='functions/working_model.rdata')
}

