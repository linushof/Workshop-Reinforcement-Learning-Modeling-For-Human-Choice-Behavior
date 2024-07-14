#### this function opens a dialog allowing the enter the name of a new model
#### the new models is then updated in three locations: the "working_model.txt" files, models and data directories.

remove_workingmodel<-function(){
load('functions/working_model.rdata')
mymodel      =dlg_list(title='select model to remove:',mymodels_list, multiple = TRUE)$res
mymodels_list=mymodels_list[mymodels_list!=mymodel]
print (paste0(mymodel,'was removed from the models list'))
save(mymodels_list,file='functions/working_model.rdata')
}

