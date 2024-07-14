#this code generates from a gui select list data_path and model_path for a working model

set_workingmodel<-function(){
#load path string for the model you are working on
#note that in order no to create code duplicates you can
#change the model name in 'working model.txt'
load('two_step_task/R/functions/working_model.rdata')



mymodel   =dlg_list(mymodels_list, multiple = TRUE)$res
data_path =paste0('two_step_task/R/data/stanmodel_',mymodel)
model_path=paste0(mymodel,'/R/',mymodel)
cat(paste0(mymodel,
           ' is the current working model',
           '\n',
          '\ndata  folder: ',data_path,
          '\nmodel folder: ',model_path))
mypath=list(data=data_path,model=model_path,name=mymodel)
return(mypath)
}

