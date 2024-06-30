#this code generates from a gui select list data_path and model_path for a working model

set_workingmodel<-function(){
#load path string for the model you are working on
#note that in order no to create code duplicates you can
#change the model name in 'working model.txt'
load('./functions/working_model.rdata')



mymodel   =dlg_list(mymodels_list, multiple = TRUE)$res
data_path =paste0('./data/stanmodel_',mymodel)
model_path=paste0('./stan_modeling/models/',mymodel,'/',mymodel)
model_name=mymodel
cat(paste0(mymodel,
           ' is the current working model',
           '\n',
          '\ndata  folder: ',data_path,
          '\nmodel folder: ',model_path))
mypath=list(data=data_path,model=model_path, name = model_name)
return(mypath)
}

