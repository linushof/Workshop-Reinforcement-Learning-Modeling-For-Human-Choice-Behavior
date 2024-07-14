#this code generates from a gui select list data_path 

set_data<-function(){
  #load path string for the data  you are working on
  load('./functions/working_model.rdata')
  
  mymodel   =dlg_list(mymodels_list, multiple = F,title="Which simulated data are you using?")$res
  data_path =paste0('./data/stanmodel_',mymodel)
  cat(paste0(mymodel,
             ' is the current data used',
             '\n',
             '\ndata  folder: ',data_path))
  return(data_path)
}

