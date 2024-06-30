simulate_convert_to_standata <-function (path,cfg,var_toinclude){

  source('./functions/make_mystandata.R')


  #load artificial data
  load(paste0(path$data,'/artificial_data.Rdata'))


  #convert
  df$fold=df$block
  data_for_stan<-make_mystandata(data                 = df, 
                                 subject_column       = df$subject,
                                 block_column         = df$block,
                                 var_toinclude        = var_toinclude,
                                 additional_arguments = list(
                                   Narms  = cfg$Narms, 
                                   Nraffle= cfg$Nraffle))

  #save
  save(data_for_stan,file=paste0('data/stan_ready_data_files/artificial_standata_', path$name, '.Rdata'))
  cat(paste0('[stan_modeling]:  "artificial_standata_',path$name,'.Rdata" was saved at stan_ready_data_files. \n Old model data file was overWritten.'))
  
  # add_standata_file(paste0('artificial_standata_', path$name, '.Rdata'))
  # cat(paste0('[stan_modeling]: Added "artificial_standata.Rdata" to model list'))
  
}