#### this function opens a dialog allowing the enter the name of a data file ready for stan
#### the new file is then updated.
### First save the file in 'data/empirical_data_files' and then add it's name.

add_standata_file<-function(file_name){
  load('data/stan_ready_data_files/standata_files.Rdata')
  
  if (!(file_name %in% standata_files))
    {
      standata_files=c(standata_files,file_name)
      
      save(standata_files, file = "data/stan_ready_data_files/standata_files.rdata")
      
  }
}


