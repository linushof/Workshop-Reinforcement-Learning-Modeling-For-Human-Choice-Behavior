get_df <- function(mydatatype,path,standata){
  
  if(standata == F){
    
    file_name = paste0(mydatatype, '_data.Rdata')
    load(paste0(path$data,'/', file_name))
    
    cat(file_name)
    return(df)
  }
  
  if (standata == T) {
    
    load(paste0(path$data,'/artificial_standata.Rdata'))
    
    return(data_for_stan)
  }
  
}