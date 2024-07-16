get_parameters <- function(mydatatype,path){
    
  load(paste0(path$data,'/model_parameters.Rdata'))
  return(model_parameters[["artificial_individual_parameters"]])

}
