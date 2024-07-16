#Aim:
#This code generate artificial model parameters in an hierarchical structure
#it works based on the definition in "_artificial_parameters.r" file that you need
#to place in the specific model folder

simulate_parameters <-function(path,cfg,plotme){

  #load the parameter setup  
  source(paste0(path$model,'_parameters.r'))

  #simulate parameters
  model_parameters=generate_individual_parameters(model_parameters,cfg$Nsubjects)

  #plot
  if (plotme){
    plot_artifical_parameters(model_parameters, plot_method='dot' )#plotme can be 'dot', 'hist' or 'density'.)
  }

  #save
  save(model_parameters,file=paste0(path$data,'/model_parameters.Rdata'))
  cat(paste0('[stan_modeling]:  "model_parameters.Rdata" was saved at "',path$data,'"'))
}
