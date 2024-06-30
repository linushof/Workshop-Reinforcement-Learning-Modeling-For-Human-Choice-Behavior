#Aim: 
#this functions take 'artificial_parameters' list with information regarding
#model parameters (names, population locations, scales and transformation)
#and generate individual parameters

generate_individual_parameters=function(model_parameters,Nsubjects,plotme){
  
  #-----------------------------------------------------------
  #sample individual parameters based on the population definitions in 'artifical parameters'  
  #pre-allocation
  x=matrix(data = NA, nrow = Nsubjects, ncol = length(model_parameters$names),)
  
  #sample individual parameters
  for (p in 1:length(model_parameters$names)){
    
    #no transformation
    if(model_parameters$transformation[p]=='none'){
      if(is.na(model_parameters$artificial_population_scale[p])) {
        
        x[,p]=model_parameters$artificial_population_location[p]
        
      } else {
        x[,p]=(model_parameters$artificial_population_location[p]+
                 model_parameters$artificial_population_scale[p]*rnorm(Nsubjects))
      }
    }
    
    
    #logit transformation (between 0 to 1)
    if(model_parameters$transformation[p]=='beta'){
      if(is.na(model_parameters$artificial_population_scale[p])) {
        
        x[,p]=model_parameters$artificial_population_location[p]
        
      } else {
        mean = model_parameters$artificial_population_location[p] #just for convenience
        sd = model_parameters$artificial_population_scale[p]
        #transform mean and sd to a and b parameters of beta distribution for R sampling.
        a = mean * ((mean * (1 - mean) / sd^2) - 1)
        b = (1 - mean) * ((mean * (1 - mean) / sd^2) - 1)
        x[,p]=rbeta(Nsubjects,a,b)
      }
    }
    
    
    #logit transformation (between 0 to 1)
    if(model_parameters$transformation[p]=='logit'){
      if(is.na(model_parameters$artificial_population_scale[p])) {
        
        x[,p]=plogis(qlogis(model_parameters$artificial_population_location[p]))
        
      } else {
        logit_mean = qlogis(model_parameters$artificial_population_location[p])
        logit_sd   = qlogis(model_parameters$artificial_population_location[p] + model_parameters$artificial_population_scale[p]) - qlogis(model_parameters$artificial_population_location[p])
        x[,p]=plogis(logit_mean + logit_sd*rnorm(Nsubjects))
      }
    }
    
    #exp transformation (>0)
    #also - this actually dont do anything (the number are not going to change here)
    #this is just as a reminder
    if(model_parameters$transformation[p]=='exp'){
      if(is.na(model_parameters$artificial_population_scale[p])) {
        
        x[,p]=log(exp(model_parameters$artificial_population_location[p]))
        
      } else {
        x[,p]=log(exp(model_parameters$artificial_population_location[p]+
                      model_parameters$artificial_population_scale[p]*rnorm(Nsubjects)))
      }
    }
    
  }
  
  #add columns names
  colnames(x)=model_parameters$names
  
  #assign back to the output file
  model_parameters$artificial_individual_parameters<-x
  
  return(model_parameters)
}
