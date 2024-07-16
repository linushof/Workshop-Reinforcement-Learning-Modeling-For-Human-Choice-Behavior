#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  #...

  
  #set initial var
  df                 = data.frame()
  #...
  
for (###){
  
  #initialize values
  
  for (###){

    #define first stage Qmb values based on MF values of second stage and state transitions.
    
    #integrate MF and MB values modulated by omega parameter
    
    #make first-stage choice

    #sample state transition based on first stage choice

    #second stage choice

    
    #sample outcome
    if(###){
    }
    else{
    
    }
 
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
              ###
            )

      
       #bind to the overall df
       df=rbind(df,dfnew)
       
    #updating Qmf values
    
    #Prediction erros
    
    #second stage update
       
    #first stage update with an eligibility trace for the second-stage prediction error
    

  }
}     
  return (df)
}