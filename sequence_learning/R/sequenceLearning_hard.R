# Simulation code for sequence learning
rm(list=ls())

#pre-allocation
subject = 1
rndwlk = read.csv('sequence_learning/R/data/rndwlk_depth3_100trials.csv',header=F)

#set parameters
alpha  = 0.5 
beta   = 3
lambda = 0.3 

#set initial var
Narms              = 2 
Nstages            = 3 
Nblocks            = 2 
Nstates            = 8 
Ntrials_perblock   = 100 
expvalues          = rndwlk
rownames(expvalues)= c('ev1','ev2','ev3','ev4','ev5','ev6','ev7','ev8')
Qval               = as.matrix(t(rep(0,Narms)))
colnames(Qval)     = sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
df                 = data.frame()

for (block in 1:Nblocks){
  
  Qval = array(0.5, dim = c(Narms, Nstates, Nstages))
  
  for (trial in 1:Ntrials_perblock){
    
    
    #simulate agent's actions according to a softmax policy
    state1  = 1
    p_1     = ______
    choice1 = ______
    
    state2  = choice1
    p_2     = ______
    choice2 = ______
    
    state3  = -2 + 2*choice1 + choice2
    p_3     = ______
    choice3 = ______
    
    #simulate outcome
    reward = sample(c(0,1),1,prob=c(1-expvalues[-2 + 2*state3 + choice3,trial],expvalues[-2 + 2*state3 + choice3,trial]))
    
    
    
    #save trial's data
    
    #create data for current trials
    dfnew=data.frame(
      subject              = subject,
      block                = block,
      trial                = trial,
      first_trial_in_block = (trial==1)*1,
      choice1              = choice1,
      choice2              = choice2,
      choice3              = choice3,
      state1               = state1,
      state2               = state2,
      state3               = state3,
      expval_ch            = expvalues[-2 + 2*state3 + choice3,trial],
      reward               = reward
    )
    
    #dfnew=cbind(dfnew,Qval)
    dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
    
    #bind to the overall df
    df=rbind(df,dfnew)
    
    #update Qvalues
    PE_1 = ______
    PE_2 = ______
    PE_3 = ______
    
    Qval[choice1, state1, 1] = ______
    
    Qval[choice2, state2, 2] = ______
    
    Qval[choice3, state3, 3] = ______
    
    
  }
}     
save(df, file="sequence_learning/R/data/sequenceLearning_simulatedData.Rdata")

#### Save in Stan format ####
make_mystandata<-function(data, subject_column,block_column,var_toinclude,var_tobenamed,additional_arguments){
  
  
  #create subjects list (only unique values)
  subjects_list      =unique(subject_column)
  blocks_list        =unique(block_column)
  
  #create an Ntrials_per_subject vector showing the number of trials for each subject
  Ntrials_per_subject           =sapply(1:length(subjects_list), function(i) {sum(subject_column==subjects_list[i])})
  
  Ntrials_per_subject_per_block =sapply(1:length(blocks_list), function(j) {
    sapply(1:length(subjects_list), function(i) {sum(subject_column==subjects_list[i] & block_column==j)})})
  
  #find the largest number of available data per subject
  max_trials_per_subject=max(Ntrials_per_subject)
  
  #loop over the variables that needs to be included
  mydata<-lapply(var_toinclude,function(myvar) { 
    
    #for each variable, loop over all subjects to create a padded matrix
    t(sapply(subjects_list,function(subject) 
      
    { #create vector for a specific variable and subject
      current_var=data[subject_column==subject,myvar]
      # data padding with Inf according to the max number of trials across subjects
      c(current_var,rep(9999,max_trials_per_subject-sum(subject_column==subject)))})) 
    
  }
  )
  #add variables names
  if (missing(var_tobenamed)==T) {names(mydata)=var_toinclude}
  if (missing(var_tobenamed)==F) {names(mydata)=var_tobenamed}
  
  #add additional variables
  
  mydata=append(list(Nsubjects                    =length(subjects_list), 
                     Nblocks                      =length(blocks_list),
                     Ntrials                      =max_trials_per_subject,  
                     Ntrials_per_subject          =Ntrials_per_subject,
                     Ntrials_per_subject_per_block=Ntrials_per_subject_per_block),
                #fold=t(matrix(block_column,nrow=Ntrials_per_subject,ncol=length(subjects_list)))),
                mydata)
  
  if (missing(additional_arguments)==F) {mydata=append(mydata,additional_arguments)}
  
  return(mydata)
}

data_for_stan<-make_mystandata(data=df, 
                               subject_column     =df$subject,
                               block_column       =df$block,
                               var_toinclude      =c(
                                 'first_trial_in_block',
                                 'trial',
                                 'state1',
                                 'state2',
                                 'state3',
                                 'choice1',
                                 'choice2',
                                 'choice3',
                                 'choice1_oneback',
                                 'reward'),
                               additional_arguments=list(Nstages=3,Nstates=4))

#save
save(data_for_stan, file="sequence_learning/R/data/sequenceLearning_data_stan_format.Rdata")


