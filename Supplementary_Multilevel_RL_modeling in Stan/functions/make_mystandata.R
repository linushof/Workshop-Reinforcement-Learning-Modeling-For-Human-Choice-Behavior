#aim: create data that can passed in to stan. Variables will be passed as subject x trial matrices.
#     These matrices are padded with Inf value to have exactly the same number of trials despite missing values
#contributors: Nitzan Shahar, Shira Niv
#inputs:
#data - is your data frame
#subject_column - is the specific vector of subjects from you data frame (e.g., data$subjectID)
#Ntrials_per_subject - is a vector 1 x Nsubj with each cell including the number of available trials per subject
#subjects_list - is a vector with the subjects id
#var_toinclude - is a vector of strings with the names of data columns that should be included in the stan data
#var_tobenamed - is a vector of strings for how to variables should be names according to stan model (in case the data and stan has different names for the same var)

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
