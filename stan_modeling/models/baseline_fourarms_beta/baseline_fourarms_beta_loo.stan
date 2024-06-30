data {
  
  //General fixed parameters for the experiment/models
  
  int<lower=1> Nsubjects;
  
  int<lower=1> Nblocks;
  
  int<lower=1> Ntrials;
  
  array[Nsubjects] int<lower=1> Ntrials_per_subject;
  
  int<lower=4> Narms;
  
  int<lower=2> Nraffle;
  
  array[Nsubjects,Ntrials] int<lower = 0> fold;
  real testfold;
  //Behavioral data:
  
  array[Nsubjects, Ntrials] int<lower=0> choice;
  
  array[Nsubjects, Ntrials] int<lower=0> reward;
  
  array[Nsubjects, Ntrials] int<lower=0> offer1;
  
  array[Nsubjects, Ntrials] int<lower=0> offer2;
  
  array[Nsubjects, Ntrials] int<lower=0> selected_offer;
  
  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;
  
}


transformed data{
  int<lower=1> Nparameters = 2;
}




parameters {
  //population level parameters 
  
  vector[1] population_locations;
  vector <lower=0,upper=1>[1] population_locations_transformed;
  
  vector<lower=0>[Nparameters] population_scales;
  
  
  
  //individuals level
  vector <lower=0, upper=1>[Nsubjects] alpha;
  vector [Nsubjects] beta;
}


transformed parameters {
  
  //vector<lower=0, upper=1>[Nsubjects] alpha;
  //vector  <lower=0>                [Nsubjects] beta;
  matrix                  [Ntrials,Nsubjects] p_ch_action;
  matrix                  [Ntrials,Nsubjects] Qdiff_external;
  matrix                  [Ntrials,Nsubjects] Qval1_external;
  matrix                  [Ntrials,Nsubjects] Qval2_external;
  matrix                  [Ntrials,Nsubjects] Qval3_external;
  matrix                  [Ntrials,Nsubjects] Qval4_external;
  matrix                  [Ntrials,Nsubjects] PE_external;
  
  
  
  //RL
  for (subject in 1:Nsubjects) {
    //internal variabels
    
    real Qdiff;
    
    real PE;
    
    array[Narms] real Qval;
    
    
    //likelihood estimation
    for (trial in 1:Ntrials_per_subject[subject]){
      
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
        Qval = rep_array(0.5, Narms);
      }
      
      //calculate probability for each action
      Qdiff        = Qval[offer2[subject,trial]]- Qval[offer1[subject,trial]];
      
      p_ch_action[trial,subject] = inv_logit(beta[subject]*Qdiff);
      
      //update Qvalues
      PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
      Qval[choice[subject,trial]] = Qval[choice[subject,trial]]+alpha[subject]*PE;
      
      //appened to external variabels
      Qdiff_external[trial,subject] = Qdiff;
      Qval1_external[trial,subject] = Qval[1];
      Qval2_external[trial,subject] = Qval[2];
      Qval3_external[trial,subject] = Qval[3];
      Qval4_external[trial,subject] = Qval[4];
      PE_external[trial,subject]    = PE;
      
      
    }
    
  }
  
}


model {
  
  // population level
  population_locations_transformed[1]  ~ beta(2,2);
  population_scales[1]     ~ gamma(4,0.1);


  population_locations[1]  ~ normal(0,4);
  population_scales[2]     ~ normal(0,2);

  for (subject in 1:Nsubjects){

    for (trial in 1:Ntrials_per_subject[subject]){
    target+= beta_proportion_lpdf(alpha[subject]|population_locations_transformed[1], population_scales[1]);
    target+= normal_lpdf(beta[subject]|population_locations[1] , population_scales[2]);
    target+= bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qdiff_external[trial,subject]);
    }
  }
}


generated quantities {

  matrix[Ntrials,Nsubjects]  log_lik;
      
    matrix[Ntrials, Nsubjects] Qdiff_g;
    
    real PE_g;
    
    array[Narms] real Qval_g;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
log_lik=rep_matrix(0,Ntrials,Nsubjects);
    for (subject in 1:Nsubjects) {
 
        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {
 
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
         Qval_g = rep_array(0.5, Narms);
      }
        Qdiff_g[trial,subject]        = Qval_g[offer2[subject,trial]]- Qval_g[offer1[subject,trial]];
      
      if (first_trial_in_block[subject, trial] != 1){
      log_lik[trial,subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]*Qdiff_g[trial,subject]);
}
      
      //update Qvalues
      PE_g  = reward[subject,trial]  - Qval_g[choice[subject,trial]];
      Qval_g[choice[subject,trial]] = Qval_g[choice[subject,trial]]+alpha[subject]*PE_g;
      
        }
        }
    }
}
