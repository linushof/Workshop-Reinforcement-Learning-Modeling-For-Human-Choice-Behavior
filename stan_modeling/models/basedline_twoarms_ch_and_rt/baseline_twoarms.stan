data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
  int<lower = 2> Narms;                                             
  int<lower = 2> Nraffle; 


  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];              
  int<lower = 0> reward[Nsubjects,Ntrials];              
  int<lower = 0> offer1[Nsubjects,Ntrials];              
  int<lower = 0> offer2[Nsubjects,Ntrials];              
  int<lower = 0> selected_offer[Nsubjects,Ntrials];      
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];

}


transformed data{
  int<lower = 1> Nparameters=2; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0, Narms);
}




parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;      
  vector<lower=0>[Nparameters] population_scales;         
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_random_effect;
}


transformed parameters {
  
  vector<lower=0, upper=1>[Nsubjects] alpha;
  vector                  [Nsubjects] beta;
  matrix                  [Nsubjects,Ntrials] p_ch_action;
  matrix                  [Nsubjects,Ntrials] Qdiff;
  real   PE;
	real   Qval[Narms]; 

  //RL
  for (subject in 1:Nsubjects) {
    
    //set indvidual parameters
    alpha[subject]   = inv_logit(population_locations[1]  + population_scales[1] * alpha_random_effect[subject]);
    beta[subject]    =          (population_locations[2]  + population_scales[2] * beta_random_effect [subject]);
    
        //likelihood estimation
        for (trial in 1:Ntrials_per_subject[subject]){
        
        //reset Qvalues (first trial only)
    		if (first_trial_in_block[subject,trial] == 1) {
        Qval = rep_array(0, Narms);
    		}
        
        //calculate probability for each action
        Qdiff[subject,trial]       = Qval[2]- Qval[1];

        p_ch_action[subject,trial] = inv_logit(beta[subject]*Qdiff[subject,trial]);
        
        //update Qvalues
        PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
        Qval[choice[subject,trial]] = Qval[choice[subject,trial]]+alpha[subject]*PE;
      }
 
  }

}


model {
  
  // population level  
  population_locations  ~ normal(0,2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_random_effect ~ std_normal();
  beta_random_effect  ~ std_normal();
 

  for (subject in 1:Nsubjects){
    target+= bernoulli_logit_lpmf(selected_offer[subject,]|beta[subject]*Qdiff[subject,]);
  }
}
