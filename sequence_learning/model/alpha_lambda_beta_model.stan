data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
                                       


  //Behavioral data:
  int<lower = 0> choice1[Nsubjects,Ntrials];           
  int<lower = 0> choice2[Nsubjects,Ntrials];           
  int<lower = 0> choice3[Nsubjects,Ntrials];           
  int<lower = 0> state1[Nsubjects,Ntrials];           
  int<lower = 0> state2[Nsubjects,Ntrials];           
  int<lower = 0> state3[Nsubjects,Ntrials];           
  int<lower = 0> reward[Nsubjects,Ntrials];           	  //reward outcome coded 0 or 1
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; //coding whether a trial is the first in a block to allow for Qval rest

}

transformed data{
  int<lower = 1> Nparameters = 3; 
}

parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;      
  vector<lower=0>[Nparameters] population_scales;         
  
  //individuals level
  vector [Nsubjects] alpha_random_effect;  //random effect for learning rate (alpha is declared in transformed parameters)
  vector [Nsubjects] beta_random_effect;   //noise parameter
  vector [Nsubjects] lambda_random_effect; //random effect for eligibility factor
}


transformed parameters {
  vector [Nsubjects] alpha; //learning rate parameter
  vector [Nsubjects] beta ; //noise parameter
  vector [Nsubjects] lambda; //eligibility parameter

  for (subject in 1:Nsubjects) {
    alpha[subject]  = inv_logit(population_locations[1]  + population_scales[1]  * alpha_random_effect[subject]);
    beta[subject]   =          (population_locations[2]  + population_scales[2]  * beta_random_effect[subject]);
    lambda[subject] = inv_logit(population_locations[3]  + population_scales[3]  * lambda_random_effect[subject]);
  }

}


model {
  
  // population level  
  population_locations ~ normal(0, 3);            
  population_scales    ~ cauchy(0,3);        

  // indvidual level  
  alpha_random_effect ~ std_normal();
  beta_random_effect  ~ std_normal();
  lambda_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    int ch1; 
  	int ch2; 
  	int ch3; 
  	int st1; 
  	int st2; 
  	int st3; 
    real PE1;
    real PE2;
    real PE3;
	  real Qval[2,4,3]; //number of arms x number of states x number of stages (for a tree of three stages the last stage will have 4 states)
    vector [2]Qnet;

    
 
      for (trial in 1:Ntrials_per_subject[subject]){
        	//reset Qvalues in the start of each block
    		if (first_trial_in_block[subject,trial] == 1) {
                  	  	Qval = rep_array(0, 2, 4, 3);
    		}

        //allocate choices
        ch1 = choice1[subject,trial];
        ch2 = choice2[subject,trial];
        ch3 = choice3[subject,trial];
        st1 = 1;
        st2 = state2[subject,trial];
        st3 = state3[subject,trial];
        	
        //calculate Qvalue in favor of empirical choice

        Qnet = to_vector(Qval[,st1,1]);
        ch1 ~ categorical_logit(beta[subject] * Qnet);

        Qnet = to_vector(Qval[,st2,2]);
        ch2 ~ categorical_logit(beta[subject] * Qnet);
        
        Qnet = to_vector(Qval[,st3,3]);
        ch3 ~ categorical_logit(beta[subject] * Qnet);
        
        //PE
        PE1 = Qval[ch2,st2,2] - Qval[ch1,st1,1];
        PE2 = Qval[ch3,st3,3] - Qval[ch2,st2,2];
        PE3 = reward[subject,trial]  - Qval[ch3,st3,3];
        
        Qval[ch1,st1,1] = Qval[ch1,st1,1]+alpha[subject]*PE1 + alpha[subject]*lambda[subject]*PE2 + alpha[subject]*(pow(lambda[subject],2))*PE3;
        Qval[ch2,st2,2] = Qval[ch2,st2,2]+alpha[subject]*PE2 + alpha[subject]*lambda[subject]*PE3;
        Qval[ch3,st3,3] = Qval[ch3,st3,3]+alpha[subject]*PE3;
      }
  }
}

generated quantities {
  real Q_val_first_chosen[Nsubjects,Ntrials];
  real Q_val_first_unchosen[Nsubjects,Ntrials];
  
  real Q_val_second_chosen[Nsubjects,Ntrials];
  real Q_val_second_unchosen[Nsubjects,Ntrials];
  
  real Q_val_third_chosen[Nsubjects,Ntrials];
  real Q_val_third_unchosen[Nsubjects,Ntrials];
  
  //matrix[Nsubjects,Ntrials] like[3];
  matrix[Nsubjects,Ntrials] like1;
  matrix[Nsubjects,Ntrials] like2;
  matrix[Nsubjects,Ntrials] like3;


  {  

    like1 = rep_matrix(0,Nsubjects,Ntrials);
    like2 = rep_matrix(0,Nsubjects,Ntrials);
    like3 = rep_matrix(0,Nsubjects,Ntrials);
    
    for (subject in 1:Nsubjects) {
      int gq_ch1;
      int gq_ch2; 
  	  int gq_ch3; 
  	  int gq_st1; 
  	  int gq_st2; 
  	  int gq_st3; 
      real gq_PE1;
      real gq_PE2;
      real gq_PE3;
	    real gq_Qval[2,4,3]; //number of arms x number of states x number of stages (for a tree of three stages the last stage will have 4 states)
      vector [2]gq_Qnet;
      
      
      for (trial in 1:Ntrials_per_subject[subject]) {
        //reset Qvalues in the start of each blockk
        if (first_trial_in_block[subject,trial] == 1) {
          gq_Qval = rep_array(0, 2, 4, 3);
          }
        
        //allocate choices
        gq_ch1 = choice1[subject,trial];
        gq_ch2 = choice2[subject,trial];
        gq_ch3 = choice3[subject,trial];
        gq_st1 = 1;
        gq_st2 = state2[subject,trial];
        gq_st3 = state3[subject,trial];
      	
      	
        //like function
        
        gq_Qnet = to_vector(gq_Qval[,gq_st1,1]);
        like1[subject,trial] = categorical_logit_lpmf(gq_ch1 | beta[subject] * gq_Qnet);
        
        if (gq_ch1 == 1) {
          Q_val_first_chosen[subject,trial] = gq_Qnet[1];
          Q_val_first_unchosen[subject,trial] = gq_Qnet[2];
        } else {
          Q_val_first_chosen[subject,trial] = gq_Qnet[2];
          Q_val_first_unchosen[subject,trial] = gq_Qnet[1];
        }


        gq_Qnet = to_vector(gq_Qval[,gq_st2,2]);
        like2[subject,trial] = categorical_logit_lpmf(gq_ch2 | beta[subject] * gq_Qnet);
        
        if (gq_ch2 == 1) {
          Q_val_second_chosen[subject,trial] = gq_Qnet[1];
          Q_val_second_unchosen[subject,trial] = gq_Qnet[2];
        } else {
          Q_val_second_chosen[subject,trial] = gq_Qnet[2];
          Q_val_second_unchosen[subject,trial] = gq_Qnet[1];
        }
        
        
        gq_Qnet = to_vector(gq_Qval[,gq_st3,3]);
        like3[subject,trial] = categorical_logit_lpmf(gq_ch3 | beta[subject] * gq_Qnet);
        
        if (gq_ch3 == 1) {
          Q_val_third_chosen[subject,trial] = gq_Qnet[1];
          Q_val_third_unchosen[subject,trial] = gq_Qnet[2];
        } else {
          Q_val_third_chosen[subject,trial] = gq_Qnet[2];
          Q_val_third_unchosen[subject,trial] = gq_Qnet[1];
        }
        
        
        //PE
        gq_PE1 = gq_Qval[gq_ch2,gq_st2,2] - gq_Qval[gq_ch1,gq_st1,1];
        gq_PE2 = gq_Qval[gq_ch3,gq_st3,3] - gq_Qval[gq_ch2,gq_st2,2];
        gq_PE3 = reward[subject,trial]  - gq_Qval[gq_ch3,gq_st3,3];

        //Qvalues update
        gq_Qval[gq_ch1,gq_st1,1] = gq_Qval[gq_ch1,gq_st1,1]+alpha[subject]*gq_PE1 + alpha[subject]*lambda[subject]*gq_PE2 + alpha[subject]*(pow(lambda[subject],2))*gq_PE3;
        gq_Qval[gq_ch2,gq_st2,2] = gq_Qval[gq_ch2,gq_st2,2]+alpha[subject]*gq_PE2 + alpha[subject]*lambda[subject]*gq_PE3;
        gq_Qval[gq_ch3,gq_st3,3] = gq_Qval[gq_ch3,gq_st3,3]+alpha[subject]*gq_PE3;
      } 
    }
  }
}
//NS code review 19-22-2022
