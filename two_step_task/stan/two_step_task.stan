data {
  
  //General fixed parameters for the experiment/models
  
  int<lower=1> Nsubjects;
  
  int<lower=1> Nblocks;
  
  int<lower=1> Ntrials;
  
  array[Nsubjects] int<lower=1> Ntrials_per_subject;
  
  int<lower=2> Narms;
  
  int<lower=2> Nstates;
  
  
  //Behavioral data:
  
  array[Nsubjects, Ntrials] int<lower=0> choice1;
  array[Nsubjects, Ntrials] int<lower=0> choice2;
  
  array[Nsubjects, Ntrials] int<lower=0> reward;
  
  array[Nsubjects, Ntrials] int<lower=0> state;
  
  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;
  
}

transformed data {
  
}

parameters {

//individual level

  //standard

  vector[Nsubjects] beta;
  
  //transformed
  vector<lower=0, upper=1>[Nsubjects] alpha;
  
  vector<lower=0, upper=1>[Nsubjects] omega;
  
  vector<lower=0, upper=1>[Nsubjects] lambda;


}

transformed parameters {

  real PE1;
  real PE2;
  vector[Narms] Qnet;

  vector[Narms] Qmf1;
  
  matrix[Nstates,Narms] Qmf2;

  vector[Narms] Qmb;

  matrix[Ntrials, Nsubjects] Qnet_diff;
  matrix[Ntrials, Nsubjects] Qmf2_diff;

  
  //RL
  for (subject in 1:Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {

    //reset Qvalues (first trial only)

      if (first_trial_in_block[subject, trial] == 1) {

        Qmf1 = rep_vector(0.5, Narms);

        Qmf2 = rep_matrix(0.5,Nstates,Narms);

        Qmb = rep_vector(0.5, Narms);

      }
      

      Qnet[1] = (1-omega[subject])*Qmf1[1]

                +omega[subject]*Qmb[1];

    
      Qnet[2] = (1-omega[subject])*Qmf1[2]

                +omega[subject]*Qmb[2];
      
      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1]; // this is the value based upon we will calculate the likelihood .

      Qmf2_diff[trial,subject]=Qmf2[state[subject,trial],2]-Qmf2[state[subject,trial],1];
      //update Qvalues
      PE2  = reward[subject,trial]  - Qmf2[state[subject,trial],choice2[subject,trial]];
      PE1  = Qmf2[state[subject,trial],choice2[subject,trial]]-Qmf1[choice1[subject,trial]];
      
      
      Qmf2[state[subject,trial],choice2[subject,trial]] = Qmf2[state[subject,trial],choice2[subject,trial]]+alpha[subject]*PE2;
      Qmf1[choice1[subject,trial]] = Qmf1[choice1[subject,trial]]+alpha[subject]*PE1+alpha[subject]*PE2*lambda[subject];
      //define first stage Qmb values based on MF values of second stage and state transitions.
      Qmb[1] = 0.7*max(Qmf2[1,])+0.3*max(Qmf2[2,]);
      Qmb[2] = 0.3*max(Qmf2[1,])+0.7*max(Qmf2[2,]);
    }
    
  }
  
}

model {
  for (subject in 1 : Nsubjects) {
    //update parameter values
    
    //normal
    target+= normal_lpdf(beta[subject]|0 , 2);
    //transformed
    target+= beta_proportion_lpdf(alpha[subject]|0.5, 5);
    target+= beta_proportion_lpdf(omega[subject]|0.5, 5);
    target+= beta_proportion_lpdf(lambda[subject]|0.5, 5);
    
    //update likelihood
    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(choice1[subject, trial]-1| beta[subject]*Qnet_diff[trial, subject]);
      target += bernoulli_logit_lpmf(choice2[subject, trial]-1| beta[subject]*Qmf2_diff[trial, subject]);

    }

  }
}
