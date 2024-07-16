data {

  //General fixed parameters for the experiment/models

  

  int<lower=1> Nsubjects;

  

  int<lower=1> Nblocks;

  

  int<lower=1> Ntrials;

  

  array[Nsubjects] int<lower=1> Ntrials_per_subject;

  

  int<lower=2> Narms;

  
  //Behavioral data:

  

  array[Nsubjects, Ntrials] int<lower=0> choice;

  

  array[Nsubjects, Ntrials] int<lower=0> reward;


  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;

}

transformed data {

  int Nparameters_standard = 1;
  int Nparameters_transformed = 1;
  real eps=1e-8;
}

parameters {

  //population level parameters 

  
  vector[Nparameters_standard] population_locations;
  vector<lower=eps, upper=1-eps>[Nparameters_transformed] population_locations_transformed;

  vector<lower=eps>[Nparameters_standard] population_scales;
  vector<lower=eps, upper=300>[Nparameters_transformed] population_scales_transformed;


  //individuals level

  vector<lower=0, upper=1>[Nsubjects] alpha;

  

  vector[Nsubjects] beta;


}

transformed parameters {
    
    //internal variabels

    real Qdiff;

    
    real PE;

    matrix[Ntrials, Nsubjects] Qdiff_external;

    array[Narms] real Qval;

  for (subject in 1 : Nsubjects) {

    
    for (trial in 1 : Ntrials_per_subject[subject]) {

      
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject, trial] == 1) {

        Qval = rep_array(0.5, Narms);

      }

      //calculate probability for each action


      Qdiff_external[trial,subject] = Qval[2] - Qval[1];

    
      //update Qvalues

      

      PE = reward[subject, trial] - Qval[choice[subject, trial]];

      

      Qval[choice[subject, trial]] = Qval[choice[subject, trial]]

                                     + alpha[subject] * PE;

    }

  }

}

model {

  // population level  


  population_locations  ~ normal(0,2);
  population_scales     ~ lognormal(0,1);


  population_locations_transformed  ~ beta(1.5,1.5);
  population_scales_transformed     ~ gamma(2,0.05);

  

  for (subject in 1 : Nsubjects) {
    //normal
    target+= normal_lpdf(beta[subject]|population_locations[1], population_scales[1]);
    //transformed
    target+= beta_proportion_lpdf(alpha[subject]|population_locations_transformed[1], population_scales_transformed[1]);
    for (trial in 1 : Ntrials_per_subject[subject]) {

      target += bernoulli_logit_lpmf(choice[subject, trial]-1 | beta[subject]* Qdiff_external[trial, subject]);

    }

  }

}




