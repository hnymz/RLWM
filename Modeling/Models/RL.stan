// Simple delta rule model - Adapted from Grill et al., 2024
data {
  int<lower=1> N;
  int<lower=1> Tmax; // trial number for each subject
  array[N, Tmax] int<lower=0, upper=3> choice; // choice for each subject in each trial
  array[N, Tmax] real outcome;  // no lower and upper bounds
  array[N, Tmax] int<lower=0, upper=1> mask;
  array[N] int<lower=0,upper=1> group; // 0 = hc, 1 = pd
}

transformed data {
  vector[3] initV = rep_vector(0, 3);  // Initial values for EV
}

parameters {
  // Declare all parameters as vectors for vectorizing hyper(group)-parameters
  // Use vectors so that you don't need a for loop or write two lines
  // Group-level means (2 parameters: alpha, beta)
  vector[2] mu_hc; // One mean value for alpha and one mean value for beta
  vector[2] mu_pd;

  // Shared group-level standard deviations
  vector<lower=0>[2] sigma;

  // Individual-level raw parameters (for Matt trick)
  vector[N] alpha_pr;
  vector[N] beta_pr;
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] alpha;
  vector<lower=0>[N] beta;

  for (i in 1:N) {
    // Phi_approx can transform any input into [0, 1]
    vector[2] mu_i;
    if (group[i] == 0)
      mu_i = mu_hc;
    else
      mu_i = mu_pd;
      
    alpha[i] = Phi_approx(mu_i[1] + sigma[1] * alpha_pr[i]);
    beta[i]  = exp(mu_i[2] + sigma[2] * beta_pr[i]);
  }
}
model {
  // Hyperparameters
  mu_hc  ~ normal(0, 1);
  mu_pd  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);
  
  // individual parameters
  alpha_pr   ~ normal(0, 1);
  beta_pr ~ normal(0, 1);
  
  // subject loop and trial loop
  for (i in 1:N) {
    vector[3] ev; // expected value
    real PE;      // prediction error
    
    ev = initV;
    
    for (t in 1:Tmax) {
      if (mask[i, t] == 1) {
        // compute action probabilities
        choice[i, t] ~ categorical_logit(beta[i] * ev);
      
        // prediction error
        PE = outcome[i, t] - ev[choice[i, t]];
      
        // value updating (learning)
        ev[choice[i, t]] += alpha[i] * PE;
      }
    }
  }
}
generated quantities {
  // Group-level summary parameters
  real<lower=0, upper=1> mu_alpha_hc;
  real<lower=0> mu_beta_hc;
  real<lower=0, upper=1> mu_alpha_pd;
  real<lower=0> mu_beta_pd;

  mu_alpha_hc = Phi_approx(mu_hc[1]);
  mu_beta_hc  = exp(mu_hc[2]);
  mu_alpha_pd = Phi_approx(mu_pd[1]);
  mu_beta_pd  = exp(mu_pd[2]);

  // For log likelihood calculation
  array[N] real log_lik;
  
  // For posterior predictive check
  array[N, Tmax] real y_pred;
  array[N, Tmax] real PEs;
  array[N, Tmax] real EVAs;
  array[N, Tmax] real EVBs;
  array[N, Tmax] real EVCs;
  
  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:Tmax) {
      y_pred[i, t] = -1;
      PEs[i, t] = -1;
      EVAs[i, t] = 0;
      EVBs[i, t] = 0;
      EVCs[i, t] = 0;
    }
  }
  
  { // local section, this saves time and space
    for (i in 1:N) {
      vector[3] ev; // expected value
      real PE;      // prediction error
      
      // Initialize values
      ev = initV;
      
      log_lik[i] = 0;
      
      for (t in 1:Tmax) {
        if (mask[i, t] == 1) {
          
          // compute log likelihood of current trial
          log_lik[i] += categorical_logit_lpmf(choice[i, t] | beta[i] * ev);
          
          // generate posterior prediction for current trial
          y_pred[i, t] = categorical_rng(softmax(beta[i] * ev));
          
          // prediction error
          PE = outcome[i, t] - ev[choice[i, t]];
          PEs[i, t] = PE;
          
          // value updating (learning)
          ev[choice[i, t]] += alpha[i] * PE;
        
          EVAs[i,t] = ev[1];
          EVBs[i,t] = ev[2];
          EVCs[i,t] = ev[3];
        }
      }
    }
  }
}
