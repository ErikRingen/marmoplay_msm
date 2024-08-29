functions{
   // Function to find the index of the minimum value in an array
  int which_min(vector arr) {
    // Initialize the index of the minimum value to the first element
    int index = 1;
    // Loop through the array starting from the second element
    for (i in 2:size(arr)) {
      // If the current element is smaller than the current minimum, update the index
      if (arr[i] < arr[index]) {
        index = i;
      }
    }
    // Return the index of the minimum value
    return index;
  }
// function to 
  matrix compose_transition_mat(int K, vector lambda_non_rest, vector lambda_rest){
    matrix[K, K] lambda;
    int ticker = 0;
    
    // Fill off-diagonals of matrix
    for (i in 1:(K-1))
      for (j in (i+1):K) {
        lambda[i, j] = lambda_non_rest[ticker + 1];
        lambda[j, i] = lambda_non_rest[ticker + 2];
        ticker += 2;
      }
    
    // rows representing transition from rest are constrained parameters  
    for (k in 2:K) lambda[1, k] = lambda_rest[k - 1];
    
    // diagonals
    for (k in 1:K) {
      lambda[k, k] = 0; // placeholder, not used in computation
    }
    return lambda;
  }
  
   vector msm_mix_log_lik(int K, matrix lambda, vector lambda_pause, vector zi_pause, real state_duration, real lag_duration, int y_lag, int y_curr, int cens){
    vector[2] lp;
    
          if (lag_duration == 0) {
           lp[1] = negative_infinity();
           lp[2] = log(exp(-sum(lambda[y_lag,])*state_duration)) + log(zi_pause[y_lag - 1]);
          }
          
          else if (lag_duration > 0) {
            lp[1] = log(exp(-sum(lambda[y_lag,])*state_duration)) + log(lambda[1, y_curr]) + log(exp(-sum(lambda[1,])*lag_duration));
            lp[2] = log(exp(-sum(lambda[y_lag,])*state_duration)) + log1m(zi_pause[y_lag - 1]) + log(lambda_pause[y_lag - 1]) + log(exp(-lambda_pause[y_lag - 1]*lag_duration));
          }
          
          if (cens == 0) {
            lp[1] += log(lambda[y_lag, 1]);
          }
          
          if (y_lag != y_curr && cens == 0) {
            lp[2] += log(lambda[y_lag, y_curr]);
          }
          
          return lp;
  }
}

data{
  int N_obs;
  int N_sessions;
  array[N_sessions] int N_times; // number of time points per session
  int<lower=0> K; // number of states
  array[N_obs] int<lower=0, upper=K> y; // observed state
  array[N_obs] real<lower=0> duration; // duration of behavior
  array[N_obs] real<lower=0> lag_duration; // duration of lag between behaviors
  real<lower=0> mean_duration;
  vector<lower=0>[N_sessions] original_sess_duration;
  int prior_only; // flag for whether to include likelihood (1) or not (1)
  array[N_sessions] int cens_first; // indicator of whether first observation is right-censored (behavior started sometime before behavioral coding)
}

parameters{
  // popuilation-level parameters
  vector<lower=0>[(K * K) - K] lambda_z; // rate of exponential distribution, transitions between non-rest states
  vector<lower=0>[K - 1] lambda_pause; // rate of exp dist, pause lags
  
  real<lower=0, upper=1> phi; // proportional total transition rate from rest to other states, constrained to be slower than pause
  vector[K - 1] lambda_rest_z; // transitions from rest to non-rest states, latent scale 
  vector<lower=0, upper=1>[K - 1] zi_pause;

  // random effect parameters
  matrix[((K * K) - K) + (K - 1) + 1, N_sessions] lambda_z_session; // transitions between non-rest states, session-level random effects unscaled and uncorrelated
  vector<lower=0>[((K * K) - K) + (K - 1) + 1] sigma_lambda_session;
  cholesky_factor_corr[((K * K) - K) + (K - 1) + 1] L_Rho_lambda_session;
}

transformed parameters{
  matrix[K, K] lambda; // composed transition matrix
  vector[K - 1] lambda_rest; // rate of transition from rest state, exp dist
  
  matrix[N_sessions, ((K * K) - K) + (K - 1) + 1] lambda_z_session_v; // scaled and correlated random effects
  array[N_sessions] matrix[K, K] lambda_session; // session-specific transition matrix

  array[N_obs] vector[2] lp; // 

  // Scale and correlate random effects
  lambda_z_session_v = (diag_pre_multiply(sigma_lambda_session, L_Rho_lambda_session) * lambda_z_session)';
  
  lambda_rest = softmax(lambda_rest_z) * phi * min( 1/((1/lambda_pause) .* (1-zi_pause)));

  // Construct transition matrices //
  // average transition matrix
  lambda = compose_transition_mat(K, lambda_z, lambda_rest);
  
  // session-specific matrices
  for (s in 1:N_sessions) {
    vector[(K * K) - K] lambda_z_session_temp;
    vector[K - 1] lambda_rest_session_temp;
    
    for (i in 1:((K * K) - K)) lambda_z_session_temp[i] = exp( log(lambda_z[i]) + lambda_z_session_v[s, i] ); 
    
    lambda_rest_session_temp = softmax(lambda_rest_z + to_vector(lambda_z_session_v[s, ((K * K) - K) + 1:((K * K) - K) + K - 1])) * inv_logit(logit(phi) + lambda_z_session_v[s, ((K * K) - K) + K - 1 + 1]) * min( 1/((1/lambda_pause) .* (1-zi_pause)) );
   
    lambda_session[s] = compose_transition_mat(K, lambda_z_session_temp, lambda_rest_session_temp);
  }
  
   {
  int ticker = 0; // index to keep track of observation number
  for (s in 1:N_sessions) {
      lp[ticker + 1] = rep_vector(-99, 2); // placeholder, not evaluated
      
    for (t in 2:(N_times[s])) {
      int cens;
      if (cens_first[s] == 1 && t == 2) cens = 1;
      else cens = 0;
      
      lp[ticker + t] = msm_mix_log_lik(K, lambda_session[s], lambda_pause, zi_pause, duration[ticker + t - 1], lag_duration[ticker + t], y[ticker + t - 1], y[ticker + t], cens);
    }
  // update the index to slice the next session
  ticker += N_times[s];
  }
  }
  
} // end transformed parameters block

model{
  // priors
  to_vector(lambda_z) ~ normal(0, 2); // half normal
  lambda_pause ~ normal(0, 2); // half normal
  lambda_rest_z ~ std_normal(); // softmax scores
  phi ~ beta(1, 1); // proportional rate of transitions out of rest, relative to pause
  zi_pause ~ beta(1, 1); // rate of zero-duration lags
  
  // random effect priors
  to_vector(lambda_z_session) ~ std_normal();
  sigma_lambda_session ~ exponential(2);
  L_Rho_lambda_session ~ lkj_corr_cholesky(2);
  
  // update the posterior density
  if (prior_only == 0) {
    int ticker = 0; // index to keep track of observation number
    for (s in 1:N_sessions) {
      
       for (t in 2:(N_times[s])) {
          target += log_sum_exp(lp[ticker + t]);
       }
    // update the index to slice the next session
    ticker += N_times[s];
    }
  }
}

generated quantities{
  vector[N_obs] log_lik;
  vector[N_obs] y_rep; // predictive checks
  vector[N_obs] post_pr_rest;

  {
  int ticker = 0; // index to keep track of observation number
  for (s in 1:N_sessions) {
    
     for (t in 2:(N_times[s])) {
            log_lik[ticker + t] = log_sum_exp(lp[ticker + t]);
            post_pr_rest[ticker + t] = exp(lp[ticker + t][1] - log_sum_exp(lp[ticker + t]));
          
      // posterior predictive checks for lags
      vector[K] prior_pr;
      int next_state;
      
      for (k in 1:K) {
        if (k != y[ticker + t - 1]) prior_pr[k] = log(lambda_session[s][y[ticker + t - 1],k]) + log(exp(-sum(lambda_session[s][y[ticker + t - 1],])*duration[ticker + t - 1]));
        if (k == y[ticker + t - 1]) prior_pr[k] = log(exp(-sum(lambda_session[s][y[ticker + t - 1],])*duration[ticker + t - 1]));
      }
      
      prior_pr = softmax(prior_pr);
      next_state = categorical_rng(prior_pr);
      
      if (next_state == 1) {
        y_rep[ticker + t] = exponential_rng(sum(lambda_session[s][next_state,]));
      }
      
      else {
        y_rep[ticker + t] = bernoulli_rng(1 - zi_pause[y[ticker + t - 1] - 1]) * exponential_rng(lambda_pause[y[ticker + t - 1] - 1]);
      }
     }
  // update the index to slice the next session
  ticker += N_times[s];
  }
  }
}
