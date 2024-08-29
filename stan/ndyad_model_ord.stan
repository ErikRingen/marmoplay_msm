functions{
  real expected_value_ordered_logistic(real eta, vector alpha, int K) {
      vector[K] probs;
      real expected_value = 0.0;
      
      // Calculate probabilities for each category
      for (k in 1:K) {
        if (k == 1) {
          probs[k] = inv_logit(alpha[k] - eta);
        } else if (k == K) {
          probs[k] = 1.0 - inv_logit(alpha[k-1] - eta);
        } else {
          probs[k] = inv_logit(alpha[k] - eta) - inv_logit(alpha[k-1] - eta);
        }
      }
      
      // Compute the expected value
      for (k in 1:K) {
        expected_value += k * probs[k];
      }
      
      return expected_value;
    }
}

data{
  int N;
  int N_session;
  array[N] int session_id;
  array[N] int signal_before;
  array[N] int n_dyads;
}

transformed data{
  int K = 5; // max number of dyads
}

parameters{
  ordered[K - 1] c_nosignal; // latent cutpoints
  ordered[K - 1] c_signal;
  matrix[N_session, 2] session_z;
  vector<lower=0>[2] sigma_session;
}

model{
  // priors
  c_signal ~ normal(0, 2);
  c_nosignal ~ normal(0, 2);
  to_vector(session_z) ~ std_normal();
  sigma_session ~ exponential(1);
  
  for (n in 1:N) {
    if (signal_before[n] == 0) n_dyads[n] ~ ordered_logistic( session_z[session_id[n], 1]*sigma_session[1], c_nosignal);
    
    else if (signal_before[n] == 1) n_dyads[n] ~ ordered_logistic( session_z[session_id[n], 1]*sigma_session[1] + (session_z[session_id[n], 2]*sigma_session[2] )*signal_before[n], c_signal);
;
  }
}

generated quantities{
    real mu_no_signal;
    real mu_signal;
    real diff;
    
    mu_no_signal = expected_value_ordered_logistic(0, c_nosignal, K);
    mu_signal = expected_value_ordered_logistic(0, c_signal, K);
        
    diff = mu_signal - mu_no_signal;
}


