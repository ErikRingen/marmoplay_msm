data{
  int N;
  int N_session;
  array[N] int session_id;
  vector[N] duration;
  array[N] int signal_before;
  array[N] int n_cat;
}

parameters{
  real b0;
  real b;
  matrix[N_session, 2] session_z;
  vector<lower=0>[2] sigma_session;
}

model{
  // priors
  b0 ~ std_normal();
  b ~ std_normal();
  to_vector(session_z) ~ std_normal();
  sigma_session ~ exponential(2);
  
  for (n in 1:N) n_cat[n] ~ poisson(exp(b0 + session_z[session_id[n], 1]*sigma_session[1] + (b + session_z[session_id[n], 2]*sigma_session[2] )*signal_before[n])) T[1, ];
}

generated quantities{
    real mu_no_signal;
    real mu_signal;
    real diff;
    
    mu_no_signal = (exp(b0)/(1 - exp(-exp(b0))));
        
    mu_signal = (exp(b0 + b)/(1 - exp(-exp(b0 + b))));
    
    diff = mu_signal - mu_no_signal;
}

