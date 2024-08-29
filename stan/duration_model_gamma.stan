data{
  int N;
  int N_session;
  array[N] int session_id;
  vector[N] duration;
  array[N] int signal_before;
}

parameters{
  real b0;
  real b;
  matrix[N_session, 2] session_z;
  vector<lower=0>[2] sigma_session;
  real<lower=0> shape;
}

model{
  // priors
  b0 ~ normal(0, 2);
  b ~ normal(0, 2);
  to_vector(session_z) ~ std_normal();
  sigma_session ~ exponential(1);
  shape ~ normal(1, 2);
  
  for (n in 1:N) {
    real mu = exp(b0 + session_z[session_id[n], 1]*sigma_session[1] + (b + session_z[session_id[n], 2]*sigma_session[2] )*signal_before[n]);
    
    duration[n] ~ gamma(shape, shape ./ mu);
  }
}

generated quantities{
  real mu_no_signal = exp(b0);
  real mu_signal = exp(b0 + b);
  real diff = mu_signal - mu_no_signal;
}
