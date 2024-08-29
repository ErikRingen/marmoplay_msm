data{
  int N;
  int N_session;
  array[N] int session_id;
  array[N] int role;
}

parameters{
  vector[2] b0;
  matrix[N_session, 2] session_z;
  vector<lower=0>[2] sigma_session;
}

model{
  // priors
  b0 ~ std_normal();
  to_vector(session_z) ~ std_normal();
  sigma_session ~ exponential(2);
  
  for (n in 1:N) {
    vector[3] theta;
    
    theta[1] = b0[1] + session_z[session_id[n], 1]*sigma_session[1];
    
    theta[2] = b0[2] + session_z[session_id[n], 2]*sigma_session[2];
    
    theta[3] = 0;
    
    theta = softmax(theta);
    role[n] ~ categorical(theta);
  }
}

generated quantities{
    vector[3] mu;
    vector[3] mu_signal;
    vector[3] diff;
    
    mu[1] = b0[1];
    mu[2] = b0[2];
    mu[3] = 0;
    mu = softmax(mu);
}
