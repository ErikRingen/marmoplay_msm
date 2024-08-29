data{
  int N;
  int N_session;
  int N_behavior;
  array[N] int session_id;
  array[N, N_behavior] int behavior;
  array[N] int signal_before;
}

parameters{
  vector[N_behavior] b0;
  vector[N_behavior] b;
  matrix[N_session, 2 * N_behavior] session_z;
  vector<lower=0>[2 * N_behavior] sigma_session;
}

model{
  // priors
  b0 ~ std_normal();
  b ~ std_normal();
  to_vector(session_z) ~ std_normal();
  sigma_session ~ exponential(2);
  
  for (j in 1:N_behavior) 
    for (n in 1:N) {
      behavior[n, j] ~ bernoulli_logit(b0[j] + session_z[session_id[n], j]*sigma_session[j] + (b[j] + session_z[session_id[n], N_behavior + j]*sigma_session[N_behavior + j])*signal_before[n]);
    }
}
