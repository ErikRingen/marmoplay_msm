stanfit_play <- function(segmented_data, stan_model, n_dyads_model = F){
  
  M <- length(segmented_data) # number of datasets to fit and combine
  
  stan_data_list <- list()
  
  for (m in 1:M) {
    d_play <- segmented_data[[m]] %>%
      filter(type == 3 & !(is.na(lag))) %>%
      mutate(signal_before = ifelse(lag_type == 2, 1, 0))
    
    behaviors <- str_split(d_play$behaviors, pattern = "-")
    behaviors <- lapply(behaviors, function(x) { ifelse(x == "wrestle_out", "wrestle", x)})
    
    unique_behaviors <- sort(unique(unlist(behaviors)))
    
    behavior_mat <- matrix(NA, nrow = nrow(d_play), ncol = length(unique_behaviors))
    
    for (j in 1:length(unique_behaviors)) {
      behavior_mat[,j] = sapply(behaviors, function(x) any(x == unique_behaviors[j])) 
    }
    
    behavior_mat = as.matrix(behavior_mat)
    
    
    data_list <- list(
      N = nrow(d_play),
      N_session = max(d_play$session_id),
      session_id = d_play$session_id,
      signal_before = d_play$signal_before,
      duration = d_play$duration,
      n_cat = d_play$n_cat,
      wrest = d_play$wrest,
      chase = d_play$chase,
      hide = d_play$lag_hide,
      stalk = d_play$lag_stalk,
      supine = d_play$lag_supine,
      n_dyads = d_play$n_dyads,
      behavior = behavior_mat,
      N_behavior = length(unique_behaviors)
    )
    stan_data_list[[m]] = data_list
  }
  
  # Now fit the model using each dataset
  fit_list <- lapply(stan_data_list, function(data) {
    stan_model$sample(data = data, seed = 123, chains = 8, parallel_chains = 8, adapt_delta = 0.98, iter_warmup = 200, iter_sampling = 200)
  })
  
  
  if (n_dyads_model == T) {
    draws_list <- lapply(fit_list, function(fit) {
      as_draws_df(fit$draws(c("mu_no_signal", "mu_signal", "diff")))
    })
  }
  
  else draws_list <- lapply(fit_list, function(fit) {
    as_draws_df(fit$draws())
  })
  
  combined_samples <- bind_draws(draws_list, along = "chain")
  
  return(combined_samples)
}