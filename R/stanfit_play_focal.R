stanfit_play_focal <- function(segmented_data, stan_model){
  
  M <- length(segmented_data) # number of datasets to fit and combine
  
  stan_data_list <- list()
  
  for (m in 1:M) {
    d_play <- segmented_data[[m]] %>%
      filter(type == 3 & !(is.na(lag))) %>%
      mutate(signal_before = ifelse(lag_type == 2, 1, 0))
    
    d_focal <- d_play %>% filter(signal_before == 1) %>% 
      mutate(focal_to_receive = ifelse(lag_focal == receiver_init, 1, 0),
             receive_to_focal = ifelse(lag_receiver == focal_init, 1, 0),
             role_reverse = ifelse(lag_focal == receiver_init & lag_receiver == focal_init, 1, 0),
             role_retain = ifelse(lag_receiver == receiver_init & lag_focal == focal_init, 1, 0)
      ) %>% 
      mutate(role_type = case_when(
        role_retain == 1 ~ 1,
        role_reverse == 1 ~ 2,
        .default = 3
      ))
    
    
    data_list <- list(
      N = nrow(d_focal),
      N_session = max(d_focal$session_id),
      session_id = d_focal$session_id,
      signal_before = d_focal$signal_before,
      role = d_focal$role_type
    )
    stan_data_list[[m]] = data_list
  }
  
  # Now fit the model using each dataset
  fit_list <- lapply(stan_data_list, function(data) {
    stan_model$sample(data = data, seed = 123, chains = 8, parallel_chains = 8, adapt_delta = 0.98, iter_warmup = 200, iter_sampling = 200)
  })
  
  draws_list <- lapply(fit_list, function(fit) {
    as_draws_df(fit$draws())
  })
  
  combined_samples <- bind_draws(draws_list, along = "chain")
  
  return(combined_samples)
}