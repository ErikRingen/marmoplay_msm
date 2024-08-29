standata_msm <- function(d){
 
  sessions <- unique(d$session_name)
  #### Markov model of rest, invite, play
  N_times <- c()
  
  out <- data.frame(duration = c(), behavior = c(), y_cat = c(), lag_pause = c(), lag_behavior = c(), focal = c())
  
  original_sess_duration <- c()
  
  for (j in 1:length(sessions)) {
    # Construct time series using end times
    # start by filtering one session
    d_sess <- d %>% 
      filter(session_name == sessions[j])
    
    d_temp <- time_series_expand(d_sess) %>% 
      mutate(lag_behavior = lag(behavior))
    
    out <- bind_rows(out, d_temp)
    N_times[j] = nrow(d_temp)
    
    original_sess_duration[j] = max(d_sess$end_time) - min(d_sess$start_time)
  }
  
  N_sessions <- length(sessions)
  session_id <- c()
  for (j in 1:N_sessions) session_id <- c(session_id, rep(j, N_times[j]))
  
  duration_s <- out$duration / mean(out$duration, na.rm = T)
  duration_s <- ifelse(is.na(duration_s), -99, duration_s)
  
  lag_duration_s <- out$lag_duration / mean(out$duration, na.rm = T)
  lag_duration_s <- ifelse(is.na(lag_duration_s), -99, lag_duration_s)
  
  y <- case_when(
    out$behavior == "other" ~ 1,
    out$behavior == "invitation" ~ 2,
    out$behavior == "play" ~ 3
  )
  
  y_lag <- case_when(
    out$lag_behavior == "other" ~ 1,
    out$lag_behavior == "invitation" ~ 2,
    out$lag_behavior == "play" ~ 3
  )
  
  y_lag[is.na(y_lag)] <- -99
  
  cens_first <- cbind(out, session_id) %>% group_by(session_id) %>% 
    slice_head(n = 1) %>% 
    mutate(cens_first = ifelse(lag_duration <= 0, 1, 0)) %>% 
    select(cens_first)
  
  
  data_list <- list(
    K = 3,
    N_sessions = length(sessions),
    N_times = N_times,
    duration = duration_s, # scaling for computational convenience
    mean_duration = mean(out$duration, na.rm = T), 
    y = y,
    N_obs = length(y),
    session_id = session_id,
    lag_duration = lag_duration_s,
    original_sess_duration = original_sess_duration,
    cens_first = cens_first$cens_first
  )
  
  return(data_list)
}
