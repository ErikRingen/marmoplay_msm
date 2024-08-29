# This function creates a full time series from time-stamped behaviors, adding spacers between coded behaviors
time_series_expand <- function(d){
  # d_sess <- d %>% filter(session_name == unique(session_name)[1])
  d2 <- d
  
  d2$behavior_type <- ifelse(d2$behavior %in% c("chase", "jump_other", "pull", "wrestle", "wrestle_out", "touch", "so_stalk"), "play", "invitation")
  
  behavior <- c()
  duration <- c()
  y_cat <- c()
  focal <- c()
  receiver <- c()
  
  lag_before_start <- d2$start_time[1] - (7.5 * 60)
  
  if (d2$start_time[1] > 0) {
    behavior[1] = "other"
    duration[1] = lag_before_start
    y_cat[1] = -99
    focal[1] = -99
    receiver[1] = -99
  }
  
  # behavior[1] <- ifelse(d2$start_time[1] > 0, "other", d2$behavior_type[1])  
  # duration[1] <- ifelse(behavior[1] == "other", d2$start_time[1], d2$duration[1])
  # y_cat[1] <- -99
  # focal[1] <- d2$focal[1]
  # receiver[1] <- d2$receiver[1]
  
  check <- c()
  
  for (i in 1:nrow(d2)) {
    # figure out whether to add an "other" spacer
    if (behavior[length(behavior)] != "other") {
      behavior <- c(behavior, "other", d2$behavior_type[i])
      y_cat <- c(y_cat, "other", d2$behavior[i])
      
      check[i] = (as.numeric(d2$start_time[i] - d2$end_time[i - 1]) < 0) 
      duration <- c(duration, as.numeric(d2$start_time[i] - d2$end_time[i - 1]), d2$duration[i])
      focal <- c(focal, rep(d2$focal[i], 2))
      receiver <- c(receiver, rep(d2$receiver[i], 2))
    }
    
    else {
      behavior <- c(behavior, d2$behavior_type[i])
      y_cat <- c(y_cat, d2$behavior[i])
      focal <- c(focal, d2$focal[i])
      receiver <- c(receiver, d2$receiver[i])
      duration <- c(duration, d2$duration[i])
    }
  } 
  
  out <- data.frame(duration = duration,
                    behavior = behavior,
                    y_cat = y_cat,
                    focal = focal,
                    receiver = receiver) %>% 
    mutate(lag_duration = lag(duration),
           # a negative lag duration implies some overlapping behaviors, in which case it makes sense to consider that a 0-duration lag
           lag_duration = ifelse(lag_duration < 0, 0, lag_duration)) %>% 
    filter(behavior != "other")
  
  out$lag_duration[1] = ifelse(lag_before_start < 0, 0, lag_before_start)
  
  return(out)
}
