classifier_mean <- function(post, d, standat){
  
  sessions <- unique(d$session_name)
  
  N_times <- c()
  
  out <- data.frame(duration = c(), behavior = c(), y_cat = c(), lag_pause = c(), lag_behavior = c(), focal = c(), session_id = c())
  
  original_sess_duration <- c()
  
  for (j in 1:length(sessions)) {
    # Construct time series using end times
    # start by filtering one session
    d_sess <- d %>% 
      filter(session_name == sessions[j])
    
    d_temp <- time_series_expand(d_sess) %>% 
      mutate(lag_behavior = lag(behavior)) %>% 
      mutate(session_id = sessions[j])
    
    out <- bind_rows(out, d_temp)
    N_times[j] = nrow(d_temp)
    
    original_sess_duration[j] = max(d_sess$end_time)
  }
  
  ### Use classifier probs to determine how signal influences duration of play
  out$rest <- round(apply(post$post_pr_rest, 2, mean))
  
  long_df <- out %>%
    mutate(id = 1:n()) %>% 
    rename(duration_lag = lag_duration, duration_now = duration) %>% 
    pivot_longer(cols = c(duration_now, duration_lag), 
                 names_to = "event_type", 
                 values_to = "duration") %>%
    arrange(id, event_type) %>% 
    group_by(session_id) %>% 
    mutate(cumu_time = cumsum(duration),
           type = case_when(
             event_type == "duration_now" ~ behavior,
             event_type == "duration_lag" & rest == 1 ~ "rest",
             event_type == "duration_lag" & rest == 0 ~ "pause",
             event_type == "duration_lag" & is.na(rest) ~ "rest"
           )
    )
  
  long_df <- long_df %>% mutate(start = lag(cumu_time))
  
  long_df$type <- factor(long_df$type, levels = rev(c("rest", "invitation", "play")))
  
  long_df2 <- long_df %>% 
    filter(!(event_type == "duration_lag" & is.na(type))) %>% 
    group_by(session_id) %>% 
    mutate(seg_end_x = lead(start), seg_end_y = lead(type))
  
  ggplot(filter(long_df2, session_id %in% c("Washington: session 10")  & !is.na(type))) +
    geom_point(aes(x = start, y = type, color = type), size = 1) +
    geom_point(aes(x = cumu_time, y = type, color = type), size = 1) +
    geom_segment(aes(x = lag(cumu_time), y = lag(type), xend = start, yend = type, color = lag(type)), lwd = 0.4, arrow = arrow(type = "closed", length = unit(0.15, "cm"))) +
    geom_segment(aes(x = start, y = type, xend = cumu_time, yend = type, color = type), lwd = 0.4) +
    scale_color_manual(values = c("violetred2", "springgreen3", "slateblue", "lightgrey")) + 
    scale_y_discrete(limits = rev(c("rest", "invitation", "play")), labels = rev(c("rest", "signal", "play")), expand = c(0.025,0.025)) +
    theme_classic(base_size = 16) + 
    theme(legend.position = "none", axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ylab("") +
    xlab("time")
    
  
  
  head(long_df)
  
  return(long_df)
}
