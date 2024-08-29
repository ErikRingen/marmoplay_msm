fig_duration_play <- function(segmented_data, msm_standata){
  
  d_descrip <- data.frame(
    duration = c(),
    signal_before = c(),
    n_cat = c(),
    draw = c()
  )
  
  for (m in 1:length(segmented_data)) {
    d_play <- segmented_data[[m]] %>%
      filter(type == 3 & !(is.na(lag))) %>%
      mutate(signal_before = ifelse(lag_type == 2, 1, 0))
    
    d_signal <- segmented_data[[m]] %>%
      filter(type == 2)
    
    d_temp <- data.frame(
      duration = d_play$duration,
      signal_before = d_play$signal_before,
      n_cat = d_play$n_cat,
      chase = d_play$chase,
      wrestle = d_play$wrest,
      touch = d_play$touch,
      pull = d_play$pull,
      catch = d_play$catch,
      jump_other = d_play$jump_other,
      catch = d_play$catch,
      session = d_play$session_id,
      n_dyads = d_play$n_dyads,
      juv_only = ifelse( str_starts(d_play$focal_init, "juv") & str_starts(d_play$receiver_init, "juv"), 1, 0), 
      draw = m
    ) 
    
    d_temp2 <- data.frame(
      hide = d_signal$hide,
      stalk = d_signal$stalk,
      supine = d_signal$supine,
      draw = m
    )
    
    d_descrip <- bind_rows(d_descrip, d_temp)
  }
  
  
  p1 <- ggplot(filter(d_descrip, signal_before == 0), aes(x = duration * msm_standata$mean_duration, group = factor(draw))) + 
    geom_density(lwd = 0.1, color = "slateblue") + 
    geom_density(data = filter(d_descrip, signal_before == 1), lwd = 0.1, color = "springgreen3") + 
    theme_classic(base_size = 16) + 
    xlab("duration of play states") +
    ylab("posterior density")
  
  p1
  
  ggsave("fig/fig3_e.pdf", height = 5, width = 7, dpi = 900)
  
  return(p1)
}