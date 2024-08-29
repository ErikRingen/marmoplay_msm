pred_check <- function(d, standata, post, prior = F){
  
  # Organize raw data
  d_msm <- data.frame(
    y = standata$y,
    behavior = case_when(
      standata$y == 2 ~ "invite",
      standata$y == 3 ~ "play"),
    duration = standata$duration,
    session = standata$session_id,
    lag_duration = standata$lag_duration
  ) %>% 
    mutate(obs = paste0("V", 1:n())) %>% 
    mutate(session_name = paste("session id", session))
  
  n_samp <- 100

  pred_long <- as.data.frame(post$y_rep) %>%
    mutate(samp = 1:n()) %>%
    pivot_longer(-samp, values_to = "pred_lag", names_to = "obs") %>%
    filter(samp <= n_samp) %>%
    left_join(d_msm, by = "obs") %>% 
    filter(!is.na(pred_lag))
  
  title <- ifelse(prior == T, "Prior predictive checks", "Posterior predictive checks")
  
  p1 <- ggplot(pred_long, aes(x = sqrt(pred_lag), group = factor(samp))) + 
    facet_wrap(~fct_reorder(session_name, session), scales = "free") +
    geom_density(lwd = 0.1, color = "lightskyblue3") +
    geom_density(data = d_msm, aes(x = sqrt(lag_duration), group = NA)) +
    theme_minimal(base_size = 14) +
    xlab("") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    ggtitle(title)
  
  p2 <- ggplot(pred_long, aes(x = sqrt(pred_lag), group = factor(samp))) + 
    facet_wrap(~fct_reorder(session_name, session), scales = "free") +
    stat_ecdf(lwd = 0.1, color = "lightskyblue3") +
    stat_ecdf(data = d_msm, aes(x = sqrt(lag_duration), group = NA)) +
    xlab("sqrt(lag duration)") +
    theme_minimal(base_size = 14) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

  p_comb <- p1 / p2
  
  return(p_comb)
}