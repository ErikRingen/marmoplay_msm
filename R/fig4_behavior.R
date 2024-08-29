fig4_behavior <- function(fit_behavior){
  
  behavs <- c("chase", "jump_other", "pull", "so_stalk", "touch", "wrestle")
  
  ## Wrestle
  mu_no_signal <- inv_logit(fit_behavior$`b0[6]`)
  mu_signal <- inv_logit(fit_behavior$`b0[6]` + fit_behavior$`b[6]`)
  diff <- mu_signal - mu_no_signal
  
  round(mean(mu_no_signal), 2)
  round(HPDI(mu_no_signal, prob = 0.9), 2)
  
  round(mean(mu_signal), 2)
  round(HPDI(mu_signal, prob = 0.9), 2)
  
  
  round(mean(diff), 2)
  round(HPDI(diff, prob = 0.9), 2)
  mean(diff > 0)
  
  
  ## Chase
  mu_no_signal <- inv_logit(fit_behavior$`b0[1]`)
  mu_signal <- inv_logit(fit_behavior$`b0[1]` + fit_behavior$`b[1]`)
  diff <- mu_signal - mu_no_signal
  
  round(mean(mu_no_signal), 2)
  round(HPDI(mu_no_signal, prob = 0.9), 2)
  
  round(mean(mu_signal), 2)
  round(HPDI(mu_signal, prob = 0.9), 2)
  
  
  round(mean(diff), 2)
  round(HPDI(diff, prob = 0.9), 2)
  mean(diff < 0)
  
  
  # plots
  behavs <- c("chase", "jump_other", "pull", "so_stalk", "touch", "wrestle")
  
  n_samps <- length(fit_behavior$lp__)
  
  d_contrast <- data.frame(
    chase = inv_logit(fit_behavior$`b0[1]` + fit_behavior$`b[1]`) - inv_logit(fit_behavior$`b0[1]`),
    jump = inv_logit(fit_behavior$`b0[2]` + fit_behavior$`b[2]`) - inv_logit(fit_behavior$`b0[2]`),
    pull = inv_logit(fit_behavior$`b0[3]` + fit_behavior$`b[3]`) - inv_logit(fit_behavior$`b0[3]`),
    so_stalk = inv_logit(fit_behavior$`b0[4]` + fit_behavior$`b[4]`) - inv_logit(fit_behavior$`b0[4]`),
    touch = inv_logit(fit_behavior$`b0[5]` + fit_behavior$`b[5]`) - inv_logit(fit_behavior$`b0[5]`),
    wrestle = inv_logit(fit_behavior$`b0[6]` + fit_behavior$`b[6]`) - inv_logit(fit_behavior$`b0[6]`),
    draw = 1:n_samps
  )
  
  d_contrast_long <- d_contrast %>% 
    pivot_longer(-draw) %>% 
    mutate(
      name = case_when(
        name == "jump" ~ "pounce",
        name == "so_stalk" ~ "catch",
        .default = name
      )
    )
  
  d_contrast_summary <- d_contrast_long %>% 
    group_by(name) %>% 
    summarise(mean = mean(value), lower = HPDI(value, prob = 0.9)[1], upper = HPDI(value, prob = 0.9)[2])
  
  p <- ggplot(d_contrast_summary, aes(x = mean, y = fct_reorder(name, mean))) +
    geom_point(position = position_dodge(width = 0.3), size = 3) + 
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, position = position_dodge(width = 0.3), lwd = 1) + 
    geom_vline(xintercept = 0, linetype = 'dashed') + 
    ylab("") + 
    xlab(expression(paste(Delta,  " pr behavior (signal before - rest before)"))) + 
    theme_classic(base_size = 16)
  
  ggsave("fig4.pdf", height = 6, width = 4, dpi = 900)
  
  return(p)
}