fig_pr_rest <- function(post_msm){
  lambda <- post_msm$lambda
  
  pr_trans <- lambda
  for ( i in 1:dim(lambda)[1] ) {
    for (j in 1:dim(lambda)[2]) {
      pr_trans[i,j,] =  pr_trans[i,j,]/sum(lambda[i,j,])
    }
  }
  
  d_pr_long <- data.frame(
    pr_rest_play = pr_trans[,1,3],
    pr_signal_play = pr_trans[,2,3],
    diff = pr_trans[,2,3] - pr_trans[,1,3],
    draw = 1:length(pr_trans[,1,3])
  ) %>% 
    pivot_longer(-draw, names_to = "transition")
  
  transition_cols <- c("diff" = "black", "pr_rest_play" = "slateblue", "pr_signal_play" = "springgreen3")
  
  d_pr_summary <- d_pr_long %>% 
    group_by(transition) %>% 
    summarise(mean = mean(value), lower = HPDI(value, prob = 0.9)[1], upper = HPDI(value, prob = 0.9)[2])
  
  p1 <- ggplot(filter(d_pr_long, transition != "diff"), aes(x = value, fill = transition, color = transition)) +
    geom_density(alpha = 0.75) +
    scale_fill_manual(values = transition_cols) +
    scale_color_manual(values = transition_cols) +
    theme_classic(base_size = 22) + 
    xlab("Pr(Transition to Play)") +
    ylab("probability density") + 
    theme(legend.position = "none")
  
  p2 <- ggplot(filter(d_pr_long, transition == "diff"), aes(x = value)) +
    geom_density(alpha = 0.6, fill = "darkgrey") +
    theme_classic(base_size = 22) + 
    xlab(expression(paste(Delta,"Pr(signal before - rest before)"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    ylab("probability density") + 
    theme(legend.position = "none")
  
  p1/p2
  
  ggsave("fig/fig3_d.pdf", height = 7, width = 6, dpi = 900)
  
  return(p1/p2)
}