fig_classifier <- function(post, standat) {
  
  n_samps <- length(post$lp__)
  
  dur_seq <- seq(from = 0, to = 100, length.out = 200)
  
  prob_rest <- function(dur, prev_state = 3) {
    
    lp_rest <- c()
    lp_pause <- c()
    pr_rest <- c()
    
    if (dur == 0) pr_rest = rep(0, n_samps)
    
    else if (dur > 0) {
      for (i in 1:n_samps) {
        lp_rest[i] = sum(post$lambda[i,1,]) * exp(-sum(post$lambda[i,1,])*dur)
        lp_pause[i] = (1 - post$zi_pause[i, prev_state - 1]) * post$lambda_pause[i,prev_state - 1] * exp(-sum(post$lambda_pause[i,prev_state - 1])*dur)
        pr_rest[i] = lp_rest[i] / (lp_rest[i] + lp_pause[i])
      }
    }
    
    return(pr_rest)
  }
  
  pr_rest_play <- matrix(NA, nrow = n_samps, ncol = length(dur_seq))
  pr_signal_play <- pr_rest_play
  
  for (j in 1:length(dur_seq)) {
    pr_rest_play[,j] = prob_rest(dur_seq[j], prev_state = 3)
    pr_signal_play[,j] = prob_rest(dur_seq[j], prev_state = 2)
  }
  
  avg <- (pr_rest_play + pr_signal_play)/2
  
  pdf("fig/fig4_classifier.pdf")
  par(cex = 1.5)
  plot(NULL, xlim = c(0, max(dur_seq)*standat$mean_duration), ylim = c(0,1), ylab = "Pr(rest | lag duration)", xlab = "duration of lag (seconds)", yaxt = 'n')
  axis(2, at = c(0, 0.5, 1), labels = c("0", ".5", "1"))
  
  for (i in 1:200) {
    lines(x = dur_seq * standat$mean_duration, y = avg[i,], col = col.alpha("slateblue"))
  }
  
  abline(h = 0.5, lty = "dashed")
  dev.off()
}

