fig_ctmc <- function(post_msm, standat){
  
  post <- post_msm
  rate_mat <- apply(post$lambda, 2:3, mean) * standat$mean_duration
  
  Q <- rate_mat
  diag(Q) <- 0
  for (j in 1:3) Q[j,j] = -sum(Q[j,])
  colnames(Q) <- c("rest", "signal", "play")
  rownames(Q) <- colnames(Q)
  
  # Convert the Q-matrix to a graph
  g <- graph_from_adjacency_matrix(Q, mode="directed", weighted=TRUE, diag=FALSE)
  
  # Set edge width proportional to the rate (absolute value since Q matrix contains negative diagonals)
  E(g)$width <- 1  # Scale the widths by multiplying by a factor (e.g., 5) for better visualization
  
  arrow_sizes <- 2
  
  vertex_cols <- c(col.alpha("slateblue", 0.7), col.alpha("springgreen3", 0.7),  col.alpha("violetred2", 0.7))
  vertex_labels <- c("rest", "signal", "play")
  
  # Plot the CTMC
  set.seed(2)
  
  pdf("fig/fig3_ctmc.pdf", width = 5, height = 5)
  plot(g, edge.arrow.size=1, vertex.size=50,
       vertex.label.color = c("white", "black", "black"),
       edge.label.color = "black",
       vertex.color=vertex_cols, vertex.label=vertex_labels,
       edge.label=round(E(g)$weight, 2), edge.label.cex=0.8, edge.curved=1/2  )
  dev.off()
}
