library(targets)
source("R/functions.R")

options(tidyverse.quiet = T)
tar_option_set(packages = c("tidyverse", "lubridate", "posterior", "rethinking", "cmdstanr", "tidygraph", "ggraph", "igraph", "patchwork", "Matrix", "wesanderson", "bayesplot", "posterior"))

list(
  tar_target(d_raw, "raw_data/compiled_results_play_poly.csv", format = "file"),
  tar_target(d, clean_data(d_raw)),
  
  ### Stage 1 Analyses: multi-state model of behavior ###
  # Compose stan data for multi-state model
  tar_target(msm_standata, standata_msm(d)),
  
  # Stan files
  tar_target(msm_stan, "stan/msm_classifier_zi.stan", format = "file"),

  # Fit multistate model
  tar_target(msm, cmdstanr::cmdstan_model(msm_stan)),
  
  tar_target(fit_msm, msm$sample(data = c(msm_standata, list(prior_only = 0)), chains = 4, parallel_chains = 4, iter_warmup = 500, iter_sampling = 5000, adapt_delta = 0.96, output_dir = "stanfits")),
  
  # Posterior draws from msm
  tar_target(post_msm, extract_samples(fit_msm)),

  # Predictive checks
  tar_target(post_pred, pred_check(d, msm_standata, post_msm, prior = F)),
  
  # Extract segmented data from msm
  # sample of segments, accounting for uncertainty
  tar_target(segmented_data, segment_behavior(post_msm, d, msm_standata, n_sims = 100)),
  
  ### Stage 2 Analyses: Using segments from MSM to analyze relationships between signal and play ###
  # Paths to Stan files
  tar_target(duration_stan, "stan/duration_model_gamma.stan", format = "file"),
  tar_target(diversity_stan, "stan/diversity_model_ord.stan", format = "file"),
  tar_target(diversity_duration_stan, "stan/diversity_model_ord_duration.stan", format = "file"),
  tar_target(behavior_stan, "stan/behavior_model.stan", format = "file"),
  tar_target(ndyads_stan, "stan/ndyad_model_ord.stan", format = "file"),
  tar_target(roles_stan, "stan/roles_model.stan", format = "file"),
  
  # Compile Stan models
  tar_target(duration, cmdstanr::cmdstan_model(duration_stan)),
  tar_target(diversity, cmdstanr::cmdstan_model(diversity_stan)),
  tar_target(diversity_duration, cmdstanr::cmdstan_model(diversity_duration_stan)),
  tar_target(behavior, cmdstanr::cmdstan_model(behavior_stan)),
  tar_target(ndyads, cmdstanr::cmdstan_model(ndyads_stan)),
  tar_target(roles, cmdstanr::cmdstan_model(roles_stan)),
  
  # Fit Stan models
  tar_target(fit_duration, stanfit_play(segmented_data, duration)),
  tar_target(fit_diversity, stanfit_play(segmented_data, diversity)),
  tar_target(fit_diversity_duration, stanfit_play(segmented_data, diversity_duration)),
  tar_target(fit_behavior, stanfit_play(segmented_data, behavior)),
  tar_target(fit_dyads, stanfit_play(segmented_data, ndyads)),
  tar_target(fit_roles, stanfit_play_focal(segmented_data, roles)),
  
  # Plot results (saves to "fig" folder)
  tar_target(fig_3_a, fig_classifier(post_msm, msm_standata)),
  tar_target(fig_3_b, fig_segment_example(post_msm, d, msm_standata)),
  tar_target(fig_3_c, fig_ctmc(post_msm, msm_standata)),
  tar_target(fig_3_d, fig_pr_rest(post_msm)),
  tar_target(fig_3_e, fig_duration_play(segmented_data, msm_standata)),
  tar_target(fig_4, fig4_behavior(fit_behavior))
  
)
