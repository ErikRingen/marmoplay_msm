# First, source all files in dir to get bigger named functions
functions <- list.files(
  path = "R",
  pattern = "*.R",
  recursive = TRUE)

# Clean up environment
sapply(paste0("R/", functions[functions != "functions.R"]), source, .GlobalEnv)
rm(functions)

# Rethinking-style posterior objects from cmdstanr fits
extract_samples <- function(fit_obj) {
  vars <- fit_obj$metadata()$stan_variables
  draws <- posterior::as_draws_rvars(fit_obj$draws())
  
  lapply(vars, \(var_name){  
    posterior::draws_of(draws[[var_name]], with_chains = FALSE)
  }) |> setNames(vars)
}

clean_data <- function(file){

    d <- read_csv(file) %>% 
    mutate(
      start_time = as.numeric(substr(start_time, 1, 2)) * 3600 + as.numeric(substr(start_time, 4, 5)) * 60 + as.numeric(substr(start_time, 7, 8)) + as.numeric(substr(start_time, 10, 11)) / 24,
      end_time = as.numeric(substr(end_time, 1, 2)) * 3600 + as.numeric(substr(end_time, 4, 5)) * 60 + as.numeric(substr(end_time, 7, 8)) + as.numeric(substr(end_time, 10, 11)) / 24,
      duration = as.numeric(substr(duration, 1, 2)) * 3600 + as.numeric(substr(duration, 4, 5)) * 60 + as.numeric(substr(duration, 7, 8)) + as.numeric(substr(duration, 10, 11)) / 24,
      session_name = paste0(group, ": ", "session ", session)
    ) %>% 
    filter(behavior != "groom") # drop grooming, will just result in the gaps being "rest"
  
  return(d)
}
