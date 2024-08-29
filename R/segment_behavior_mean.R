classifier_mean <- function(post, d, standat){
  
  sessions <- unique(d$session_name)
  
  N_times <- c()
  
  out <- data.frame(duration = c(), behavior = c(), y_cat = c(), lag_pause = c(), lag_behavior = c(), focal = c())
  
  original_sess_duration <- c()
  
  for (j in 1:length(sessions)) {
    # Construct time series using end times
    # start by filtering one session
    d_sess <- d %>% 
      filter(session_name == sessions[j])
    
    d_temp <- time_series_expand(d_sess) %>% 
      mutate(lag_behavior = lag(behavior))
    
    out <- bind_rows(out, d_temp)
    N_times[j] = nrow(d_temp)
    
    original_sess_duration[j] = max(d_sess$end_time)
  }
  
  ### Use classifier probs to determine how signal influences duration of play
  rest <- rbinom(length(post$post_pr_rest[1,]), 1, apply(post$post_pr_rest, 2, mean))
  
  rest_before_play <- standat$duration[which(lag(rest == 1) & standat$y == 3)]
  signal_before_play <- standat$duration[which(lag(rest == 0))]
  
  #out$y_cat <- ifelse(out$y_cat == "wrestle_out", "wrestle", out$y_cat)
  
  # Construct new durations for play
  play_duration <- c()
  lag_duration <- c()
  type <- c()
  type_cat <- c()
  lag_type <- c()
  behavior <- c()
  session_id <- c()
  focal <- c()
  receiver <- c()
  
  ticker <- 1
  session_ticker <- 1
  play_duration[ticker] <- standat$duration[1]
  lag_duration[ticker] <- standat$lag_duration[1]
  type[ticker] <- standat$y[1]
  type_cat[ticker] <- out$y_cat[1]
  lag_type[ticker] <- NA
  behavior[ticker] <- out$behavior[1]
  session_id[ticker] <- session_ticker
  focal[ticker] <- out$focal[1]
  receiver[ticker] <- out$receiver[1]
  
  for (i in 2:length(standat$y)) {
    
    session_id[ticker] <- session_ticker
    
    if (is.na(rest[i])) {
      session_ticker <- session_ticker + 1
      ticker <- ticker + 1 # move on to new duration
      play_duration[ticker] <- standat$duration[i]
      type[ticker] <- standat$y[i]
      type_cat[ticker] <- out$y_cat[i]
      lag_type[ticker] <- NA
      behavior[ticker] <- out$behavior[i]
      focal[ticker] <- out$focal[i]
      receiver[ticker] <- out$receiver[i]
    }
    
    if (!(is.na(rest[i]))) {
      
      if (standat$y[i] == standat$y[i - 1] & rest[i] == 0) {
        play_duration[ticker] = play_duration[ticker] + standat$duration[i]
        lag_duration[ticker] = standat$lag_duration[i]
        
        type_cat[ticker] <- paste(type_cat[ticker], out$y_cat[i], sep = ",")
        
        focal[ticker] <- paste(focal[ticker], out$focal[i], sep = ",")
        receiver[ticker] <- paste(receiver[ticker], out$receiver[i], sep = ",")
      }
      
      if (standat$y[i] != standat$y[i - 1] | rest[i] == 1) {
        ticker <- ticker + 1 # move on to new duration
        play_duration[ticker] <- standat$duration[i]
        lag_duration[ticker] = standat$lag_duration[i]
        type[ticker] <- standat$y[i]
        
        lag_type[ticker] <- ifelse(rest[i] == 1, 1, type[ticker - 1])
        type_cat[ticker] <- out$y_cat[i]
        behavior[ticker] <- out$behavior[i]
        focal[ticker] <- out$focal[i]
        receiver[ticker] <- out$receiver[i]
      }
    }
  }
  
  ## Count number of unique behaviors
  values_list <- strsplit(type_cat, ",")
  
  n_cat <- sapply(values_list, function(x) length(unique(x)))
  
  # Figure out whether wrestle occurred during play
  wrest <- sapply(values_list, function(x) any(x == "wrestle"))
  chase <- sapply(values_list, function(x) any(x == "chase"))
  supine <- sapply(values_list, function(x) any(x == "supine"))
  stalk <- sapply(values_list, function(x) any(x == "si_stalk"))
  hide <- sapply(values_list, function(x) any(x == "hide"))
  wrestle_out <- sapply(values_list, function(x) any(x == "wrestle_out"))
  
  # Unlist to convert the list of lists to a single vector
  focal_list <- strsplit(focal, ",")
  receiver_list <- strsplit(receiver, ",")
  
  # Figure out the unique dyads within each bout
  
  # Create a function that sorts and pastes together elements of dyads
  paste_sorted_dyads <- function(x, y) {
    sapply(1:length(x), function(i) paste(sort(c(x[i], y[i])), collapse = "-"))
  }
  
  # Use Map to apply the function to each corresponding sublist
  sorted_dyads <- Map(paste_sorted_dyads, focal_list, receiver_list)
  
  # Now count the unique dyads within each sublist
  unique_dyads_count_per_sublist <- lapply(sorted_dyads, function(dyad_list) {
    table(dyad_list)
  })
  
  count_unique_dyads_excluding_NA <- function(dyad_table) {
    # Convert the names of the table (which are the dyad strings) into a vector
    dyad_strings <- names(dyad_table)
    # Exclude dyads ending with "NA"
    valid_dyad_strings <- dyad_strings[!grepl("NA$", dyad_strings)]
    # Return the count of valid dyads
    length(valid_dyad_strings)
  }
  
  # Use lapply to apply the function to each sublist's table of dyads
  unique_dyads_counts_excluding_NA <- lapply(unique_dyads_count_per_sublist, count_unique_dyads_excluding_NA)
  
  # Print the count of unique dyads excluding those that end with "NA" for each sublist
  n_dyads <- unlist(unique_dyads_counts_excluding_NA)
  
  
  ## Organize into a dataframe
  d_types <- data.frame(
    duration = play_duration,
    lag_duration = lag_duration,
    type = type,
    lag_type = lag_type,
    n_cat = n_cat,
    n_dyads = n_dyads,
    wrest = wrest,
    chase = chase,
    hide = hide,
    supine = supine,
    stalk = stalk,
    session_id = session_id,
    focal_init = unlist(lapply(focal_list, `[[`, 1)),
    receiver_init = unlist(lapply(receiver_list, `[[`, 1)),
    wrestle_out = wrestle_out,
    focal_final = unlist(lapply(focal_list, function(x) x[length(x)])),
    receiver_final = unlist(lapply(receiver_list, function(x) x[length(x)])),
    behaviors = sapply(values_list, paste, collapse = "-")
  ) %>% 
    mutate(lag = case_when(
      lag_type == 1 ~ "rest before",
      lag_type == 2 ~ "signal before",
      lag_type == 3 ~ "play before"
    )) %>% 
    group_by(session_id) %>% 
    mutate(lag_focal = lag(focal_init), lag_receiver = lag(receiver_init), lag_hide = lag(hide),
           lag_stalk = lag(stalk),
           lag_supine = lag(supine)) %>% 
    ungroup()
  
  return(d_types)
}
