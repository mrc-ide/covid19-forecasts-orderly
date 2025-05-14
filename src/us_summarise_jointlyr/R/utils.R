#####

# Function to extract daily quantiles

extract_predictions_qntls_multi <- function (y, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  result <- purrr::imap_dfr(y, function(week_data, week_name) {
    purrr::imap_dfr(week_data, function(state_data, state_name) {
      
      if (is.null(state_data)) {
        message("Skipping state: ", state_name, " in week: ", week_name, " because it is NULL")
        return(NULL)
      }
      
      if (is.null(colnames(state_data))) {
        colnames(state_data) <- as.character(
          seq.Date(as.Date(week_name) + 1,
                   as.Date(week_name) + 28,
                   by = "day")
        )
      }
      
      out2 <- t(apply(state_data, 2, stats::quantile, prob = prob, na.rm = TRUE))
      out2 <- as.data.frame(out2)
      
      # Add rownames as a column called "date"
      out2 <- tibble::rownames_to_column(out2, var = "date")
      
      # Add columns for projection week and state
      out2$projection_week <- week_name
      out2$state <- state_name
      
      return(out2)
      
    }) # End of state iteration
    
  }) # End of projection week iteration
  
  return(result)
}

#####

# Function for weekly quantiles

daily_to_weekly_multi <- function(y, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  result <- purrr::imap_dfr(y, function(week_data, week_name) {
    purrr::imap_dfr(week_data, function(state_data, state_name) {
      
      # Check if the state data is NULL, if so, skip it:
      if (is.null(state_data)) {
        message("Skipping state: ", state_name, " in week: ", week_name, " because it is NULL")
        return(NULL)
      }
      n_days <- ncol(state_data)
      n_weeks <- n_days / 7
      
      # Group data by week and calculate weekly totals
      weekly_totals <- matrix(0, nrow = nrow(state_data), ncol = n_weeks)
      
      for (week_index in seq_len(n_weeks)) {
        # Calculate the column range for each week
        start_col <- (week_index - 1) * 7 + 1
        end_col <- start_col + 6
        
        # Sum the daily data over the week
        weekly_totals[, week_index] <- rowSums(state_data[, start_col:end_col, drop = FALSE], na.rm = TRUE)
      }
      
      # Quantiles for each week's totals
      weekly_quantiles <- apply(weekly_totals, 2, stats::quantile, prob = prob, na.rm = TRUE)
      weekly_df <- as.data.frame(t(weekly_quantiles))
      if (is.null(colnames(state_data))) {
        colnames(state_data) <- as.character(
          seq.Date(as.Date(week_name) + 1,
                   as.Date(week_name) + 28,
                   by = "day")
        )
      }
      weekly_df$week_ending <- as.Date(colnames(state_data)[seq(7, n_days, by = 7)])
      
      weekly_df <- tibble::rownames_to_column(weekly_df, var = "forecast_week")
      
      # Add columns for projection week and state
      weekly_df$projection_week <- week_name
      weekly_df$state <- state_name
      
      return(weekly_df)
    })
  })
  return(result)
}

#####


# Daily CRPS
crps_estimates_daily <- function (y, true_inc) {
  result <- purrr::imap_dfr(y, function(week_data, week_name) {
    purrr::imap_dfr(week_data, function(state_data, state_name) {
      
      if (is.null(state_data)) {
        message("Skipping state: ", state_name, " in week: ", week_name, " because it is NULL")
        return(NULL)
      }
      
      if (is.null(colnames(state_data))) {
        colnames(state_data) <- as.character(
          seq.Date(as.Date(week_name) + 1,
                   as.Date(week_name) + 28,
                   by = "day")
        )
      }
      
      inc_with_dates <- true_inc %>%
        select(dates, state_name) %>%
        filter(
          dates >= as.Date(week_name) + 1 & dates <= as.Date(week_name) + 28
        )
      
      inc <- inc_with_dates %>%
        select(-dates) %>%
        rename(state = state_name)
      
      # Apply log(x + 1) transformation to both true values and predictions
      transformed_inc <- log(inc$state + 1)
      transformed_predictions <- log(as.matrix(t(state_data)) + 1)
      
      crps <- crps_sample(true_values = as.vector(transformed_inc), predictions = transformed_predictions)
      
      # out2 <- t(apply(state_data, 2, crps_sample, observed = inc))
      crps <- as.data.frame(crps, row.names = as.character(inc_with_dates$dates))
      crps$true_inc_transformed <- transformed_inc
      
      # Add rownames as a column called "date"
      crps <- tibble::rownames_to_column(crps, var = "date")
      
      # Add columns for projection week and state
      crps$projection_week <- week_name
      crps$state <- state_name
      
      return(crps)
      
    }) # End of state iteration
    
  }) # End of projection week iteration
  
  return(result)
}







# Weekly CRPS

crps_estimates_weekly <- function (y, true_inc) {
  result <- purrr::imap_dfr(y, function(week_data, week_name) {
    purrr::imap_dfr(week_data, function(state_data, state_name) {
      
      if (is.null(state_data)) {
        message("Skipping state: ", state_name, " in week: ", week_name, " because it is NULL")
        return(NULL)
      }
      
      if (is.null(colnames(state_data))) {
        colnames(state_data) <- as.character(
          seq.Date(as.Date(week_name) + 1,
                   as.Date(week_name) + 28,
                   by = "day")
        )
      }
      
      inc_with_dates <- true_inc %>%
        select(dates, state_name) %>%
        filter(
          dates >= as.Date(week_name) + 1 & dates <= as.Date(week_name) + 28
        )
      
      inc <- inc_with_dates %>%
        select(-dates) %>%
        rename(state = state_name)
      
      n_days <- ncol(state_data)
      n_weeks <- n_days / 7
      
      # Group data by week and calculate weekly totals
      weekly_totals <- matrix(0, nrow = nrow(state_data), ncol = n_weeks)
      weekly_inc <- vector()
      
      for (week_index in seq_len(n_weeks)) {
        # Calculate the column range for each week
        start_idx <- (week_index - 1) * 7 + 1
        end_idx <- start_idx + 6
        
        # Sum the daily data over the week
        weekly_totals[, week_index] <- rowSums(state_data[, start_idx:end_idx, drop = FALSE], na.rm = TRUE)
        weekly_inc[week_index] <- sum(inc$state[start_idx:end_idx], na.rm = TRUE)
      }
      
      # Apply log(x + 1) transformation
      transformed_weekly_inc <- log(weekly_inc + 1)
      transformed_weekly_totals <- log(weekly_totals + 1)
      
      crps <- crps_sample(true_values = as.vector(transformed_weekly_inc), predictions = as.matrix(t(transformed_weekly_totals)))
      
      weekly_df <- as.data.frame(crps)
      weekly_df$week_ending <- as.Date(colnames(state_data)[seq(7, n_days, by = 7)])
      weekly_df$true_inc_transformed <- transformed_weekly_inc
      
      weekly_df <- tibble::rownames_to_column(weekly_df, var = "forecast_week")
      
      # Add columns for projection week and state
      weekly_df$projection_week <- week_name
      weekly_df$state <- state_name
      
      return(weekly_df)
    })
  })
  return(result)
}
