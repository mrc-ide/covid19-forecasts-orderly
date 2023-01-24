## Incidence object from time-series
## ts is a data frame containing two columns only
## a column that contains dates, and another one that
## contains a case count for the corresponding date.
## Returns an incidence object
ts_to_incid <- function(ts, date_col, case_col) {
  
  first_date <- min(ts[[date_col]])
  last_date <- max(ts[[date_col]])
  if(any(ts[[case_col]]%%1!=0)){
   ts[[case_col]] <- round(ts[[case_col]])
  }
  x <- tidyr::uncount(ts, weights = ts[[case_col]])
  out <- incidence(
    x[[date_col]],
    first_date = first_date,
    last_date = last_date
  )
  out
}

r_samples <- function(res, n = 10000) {
  
  in_last_window <- tail(res, 1)
  mean_r <- in_last_window$`Mean(R)`
  std_r <- in_last_window$`Std(R)`
  reparam <- epitrix::gamma_mucv2shapescale(
    mu = mean_r,
    cv = std_r / mean_r
  )
  out <- rgamma(
    n = n,
    shape = reparam$shape,
    scale = reparam$scale
  )
  out
  
}

first_nonzero_incidence <- function(x) {
  
  counts <- as.numeric(get_counts(x))
  which(counts > 0)[1]
  
  
}

## x is an incidence object.
## Get the date of first non-null incidence
windows_endpoints <- function(x, tw = 7) {
  
  
  idx <- first_nonzero_incidence(x)
  ndays <- dim(x)[1]
  
  ## From a day after the first day with non-null incidence
  if (idx >= (ndays - tw)) {
    
    warning("Default time window of ", tw, "days cannot be used.")
    warning("Not enough days with non-null incidence.")
    
    if (ndays - idx - 2 > 0) {
      tw <- ndays - idx - 2
      message("Using time window ", tw)
    }
    else {
      stop("Error, cannot use the smallest allowed time window")
    }
    
  }
  
  t_start <- seq(from = idx + 1, to = ndays - tw, by = 1)
  t_end <- t_start + tw
  
  list(
    t_start = t_start,
    t_end = t_end
  )
  
}