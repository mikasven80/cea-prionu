extract_discounted_totals_pp <- function(res, disc_tbl, cohort_size) {
  vals <- get_values(res)
  
  dcost <- vals %>%
    filter(value_names == "cost") %>%
    left_join(disc_tbl, by = "model_time") %>%
    summarise(x = sum(value * w)) %>%
    pull(x)
  
  dqaly <- vals %>%
    filter(value_names == "utility") %>%
    left_join(disc_tbl, by = "model_time") %>%
    summarise(x = sum(value * w)) %>%
    pull(x)
  
  tibble(
    dcost_pp = dcost / cohort_size,
    dqaly_pp = dqaly / cohort_size
  )
}

extract_cost_per_cycle <- function(res, strategy_label, disc_tbl = NULL, cohort_size = NULL) {
  vals <- get_values(res) %>%
    filter(value_names == "cost") %>%
    transmute(
      strategy = strategy_label,
      model_time,
      cost = value
    )
  
  if (!is.null(cohort_size)) {
    vals <- vals %>% mutate(cost_pp = cost / cohort_size)
  }
  
  if (!is.null(disc_tbl)) {
    vals <- vals %>%
      left_join(disc_tbl, by = "model_time") %>%
      mutate(
        dcost = cost * w,
        dcost_pp = if (!is.null(cohort_size)) dcost / cohort_size else NA_real_
      )
  }
  
  vals
}
