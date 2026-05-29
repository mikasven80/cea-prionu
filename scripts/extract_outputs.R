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

extract_state_membership <- function(res_soc, res_int, cohort_size) {
  # Extract counts from SoC (long format -> wide)
  counts_soc <- get_counts(res_soc) %>%
    select(model_time, state_names, count) %>%
    pivot_wider(names_from = state_names, values_from = count) %>%
    rename(Cycle = model_time) %>%
    rename_with(~ paste0(.x, "_SoC"), -Cycle)

  # Extract counts from Intervention (long format -> wide)
  counts_int <- get_counts(res_int) %>%
    select(model_time, state_names, count) %>%
    pivot_wider(names_from = state_names, values_from = count) %>%
    rename(Cycle = model_time)

  # Combine EF_noTx and EF_Tx into Event_free for intervention
  counts_int <- counts_int %>%
    mutate(Event_free = EF_noTx + EF_Tx) %>%
    select(Cycle, Event_free, MI, Post_MI, Dead) %>%
    rename_with(~ paste0(.x, "_Int"), -Cycle)

  # Join both arms

  state_tbl <- counts_soc %>%
    left_join(counts_int, by = "Cycle") %>%
    select(
      Cycle,
      Event_free_SoC, Event_free_Int,
      MI_SoC, MI_Int,
      Post_MI_SoC, Post_MI_Int,
      Dead_SoC, Dead_Int
    )

  state_tbl
}
