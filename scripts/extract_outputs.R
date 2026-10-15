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
  # Collapse the CAC-stratified event-free states (EF_low, EF_high, EF_high_Tx)
  # into a single Event_free column for reporting.
  collapse_ef <- function(res) {
    get_counts(res) %>%
      select(model_time, state_names, count) %>%
      mutate(state_names = ifelse(grepl("^EF", state_names), "Event_free", state_names)) %>%
      group_by(model_time, state_names) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      pivot_wider(names_from = state_names, values_from = count) %>%
      rename(Cycle = model_time) %>%
      select(Cycle, Event_free, MI, Post_MI, Dead)
  }

  counts_soc <- collapse_ef(res_soc) %>% rename_with(~ paste0(.x, "_SoC"), -Cycle)
  counts_int <- collapse_ef(res_int) %>% rename_with(~ paste0(.x, "_Int"), -Cycle)

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

# ---- Cost disaggregation (reviewer 2, comment 6) ----
# Discounted cost per person split into the components that make up the total.
# The components are carried as state values, so they sum exactly to `cost`.
extract_cost_components <- function(res, strategy_label, disc_tbl, cohort_size, fx = 1) {
  comps <- c(c_screen_survey = "Screening administration",
             c_screen_ct     = "CT diagnostics",
             c_screen_incid  = "Incidental finding management",
             c_llt           = "Lipid-lowering therapy",
             c_mi            = "Acute myocardial infarction",
             c_pm            = "Post-infarction care",
             cost            = "Total")
  get_values(res) %>%
    filter(value_names %in% names(comps)) %>%
    left_join(disc_tbl, by = "model_time") %>%
    group_by(value_names) %>%
    summarise(cost_pp = sum(value * w) / cohort_size / fx, .groups = "drop") %>%
    mutate(strategy = strategy_label,
           component = unname(comps[value_names])) %>%
    select(strategy, component, cost_pp)
}
