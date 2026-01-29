draw_psa_parameters <- function(cfg_psa) {
  n <- cfg_psa$n_sim
  
  tibble(
    HR_tx = pmin(rlnorm_from_mean_sd(n, cfg_psa$HR_tx_mean, cfg_psa$HR_tx_sd), 1.0),
    
    c_MI     = rgamma_mean_sd(n, cfg_psa$c_MI_mean, cfg_psa$c_MI_sd),
    c_PostMI = rgamma_mean_sd(n, cfg_psa$c_PostMI_mean, cfg_psa$c_PostMI_sd),
    c_tx_annual = rgamma_mean_sd(n, cfg_psa$c_tx_mean, cfg_psa$c_tx_sd),
    
    c_survey_per_person = rgamma_mean_sd(n, cfg_psa$c_survey_mean, cfg_psa$c_survey_sd),
    c_ct = rgamma_mean_sd(n, cfg_psa$c_ct_mean, cfg_psa$c_ct_sd),
    c_incidental_follow = rgamma_mean_sd(n, cfg_psa$c_inc_follow_mean, cfg_psa$c_inc_follow_sd),
    
    p_resp     = rbeta_mean_sd(n, cfg_psa$p_resp_mean, cfg_psa$p_resp_sd),
    p_highrisk = rbeta_mean_sd(n, cfg_psa$p_highrisk_mean, cfg_psa$p_highrisk_sd),
    p_attendCT = rbeta_mean_sd(n, cfg_psa$p_attendCT_mean, cfg_psa$p_attendCT_sd),
    p_cac100   = rbeta_mean_sd(n, cfg_psa$p_cac100_mean, cfg_psa$p_cac100_sd),
    
    p_incidental = rbeta_mean_sd(n, cfg_psa$p_incidental_mean, cfg_psa$p_incidental_sd)
  )
}

run_psa <- function(cfg, tables, disc_tbl) {
  set.seed(cfg$psa$seed)
  
  draws <- draw_psa_parameters(cfg$psa)
  n <- nrow(draws)
  
  res <- vector("list", n)
  
  for (i in seq_len(n)) {
    cfg_i <- cfg
    
    cfg_i$HR_tx       <- draws$HR_tx[i]
    cfg_i$c_MI        <- draws$c_MI[i]
    cfg_i$c_PostMI    <- draws$c_PostMI[i]
    cfg_i$c_tx_annual <- draws$c_tx_annual[i]
    
    cfg_i$c_survey_per_person <- draws$c_survey_per_person[i]
    cfg_i$c_ct                <- draws$c_ct[i]
    cfg_i$c_incidental_follow <- draws$c_incidental_follow[i]
    
    cfg_i$p_resp     <- draws$p_resp[i]
    cfg_i$p_highrisk <- draws$p_highrisk[i]
    cfg_i$p_attendCT <- draws$p_attendCT[i]
    cfg_i$p_cac100   <- draws$p_cac100[i]
    cfg_i$p_incidental <- draws$p_incidental[i]
    
    out_i <- tryCatch({
      soc  <- build_and_run_soc(cfg_i, tables)
      inte <- build_and_run_intervention(cfg_i, tables)
      
      tot_soc <- extract_discounted_totals_pp(soc, disc_tbl, cfg_i$cohort_size)
      tot_int <- extract_discounted_totals_pp(inte, disc_tbl, cfg_i$cohort_size)
      
      tibble(
        iter = i,
        cost_soc = tot_soc$dcost_pp,
        qaly_soc = tot_soc$dqaly_pp,
        cost_int = tot_int$dcost_pp,
        qaly_int = tot_int$dqaly_pp,
        inc_cost = cost_int - cost_soc,
        inc_qaly = qaly_int - qaly_soc
      )
    }, error = function(e) {
      message("PSA failed at iteration: ", i)
      message("Draws at failure:\n", paste(capture.output(print(draws[i, ])), collapse = "\n"))
      stop(e)
    })
    
    res[[i]] <- out_i
  }
  
  outcomes <- bind_rows(res)
  bind_cols(draws %>% mutate(iter = row_number()), outcomes %>% select(-iter))
}
