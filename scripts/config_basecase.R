get_config_basecase <- function() {
  list(
    # Global
    age_start   = 60,
    cycles      = 20,
    cohort_size = 1000,
    disc        = 0.03,

    # Currency display (model computes in SEK; figures shown in EUR)
    fx_sek_per_eur = 10.8,
    currency_label = "EUR",

    # Input paths
    xlsx_path = "data/transitions_soc.xlsx",
    
    # SoC / clinical costs
    c_MI     = 124600,
    c_PostMI = 18800,
    
    # Intervention: funnel (general population roll-out)
    p_resp     = 0.30,
    p_highrisk = 0.30,
    p_attendCT = 0.90,
    p_cac100   = 0.30,
    p_uptake   = 1.00,
    
    # Intervention: treatment
    HR_tx       = 0.40,
    c_tx_annual = 1900,  # only in event-free treated state
    
    # Intervention: program costs (cycle 1 expected value per targeted person)
    c_survey_per_person = 120,
    c_ct               = 1500,
    p_incidental        = 0.09,
    c_incidental_follow = 15000,
    
    # PSA settings
    psa = list(
      seed = 123,
      n_sim = 2000,
      
      # Uncertainty specs (starting values; tune to evidence)
      HR_tx_mean = 0.40,
      HR_tx_sd   = 0.05,
      
      c_MI_mean     = 124600, c_MI_sd     = 20000,
      c_PostMI_mean = 18800,  c_PostMI_sd = 2000,
      c_tx_mean     = 1900,   c_tx_sd     = 150,
      
      c_survey_mean = 120,  c_survey_sd = 20,
      c_ct_mean     = 1500, c_ct_sd     = 150,
      c_inc_follow_mean = 15000, c_inc_follow_sd = 3000,
      
      p_resp_mean     = 0.30, p_resp_sd     = 0.03,
      p_highrisk_mean = 0.30, p_highrisk_sd = 0.03,
      p_attendCT_mean = 0.90, p_attendCT_sd = 0.03,
      p_cac100_mean   = 0.30, p_cac100_sd   = 0.03,
      
      p_incidental_mean = 0.09, p_incidental_sd = 0.015,
      
      # CEAC grid
      wtp_max = 1000000,
      wtp_by  = 25000
    )
  )
}
