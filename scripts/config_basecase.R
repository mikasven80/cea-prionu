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
    # Gallone et al. adjusted pooled estimate for CAC >100 is an ODDS ratio of
    # 0.40; converted to a hazard ratio using the control-group ASCVD risk in
    # that stratum (400/4506 = 8.9%) this is 0.411.
    HR_tx       = 0.411,

    # Multiplier on the event-free -> MI hazard for the CAC >=100 stratum.
    # Calibrated so the 10-year first-MI risk from age 60 matches the untreated
    # CAC >100 risk observed in SCAPIS (~6.9%). Set to 1 for the unstratified model.
    # Alternative anchors: 0.91 = published observed risk (~5.0%, partly treated);
    # 1.45 = untreated risk crudely age-adjusted to 60 (~7.9%).
    cac100_risk_mult = 1.26,
    c_tx_annual = 1900,  # only in event-free treated state
    
    # Intervention: program costs (cycle 1 expected value per targeted person)
    c_survey_per_person = 120,
    c_ct               = 1500,
    # Incidental findings are modelled specifically as pulmonary nodules requiring
    # radiological follow-up. In PRIO-Now, 42 of 563 scanned participants (7.5%) had
    # lung parenchymal abnormalities and 32 of those were referred for follow-up
    # thoracic CT, i.e. 5.7% of those scanned; SCAPIS reports 6.1% on full-lung
    # imaging. We use 6%, which is if anything an overestimate, since a narrower
    # field of view in routine implementation would detect fewer.
    p_incidental        = 0.06,
    # Follow-up cost per nodule requiring investigation: mainly surveillance CT
    # (3,000-4,000 SEK at Sahlgrenska) with an allowance for radiologist and
    # clinician time. The previous value of 15,000 SEK is retained as a high-cost
    # scenario covering less well-defined downstream use.
    c_incidental_follow = 3780,
    
    # PSA settings
    psa = list(
      seed = 123,
      n_sim = 2000,
      
      # Uncertainty specs (starting values; tune to evidence)
      # SD on the natural scale chosen so that sigma(log) = 0.244, which is what
      # Gallone's CI for the adjusted CAC >100 estimate (0.40, 0.25 to 0.65)
      # implies. The previous value of 0.05 corresponded to sigma(log) = 0.12 and
      # understated the uncertainty in the most influential parameter.
      HR_tx_mean = 0.411,
      HR_tx_sd   = 0.102,
      
      c_MI_mean     = 124600, c_MI_sd     = 20000,
      c_PostMI_mean = 18800,  c_PostMI_sd = 2000,
      c_tx_mean     = 1900,   c_tx_sd     = 150,
      
      c_survey_mean = 120,  c_survey_sd = 20,
      c_ct_mean     = 1500, c_ct_sd     = 150,
      c_inc_follow_mean = 3780,  c_inc_follow_sd = 756,
      
      p_resp_mean     = 0.30, p_resp_sd     = 0.03,
      p_highrisk_mean = 0.30, p_highrisk_sd = 0.03,
      p_attendCT_mean = 0.90, p_attendCT_sd = 0.03,
      p_cac100_mean   = 0.30, p_cac100_sd   = 0.03,
      
      p_incidental_mean = 0.06, p_incidental_sd = 0.010,

      # MI risk multiplier for the CAC >=100 stratum. Lognormal, mean at the
      # base-case value with a coefficient of variation of 15%, giving a 95%
      # range of about 0.94 to 1.69 - wide enough to span the alternative
      # anchors (0.91 observed, 1.45 untreated age-adjusted).
      cac100_mult_mean = 1.26, cac100_mult_sd = 0.189,
      
      # CEAC grid
      wtp_max = 1000000,
      wtp_by  = 25000
    )
  )
}
