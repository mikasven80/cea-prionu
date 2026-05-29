build_and_run_soc <- function(cfg, tables) {
  trans_tbl <- tables$trans_tbl
  util_tbl  <- tables$util_tbl
  
  param_soc <- define_parameters(
    age = cfg$age_start + model_time - 1,
    
    p_EF_MI = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_MI"),
    p_EF_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_D"),
    p_MI_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_MI_D"),
    p_PM_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_PostMI_D"),
    
    p_EF_EF = 1 - p_EF_MI - p_EF_D,
    p_MI_PM = 1 - p_MI_D,
    p_PM_PM = 1 - p_PM_D,
    
    u_EF     = look_up(util_tbl, age = age, bin = "age", value = "u_EF"),
    u_MI     = look_up(util_tbl, age = age, bin = "age", value = "u_MI"),
    u_PostMI = look_up(util_tbl, age = age, bin = "age", value = "u_PostMI")
  )
  
  state_EF   <- define_state(cost = 0,             utility = u_EF,     ly = 1)
  state_MI   <- define_state(cost = cfg$c_MI,      utility = u_MI,     ly = 1)
  state_PM   <- define_state(cost = cfg$c_PostMI,  utility = u_PostMI, ly = 1)
  state_dead <- define_state(cost = 0,             utility = 0,        ly = 0)

  mat_soc <- define_transition(
    state_names = c("Event_free", "MI", "Post_MI", "Dead"),
    p_EF_EF, p_EF_MI, 0,       p_EF_D,
    0,       0,       (1 - p_MI_D), p_MI_D,
    0,       0,       (1 - p_PM_D), p_PM_D,
    0,       0,       0,       1
  )
  
  strat_soc <- define_strategy(
    transition = mat_soc,
    Event_free = state_EF,
    MI         = state_MI,
    Post_MI    = state_PM,
    Dead       = state_dead
  )
  
  run_model(
    soc = strat_soc,
    parameters = param_soc,
    cycles = cfg$cycles,
    init = c(Event_free = cfg$cohort_size, MI = 0, Post_MI = 0, Dead = 0),
    method = "life-table",
    cost = cost,
    effect = utility
  )
}

build_and_run_intervention <- function(cfg, tables) {
  trans_tbl <- tables$trans_tbl
  util_tbl  <- tables$util_tbl
  
  # Funnel → fractions (general population)
  p_scan  <- cfg$p_resp * cfg$p_highrisk * cfg$p_attendCT
  p_treat <- p_scan * cfg$p_cac100 * cfg$p_uptake
  
  # Program cost per person in cycle 1 (expected value in general population)
  c_program_0 <- cfg$c_survey_per_person +
    p_scan * (cfg$c_ct + cfg$p_incidental * cfg$c_incidental_follow)
  
  param_int <- define_parameters(
    age = cfg$age_start + model_time - 1,
    
    p_EF_MI = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_MI"),
    p_EF_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_D"),
    p_MI_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_MI_D"),
    p_PM_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_PostMI_D"),
    
    p_EF_EF = 1 - p_EF_MI - p_EF_D,
    p_MI_PM = 1 - p_MI_D,
    p_PM_PM = 1 - p_PM_D,
    
    u_EF     = look_up(util_tbl, age = age, bin = "age", value = "u_EF"),
    u_MI     = look_up(util_tbl, age = age, bin = "age", value = "u_MI"),
    u_PostMI = look_up(util_tbl, age = age, bin = "age", value = "u_PostMI"),
    
    # HR on hazard/rate scale for treated EF->MI
    r_EF_MI     = p_to_rate(p_EF_MI),
    r_EF_MI_tx  = r_EF_MI * cfg$HR_tx,
    p_EF_MI_tx  = rate_to_p(r_EF_MI_tx),
    p_EFtx_EFtx = 1 - p_EF_MI_tx - p_EF_D,
    
    # one-off program cost in cycle 1; apply only in EF states
    c_program = ifelse(model_time == 1, c_program_0, 0)
  )
  
  state_EF_noTx <- define_state(cost = 0 + c_program,                  utility = u_EF,     ly = 1)
  state_EF_Tx   <- define_state(cost = cfg$c_tx_annual + c_program,    utility = u_EF,     ly = 1)
  
  # MI and Post-MI costs same as SoC; c_PostMI already includes drug treatment costs
  state_MI      <- define_state(cost = cfg$c_MI,      utility = u_MI,     ly = 1)
  state_PM      <- define_state(cost = cfg$c_PostMI,  utility = u_PostMI, ly = 1)
  state_dead    <- define_state(cost = 0,             utility = 0,        ly = 0)
  
  mat_int <- define_transition(
    state_names = c("EF_noTx", "EF_Tx", "MI", "Post_MI", "Dead"),
    p_EF_EF,      0,          p_EF_MI,     0,       p_EF_D,
    0,       p_EFtx_EFtx,     p_EF_MI_tx,  0,       p_EF_D,
    0,            0,          0,           (1 - p_MI_D), p_MI_D,
    0,            0,          0,           (1 - p_PM_D), p_PM_D,
    0,            0,          0,           0,       1
  )
  
  strat_int <- define_strategy(
    transition = mat_int,
    EF_noTx  = state_EF_noTx,
    EF_Tx    = state_EF_Tx,
    MI       = state_MI,
    Post_MI  = state_PM,
    Dead     = state_dead
  )
  
  res_int <- run_model(
    intervention = strat_int,
    parameters = param_int,
    cycles = cfg$cycles,
    init = c(
      EF_noTx = cfg$cohort_size * (1 - p_treat),
      EF_Tx   = cfg$cohort_size * p_treat,
      MI      = 0,
      Post_MI = 0,
      Dead    = 0
    ),
    method = "life-table",
    cost = cost,
    effect = utility
  )
  
  attr(res_int, "p_scan") <- p_scan
  attr(res_int, "p_treat") <- p_treat
  attr(res_int, "c_program_0") <- c_program_0
  
  res_int
}
