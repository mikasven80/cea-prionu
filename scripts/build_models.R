# Markov models for the two strategies.
#
# The event-free state is stratified by coronary artery calcium status. Everyone
# the pathway treats has CAC >=100 and therefore a higher risk of myocardial
# infarction than the cohort average; applying the treatment effect to the
# cohort-average hazard would understate the absolute benefit. The same
# individuals carry the same elevated risk under standard of care, so the
# stratification is applied to both arms and cancels for everyone else.
#
#   Standard of care : EF_low + EF_high        -> MI -> Post_MI -> Dead
#   Intervention     : EF_low + EF_high_Tx     -> MI -> Post_MI -> Dead
#
# cfg$cac100_risk_mult is the multiplier on the event-free -> MI hazard for the
# CAC >=100 stratum. Setting it to 1 reproduces the unstratified model.

# Screening funnel fractions and the one-off programme cost per invitee.
funnel_fractions <- function(cfg) {
  p_scan  <- cfg$p_resp * cfg$p_highrisk * cfg$p_attendCT
  p_treat <- p_scan * cfg$p_cac100 * cfg$p_uptake
  c_program_0 <- cfg$c_survey_per_person +
    p_scan * (cfg$c_ct + cfg$p_incidental * cfg$c_incidental_follow)
  list(p_scan = p_scan, p_treat = p_treat, c_program_0 = c_program_0)
}

build_and_run_soc <- function(cfg, tables) {
  trans_tbl <- tables$trans_tbl
  util_tbl  <- tables$util_tbl
  f <- funnel_fractions(cfg)

  param_soc <- define_parameters(
    age = cfg$age_start + model_time - 1,

    p_EF_MI = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_MI"),
    p_EF_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_D"),
    p_MI_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_MI_D"),
    p_PM_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_PostMI_D"),

    # CAC >=100 stratum: same competing mortality, elevated MI hazard
    r_EF_MI      = p_to_rate(p_EF_MI),
    p_EF_MI_high = rate_to_p(r_EF_MI * cfg$cac100_risk_mult),

    p_EF_EF      = 1 - p_EF_MI - p_EF_D,
    p_EFhi_EFhi  = 1 - p_EF_MI_high - p_EF_D,
    p_MI_PM = 1 - p_MI_D,
    p_PM_PM = 1 - p_PM_D,

    u_EF     = look_up(util_tbl, age = age, bin = "age", value = "u_EF"),
    u_MI     = look_up(util_tbl, age = age, bin = "age", value = "u_MI"),
    u_PostMI = look_up(util_tbl, age = age, bin = "age", value = "u_PostMI")
  )

  # Cost components are carried as separate state values so that they sum exactly
  # to `cost` under the same half-cycle correction (answers reviewer 2, comment 6).
  state_EF_low  <- define_state(cost = 0, utility = u_EF, ly = 1,
                                c_screen_survey = 0, c_screen_ct = 0,
                                c_screen_incid = 0, c_llt = 0, c_mi = 0, c_pm = 0)
  state_EF_high <- define_state(cost = 0, utility = u_EF, ly = 1,
                                c_screen_survey = 0, c_screen_ct = 0,
                                c_screen_incid = 0, c_llt = 0, c_mi = 0, c_pm = 0)
  state_MI      <- define_state(cost = cfg$c_MI, utility = u_MI, ly = 1,
                                c_screen_survey = 0, c_screen_ct = 0,
                                c_screen_incid = 0, c_llt = 0, c_mi = cfg$c_MI, c_pm = 0)
  state_PM      <- define_state(cost = cfg$c_PostMI, utility = u_PostMI, ly = 1,
                                c_screen_survey = 0, c_screen_ct = 0,
                                c_screen_incid = 0, c_llt = 0, c_mi = 0, c_pm = cfg$c_PostMI)
  state_dead    <- define_state(cost = 0, utility = 0, ly = 0,
                                c_screen_survey = 0, c_screen_ct = 0,
                                c_screen_incid = 0, c_llt = 0, c_mi = 0, c_pm = 0)

  mat_soc <- define_transition(
    state_names = c("EF_low", "EF_high", "MI", "Post_MI", "Dead"),
    p_EF_EF,      0,            p_EF_MI,      0,            p_EF_D,
    0,            p_EFhi_EFhi,  p_EF_MI_high, 0,            p_EF_D,
    0,            0,            0,            (1 - p_MI_D), p_MI_D,
    0,            0,            0,            (1 - p_PM_D), p_PM_D,
    0,            0,            0,            0,            1
  )

  strat_soc <- define_strategy(
    transition = mat_soc,
    EF_low  = state_EF_low,
    EF_high = state_EF_high,
    MI      = state_MI,
    Post_MI = state_PM,
    Dead    = state_dead
  )

  res_soc <- run_model(
    soc = strat_soc,
    parameters = param_soc,
    cycles = cfg$cycles,
    init = c(
      EF_low  = cfg$cohort_size * (1 - f$p_treat),
      EF_high = cfg$cohort_size * f$p_treat,
      MI = 0, Post_MI = 0, Dead = 0
    ),
    method = "life-table",
    cost = cost,
    effect = utility
  )

  attr(res_soc, "p_treat") <- f$p_treat
  res_soc
}

build_and_run_intervention <- function(cfg, tables) {
  trans_tbl <- tables$trans_tbl
  util_tbl  <- tables$util_tbl
  f <- funnel_fractions(cfg)

  param_int <- define_parameters(
    age = cfg$age_start + model_time - 1,

    p_EF_MI = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_MI"),
    p_EF_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_EF_D"),
    p_MI_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_MI_D"),
    p_PM_D  = look_up(trans_tbl, age = age, bin = "age", value = "p_PostMI_D"),

    u_EF     = look_up(util_tbl, age = age, bin = "age", value = "u_EF"),
    u_MI     = look_up(util_tbl, age = age, bin = "age", value = "u_MI"),
    u_PostMI = look_up(util_tbl, age = age, bin = "age", value = "u_PostMI"),

    # CAC >=100 stratum, treated: elevated hazard, then the treatment effect
    r_EF_MI         = p_to_rate(p_EF_MI),
    r_EF_MI_high    = r_EF_MI * cfg$cac100_risk_mult,
    p_EF_MI_high_tx = rate_to_p(r_EF_MI_high * cfg$HR_tx),

    p_EF_EF        = 1 - p_EF_MI - p_EF_D,
    p_EFhitx_EFhitx = 1 - p_EF_MI_high_tx - p_EF_D,

    # one-off programme cost in cycle 1; applied in the event-free states,
    # carried both in total and split into its three components
    c_program     = ifelse(model_time == 1, f$c_program_0, 0),
    c_prog_survey = ifelse(model_time == 1, cfg$c_survey_per_person, 0),
    c_prog_ct     = ifelse(model_time == 1, f$p_scan * cfg$c_ct, 0),
    c_prog_incid  = ifelse(model_time == 1,
                           f$p_scan * cfg$p_incidental * cfg$c_incidental_follow, 0)
  )

  state_EF_low     <- define_state(cost = 0 + c_program, utility = u_EF, ly = 1,
                                   c_screen_survey = c_prog_survey, c_screen_ct = c_prog_ct,
                                   c_screen_incid = c_prog_incid, c_llt = 0, c_mi = 0, c_pm = 0)
  state_EF_high_Tx <- define_state(cost = cfg$c_tx_annual + c_program, utility = u_EF, ly = 1,
                                   c_screen_survey = c_prog_survey, c_screen_ct = c_prog_ct,
                                   c_screen_incid = c_prog_incid, c_llt = cfg$c_tx_annual,
                                   c_mi = 0, c_pm = 0)
  state_MI         <- define_state(cost = cfg$c_MI, utility = u_MI, ly = 1,
                                   c_screen_survey = 0, c_screen_ct = 0, c_screen_incid = 0,
                                   c_llt = 0, c_mi = cfg$c_MI, c_pm = 0)
  state_PM         <- define_state(cost = cfg$c_PostMI, utility = u_PostMI, ly = 1,
                                   c_screen_survey = 0, c_screen_ct = 0, c_screen_incid = 0,
                                   c_llt = 0, c_mi = 0, c_pm = cfg$c_PostMI)
  state_dead       <- define_state(cost = 0, utility = 0, ly = 0,
                                   c_screen_survey = 0, c_screen_ct = 0, c_screen_incid = 0,
                                   c_llt = 0, c_mi = 0, c_pm = 0)

  mat_int <- define_transition(
    state_names = c("EF_low", "EF_high_Tx", "MI", "Post_MI", "Dead"),
    p_EF_EF, 0,               p_EF_MI,         0,            p_EF_D,
    0,       p_EFhitx_EFhitx, p_EF_MI_high_tx, 0,            p_EF_D,
    0,       0,               0,               (1 - p_MI_D), p_MI_D,
    0,       0,               0,               (1 - p_PM_D), p_PM_D,
    0,       0,               0,               0,            1
  )

  strat_int <- define_strategy(
    transition = mat_int,
    EF_low     = state_EF_low,
    EF_high_Tx = state_EF_high_Tx,
    MI         = state_MI,
    Post_MI    = state_PM,
    Dead       = state_dead
  )

  res_int <- run_model(
    intervention = strat_int,
    parameters = param_int,
    cycles = cfg$cycles,
    init = c(
      EF_low     = cfg$cohort_size * (1 - f$p_treat),
      EF_high_Tx = cfg$cohort_size * f$p_treat,
      MI = 0, Post_MI = 0, Dead = 0
    ),
    method = "life-table",
    cost = cost,
    effect = utility
  )

  attr(res_int, "p_scan")      <- f$p_scan
  attr(res_int, "p_treat")     <- f$p_treat
  attr(res_int, "c_program_0") <- f$c_program_0

  res_int
}
