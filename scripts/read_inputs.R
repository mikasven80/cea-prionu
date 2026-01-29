clean_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_trim() %>%
                  str_replace_all("[^A-Za-z0-9]+", "_") %>%
                  str_replace_all("_+$", "") %>%
                  str_to_lower()
    )
}

read_model_inputs <- function(xlsx_path, age_start, cycles) {
  stopifnot(file.exists(xlsx_path))
  ages_needed <- age_start:(age_start + cycles - 1)
  
  trans_raw <- read_excel(xlsx_path, sheet = 1)
  util_raw  <- read_excel(xlsx_path, sheet = 2)
  
  trans_tbl <- clean_names(trans_raw)
  util_tbl  <- clean_names(util_raw)
  
  # transitions
  required_trans <- c("age", "p_ef_mi", "p_ef_d", "p_mi_d", "p_postmi_d")
  stopifnot(all(required_trans %in% names(trans_tbl)))
  
  trans_tbl <- trans_tbl %>%
    transmute(
      age = as.integer(age),
      p_EF_MI     = as.numeric(p_ef_mi),
      p_EF_D      = as.numeric(p_ef_d),
      p_MI_D      = as.numeric(p_mi_d),
      p_PostMI_D  = as.numeric(p_postmi_d)
    ) %>%
    arrange(age)
  
  stopifnot(all(ages_needed %in% trans_tbl$age))
  stopifnot(all(trans_tbl$p_EF_MI >= 0 & trans_tbl$p_EF_MI <= 1))
  stopifnot(all(trans_tbl$p_EF_D  >= 0 & trans_tbl$p_EF_D  <= 1))
  stopifnot(all(trans_tbl$p_MI_D  >= 0 & trans_tbl$p_MI_D  <= 1))
  stopifnot(all(trans_tbl$p_PostMI_D >= 0 & trans_tbl$p_PostMI_D <= 1))
  stopifnot(all(trans_tbl$p_EF_MI + trans_tbl$p_EF_D <= 1))
  
  # utilities
  required_util <- c("age", "u_ef", "u_mi", "u_post_mi")
  stopifnot(all(required_util %in% names(util_tbl)))
  
  util_tbl <- util_tbl %>%
    transmute(
      age = as.integer(age),
      u_EF     = as.numeric(u_ef),
      u_MI     = as.numeric(u_mi),
      u_PostMI = as.numeric(u_post_mi)
    ) %>%
    arrange(age)
  
  stopifnot(all(ages_needed %in% util_tbl$age))
  stopifnot(all(util_tbl$u_EF >= 0 & util_tbl$u_EF <= 1))
  stopifnot(all(util_tbl$u_MI >= 0 & util_tbl$u_MI <= 1))
  stopifnot(all(util_tbl$u_PostMI >= 0 & util_tbl$u_PostMI <= 1))
  
  list(trans_tbl = trans_tbl, util_tbl = util_tbl)
}
