# probability <-> continuous-time rate
p_to_rate <- function(p) -log(pmax(1 - p, 1e-12))
rate_to_p <- function(r) 1 - exp(-pmax(r, 0))

make_disc_tbl <- function(age_start, cycles, disc) {
  tibble(
    model_time = 1:cycles,
    age = age_start + model_time - 1,
    t = model_time - 1,
    w = 1 / (1 + disc)^t
  )
}

# ---- PSA distribution helpers ----

# Robust Beta: clamps mean into (0,1), checks feasible variance
beta_params_from_mean_sd <- function(mean, sd, eps = 1e-6) {
  if (!is.finite(mean) || !is.finite(sd)) stop("Non-finite mean/sd.")
  if (sd <= 0) stop("SD must be > 0 for Beta parameterization.")
  
  mean <- min(max(mean, eps), 1 - eps)
  var <- sd^2
  
  max_var <- mean * (1 - mean)
  if (var >= max_var) {
    stop("Invalid SD: variance too large for Beta given mean.")
  }
  
  tmp <- mean * (1 - mean) / var - 1
  alpha <- mean * tmp
  beta  <- (1 - mean) * tmp
  
  if (alpha <= 0 || beta <= 0) {
    stop("Invalid Beta parameters computed (alpha/beta <= 0). Check mean/sd.")
  }
  
  c(alpha = alpha, beta = beta)
}

rbeta_mean_sd <- function(n, mean, sd, eps = 1e-6) {
  pars <- beta_params_from_mean_sd(mean, sd, eps = eps)
  rbeta(n, shape1 = pars["alpha"], shape2 = pars["beta"])
}

gamma_params_from_mean_sd <- function(mean, sd) {
  if (!is.finite(mean) || !is.finite(sd)) stop("Non-finite mean/sd.")
  if (mean <= 0 || sd <= 0) stop("Mean and SD must be > 0 for Gamma.")
  shape <- (mean / sd)^2
  scale <- (sd^2) / mean
  c(shape = shape, scale = scale)
}

rgamma_mean_sd <- function(n, mean, sd) {
  pars <- gamma_params_from_mean_sd(mean, sd)
  rgamma(n, shape = pars["shape"], scale = pars["scale"])
}

lnorm_params_from_mean_sd <- function(mean, sd) {
  if (!is.finite(mean) || !is.finite(sd)) stop("Non-finite mean/sd.")
  if (mean <= 0 || sd <= 0) stop("Mean and SD must be > 0 for lognormal.")
  var <- sd^2
  mu <- log(mean^2 / sqrt(var + mean^2))
  sigma <- sqrt(log(1 + var / mean^2))
  c(meanlog = mu, sdlog = sigma)
}

rlnorm_from_mean_sd <- function(n, mean, sd) {
  pars <- lnorm_params_from_mean_sd(mean, sd)
  rlnorm(n, meanlog = pars["meanlog"], sdlog = pars["sdlog"])
}

# ---- ICER calculation with validation ----

calculate_icer <- function(inc_cost, inc_qaly, tol = 1e-6) {
  if (abs(inc_qaly) < tol) {
    warning("Incremental QALYs near zero (", round(inc_qaly, 6),
            "); ICER may be undefined or extreme.")
  }
  if (inc_qaly < 0 && inc_cost > 0) {
    warning("Intervention is dominated (higher cost, lower QALYs).")
  }
  if (inc_qaly > 0 && inc_cost < 0) {
    message("Intervention is dominant (lower cost, higher QALYs).")
  }

  icer <- inc_cost / inc_qaly

  if (!is.finite(icer)) {
    warning("ICER is not finite: ", icer)
  }

  icer
}
