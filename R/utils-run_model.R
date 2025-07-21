#' Dummy model fitted based on a standard GAM
#'
#' Nothing fancy here! Just a simple GAM, followed by some uncertainty
#' estimation using posterior sampling. More information at
#' https://github.com/NERC-CEH/soccatoa_app/issues/4
#'
#' @param n_post_samples how many posterior samples to use
#' @param n_chains number of chains (just a multiplier on the number of samples
#' here but when we do Real MCMC we will need use this)
#' @param df a `data.frame` of data
#' @param df_grid prediction grid to use
#' @return df_results
#' @importFrom stats coef vcov predict
#' @importFrom mgcv gam rmvn predict.gam
#' @importFrom abind abind
#' @importFrom posterior as_draws_array
#' @export
dummy_model <- function(df, df_grid, n_post_samples, n_chains) {
  # check
  v_year <- unique(df$fyear)
  if (length(v_year) < 2) {
    stop("You need at least 2 time points to estimate change")
  }
  # a very very simple GAM :)
  jg <- mgcv::gam(
    log_rho_c ~
      fyear +
        z,
    data = df
  )

  # since we're not doing multi-chain MCMC here just generate
  # samples x chains independent samples
  samps <- rmvn(n_post_samples * n_chains, coef(jg), vcov(jg))

  lp <- predict(jg, newdata = df_grid, type = "lpmatrix")
  lpp <- lp %*% t(samps)

  # put into array format
  llpp <- list()
  st <- 0
  for (i in 1:n_chains) {
    llpp[[i]] <- lpp[, (st + 1):(n_post_samples * i)]
    st <- n_post_samples * i
  }
  arr <- do.call(abind, list(llpp, along = 3))
  # get the dimensions in the right order
  arr <- aperm(arr, c(2, 3, 1))
  # give the array some useful names
  dimnames(arr)[[3]] <- paste0("pred[", dimnames(arr)[[3]], "]")
  # return as posterior object
  return(posterior::as_draws_array(arr))
}

#' Make a prediction grid
#'
#' Generate a grid to make predictions over
#'
#' @param df the data.frame of the original data to use to get limits etc
#' @return a `data.frame` with locations in covariate space to predict at
make_prediction_grid <- function(df) {
  n_east <- n_north <- 20
  df_grid <- expand.grid(
    easting = seq(
      min(df$easting),
      max(df$easting),
      length.out = n_east
    ),
    northing = seq(
      min(df$northing),
      max(df$northing),
      length.out = n_north
    ),
    # since we assume log_e rho_c is linear in depth
    # we only need 2 points
    z = c(0.25, 0.55),
    # may want to specify this based on the time
    # period given by the user?
    fyear = factor(range(df$year), levels = levels(df$fyear))
  )

  # these need to be revisited depending on how we change the grid
  df_grid$d <- 0.3
  df_grid$area <- ((max(df$easting) - min(df$easting)) / n_east) *
    (max(df$northing) - min(df$northing) / n_north)
  # need to think about whether this is fixed or if
  # marginalize?
  # S_fez=mean(df_fix$S_fez))
  return(df_grid)
}

#' Spatio-temporal model of soil carbon
#'
#' This fits our model of soil carbon. At the moment this is just a dummy
#' function and will eventually call-out to other stuff.
#' @param df a `data.frame` of data loaded
#' @return df_results
#' @export
run_model_A <- function(df) {
  # generate prediction grid
  df_grid <- make_prediction_grid(df)

  ## probably want to define this in the UI later (as an advanced option?)
  # number of posterior samples to generate
  n_post_samples <- 1000
  # number of chains to run
  n_chains <- 4
  # for the dummy model we just generate n_post_samples*n_chains samples

  # this returns 3D array of (iteration) x (chain) x (variable)
  model_result <- dummy_model(df, df_grid, n_post_samples, n_chains)

  # return both bits
  list(df_grid = df_grid, results = model_result)
}

#' Summarize results in a very simple way
#'
#' This just calculates basic sample statistics of our results
#' [run_model_A] returns a list with 2 elements, one is an array estimates of
#' the log carbon density over iterations, chains and prediction locations and
#' the other is the prediction grid `data.frame`
#'
#' @param results a result object from [run_model_A]
#' @param alpha the alpha level used to generate the quantile intervals
#' @return a `data.frame` with columns:
#' - `rho_c` carbon density
#' - `lower_rho_c` lower quantile interval of `rho_c`
#' - `upper_rho_c` upper quantile interval of `rho_c`
#' - `sd_rho_c` standard deviation of `rho_c`
#' @importFrom stats sd quantile
#' @export
summarize_results_simple <- function(results, alpha = 0.05) {
  df_grid <- results$df_grid
  results <- results$results

  results <- as_draws_df(results)
  results <- results[, -((ncol(results) - 2):ncol(results))]

  results <- apply(results, 2, function(x) {
    x <- exp(x)
    data.frame(
      rho_c = mean(x),
      lower_rho_c = quantile(x, probs = c(alpha / 2)),
      upper_rho_c = quantile(x, probs = c(1 - alpha / 2)),
      sd_rho_c = sd(x)
    )
  })

  results <- do.call(rbind, results)

  # bind to prediction data so we can reference to time/space for plots later
  cbind(df_grid, results)
}

#' Summarize results for graph of changes
#'
#' Calculates statistics needed for the changes plot
#'
#' @param results a result object from [run_model_A]
#' @param a `data.frame` with columns:
#' - `time` two time periods to be used
#' - `total` soil carbon
#' - `total_error` uncertainty in soil carbon
#' @export
#' @importFrom posterior as_draws_df
#' @importFrom stats median aggregate
summarize_results_change <- function(results) {
  df_grid <- results$df_grid
  md <- exp(results$results)

  dd <- as_draws_df(md)

  # calculate S_cz for the two time periods
  # apply preserves the iterations
  dd <- apply(dd[, -((ncol(dd) - 2):ncol(dd))], 1, function(x) {
    # get s_ci
    x <- x * df_grid$d / df_grid$area
    # sum over space per year
    x <- aggregate(x, list(year = df_grid$fyear), sum)
  })

  # one row per iteration/year
  dd <- do.call(rbind, dd)

  # calculate summary statistics per year
  dm <- aggregate(dd$x, list(year = dd$year), median)
  derr <- aggregate(dd$x, list(year = dd$year), sd)

  # return object for plotting
  data.frame(
    time = dm$year,
    total = dm$x, # orange line
    total_error = derr$x
  )
}

#' Summarize results for uncertainty plot
#'
#' Calculates the posterior of the change in carbon
#'
#' @param results a result object from [run_model_A]
#' @param a `data.frame` with columns:
#' @export
summarize_results_dist <- function(results) {
  df_grid <- results$df_grid
  md <- exp(results$results)

  dd <- as_draws_df(md)

  # calculate S_cz for the two time periods
  # apply preserves the iterations
  dd <- apply(dd[, -((ncol(dd) - 2):ncol(dd))], 1, function(x) {
    # get s_ci
    x <- x * df_grid$d / df_grid$area
    # sum over space per year
    x <- aggregate(x, list(year = df_grid$fyear), sum)
  })

  # one row per iteration/year
  dd <- do.call(rbind, dd)

  # calculate distribution
  diff_dist <- dd$x[dd$year == unique(dd$year)[1]] -
    dd$x[dd$year == unique(dd$year)[2]]

  # return object for plotting
  data.frame(
    iter = 1:length(diff_dist),
    value = diff_dist
  )
}

#' Random Gamma Distribution within a specified range
#'
#' Generates random numbers from a Gamma distribution within a specified range.
#'
#' @param n Number of observations to generate.
#' @param range A numeric vector of length 2 specifying the minimum and maximum values of the range.
#' @param shape Shape parameter of the Gamma distribution.
#' @param rate Rate parameter of the Gamma distribution. Defaults to 1.
#'
#' @return A numeric vector of length `n` containing random numbers from a Gamma distribution within the specified range.
#'
#' @importFrom stats pgamma runif qgamma
#' @examples
#' set.seed(123)
#' rgammat(10, range = c(0, 10), shape = 2, rate = 1)
rgammat <- function(n, range, shape, rate = 1) {
  F.a <- pgamma(min(range), shape = shape, rate = rate)
  F.b <- pgamma(max(range), shape = shape, rate = rate)

  u <- runif(n, min = F.a, max = F.b)
  qgamma(u, shape = shape, rate = rate)
}

#' Calculate the change in soil carbon and its uncertainty arising from change
#' in CO2 and temperature.
#'
#' This function calculates the change in soil carbon stock from the start year
#' to the end year, and its standard deviation, given the parameters
#' describing the sensitivity of SOC to change in CO2 and temperature.
#'
#' @param n_sim The number of simulations to run (default: 10)
#' @param S_c_start The initial soil carbon stock (default: 10 kg C m^-2)
#' @param start_year The starting year for the calculation (default: 2025)
#' @param end_year The ending year for the calculation (default: 2050)
#' @param this_scenario The climate scenario to use (default: "RCP2.6")
#' @param shape The shape parameter for the gamma distribution (default: 3.920758)
#' @param rate The rate parameter for the gamma distribution (default: 1371.239)
#' @param intercept The intercept for the linear relationship between beta and gamma (default: 0.004518087)
#' @param slope The slope for the linear relationship between beta and gamma (default: -3.611591834)
#' @param sigma The standard deviation for the normal distribution of residuals (default: 0.007210343)
#'
#' @return A list with two elements:
#'   \item{sigma}{The standard deviation of the estimated soil carbon stock in the end year}
#'   \item{p}{A ggplot object showing the distribution of predicted soil carbon stocks}
#'
#' @examples
#' \dontrun{
#' x <- get_co2climate_effect(
#'   n_sim = 10000,
#'   start_year = 2025,
#'   end_year = 2050,
#'   this_scenario = "RCP8.5"
#' )
#' }
get_co2climate_effect <- function(
  n_sim = 10,
  S_c_start = 10,
  start_year = 2025,
  end_year = 2050,
  this_scenario = c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5"),
  shape = 3.920758,
  rate = 1371.239,
  intercept = 0.004518087,
  slope = -3.611591834,
  sigma = 0.007210343,
  maxbeta
) {
  this_scenario <- match.arg(this_scenario)
  co2_start <- df_scenario$co2[
    df_scenario$year == start_year & df_scenario$scenario == this_scenario
  ]
  co2_end <- df_scenario$co2[
    df_scenario$year == end_year & df_scenario$scenario == this_scenario
  ]
  dco2 <- co2_end - co2_start
  ta_start <- df_scenario$ta[
    df_scenario$year == start_year & df_scenario$scenario == this_scenario
  ]
  ta_end <- df_scenario$ta[
    df_scenario$year == end_year & df_scenario$scenario == this_scenario
  ]
  dta <- ta_end - ta_start

  v_beta <- rgammat(
    n = n_sim,
    range = c(0, maxbeta),
    shape = shape,
    rate = rate
  )

  # force to be always negative
  v_gamma <- -1 * abs(intercept + slope * v_beta + rnorm(n_sim, 0, sigma))
  v_dc <- v_beta * dco2 + v_gamma * dta
  v_S_c_end <- S_c_start + v_dc

  return(v_S_c_end)
}

#' run_model_B
#' @description function ...
#' @param yrstart start year
#' @param yrend end year
#' @return df_results
#' @importFrom utils read.csv
#' @export
#'
run_model_B <- function(yrstart, yrend) {
  df_gamma <- read.csv("data-raw/files/df_gamma.csv")

  l_sigma <- get_co2climate_effect(
    n_sim = 10000,
    start_year = 2025,
    end_year = 2050,
    this_scenario = "RCP8.5",
    shape = df_gamma$shape,
    rate = df_gamma$rate,
    intercept = df_gamma$intercept,
    slope = df_gamma$slope,
    sigma = df_gamma$sigma,
    maxbeta = df_gamma$maxbeta
  )
  return(l_sigma)
}
