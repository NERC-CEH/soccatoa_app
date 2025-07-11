#' Dummy model fitted based on a standard GAM
#'
#' Nothing fancy here! Just a simple GAM, followed by some uncertainty
#' estimation using posterior sampling. More information at
#' https://github.com/NERC-CEH/soccatoa_app/issues/4
#'
#' @param n_post_samples how many posterior samples to use
#' @param n_chains number of chains (just a multiplier on the number of samples
#' here but when we do Real MCMC we will need use this)
#' @param df_loaded a `data.frame` of data
#' @return df_results
#' @importFrom mgcv gam rmvn predict.gam
#' @importFrom abind abind
#' @export
dummy_model <- function(df_loaded, predgrid, n_post_samples, n_chains) {
  # a very very simple GAM :)
  jg <- mgcv::gam(
    log_rho_c ~
      fyear +
        z,
    data = df_loaded
  )

  # since we're not doing multi-chain MCMC here just generate
  # samples x chains independent samples
  samps <- rmvn(n_post_samples * n_chains, coef(jg), vcov(jg))

  lp <- predict(jg, newdata = predgrid, type = "lpmatrix")

  # for now ignore the scale parameter stuff, drop last col
  lpp <- lp[, -ncol(lp)] %*% t(samps[, -ncol(samps)])

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
  # store the temporal information in the right dimension
  attr(arr, "dimnames")[[3]] <- as.character(predgrid$fyear)
  arr
}

#' Make a prediction grid
#'
#' Generate a grid to make predictions over
#'
#' @param ... nothing for now but we might want options in the future
#' @return a `data.frame` with locations in covariate space to predict at
make_prediction_grid <- function(df_loaded) {
  expand.grid(
    easting = seq(
      min(df_loaded$easting),
      max(df_loaded$easting),
      length.out = 20
    ),
    northing = seq(
      min(df_loaded$northing),
      max(df_loaded$northing),
      length.out = 20
    ),
    # since we assume log_e rho_c is linear in depth
    # we only need 2 points
    z = c(0.25, 0.55),
    d = c(0.3, 0.3),
    # may want to specify this based on the time
    # period given by the user?
    fyear = unique(df_loaded$fyear)
  ) #,
  # need to think about whether this is fixed or if
  # marginalize?
  #S_fez=mean(df_fix$S_fez))
}

#' Spatio-temporal model of soil carbon
#'
#' This fits our model of soil carbon. At the moment this is just a dummy
#' function and will eventually call-out to other stuff.
#' @param df_loaded a `data.frame` of data loaded
#' @return df_results
#' @export
#'
run_model_A <- function(df_loaded) {
  # generate prediction grid
  predgrid <- make_prediction_grid(df_loaded)

  ## probably want to define this in the UI later (as an advanced option?)
  # number of posterior samples to generate
  n_post_samples <- 1000
  # number of chains to run
  n_chains <- 4
  # for the dummy model we just generate n_post_samples*n_chains samples

  # this returns 3D array of (iteration) x (chain) x (variable)
  model_result <- dummy_model(df_loaded, predgrid, n_post_samples, n_chains)

  # now generate some summary statistics
  # can fiddle with alpha or allow user specification
  model_result <- apply(model_result, 3, function(x, alpha = 0.05) {
    # rho_c scale from log(rho_c)
    x <- exp(x)
    xx <- matrix(
      c(
        mean(x),
        quantile(x, probs = c(alpha / 2)),
        quantile(x, probs = c(1 - alpha / 2)),
        sd(x)
      ),
      nrow = 1
    )
    xx
  })

  model_result <- as.data.frame(t(model_result), make.names = NA)
  colnames(model_result) <- c("rho_c", "lower_rho_c", "upper_rho_c", "sd_rho_c")
  # bind to prediction data so we can reference to time/space for plots later
  model_result <- cbind(predgrid, model_result)

  return(model_result)
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
#' @param df_loaded a df of data loaded
#' @param yrstart start year
#' @param yrend end year
#' @return df_results
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
