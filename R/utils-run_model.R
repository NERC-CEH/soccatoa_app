#' run_model_A
#' @description function ...
#' @param df_loaded a df of data loaded
#' @return df_results
#' @export
#'
run_model_A <- function(df_loaded) {
  # model here
  # example output for now
  model_result <- soccatoa::example_output
  return(model_result)
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
    co2_start <- df_scenario$co2[df_scenario$year == start_year & df_scenario$scenario == this_scenario]
    co2_end <- df_scenario$co2[df_scenario$year == end_year & df_scenario$scenario == this_scenario]
    dco2 <- co2_end - co2_start
    ta_start <- df_scenario$ta[df_scenario$year == start_year & df_scenario$scenario == this_scenario]
    ta_end <- df_scenario$ta[df_scenario$year == end_year & df_scenario$scenario == this_scenario]
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
