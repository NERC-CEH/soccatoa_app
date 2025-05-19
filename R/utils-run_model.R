#' run_model_A
#' @description function ...
#' @param df_loaded a df of data loaded
#' @return df_results
#' @export
#'
run_model_A <- function(df_loaded){
  #model here
  #example output for now
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
run_model_B <- function(df_loaded, yrstart, yrend){

  model <- function(beta,gamma,dco2,dta){
    dscarb = beta*dco2 + gamma*dta
    return(dscarb)
  }

  chcarb <- function(nsamples,beta_mn,beta_sd,gamma_mn,gamma_sd,dco2,dta){
    betas  <- rnorm(nsamples,mean=beta_mn, sd=beta_sd)
    gammas <- rnorm(nsamples,mean=gamma_mn,sd=gamma_sd)

    df     = data.frame(beta=betas,gamma=gammas,dco2=dco2,dta=dta)
    chcarb = model(beta=df$beta,gamma=df$gamma,dco2=df$dco2,dta=df$dta)
    df     = cbind(df,chcarb)

    return(df)
  }

  calc_betagamma <-function(n){
    cat(n,"times co2","\n")

    n_years <- 140
    co2 <- 285 * (1.01 ^ (0:(n_years - 1)))
    co2ratio <- co2 / 285
    i_index <- which.min(abs(co2ratio - n))
    deltaCO2 <- co2[i_index] - co2[1]

    cmip4 <- c('1pctCO2-bgc','1pctCO2-rad')

    ## Beta calc
    filename <- paste0('data-raw/files/Csdata_', cmip4[1], '_annual_global.csv')
    Cs_data <- read.csv(filename)

    beta <- colMeans(Cs_data[(i_index-4):(i_index), , drop = FALSE], na.rm = TRUE) - colMeans(Cs_data[1:5, , drop = FALSE], na.rm = TRUE)
    beta <- beta / deltaCO2
    mean_beta <- mean(beta)
    std_beta  <- sd(beta)

    ## Gamma calc
    filename <- paste0('data-raw/files/Csdata_', cmip4[2], '_annual_global.csv')
    Cs_data <- read.csv(filename)

    filename <- paste0('data-raw/files/tasdata_', cmip4[2], '_annual_global.csv')
    tas_data <- read.csv(filename)

    gamma    <- colMeans(Cs_data[(i_index-4):(i_index), , drop = FALSE], na.rm = TRUE) - colMeans(Cs_data[1:5, , drop = FALSE], na.rm = TRUE)
    deltatas <- colMeans(tas_data[(i_index-4):(i_index), , drop = FALSE], na.rm = TRUE) - colMeans(tas_data[1:5, , drop = FALSE], na.rm = TRUE)
    gamma <- gamma / deltatas

    mean_gamma <- mean(gamma)
    std_gamma <- sd(gamma)

    return(data.frame(beta_mn=mean_beta,beta_sd=std_beta,gamma_mn=mean_gamma,gamma_sd=std_gamma))
  }

  # yrstart = '2010'
  # yrend   = '2090'

  yrstart <- yrstart
  yrend <- yrend
  rcp     = 'RCP6.0'
  nsamples = 10000

  cat("start year:", yrstart, "end year:", yrend, "\n")
  cat("rcp scenario:", rcp, "\n")

  dtco2    = read.table("data-raw/files/AR5-RCP-CO2.tsv",header=TRUE,sep="\t")
  dttemp   = read.csv("data-raw/files/AR5-RCP-TAS-50.csv",skip=1)

  dco2_s = dtco2[dtco2$Year == yrstart,rcp]
  dco2_e = dtco2[dtco2$Year == yrend  ,rcp]
  rco2 = dco2_e/dco2_s
  dco2 = dco2_e-dco2_s

  dta  <- dttemp[dttemp$Year == yrend,rcp] - dttemp[dttemp$Year == yrstart,rcp]

  param <- calc_betagamma(n = rco2)
  soil = chcarb(nsamples = nsamples,
                beta_mn  = param$beta_mn,   beta_sd = param$beta_sd,
                gamma_mn = param$gamma_mn, gamma_sd = param$gamma_sd,
                dco2 = dco2, dta = dta)

  # for units of g per m2 per yr
  nyrs = as.numeric(yrend)-as.numeric(yrstart)
  # 510000000 globe total sq km
  # 148000000 land sq km
  #1 square kilometer (km²) = 1,000,000 square meters (m²)

  area = 148000000 * 1000000
  divunit = area * nyrs
  mulunit = 1E15

  ch_soilcarb = soil$chcarb * (mulunit/divunit)
  #summary(ch_soilcarb)

  # read in site data
  #model_result <- soccatoa::example_output

  # soil carbon model in stan written by Dave Miller
  # this is a place holder
  soilCarb <- 50*rnorm(nsamples,mean=1.0,sd=0.1)

  soilCarb - ch_soilcarb

  return(soilCarb)
}
