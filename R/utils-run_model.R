#' run_model
#' @description function ...
#' @param df_loaded a df of data loaded
#' @return df_results
#' @export
#'

run_model <- function(df_loaded){

  # model here

  #example output for now
  model_result <- soccatoa::example_output
  return(model_result)
}

run_model_new <- function(df_loaded){

  model <- function(beta,gamma,dco2,dta){
    dscarb = beta*dco2 + gamma*dta
    return(dscarb)
  }

  chcarb <- function(nsamples,beta_mn,beta_sd,gamma_mn,gamma_sd,dco2,dta){
    betas  <- rnorm(nsamples,mean=beta_mn, sd=beta_sd)
    gammas <- rnorm(nsamples,mean=gamma_mn,sd=gamma_sd)

    #dt <- data.table::data.table(beta=betas,gamma=gammas,dco2=dco2,dta=dta)
    #dt[, chcarb := model(beta,gamma,dco2,dta)]
    df=data.frame(beta=betas,gamma=gammas,dco2=dco2,dta=dta)
    chcarb=model(beta=df$beta,gamma=df$gamma,dco2=df$dco2,dta=df$dta)
    df=cbind(df,chcarb)

    #return(as.data.frame(dt))
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
  library(data.table)
  yrstart = '2010'
  yrend   = '2090'
  rcp     = 'RCP6.0'

  cat("start year:", yrstart, "end year:", yrend, "\n")
  cat("rcp scenario:", rcp, "\n")

  #dtco2    = data.table::fread("data-raw/files/AR5-RCP-CO2.tsv")
  #dttemp   = data.table::fread("data-raw/files/AR5-RCP-TAS-50.csv",skip=1)
  #dco2_s = as.numeric(dtco2[Year == yrstart, ..rcp])
  #dco2_e = as.numeric(dtco2[Year == yrend, ..rcp])
  dco2_s = 389.1
  dco2_e = 635.6

  rco2 = dco2_e/dco2_s
  dco2 = dco2_e-dco2_s

  #dta  <- as.numeric(dttemp[Year == yrend,..rcp] - dttemp[Year == yrstart,..rcp])
  dta  <- 2.0314 - 0.3591

  param <- calc_betagamma(n = rco2)
  soil = chcarb(nsamples = 10000,
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

  return(ch_soilcarb)
}
