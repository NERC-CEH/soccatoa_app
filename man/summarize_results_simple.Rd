% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-run_model.R
\name{summarize_results_simple}
\alias{summarize_results_simple}
\title{Summarize results in a very simple way}
\usage{
summarize_results_simple(results, alpha = 0.05)
}
\arguments{
\item{results}{a result object from [run_model_A]}

\item{alpha}{the alpha level used to generate the quantile intervals}
}
\value{
a `data.frame` with columns:
- `rho_c` carbon density
- `lower_rho_c` lower quantile interval of `rho_c`
- `upper_rho_c` upper quantile interval of `rho_c`
- `sd_rho_c` standard deviation of `rho_c`
}
\description{
This just calculates basic sample statistics of our results
[run_model_A] returns a list with 2 elements, one is an array estimates of
the log carbon density over iterations, chains and prediction locations and
the other is the prediction grid `data.frame`
}
