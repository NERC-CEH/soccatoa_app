% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-run_model.R
\name{summarize_results_change}
\alias{summarize_results_change}
\title{Summarize results for graph of changes}
\usage{
summarize_results_change(results)
}
\arguments{
\item{results}{a result object from [run_model_A]}

\item{a}{`data.frame` with columns:
- `time` two time periods to be used
- `total` soil carbon
- `total_error` uncertainty in soil carbon}
}
\description{
Calculates statistics needed for the changes plot
}
