% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conjuror.R
\name{conjure.simulation_machine}
\alias{conjure.simulation_machine}
\title{Simulate Claim Histories}
\usage{
\method{conjure}{simulation_machine}(charm, seed = NULL,
  seed_features = seed, rows_per_partition = NULL, ...)
}
\arguments{
\item{charm}{An \code{charm} object returned by \code{simulation_machine()}.}

\item{seed}{Seed for random number generation.}

\item{seed_features}{(Optional) For backwards compatibility; see Details.}

\item{rows_per_partition}{(Optional, currently unused) For parallel processing,
number of observations that are treated at the same time.}

\item{...}{Optional additional arguments, currently unused.}
}
\description{
Simulate Claim Histories
}
\details{
In the original Simulation Machine paper, a second "seed" parameter
is available to be set that controls the randomness of the feature generation
stage of the simulation. The \code{seed_features} parameter is included for
users who wish to reproduce the datasets generated by the original accompanying
code. However, for most usage this parameter can be ignored, and is set
equal to the \code{seed} argument by default.
}
\examples{
charm <- simulation_machine(
  num_claims = 5000, 
  lob_distribution = c(0.25, 0.25, 0.30, 0.20), 
  inflation = c(0.01, 0.01, 0.01, 0.01), 
  sd_claim = 0.85, 
  sd_recovery = 0.85
)

conjure(charm, seed = 100)

}
