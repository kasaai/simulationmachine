#' Create a Simulation Machine Specification
#' 
#' @param num_claims Number of expected records to generate.
#' @param lob_distribution A length 4 vector. Distribution of claims among the 
#'   four LOBs.
#' @param inflation A length 4 vector. Grow parameters (per LOB) for the numbers
#'    of claims in the 12 accident years.
#' @param sd_claim Value of the standard deviation used in the log-normal 
#'   distribution of the claim sizes.
#' @param sd_recovery  Value of the standard deviation used in the log-normal 
#'   distribution of the recovery sizes.
#' @return A `charm` object.
#' 
#' @examples
#' charm <- simulation_machine(
#'   num_claims = 50000, 
#'   lob_distribution = c(0.25, 0.25, 0.30, 0.20), 
#'   inflation = c(0.01, 0.01, 0.01, 0.01), 
#'   sd_claim = 0.85, 
#'   sd_recovery = 0.85
#' )
#' charm
#' 
#' @export
simulation_machine <- function(num_claims,
                               lob_distribution,
                               inflation, sd_claim, sd_recovery) {
  num_claims <- forge::cast_scalar_double(num_claims)
  lob_distribution <- forge::cast_double(lob_distribution, n = 4)
  if (sum(lob_distribution) != 1) stop(
    "The elements of `lob_distribution` must sum to 1.",
    call. = FALSE
  )
  inflation <- forge::cast_double(inflation, n = 4)
  sd_claim <- forge::cast_scalar_double(sd_claim)
  sd_recovery <- forge::cast_scalar_double(sd_recovery)
  
  conjuror::scribe(
    simulator = "simulation_machine",
    num_claims = num_claims,
    lob_distribution = lob_distribution,
    inflation = inflation,
    sd_claim = sd_claim,
    sd_recovery = sd_recovery
  )
}

#' @importFrom conjuror conjure
#' @export
conjuror::conjure

#' Simulate Claim Histories
#' 
#' @param charm An `charm` object returned by `simulation_machine()`.
#' @param ... Optional additional arguments, currently unused.
#' @param seed Seed for random number generation.
#' @param seed_features (Optional) For backwards compatibility; see Details.
#' @param rows_per_partition (Optional, currently unused) For parallel processing, 
#' number of observations that are treated at the same time.
#'   
#' @details In the original Simulation Machine paper, a second "seed" parameter
#'   is available to be set that controls the randomness of the feature generation
#'   stage of the simulation. The `seed_features` parameter is included for
#'   users who wish to reproduce the datasets generated by the original accompanying
#'   code. However, for most usage this parameter can be ignored, and is set
#'   equal to the `seed` argument by default.
#'   
#' @examples
#' charm <- simulation_machine(
#'   num_claims = 5000, 
#'   lob_distribution = c(0.25, 0.25, 0.30, 0.20), 
#'   inflation = c(0.01, 0.01, 0.01, 0.01), 
#'   sd_claim = 0.85, 
#'   sd_recovery = 0.85
#' )
#'
#' conjure(charm, seed = 100)
#' 
#' @export
conjure.simulation_machine <- function(charm, seed = NULL, 
                                       seed_features = seed,
                                       rows_per_partition = NULL, ...) {
  features <- Feature.Generation(
    V = charm[["num_claims"]],
    LoB.dist = charm[["lob_distribution"]],
    inflation = charm[["inflation"]],
    seed = seed_features
  )
  
  if (is.null(rows_per_partition)) {
    rows_per_partition <- nrow(features)
  }
  
  records <- Simulation.Machine(
    features,
    npb = rows_per_partition,
    seed1 = seed,
    std1 = charm[["sd_claim"]],
    std2 = charm[["sd_recovery"]]
  )
  
  tidy_records(records)
}

#' @export
print.simulation_machine <- function(x, ...) {
  num_claims <- x[["num_claims"]]
  lob_distribution <- x[["lob_distribution"]]
  inflation <- x[["inflation"]]
  sd_claim <- x[["sd_claim"]]
  sd_recovery <- x[["sd_recovery"]]
  
  print(glue::glue("
A simulation charm for `simulation_machine`

Each record is:
 - A snapshot of a claim's incremental paid loss and claim status
   at a development year.

Specs:
 - Expected number of claims: {format(num_claims, big.mark = ',', scientific = FALSE)}
 - LOB distribution: {paste0(lob_distribution, collapse = ', ')}
 - Inflation: {paste0(inflation, collapse = ', ')}
 - SD of claim sizes: {sd_claim},
 - SD of recovery sizes: {sd_recovery}
"))
}
