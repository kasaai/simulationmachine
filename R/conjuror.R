#' Create a Simulation Machine Specification
#' 
#' @param num_records Number of expected records to generate.
#' @param lob_distribution A length 4 vector. Distribution of claims among the 
#'   four LOBs.
#' @param inflation A length 4 vector. Grow parameters (per LOB) for the numbers
#'    of claims in the 12 accident years.
#' @param sd_claim Value of the standard deviation used in the log-normal 
#'   distribution of the claim sizes.
#' @param sd_recovery  Value of the standard deviation used in the log-normal 
#'   distribution of the recovery sizes.
#' @return An incantation object.
#' @export
simulation_machine <- function(num_records,
                               lob_distribution,
                               inflation, sd_claim, sd_recovery) {
  conjuror::scribe(
    simulator = "simulation_machine",
    num_records = num_records,
    lob_distribution = lob_distribution,
    inflation = inflation,
    sd_claim = sd_claim,
    sd_recovery = sd_recovery
  )
}

#' Simulate Claim Histories
#' 
#' @importFrom conjuror conjure
#' @param seed Seed for random number generation.
#' @param rows_per_partition (Optional) For parallel processing, number of 
#'   observations that are treated at the same time.
#' @export
conjure.simulation_machine <- function(incantation, seed = NULL, 
                                       rows_per_partition = NULL, ...) {
  features <- Feature.Generation(
    V = incantation[["num_records"]],
    LoB.dist = incantation[["lob_distribution"]],
    inflation = incantation[["inflation"]],
    seed = seed
  )
  
  if (is.null(rows_per_partition)) {
    rows_per_partition <- nrow(features)
  }
  
  records <- Simulation.Machine(
    features,
    npb = rows_per_partition,
    seed1 = seed,
    std1 = incantation[["sd_claim"]],
    std2 = incantation[["sd_recovery"]]
  )
  
  records
}

#' @export
print.simulation_machine <- function(x, ...) {
  num_records <- incantation[["num_records"]]
  lob_distribution <- incantation[["lob_distribution"]]
  inflation <- incantation[["inflation"]]
  sd_claim <- incantation[["sd_claim"]]
  sd_recovery <- incantation[["sd_recovery"]]
  
  print(glue::glue("
A simulation incantation for `simulation_machine`

Specs:
 - Expected number of claims: {format(num_records, big.mark = ',')}
 - LOB distribution: {paste0(lob_distribution, collapse = ', ')}
 - Inflation: {paste0(inflation, collapse = ', ')}
 - SD of claim sizes: {sd_claim},
 - SD of recovery sizes: {sd_recovery}
"))
}
