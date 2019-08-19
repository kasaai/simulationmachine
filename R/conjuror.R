#' Create a Simulation Machine Specification
#' 
#' @param num_records Number of expected records to generate.
#' @param lob_distribution A length 4 vector. Distribution of claims among the 
#'   four LOBs.
#' @param inflation A length 4 vector. Grow parameters (per LOB) for the numbers
#'    of claims in the 12 accident years.
#' @return An incantation object.
#' @export
simulation_machine <- function(num_records,
                               lob_distribution,
                               inflation) {
  conjuror::scribe(
    simulator = "simulation_machine",
    num_records = num_records,
    lob_distribution = lob_distribution,
    inflation = inflation
  )
}

#' Simulate Claim Histories
#' 
#' @importFrom conjuror conjure
#' @param sd_claim Value of the standard deviation used in the log-normal 
#'   distribution of the claim sizes.
#' @param sd_recovery  Value of the standard deviation used in the log-normal 
#'   distribution of the recovery sizes.
#' @param seed Seed for random number generation.
#' @param rows_per_partition (Optional) For parallel processing, number of 
#'   observations that are treated at the same time.
#' @export
conjure.simulation_machine <- function(incantation, sd_claim, sd_recovery,
                                       seed = NULL, rows_per_partition = NULL, ...) {
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
    std1 = sd_claim,
    std2 = sd_recovery
  )
  
  records
}