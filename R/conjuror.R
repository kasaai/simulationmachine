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
