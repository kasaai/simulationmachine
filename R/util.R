#' @importFrom magrittr %>%
magrittr::`%>%`

maybe_set_seed <- function(seed) {
  if (length(seed))
    set.seed(seed)
  else
    set.seed(NULL)
}

tidy_records <- function(records) {
  records %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.factor, as.character) %>% 
    dplyr::rename(
      claim_id = .data$ClNr,
      lob = .data$LoB,
      cc = .data$cc,
      accident_year = .data$AY,
      accident_quarter = .data$AQ,
      age = .data$age,
      injured_part = .data$inj_part,
      report_delay = .data$RepDel
    ) %>% 
    tidyr::gather("key", "value", Pay00:Open11) %>%
    dplyr::arrange(.data$claim_id) %>% 
    tidyr::separate("key", into = c("variable", "development_year"), sep = "(?<=[a-z])(?=[0-9])") %>% 
    tidyr::spread("variable", "value") %>% 
    dplyr::rename(claim_status_open = .data$Open,
                  paid_loss = .data$Pay) %>% 
    dplyr::mutate(claim_id = as.character(.data$claim_id),
                  development_year = as.integer(.data$development_year),
                  claim_status_open = as.integer(.data$claim_status_open),
                  accident_year = as.integer(.data$accident_year),
                  report_delay = as.integer(.data$report_delay),
                  age = as.integer(.data$age)) %>% 
    dplyr::select(.data$claim_id, .data$accident_year, .data$development_year, 
                  .data$accident_quarter, .data$report_delay,
                  .data$lob, .data$cc, .data$age, .data$injured_part, .data$paid_loss, 
                  .data$claim_status_open)
}