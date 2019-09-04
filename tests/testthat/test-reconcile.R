test_that("Generated claim counts agrees with original version", {
  
  # Skip on travis/CRAN since the dataset exceeds RAM limits
  skip_on_travis()
  skip_on_cran()
  
  library(dplyr)
  spec1 <- simulation_machine(
    num_claims = 1000000,
    lob_distribution = c(0.25, 0.25, 0.25, 0.25),
    inflation = c(0.01, 0.01, 0.01, 0.01),
    sd_claim = 0.85,
    sd_recovery = 0.85
  )
  
  data1 <- conjure(spec1, seed = 75)
  
  spec2 <- simulation_machine(
    num_claims = 200000,
    lob_distribution = c(0.50,0,0,0.50),
    inflation = c(0.05,0,0,0.05),
    sd_claim = 0.85,
    sd_recovery = 0.85
  )
  
  data2 <- conjure(spec2, seed = 75)
  
  combined_data <- bind_rows(
    data1 %>% 
      mutate(lob = ifelse(lob == "3", "5", lob)),
    data2 %>% 
      mutate(
        lob = case_when(
          lob == "1" ~ "3",
          lob == "4" ~ "6"
        ),
        claim_id = as.character(
          as.numeric(claim_id) + nrow(distinct(data1, claim_id))
        )
      )
  )
  
  claim_counts_by_ay <- combined_data %>% 
    distinct(claim_id, accident_year) %>% 
    group_by(accident_year) %>% 
    count() %>% 
    pull(n)
  
  expect_identical(
    claim_counts_by_ay,
    c(92620L, 
      94910L,
      95484L,
      95916L,
      97331L,
      96928L,
      99134L,
      102837L,
      103698L,
      105712L,
      106748L,
      107570L)
  )
  
  claim_counts_by_lob <- combined_data %>% 
    distinct(claim_id, lob) %>% 
    group_by(lob) %>% 
    count() %>% 
    pull(n)
  
  expect_identical(
    claim_counts_by_lob,
    c(250040L,
      250197L,
      99969L,
      249683L,
      249298L,
      99701L)
  )
})
