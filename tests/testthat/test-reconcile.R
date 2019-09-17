test_that("Generated claim counts agrees with original version", {

  library(dplyr)
  spec1 <- simulation_machine(
    num_claims = 100000,
    lob_distribution = c(0.25, 0.25, 0.25, 0.25),
    inflation = c(0.01, 0.01, 0.01, 0.01),
    sd_claim = 0.85,
    sd_recovery = 0.85
  )
  
  data1 <- conjure(spec1, seed = 75)
  
  claim_counts_by_ay <- data1 %>% 
    distinct(claim_id, accident_year) %>% 
    group_by(accident_year) %>% 
    count() %>% 
    pull(n)
  
  expect_identical(
    claim_counts_by_ay,
    c(7790L, 
      8120L,
      8104L,
      8180L,
      8248L,
      8199L,
      8312L,
      8545L,
      8505L,
      8641L,
      8516L,
      8775L)
  )
  
  claim_counts_by_lob <- data1 %>% 
    distinct(claim_id, lob) %>% 
    group_by(lob) %>% 
    count() %>% 
    pull(n)
  
  expect_identical(
    claim_counts_by_lob,
    c(25056L,
      25195L,
      24678L,
      25006L)
  )
  
  expect_identical(
    sum(data1$paid_loss),
    194791278
  )
})
