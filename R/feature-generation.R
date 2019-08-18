#' Feature Generation
#' 
#' @param V Total expected number of claims.
#' @param LoB.dist Categorical distribution for the allocation of the claims to the four lines of business.
#' @param inflation Grow parameters (per LOB) for the numbers of claims in the 12 accident years.
#' @param seed Random seed.
Feature.Generation <- function(V = 1000000, LoB.dist = c(0.25, 0.30, 0.20, 0.25),
                               inflation = c(0.01, 0.01, 0.01, 0.01), seed = 100) {
  
  ### Weights in LoB.dist have to be nonnegative
  if (any(LoB.dist) < 0)
    stop("The weights determining the distribution amongst the lines of business cannot be negative.", 
         call. = FALSE)
  
  # We will store the number of claims for each combination of accident year and line of business in V.dist
  V.dist <- array(NA, c(13, 4))
  
  # Determine the number of claims per line of business
  set.seed(seed)
  V.dist[1, ] <- rmultinom(n = 1, size = V, prob = LoB.dist)
  
  # Determine the number of claims per accident year (for all lines of business)
  V.dist[2, ] <- 0
  set.seed(seed + 1)
  V.dist[-c(1, 2), ] <- rnorm(44, mean = rep(inflation, each = 11), sd = rep(abs(inflation), each = 11))
  V.dist[-c(1, 2), ] <- apply(V.dist[-c(1, 2), ], 2, cumsum)
  V.dist[-1, ] <- exp(V.dist[-1, ])
  V.dist[-1, ] <- t(t(V.dist[-1, ]) / colSums(V.dist[-1, ]))
  V.dist[-1, ] <- t(t(V.dist[-1, ]) * V.dist[1, ])
  set.seed(seed + 2)
  V.dist[-1, ] <- rpois(n = 48, lambda = V.dist[-1, ])
  V.dist[1, ] <- colSums(V.dist[-1, ])
  
  # Create the array where we will store the observations
  features <- as.data.frame(array(NA, c(sum(V.dist[1, ]), 7)))
  colnames(features) <- c("ClNr", "LoB", "cc", "AY", "AQ", "age", "inj_part")
  
  # Store line of business
  features[, 2] <- rep(1:4, times = V.dist[1, ])
  
  # Store accident year
  features[, 4] <- rep(which(V.dist[-1, ] > 0) %% 12, times = V.dist[-1, ][which(V.dist[-1, ] > 0)])
  features[, 4][which(features[, 4] == 0)] <- 12
  features[, 4] <- 1993 + features[, 4]
  
  # Add artificial observations that prevent data sets below from being empty
  features[nrow(features) + 1:2, ] <- c(-1, -1, 1, 1, NA, NA, 1994, 1994, NA, NA, NA, NA, NA, NA)
  features[nrow(features) + 1:2, ] <- c(-1, -1, 3, 3, NA, NA, 1994, 1994, NA, NA, NA, NA, NA, NA)
  
  # Feature generation for LOB 1 and 2 
  features_12 <- generate_features_lob(
    features = features,
    Sigma = .lob_12$Covariances$Covariance,
    param_cc = .lob_12$Parameters$cc,
    translator_cc = .lob_12$Translators$cc,
    param_age = .lob_12$Parameters$age,
    param_inj_part = .lob_12$Parameters$inj_part,
    translator_inj_part = .lob_12$Translators$inj_part,
    f.cc = f.cc_12,
    f.age1 = f.age1_12,
    f.age2 = f.age2_12,
    f.inj_part = f.inj_part_12,
    seed = seed + 3
  )
  
  features[which(features$LoB <= 2), c(3, 5, 6, 7)] <- features_12
  
  # Feature generation for LOB 3 and 4
  features_34 <- generate_features_lob(
    features = features,
    Sigma = .lob_34$Covariances$Covariance,
    param_cc = .lob_34$Parameters$cc,
    translator_cc = .lob_34$Translators$cc,
    param_age = .lob_34$Parameters$age,
    param_inj_part = .lob_34$Parameters$inj_part,
    translator_inj_part = .lob_34$Translators$inj_part,
    f.cc = f.cc_34,
    f.age1 = f.age1_34,
    f.age2 = f.age2_34,
    f.inj_part = f.inj_part_34,
    seed = seed + 3
  )
  
  features[which(features$LoB <= 2), c(3, 5, 6, 7)] <- features_34

  # Order the data: first random order then order according to the accident year AY
  set.seed(seed + 5)
  order1 <- sample(1:nrow(features), nrow(features))
  features <- features[order1, ]
  order2 <- order(features$AY)
  features <- features[order2, ]
  
  # Remove the artificial observations
  features <- features[-which(features$ClNr == -1), ]
  
  # Convert LoB, cc and inj_part to factors
  features[nrow(features) + 1, ] <- c(NA, 1, 15, 1994, 4, 37, 30) ### add an additional observation for the case that features is empty
  features$LoB <- factor(features$LoB)
  features$cc <- factor(features$cc)
  features$inj_part <- factor(features$inj_part)
  
  # Number the claims from 1 to nrow(features)
  features[, 1] <- 1:nrow(features)

  # Adjust the rownames
  rownames(features) <- paste("", 1:nrow(features), sep = "")
  features <- features[-nrow(features), ] ### delete the additional observation

  features
}

generate_features_lob <- function(features, 
                              # parameters
                              Sigma, param_cc, translator_cc, param_age, param_inj_part,
                              translator_inj_part, 
                              # functions
                              f.cc, f.age1, f.age2, f.inj_part,
                              seed) {
  
  # Generate observations from a multivariate normal distribution
  # set.seed(seed + 3)
  features_new <- mvrnorm(n = nrow(features[which(features$LoB <= 2), ]), mu = rep(0, 4), Sigma = Sigma)
  
  # Transform marginals such that they have a uniform distribution on [0,1]
  features_new <- pnorm(features_new, 0, 1)
  
  # Transform marginals such that they have the appropriate distribution
  # Claim code
  
  features_new[, 1] <- ceiling(f.cc(features_new[, 1], alpha = param_cc[1], beta = param_cc[2], const = param_cc[3]))
  features_new[, 1] <- translator_cc[features_new[, 1]]
  
  # Accident quarter (AQ)
  features_new[, 2] <- ceiling(features_new[, 2] * 4)
  
  # Age of the injured
  features_new[which(features_new[, 3] <= param_age[5]), 3] <- ceiling(f.age1(features_new[which(features_new[, 3] <= param_age[5]), 3], alpha = param_age[1], beta = param_age[2], const = param_age[4]))
  features_new[which(features_new[, 3] <= 1), 3] <- ceiling(f.age2(features_new[which(features_new[, 3] <= 1), 3], alpha = param_age[1], beta = param_age[2], gamma = param_age[3], const = param_age[4]))
  
  # Injured part
  features_new[, 4] <- ceiling(f.inj_part(features_new[, 4], alpha = param_inj_part[1], beta = param_inj_part[2], const = param_inj_part[3]))
  features_new[, 4] <- translator_inj_part[features_new[, 4]]
  features_new
}