#' Simulate Claims
#' 
#' @param features Data frame with features for which we would like to model the cash flow patterns.
#' @param npb Number of observations that are treated at the same time (number per block).
#' @param seed1 Seed for reproducibility.
#' @param std1 Value of the standard deviation used in the log-normal distribution of the claim sizes.
#' @param std2 Value of the standard deviation used in the log-normal distribution of the recovery sizes.
Simulation.Machine <- function(features, npb = nrow(features), seed1 = 100, std1 = 0.85, std2 = 0.85) {
  
  # Features have to have the right format
  if (!is.factor(features[["LoB"]])) stop("`LoB` must be a factor column.", call. = FALSE)
  if (!is.factor(features[["cc"]])) stop("`cc` must be a factor column.", call. = FALSE)
  if (!is.numeric(features[["AY"]])) stop("`AY` must be a numeric column.", call. = FALSE)
  if (!is.numeric(features[["AQ"]])) stop("`AQ` must be a numeric column.", call. = FALSE)
  if (!is.numeric(features[["age"]])) stop("`age` must be a numeric column.", call. = FALSE)
  if (!is.factor(features[["inj_part"]])) stop("`inj_part` must be a factor column.", call. = FALSE)
  
  ### Determine the number of blocks we have
  number.of.blocks <- ceiling(nrow(features) / npb)
  
  ### LoB, cc and inj_part are stored as factors, change them to integers
  features$LoB <- as.integer(levels(features$LoB))[features$LoB]
  features$cc <- as.integer(levels(features$cc))[features$cc]
  features$inj_part <- as.integer(levels(features$inj_part))[features$inj_part]
  
  ### Define the three different neural network functions
  ### for LoB categorical (with values 1,2,3,4)
  neural.network.1 <- function(var1, input, m) {
    
    ### We use the list of variables to get the numbers of hidden neurons as well as the features involved
    q1 <- .list_of_variables[which(.list_of_variables$Variable == var1), 2]
    q2 <- .list_of_variables[which(.list_of_variables$Variable == var1), 3]
    d1 <- sum(.list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)])
    d2 <- .list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)]
    
    ### Create a new dataset which will be the input of the neural network
    ### The first column will be the intercept term
    data.2 <- as.data.frame(rep(1, m), nrow = m)
    
    ### Transform the features to interval [-1,1]
    for (i in 1:length(d2)) {
      if (d2[i] == 1 && is.element(i, c(1, 2, 6))) {
        # data.2[, (ncol(data.2) + 1)] <- as.matrix(read.table(file = paste("./Translators/", var1, "/", colnames(d2)[i], ".txt", sep = ""), sep = "\t", header = TRUE))[input[, i + 1]]
        data.2[, (ncol(data.2) + 1)] <- .translators[[var1]][[colnames(d2)[i]]][input[, i + 1]]
      }
      else if (d2[i] == 1) {
        # translator <- as.matrix(read.table(file = paste("./Translators/", var1, "/", colnames(d2)[i], ".txt", sep = ""), sep = "\t", header = TRUE))
        translator <- .translators[[var1]][[colnames(d2)[i]]]
        data.2[, (ncol(data.2) + 1)] <- round(2 * (pmin(pmax(input[, i + 1], translator[1]), translator[2]) - translator[1]) / (translator[2] - translator[1]) - 1, 2)
      }
    }
    
    ### Load the parameters beta, W1, W2
    beta <- .parameters[[var1]][["beta"]]
    W1 <- .parameters[[var1]][["W1"]]
    W2 <- .parameters[[var1]][["W2"]]
    
    ### Apply the neural network with two hidden layers to data.2
    z1_j <- array(1, c(q1 + 1, m))
    z1_j[-1, ] <- (1 + exp(-W1 %*% t(data.2)))^(-1)
    z2_j <- array(1, c(q2 + 1, m))
    z2_j[-1, ] <- (1 + exp(-W2 %*% (2 * z1_j - 1)))^(-1)
    mu_t <- t(beta) %*% (2 * z2_j - 1)
  }
  
  ### LoB Bernoulli (with values 0,1)
  neural.network.2 <- function(var1, input, m) {
    
    ### We use the list of variables to get the numbers of hidden neurons as well as the features
    q1 <- .list_of_variables[which(.list_of_variables$Variable == var1), 2]
    q2 <- .list_of_variables[which(.list_of_variables$Variable == var1), 3]
    d1 <- sum(.list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)])
    d2 <- .list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)]
    
    ### Create a new dataset which will be the input of the neural network
    ### The first column will be the intercept term
    data.2 <- as.data.frame(rep(1, m), nrow = m)
    
    ### Transform the features to interval [-1,1]
    for (i in 1:length(d2)) {
      if (d2[i] == 1) {
        # translator <- as.matrix(read.table(file = paste("./Translators/", var1, "/", colnames(d2)[i], ".txt", sep = ""), sep = "\t", header = TRUE))
        translator <- .translators[[var1]][[colnames(d2)[[i]]]] 
        data.2[, (ncol(data.2) + 1)] <- round(2 * (pmin(pmax(input[, i + 1], translator[1]), translator[2]) - translator[1]) / (translator[2] - translator[1]) - 1, 2)
      }
    }
    
    ### Load the parameters beta, W1, W2
    beta <- .parameters[[var1]][["beta"]]
    W1 <- .parameters[[var1]][["W1"]]
    W2 <- .parameters[[var1]][["W2"]]
    
    ### Apply the neural network with two hidden layers to data.2
    z1_j <- array(1, c(q1 + 1, m))
    z1_j[-1, ] <- (1 + exp(-W1 %*% t(data.2)))^(-1)
    z2_j <- array(1, c(q2 + 1, m))
    z2_j[-1, ] <- (1 + exp(-W2 %*% (2 * z1_j - 1)))^(-1)
    mu_t <- exp(t(beta) %*% (2 * z2_j - 1))
    pi_t <- mu_t / colSums(mu_t)[col(mu_t)]
  }
  
  ### Neural network with one hidden layer (for closing date)
  neural.network.3 <- function(var1, input, m) {
    
    ### We use the list of variables to get the number of hidden neurons as well as the features
    q <- .list_of_variables[which(.list_of_variables$Variable == var1), 2]
    d1 <- sum(.list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)])
    d2 <- .list_of_variables[which(.list_of_variables$Variable == var1), 4:ncol(.list_of_variables)]
    
    ### Create a new dataset which will be the input of the neural network
    ### The first column will be the intercept term
    data.2 <- as.data.frame(rep(1, m), nrow = m)
    
    ### Transform the features to interval [-1,1]
    for (i in 1:13) {
      if (d2[i] == 1) {
        # translator <- as.matrix(read.table(file = paste("./Translators/", var1, "/", colnames(d2)[i], ".txt", sep = ""), sep = "\t", header = TRUE))
        translator <- .translators[[var1]][[colnames(d2)[[i]]]] 
        data.2[, (ncol(data.2) + 1)] <- round(2 * (pmin(pmax(input[, i + 1], translator[1]), translator[2]) - translator[1]) / (translator[2] - translator[1]) - 1, 2)
      }
    }
    
    data.2[, ((ncol(data.2) + 1):(ncol(data.2) + 12))] <- input[, c("Pay00", "Pay01", "Pay02", "Pay03", "Pay04", "Pay05", "Pay06", "Pay07", "Pay08", "Pay09", "Pay10", "Pay11")]
    
    ### Load the parameters beta, W
    beta <- .parameters[[var1]][["beta"]]
    W <- .parameters[[var]][["W"]]
    ### Apply the neural network with one hidden layer to data.2
    z_j <- array(1, c(q + 1, m))
    z_j[-1, ] <- (1 + exp(-W %*% t(data.2)))^(-1)
    pi0 <- (1 + exp(-t(beta) %*% (2 * z_j - 1)))^(-1)
  }
  
  ### Define the two different cash flow preparation functions
  cash.flow.prep.1 <- function(np, input2, art.obs) {
    
    ### We only look at observations with exactly np payments
    ### We add an artificial observation
    x.np <- rbind(input2[which(input2$K == np), ], art.obs)
    
    ### We simulate the distribution pattern of the payments
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.2(paste("P", np, "K", sep = ""), x.np, nrow(x.np)), ncol = nrow(x.np))
    
    ### The possible distribution patterns are coded as follows
    var1 <- paste("P", np, "K", sep = "")
    # distribution.codes <- as.matrix(read.table(file = paste("./Parameters/", var1, "/distribution.codes.txt", sep = ""), sep = "\t", header = TRUE))
    distribution.codes <- .parameters[[var1]][["distribution.codes"]] 
    
    ### It depends on the reporting delay what distribution patterns of the payments are possible:
    pi_t <- matrix(t(t(pi_t - pi_t * matrix(c((rep(distribution.codes, each = nrow(x.np)) %% 2^(x.np[, 8] + 1)) > 0), nrow = length(distribution.codes), byrow = TRUE)) /
                       colSums(pi_t - pi_t * matrix(c((rep(distribution.codes, each = nrow(x.np)) %% 2^(x.np[, 8] + 1)) > 0), nrow = length(distribution.codes), byrow = TRUE))), ncol = nrow(x.np))
    
    ### We have to separate the cases where none of the distribution.codes is possible because of the reporting delay
    
    ### First we take the observations where we have at least one distribution pattern that is possible
    ### We add one artificial probability vector
    pi_t.tilde <- matrix(cbind(pi_t[, which(1 - is.na(pi_t)[1, ] == 1)], pi_t[, ncol(pi_t)]), nrow = length(distribution.codes))
    
    ### Generate the distribution pattern according to pi_t.tilde
    set.seed(seed1 + 20 + np)
    random.generation <- runif(ncol(pi_t.tilde))
    cumul.pi_t <- lower.tri(diag(nrow(pi_t.tilde)), diag = TRUE) %*% pi_t.tilde / colSums(pi_t.tilde)
    distribution.pattern <- distribution.codes[rowSums(random.generation > t(cumul.pi_t)) + 1]
    
    ### Add the distribution pattern (correcting for the artificially added probability vector)
    x.np[which(1 - is.na(pi_t)[1, ] == 1), 27] <- distribution.pattern[-length(distribution.pattern)]
    
    ### For the observations with reporting delay equal to 1, only few patterns are available
    ### Thus, for a random half of these observations, we distribute the payments arbitrarily
    ### First we add an artificial observation to prevent the code from crashing
    art.obs.new <- art.obs
    art.obs.new$V27 <- NA
    art.obs.new$RepDel <- 1
    x.np <- rbind(x.np, art.obs.new)
    pi_t <- cbind(pi_t, NA) ### need this below
    set.seed(seed1 + 100 + np)
    choose <- sample(1:nrow(x.np[which(x.np$RepDel == 1), ]), ceiling(nrow(x.np[which(x.np$RepDel == 1), ]) / 2))
    set.seed(seed1 + 120 + np)
    samp <- replicate(length(choose), sample(2:12, np))
    x.np[which(x.np$RepDel == 1)[choose], ncol(x.np[which(x.np$RepDel == 1), ])] <- colSums(2^(samp))
    
    
    ### Now we take the observations where we have no distribution pattern that is possible (because of the reporting delay)
    n1 <- sum(is.na(pi_t)[1, ])
    samp <- matrix(NA, ncol = np, nrow = n1)
    set.seed(seed1 + 140 + np)
    dist <- function(x) {
      sample((x + 1):12, size = np)
    }
    samp <- t(sapply(x.np[is.na(pi_t)[1, ], 8], dist))
    x.np[is.na(pi_t)[1, ], 27] <- rowSums(2^(samp))
    
    ### Correct for the observations that we added artificially
    x.np <- x.np[-which(x.np$ClNr == -1), ]
    
    ### Output of the function
    x.np
  }
  
  cash.flow.prep.2 <- function(np, input2, art.obs) {
    
    ### We only look at observations with no recovery payment
    ### We add an artificial observation
    x.np.0 <- rbind(input2[which(input2$Zmnew == 0), ], art.obs)
    
    ### We simulate the proportions paid in the positive payments
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.2(paste("P", np, "pRA", sep = ""), x.np.0, nrow(x.np.0)), ncol = nrow(x.np.0))
    
    ### Determine the cash flow
    x.np.0[, 15:26] <- cash.flow.pattern.1(x.np.0, pi_t)
    
    ### Output of the function
    x.np.0
  }
  
  ### Define the three different cash flow pattern functions
  ### No recovery payments
  cash.flow.pattern.1 <- function(data3, proportions) {
    
    ### Initialize the matrix that will store the positions of the payments
    positions <- matrix(rep(NA, nrow(data3) * 12), nrow = nrow(data3))
    
    ### Determine the positions of the payments
    for (i in 1:12)
    {
      positions[, i] <- pmax(data3[, 27], 0) %% 2^(i + 1)
      data3[, 27] <- data3[, 27] - as.numeric(positions[, i] > 0) * 2^(i)
    }
    
    ### Determine the payments
    positions.tilde <- t(positions)
    positions.tilde[which(positions.tilde != 0)] <- t(ceiling(t(proportions) * exp(data3[, 10])))
    
    ### Output of the function: cash flow patterns
    t(positions.tilde)
  }
  
  ### One recovery payment
  cash.flow.pattern.2 <- function(data3, proportions) {
    ### Initialize the matrix that will store the positions of the payments
    positions <- matrix(rep(NA, nrow(data3) * 12), nrow = nrow(data3))
    
    ### Determine the positions of the payments
    for (i in 1:12)
    {
      positions[, i] <- pmax(data3[, 27], 0) %% 2^(i + 1)
      data3[, 27] <- data3[, 27] - as.numeric(positions[, i] > 0) * 2^(i)
    }
    
    ### Determine the payments
    positions.tilde <- t(positions)
    positions.tilde[which(positions.tilde != 0)] <- t(cbind(ceiling(t(proportions) * (exp(data3[, 10]) + exp(data3[, 14]))), -ceiling(exp(data3[, 14]))))
    
    ### Output of the function: cash flow patterns
    t(positions.tilde)
  }
  
  ### Two recovery payments
  cash.flow.pattern.3 <- function(data3, np, proportions1, proportions2) {
    ### Initialize the matrix that will store the positions of the payments
    positions <- matrix(rep(NA, nrow(data3) * 12), nrow = nrow(data3))
    
    ### Determine the positions of the payments
    for (i in 1:12)
    {
      positions[, i] <- pmax(data3[, 27], 0) %% 2^(i + 1)
      data3[, 27] <- data3[, 27] - as.numeric(positions[, i] > 0) * 2^(i)
    }
    
    ### Determine the positions of the positive payments and of the negative payments
    ### First simulate from a uniform distribution and cumulatively add np to the positions, starting with 0
    set.seed(seed1 + 60 + np)
    random.generation <- sample(2:(np - 1), nrow(data3), replace = TRUE) + seq(from = 0, to = np * (nrow(data3) - 1), by = np)
    positions.tilde <- t(positions)
    positions.tilde[which(positions.tilde > 0)][random.generation] <- -1
    positions.tilde[which(positions.tilde != 0)][seq(from = np, to = np * nrow(data3), by = np)] <- -1
    
    positions.tilde[which(positions.tilde > 0)] <- t(cbind(ceiling(t(proportions1) * (exp(data3[, 10]) + exp(data3[, 14])))))
    positions.tilde[which(positions.tilde < 0)] <- -t(cbind(ceiling(t(proportions2) * exp(data3[, 14]))))
    
    ### Output of the function: cash flow patterns
    t(positions.tilde)
  }
  
  ### The three cash flow pattern functions above are used in the first cash flow function
  ### Define the two cash flow functions
  cash.flow.1 <- function(np, input, art.obs) {
    
    ### First we only look at observations with no recovery payment
    ### We add an artificial observation
    x.np.0 <- rbind(input[which(input$Zmnew == 0), ], art.obs) ### no recovery payment
    
    ### We simulate the proportions paid in the positive payments
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.2(paste("P", np, "pRA", sep = ""), x.np.0, nrow(x.np.0)), ncol = nrow(x.np.0))
    
    ### Determine the cash flow
    x.np.0[, 15:26] <- cash.flow.pattern.1(x.np.0, pi_t)
    
    ### Now we only look at observations with exactly one recovery payment
    ### We add an artificial observation
    x.np.1 <- rbind(input[which(input$Zmnew == 1), ], art.obs) ### one recovery payment
    
    ### We simulate the proportions paid in the positive payments
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.2(paste("P", np - 1, "pRA", sep = ""), x.np.1, nrow(x.np.1)), ncol = nrow(x.np.1))
    
    ### Determine the cash flow
    x.np.1[, 15:26] <- cash.flow.pattern.2(x.np.1, pi_t)
    
    ### Now we only look at observations with exactly two recovery payments
    ### We add an artificial observation
    x.np.2 <- rbind(input[which(input$Zmnew == 2), ], art.obs) ### two recovery payments
    
    ### We simulate the proportions paid in the positive payments
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.2(paste("P", np - 2, "pRA", sep = ""), x.np.2, nrow(x.np.2)), ncol = nrow(x.np.2))
    
    ### We simulate the proportions paid in the two recovery payments
    ### Get the output of the neural network
    pi_t.2 <- matrix(neural.network.1("RecRA", x.np.2, nrow(x.np.2)), ncol = nrow(x.np.2))
    pi_t.2 <- (1 + exp(-pi_t.2))^(-1)
    pi_t.2 <- rbind(pi_t.2, 1 - pi_t.2)
    
    ### Determine the cash flow
    x.np.2[, 15:26] <- cash.flow.pattern.3(x.np.2, np, pi_t, pi_t.2)
    
    ### Output of the function
    x.np <- rbind(x.np.0[-nrow(x.np.0), 1:26], x.np.1[-nrow(x.np.1), 1:26], x.np.2[-nrow(x.np.2), 1:26])
    x.np
  }
  
  ### The second cash flow function applies the first one
  cash.flow.2 <- function(np, input, art.obs1, art.obs2) {
    
    ### For np payments, x.art.wp looks as follows
    art.obs2[27] <- sum(2^(1:np))
    
    ### All observations with exactly np payments together with the distribution patterns of the payments
    x.np <- cash.flow.prep.1(np, input, art.obs1)
    
    ### All observations with exactly np payments together with the cash flows
    x.np <- cash.flow.1(np, x.np, art.obs2)
  }
  
  
  ### Define the main function that simulates:
  ### - the reporting delay
  ### - the cash flow patterns and
  ### - the claims status
  ### of the chosen block of data
  simulations <- function(which.block) {
    
    ### Initialize the data.frame in which we will store the output
    ### n = number of observations in the current block
    n <- min(nrow(features), which.block * npb) - (which.block - 1) * npb
    x <- as.data.frame(array(NA, c(n, 26)))
    colnames(x) <- c(
      "ClNr", "LoB", "cc", "AY", "AQ", "age", "inj_part", "RepDel", "Z", "logY", "K1", "K", "Zmnew",
      "logYm", "Pay00", "Pay01", "Pay02", "Pay03", "Pay04", "Pay05", "Pay06", "Pay07", "Pay08", "Pay09", "Pay10", "Pay11"
    )
    
    ### Choose the observations for which we will generate the cash flow patterns
    x[, 1:7] <- features[((which.block - 1) * npb + 1):min(nrow(features), which.block * npb), ]
    
    ### Create artificial observations that prevent future data.frames from being empty and thus from the function to crash
    x.art <- x[1, ]
    x.art[1:14] <- c(-1, 4, 19, 2001, 1, 40, 70, 0, 1, 5, 0, 4, 1, 4)
    ### We also need a version of the artificial observation with a payment pattern
    x.art.wp <- x.art
    x.art.wp[27] <- NA
    x.art.claimsclosing <- x.art[-c(9:14)]
    x.art.claimsclosing[9:22] <- 0
    colnames(x.art.claimsclosing)[21:22] <- c("ReOp", "maxPayDel")
    
    #######################
    ### Reporting Delay ###
    #######################
    {
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("RepDel", x, nrow(x)), ncol = nrow(x))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### Generate the reporting delay according to pi_t and add it
      set.seed(seed1 + 1)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x[, 8] <- (0:11)[rowSums(random.generation > t(cumul.pi_t)) + 1]
    }
    
    
    #########################
    ### Payment Indicator ###
    #########################
    {
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("Z", x, nrow(x)), ncol = nrow(x))
      pi_t <- (1 + exp(-pi_t))^(-1)
      
      ### Generate the payment indicator according to pi_t and add it
      set.seed(seed1 + 2)
      random.generation <- runif(ncol(pi_t))
      x[, 9] <- colSums(random.generation <= pi_t)
      
      ### We store the observations with no payment in the set x.0
      ### We add an artificial observation
      x.0 <- rbind(x[which(x$Z == 0), ], x.art)
      
      ### We continue only with the observations that have payment indicator = 1
      ### We add an artificial observation
      x1 <- rbind(x[which(x$Z == 1), ], x.art)
    }
    
    
    ####################################
    ### Number of Payments Indicator ###
    ####################################
    {
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("K1", x1, nrow(x1)), ncol = nrow(x1))
      pi_t <- (1 + exp(-pi_t))^(-1)
      
      ### Generate the number of payments indicator according to pi_t and add it
      set.seed(seed1 + 3)
      random.generation <- runif(ncol(pi_t))
      x1[, 11] <- colSums(random.generation <= pi_t)
      
      ### If the reporting delay is 11, then if there is a payment we can only have one payment
      x1[which(x1$RepDel == 11), 11] <- 1
      
      ### Correct for the observation that we added artificially
      x1 <- x1[-nrow(x1), ]
      
      ### We store the observations with only one payment in x.1.0
      ### We add an artificial observation
      x.1.0 <- rbind(x1[which(x1$K1 == 1), ], x.art)
      x.1.0[, 12] <- 1
    }
    
    
    ###################################################
    ### Number of Payments Conditional Distribution ###
    ###################################################
    {
      ### We continue only with the observations that have number of payments indicator = 0, i.e. that have more than one payment
      x2 <- rbind(x1[which(x1$K1 == 0), ], x.art)
      
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("K", x2, nrow(x2)), ncol = nrow(x2))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### It depends on the reporting delay which number of payments are possible
      pi_t <- t(t(pi_t - t(matrix(rep(10:0, each = nrow(x2)), nrow = nrow(x2)) < x2[, 8]) * pi_t) / colSums(pi_t - t(matrix(rep(10:0, each = nrow(x2)), nrow = nrow(x2)) < x2[, 8]) * pi_t))
      
      ### Generate the number of payments according to pi_t and add it
      set.seed(seed1 + 4)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x2[, 12] <- (2:12)[rowSums(random.generation > t(cumul.pi_t)) + 1]
    }
    
    
    ####################
    ### Payment Size ###
    ####################
    {
      ### Get the output of the neural network (for x.1.0)
      mu_t <- neural.network.1("logY", x.1.0, nrow(x.1.0))
      
      ### Generate the payment size according to mu_t and add it (for x.1.0)
      set.seed(seed1 + 15)
      x.1.0[, 10] <- rnorm(nrow(x.1.0), mean = mu_t, sd = std1)
      
      ### Get the output of the neural network (for x2)
      mu_t <- neural.network.1("logY", x2, nrow(x2))
      
      ### Generate the payment size according to mu_t and add it (for x2)
      set.seed(seed1 + 5)
      x2[, 10] <- rnorm(nrow(x2), mean = mu_t, sd = std1)
      
      ### Correct for the observation that we added artificially
      x2 <- x2[-nrow(x2), ]
    }
    
    
    ##########################
    ### Recovery Indicator ###
    ##########################
    {
      ### We separate between two payments and more than two payments
      
      ### First we look at the observations with exactly two payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("Zmnew", rbind(x2[which(x2$K == 2), ], x.art), nrow(rbind(x2[which(x2$K == 2), ], x.art))), ncol = nrow(rbind(x2[which(x2$K == 2), ], x.art)))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### If we have only two payments, the recovery indicator must be 0 or 1
      ### We look at it as if it was a binary outcome 0/1 as in the case of the payment indicator
      pi_t <- matrix(pi_t[1, ] / colSums(matrix(pi_t[-3, ], ncol = nrow(rbind(x2[which(x2$K == 2), ], x.art)))), ncol = nrow(rbind(x2[which(x2$K == 2), ], x.art)))
      
      ### Generate the recovery indicator according to pi_t and add it
      set.seed(seed1 + 6)
      random.generation <- runif(ncol(pi_t))
      x2[which(x2$K == 2), 13] <- colSums(random.generation > pi_t)[-length(colSums(random.generation > pi_t))]
      
      ### Now we look at the observations with more than two payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("Zmnew", rbind(x2[which(x2$K > 2), ], x.art), nrow(rbind(x2[which(x2$K > 2), ], x.art))), ncol = nrow(rbind(x2[which(x2$K > 2), ], x.art)))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### Generate the recovery indicator according to pi_t and add it
      set.seed(seed1 + 7)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x2[which(x2$K > 2), 13] <- (0:2)[rowSums(random.generation > t(cumul.pi_t)) + 1][-length((0:2)[rowSums(random.generation > t(cumul.pi_t)) + 1])]
    }
    
    
    #############################
    ### Recovery Payment Size ###
    #############################
    {
      ### We only look at the observations with recovery indicator = 1 or 2
      ### Get the output of the neural network
      mu_t <- neural.network.1("logYm", x2[which(x2$Zmnew > 0), ], nrow(x2[which(x2$Zmnew > 0), ]))
      
      ### Generate the recovery payment size according to mu_t and add it
      set.seed(seed1 + 8)
      x2[which(x2$Zmnew > 0), 14] <- rnorm(nrow(x2[which(x2$Zmnew > 0), ]), mean = mu_t, sd = std2)
      x2 <- rbind(x.art, x2)
      x2[1, 13] <- 0
      
      ### if there is no recovery payment, we have 0 recovery payment
      x2[which(x2$Zmnew == 0), 14] <- 0
      x2 <- x2[-1, ]
    }
    
    
    
    ###############################
    ###### Cash Flow Patterns #####
    ###############################
    
    
    
    ########################
    ###### 0 payments ######
    ########################
    {
      ### The cash flows are just 0 in the case of no payments
      x.0[, 15:26] <- 0
      
      ### Correct for the observation that we added artificially
      x.0 <- x.0[-nrow(x.0), ]
    }
    
    
    
    #######################
    ###### 1 payment ######
    #######################
    {
      ### We only look at observations with exactly one payment
      ### First we model whether the payment delay is equal to 0 or not
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("P0", x.1.0, nrow(x.1.0)), ncol = nrow(x.1.0))
      pi_t <- (1 + exp(-pi_t))^(-1)
      
      ### Generate the payment delay indicator according to pi_t and add it
      set.seed(seed1 + 9)
      random.generation <- runif(ncol(pi_t))
      x.1.0[, 27] <- colSums(random.generation <= pi_t)
      
      ### If the reporting delay is 11, then if there is a payment, the payment delay must be equal to 0
      ### 1 stands for zero payment delay
      x.1.0[which(x.1.0$RepDel == 11), 27] <- 1
      
      ### Correct for the observation that we added artificially
      x.1.0 <- x.1.0[-nrow(x.1.0), ]
      
      ### Determine cash flow if the payment delay = 0
      ### We add two artificial observations
      x.1.0.1 <- rbind(x.1.0[which(x.1.0$V27 == 1), -27], x.art, x.art)
      
      ### Determine the position of the payment
      positions <- rep(0, 12 * nrow(x.1.0.1))
      positions[x.1.0.1$RepDel + c(1, seq(13, (nrow(x.1.0.1) - 1) * 12 + 1, 12))] <- 1
      positions <- matrix(positions, ncol = 12, byrow = TRUE)
      
      ### Determine the cash flow
      positions.tilde <- t(positions)
      positions.tilde[which(positions.tilde != 0)] <- t(ceiling(exp(x.1.0.1[, 10])))
      x.1.0.1[, 15:26] <- t(positions.tilde)
      
      ### We correct later for the observation that we added artificially (when we join x.1.0.1 and x.1.0.0)
      
      
      ### Determine payment delay if it is greater than 0
      ### We add two artificial observations
      x.1.0.0 <- rbind(x.1.0[which(x.1.0$V27 == 0), -27], x.art, x.art)
      
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("P", x.1.0.0, nrow(x.1.0.0)), ncol = nrow(x.1.0.0))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### It depends on the reporting delay, which payment delays are possible
      pi_t <- t(t(pi_t - t(matrix(rep(10:0, each = nrow(x.1.0.0)), nrow = nrow(x.1.0.0)) < x.1.0.0[, 8]) * pi_t) / colSums(pi_t - t(matrix(rep(10:0, each = nrow(x.1.0.0)), nrow = nrow(x.1.0.0)) < x.1.0.0[, 8]) * pi_t))
      
      ### Generate the number of payments according to pi_t
      set.seed(seed1 + 10)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      payment.delay <- (1:11)[rowSums(random.generation > t(cumul.pi_t)) + 1]
      
      ### Determine cash flows if payment delay > 0
      ### Determine the positions of the payments
      positions <- rep(0, 12 * nrow(x.1.0.0))
      positions[x.1.0.0$RepDel + payment.delay + c(1, seq(13, (nrow(x.1.0.0) - 1) * 12 + 1, 12))] <- 1
      positions <- matrix(positions, ncol = 12, byrow = TRUE)
      
      ### Determine the cash flow
      positions.tilde <- t(positions)
      positions.tilde[which(positions.tilde != 0)] <- t(ceiling(exp(x.1.0.0[, 10])))
      x.1.0.0[, 15:26] <- t(positions.tilde)
      
      ### Put the observations with exactly one payment together
      x.1 <- rbind(x.1.0.1[-c((nrow(x.1.0.1) - 1):nrow(x.1.0.1)), ], x.1.0.0[-c((nrow(x.1.0.0) - 1):nrow(x.1.0.0)), ])
    }
    
    
    
    ########################
    ###### 2 payments ######
    ########################
    {
      ### We only look at observations with exactly two payments
      x.2 <- x2[which(x2$K == 2), ]
    }
    
    
    #############################################
    ### 2 payments (with 0 recovery payments) ###
    #############################################
    {
      ### We only look at observations with zero recovery payments
      x.2.0 <- x.2[which(x.2$Zmnew == 0), ]
      
      ### First we simulate the distributions of the payments
      ### We distinguish between the two cases: reporting delay = 0 and reporting delay > 0
      
      ### Get the output of the neural network if the reporting delay = 0
      ### We add an artificial observation
      x.2.0.0 <- rbind(x.2.0[which(x.2.0$RepDel == 0), ], x.art) ### reporting delay = 0
      pi_t <- matrix(neural.network.1("Ppp", x.2.0.0, nrow(x.2.0.0)), ncol = nrow(x.2.0.0))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### Generate the distribution pattern according to pi_t and add it
      set.seed(seed1 + 11)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x.2.0.0[, 27] <- (1:66)[rowSums(random.generation > t(cumul.pi_t)) + 1]
      
      
      ### Get the output of the neural network if the reporting delay > 0
      ### We add an artificial observation
      x.2.0.1 <- rbind(x.2.0[which(x.2.0$RepDel > 0), ], x.art) ### reporting delay > 0
      pi_t <- matrix(neural.network.1("Ppp", x.2.0.1, nrow(x.2.0.1)), ncol = nrow(x.2.0.1))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### It depends on the reporting delay, which distribution patterns are possible
      pi_t <- t(t(pi_t - t(matrix(rep(1:66, each = nrow(x.2.0.1)), nrow = nrow(x.2.0.1)) < (x.2.0.1[, 8] * (23 - x.2.0.1[, 8]) / 2 + 1)) * pi_t) /
                  colSums(pi_t - t(matrix(rep(1:66, each = nrow(x.2.0.1)), nrow = nrow(x.2.0.1)) < (x.2.0.1[, 8] * (23 - x.2.0.1[, 8]) / 2 + 1)) * pi_t))
      
      ### Generate the distribution pattern according to pi_t and add them
      set.seed(seed1 + 12)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x.2.0.1[, 27] <- (1:66)[rowSums(random.generation > t(cumul.pi_t)) + 1]
      
      
      ### Put the observations with exactly two payments and no recovery payment together
      ### Note that we still need one artificial observation in our data.frame, so we eliminate only the first one
      x.2.0 <- rbind(x.2.0.1[-nrow(x.2.0.1), ], x.2.0.0)
      
      
      ### Now we simulate the proportions paid in the two positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("PppRA", x.2.0, nrow(x.2.0)), ncol = nrow(x.2.0))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### Get the position of the first payment
      position.1 <- colSums(t(x.2.0[, 27] > t(matrix(rep(cumsum(c(0, 11:1)), nrow(x.2.0)), ncol = nrow(x.2.0)))))
      
      ### Get the position of the second payment
      reference <- c(0, cumsum(11:1))
      position.2 <- x.2.0[, 27] - reference[position.1] + position.1
      
      ### Determine the positions of the payments (in matrix form)
      positions.3 <- cbind(position.1 + seq(0, (nrow(x.2.0) - 1) * 12, 12), position.2 + seq(0, (nrow(x.2.0) - 1) * 12, 12))
      positions <- rep(0, 12 * nrow(x.2.0))
      positions[positions.3] <- 1
      positions <- matrix(positions, ncol = 12, byrow = TRUE)
      
      ### Determine the cash flow
      positions.tilde <- t(positions)
      positions.tilde[which(positions.tilde != 0)] <- t(ceiling(t(pi_t) * exp(x.2.0[, 10])))
      x.2.0[, 15:26] <- t(positions.tilde)
    }
    
    
    ############################################
    ### 2 payments (with 1 recovery payment) ###
    ############################################
    {
      ### We only look at observations with exactly one recovery payment
      x.2.1 <- x.2[which(x.2$Zmnew == 1), ] ### one recovery payment
      
      ### First we simulate the distribution of the payments
      ### The 35 possible distributions are coded as follows
      var1 <- "Ppm"
      
      # distribution.codes <- as.matrix(read.table(file = paste("./Parameters/", var1, "/distribution.codes.txt", sep = ""), sep = "\t", header = TRUE))
      distribution.codes <- .parameters[[var1]][["distribution.codes"]] 
      
      
      ### We distinguish between the two cases: reporting delay = 0 and reporting delay > 0
      
      ### Get the output of the neural network if the reporting delay = 0
      ### We add an artificial observation
      x.2.1.0 <- rbind(x.2.1[which(x.2.1$RepDel == 0), ], x.art)
      pi_t <- matrix(neural.network.1("Ppm", x.2.1.0, nrow(x.2.1.0)), ncol = nrow(x.2.1.0))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### Generate the distribution pattern according to pi_t and add it
      set.seed(seed1 + 13)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x.2.1.0[, 27] <- distribution.codes[rowSums(random.generation > t(cumul.pi_t)) + 1]
      
      
      ### Get the output of the neural network if the reporting delay > 0
      ### We add an artificial observation
      x.2.1.1 <- rbind(x.2.1[which(x.2.1$RepDel > 0), ], x.art)
      pi_t <- matrix(neural.network.1("Ppm", x.2.1.1, nrow(x.2.1.1)), ncol = nrow(x.2.1.1))
      pi_t <- exp(pi_t) / colSums(exp(pi_t))[col(exp(pi_t))]
      
      ### It depends on the reporting delay, which distribution patterns are possible
      pi_t <- t(t(pi_t - t(matrix(rep(distribution.codes, each = nrow(x.2.1.1)), nrow = nrow(x.2.1.1)) < (x.2.1.1[, 8] * (23 - x.2.1.1[, 8]) / 2 + 1)) * pi_t) /
                  colSums(pi_t - t(matrix(rep(distribution.codes, each = nrow(x.2.1.1)), nrow = nrow(x.2.1.1)) < (x.2.1.1[, 8] * (23 - x.2.1.1[, 8]) / 2 + 1)) * pi_t))
      
      ### Generate the distribution pattern according to pi_t and add it
      set.seed(seed1 + 14)
      random.generation <- runif(ncol(pi_t))
      cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
      x.2.1.1[, 27] <- distribution.codes[rowSums(random.generation > t(cumul.pi_t)) + 1]
      
      
      ### Put the observations with exactly two payments and one recovery payment together
      ### Note that we still need one artificial observation in our data.frame, so we eliminate only the first one
      x.2.1 <- rbind(x.2.1.1[-nrow(x.2.1.1), ], x.2.1.0)
      
      
      ### Determine the cash flow
      ### Get the position of the first payment
      position.1 <- colSums(t(x.2.1[, 27] > t(matrix(rep(cumsum(c(0, 11:1)), nrow(x.2.1)), ncol = nrow(x.2.1)))))
      
      ### Get the position of the second payment
      position.2 <- x.2.1[, 27] - c(0, cumsum(11:1))[position.1] + position.1
      
      ### Determine the positions of the payments (in matrix form)
      positions.3 <- cbind(position.1 + seq(0, (nrow(x.2.1) - 1) * 12, 12), position.2 + seq(0, (nrow(x.2.1) - 1) * 12, 12))
      positions <- rep(0, 12 * nrow(x.2.1))
      positions[positions.3] <- 1
      positions <- matrix(positions, ncol = 12, byrow = TRUE)
      
      ### Determine the cash flow
      positions.tilde <- t(positions)
      positions.tilde[which(positions.tilde != 0)] <- t(cbind(ceiling(exp(x.2.1[, 10]) + exp(x.2.1[, 14])), -ceiling(exp(x.2.1[, 14]))))
      x.2.1[, 15:26] <- t(positions.tilde)
    }
    
    
    ### All the observations with exactly two payments are stored in x.2
    x.2 <- rbind(x.2.0[-nrow(x.2.0), 1:26], x.2.1[-nrow(x.2.1), 1:26])
    
    
    
    ########################
    ###### 3 payments ######
    ########################
    {
      ### For three payments, x.art.wp looks as follows
      x.art.wp[27] <- sum(2^(1:3))
      
      ### All observations with exactly three payments together with the distribution patterns of the payments
      x.3 <- cash.flow.prep.1(3, x2, x.art)
    }
    
    
    #############################################
    ### 3 payments (with 0 recovery payments) ###
    #############################################
    {
      ### Determine the cash flow when there is no recovery payment
      x.3.0 <- cash.flow.prep.2(3, x.3, x.art.wp)
    }
    
    
    ############################################
    ### 3 payments (with 1 recovery payment) ###
    ############################################
    {
      ### We only look at observations with exactly one recovery payment
      ### We add an artificial observation
      x.3.1 <- rbind(x.3[which(x.3$Zmnew == 1), ], x.art.wp)
      
      ### We simulate the proportions paid in the two positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("P2pRA", x.3.1, nrow(x.3.1)), ncol = nrow(x.3.1))
      pi_t <- (1 + exp(-pi_t))^(-1)
      pi_t <- rbind(pi_t, 1 - pi_t)
      
      ### Determine the cash flow
      x.3.1[, 15:26] <- cash.flow.pattern.2(x.3.1, pi_t)
    }
    
    
    #############################################
    ### 3 payments (with 2 recovery payments) ###
    #############################################
    {
      ### We only look at observations with exactly two recovery payments
      ### We add an artificial observation
      x.3.2 <- rbind(x.3[which(x.3$Zmnew == 2), ], x.art.wp)
      
      ### We simulate the proportions paid in the two recovery payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("RecRA", x.3.2, nrow(x.3.2)), ncol = nrow(x.3.2))
      pi_t <- (1 + exp(-pi_t))^(-1)
      pi_t <- rbind(pi_t, 1 - pi_t)
      
      ### Initialize the matrix that will store the positions of the payments
      positions <- matrix(rep(NA, nrow(x.3.2) * 12), nrow = nrow(x.3.2))
      
      ### Determine the positions of the payments
      for (i in 1:12)
      {
        positions[, i] <- pmax(x.3.2[, 27], 0) %% 2^(i + 1)
        x.3.2[, 27] <- x.3.2[, 27] - as.numeric(positions[, i] > 0) * 2^(i)
      }
      
      ### Determine the cash flow
      positions.tilde <- t(positions)
      positions.tilde[which(positions.tilde != 0)] <- t(cbind(ceiling(exp(x.3.2[, 10]) + exp(x.3.2[, 14])), -ceiling(t(pi_t) * exp(x.3.2[, 14]))))
      x.3.2[, 15:26] <- t(positions.tilde)
    }
    
    
    ### All the observations with exactly three payments are stored in x.3
    x.3 <- rbind(x.3.0[-nrow(x.3.0), 1:26], x.3.1[-nrow(x.3.1), 1:26], x.3.2[-nrow(x.3.2), 1:26])
    
    
    
    ########################
    ###### 4 payments ######
    ########################
    {
      ### For four payments, x.art.wp looks as follows
      x.art.wp[27] <- sum(2^(1:4))
      
      ### All observations with exactly four payments together with the distribution patterns of the payments
      x.4 <- cash.flow.prep.1(4, x2, x.art)
    }
    
    
    #############################################
    ### 4 payments (with 0 recovery payments) ###
    #############################################
    {
      ### Determine the cash flow when there is no recovery payment
      x.4.0 <- cash.flow.prep.2(4, x.4, x.art.wp)
    }
    
    
    ############################################
    ### 4 payments (with 1 recovery payment) ###
    ############################################
    {
      ### We only look at observations with exactly one recovery payment
      ### We add an artificial observation
      x.4.1 <- rbind(x.4[which(x.4$Zmnew == 1), ], x.art.wp)
      
      ### We simulate the proportions paid in the three positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.2("P3pRA", x.4.1, nrow(x.4.1)), ncol = nrow(x.4.1))
      
      ### Determine the cash flow
      x.4.1[, 15:26] <- cash.flow.pattern.2(x.4.1, pi_t)
    }
    
    
    #############################################
    ### 4 payments (with 2 recovery payments) ###
    #############################################
    {
      ### We only look at observations with exactly two recovery payments
      ### We add an artificial observation
      x.4.2 <- rbind(x.4[which(x.4$Zmnew == 2), ], x.art.wp)
      
      ### We simulate the proportions paid in the two positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.1("P2pRA", x.4.2, nrow(x.4.2)), ncol = nrow(x.4.2))
      pi_t <- (1 + exp(-pi_t))^(-1)
      pi_t <- rbind(pi_t, 1 - pi_t)
      
      ### We simulate the proportions paid in the two recovery payments
      ### Get the output of the neural network
      pi_t.2 <- matrix(neural.network.1("RecRA", x.4.2, nrow(x.4.2)), ncol = nrow(x.4.2))
      pi_t.2 <- (1 + exp(-pi_t.2))^(-1)
      pi_t.2 <- rbind(pi_t.2, 1 - pi_t.2)
      
      ### Determine the cash flow
      x.4.2[, 15:26] <- cash.flow.pattern.3(x.4.2, 4, pi_t, pi_t.2)
    }
    
    
    ### All the observations with exactly four payments are stored in x.4
    x.4 <- rbind(x.4.0[-nrow(x.4.0), 1:26], x.4.1[-nrow(x.4.1), 1:26], x.4.2[-nrow(x.4.2), 1:26])
    
    
    
    ###########################
    ###### 5-11 payments ######
    ###########################
    x.5 <- cash.flow.2(5, x2, x.art, x.art.wp)
    x.6 <- cash.flow.2(6, x2, x.art, x.art.wp)
    x.7 <- cash.flow.2(7, x2, x.art, x.art.wp)
    x.8 <- cash.flow.2(8, x2, x.art, x.art.wp)
    x.9 <- cash.flow.2(9, x2, x.art, x.art.wp)
    x.10 <- cash.flow.2(10, x2, x.art, x.art.wp)
    x.11 <- cash.flow.2(11, x2, x.art, x.art.wp)
    
    
    
    #########################
    ###### 12 payments ######
    #########################
    {
      ### We only look at observations with exactly twelve payments
      x.12 <- x2[which(x2$K == 12), ]
    }
    
    
    ##############################################
    ### 12 payments (with 0 recovery payments) ###
    ##############################################
    {
      ### We only look at observations with no recovery payment
      ### We add an artificial observation
      x.12.0 <- rbind(x.12[which(x.12$Zmnew == 0), ], x.art)
      
      ### We simulate the proportions paid in the positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.2("P12pRA", x.12.0, nrow(x.12.0)), ncol = nrow(x.12.0))
      
      ### Determine the cash flow
      x.12.0[, 15:26] <- ceiling(t(pi_t) * as.numeric(exp(x.12.0[, 10])))
    }
    
    
    #############################################
    ### 12 payments (with 1 recovery payment) ###
    #############################################
    {
      ### We only look at observations with exactly one recovery payment
      ### We add an artificial observation
      x.12.1 <- rbind(x.12[which(x.12$Zmnew == 1), ], x.art)
      
      ### We simulate the proportions paid in the eleven positive payments
      ### Get the output of the neural network
      pi_t <- matrix(neural.network.2("P11pRA", x.12.1, nrow(x.12.1)), ncol = nrow(x.12.1))
      
      ### Determine the cash flow
      x.12.1[, 15:25] <- ceiling(t(pi_t) * as.numeric(exp(x.12.1[, 10]) + exp(x.12.1[, 14])))
      x.12.1[, 26] <- ceiling(-as.numeric(exp(x.12.1[, 14])))
    }
    
    
    ##############################################
    ### 12 payments (with 2 recovery payments) ###
    ##############################################
    {
      ### We only look at observations with exactly two recovery payments
      ### We add an artificial observation
      x.12.2 <- rbind(x.12[which(x.12$Zmnew == 2), ], x.art)
      
      ### We simulate the proportions paid in the two recovery payments
      ### Get the output of the neural network
      pi_t.1 <- matrix(neural.network.1("RecRA", x.12.2, nrow(x.12.2)), ncol = nrow(x.12.2))
      pi_t.1 <- (1 + exp(-pi_t.1))^(-1)
      pi_t.1 <- rbind(pi_t.1, 1 - pi_t.1)
      
      ### We simulate the proportions paid in the ten positive payments
      ### Get the output of the neural network
      pi_t.2 <- matrix(neural.network.2("P10pRA", x.12.2, nrow(x.12.2)), ncol = nrow(x.12.2))
      
      
      ### Determine the positions of the positive and the negative payments
      x.12.2[, 15:26] <- 1
      set.seed(seed1 + 80)
      random.generation <- sample(2:11, nrow(x.12.2), replace = TRUE) + seq(from = 0, to = 12 * (nrow(x.12.2) - 1), by = 12)
      positions.tilde <- t(x.12.2[, 15:26])
      positions.tilde[random.generation] <- -1
      positions.tilde[12, ] <- -1
      
      ### Determine the cash flow
      positions.tilde[which(positions.tilde > 0)] <- t(cbind(ceiling(t(pi_t.2) * (exp(x.12.2[, 10]) + exp(x.12.2[, 14])))))
      positions.tilde[which(positions.tilde < 0)] <- -t(cbind(ceiling(t(pi_t.1) * exp(x.12.2[, 14]))))
      x.12.2[, 15:26] <- t(positions.tilde)
    }
    
    
    ### All the observations with exactly twelve payments are stored in x.12
    x.12 <- rbind(x.12.0[-nrow(x.12.0), 1:26], x.12.1[-nrow(x.12.1), 1:26], x.12.2[-nrow(x.12.2), 1:26])
    
    
    
    ### Combine all the observations
    final <- rbind(x.0, x.1, x.2, x.3, x.4, x.5, x.6, x.7, x.8, x.9, x.10, x.11, x.12)
    final <- final[, c(1:8, 15:26)]
    
    
    
    ################################
    ### Re-Opening YES=1 or NO=0 ###
    ################################
    
    ### Use a help dataset
    final.tilde <- final
    
    ### We distinguish between no payments, small payments and big payments
    for (i in 0:11) {
      final.tilde[, 9 + i] <- (-1 + as.integer(final.tilde[, 9 + i] != 0) + as.integer(final.tilde[, 9 + i] > 1000)) * 0.5
    }
    
    ### We simulate the probability of re-opening
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.3("ReOp", final.tilde, nrow(final.tilde)), ncol = nrow(final.tilde))
    
    ### Generate the reopening indicator according to pi_t and add it
    set.seed(seed1 + 40)
    random.generation <- runif(ncol(pi_t))
    final$ReOp <- colSums(random.generation <= pi_t)
    
    ### Determine the time point of the last payment (for claims with no payment it is the reporting delay)
    final <- cbind(final, final[, c(9:20)])
    for (i in (0:11)) {
      final[, 22 + i] <- (i) * as.integer(final[, 22 + i] != 0)
    }
    final$maxPayDel <- pmax(
      final[, 8], final[, 22], final[, 23], final[, 24], final[, 25], final[, 26],
      final[, 27], final[, 28], final[, 29], final[, 30], final[, 31], final[, 32], final[, 33]
    )
    final <- final[, c(1:21, 34)]
    
    ### Split the data set into two parts according to whether we have a re-opening or not
    final1 <- rbind(final[which(final$ReOp == 0), ], x.art.claimsclosing)
    final2 <- rbind(final[which(final$ReOp == 1), ], x.art.claimsclosing)
    
    
    ################################
    ### No Re-Opening (ReOp = 0) ###
    ################################
    
    ### Use a help dataset
    final.tilde <- final1
    
    ### We distinguish between no payments, small payments and big payments
    for (i in 0:11) {
      final.tilde[, 9 + i] <- (-1 + as.integer(final.tilde[, 9 + i] != 0) + as.integer(final.tilde[, 9 + i] > 1000)) * 0.5
    }
    
    ### We simulate the probability of closing not in the same year as the last payment (or as the reporting year)
    ### Get the output of the neural network
    pi_t <- matrix(neural.network.3("Close1", final.tilde, nrow(final.tilde)), ncol = nrow(final.tilde))
    
    ### Determine the probability distribution of the closing year
    final.tilde[, 9] <- 1 - t(pi_t)
    final.tilde[, 10] <- t(pi_t) * 0.9
    final.tilde[, 11:21] <- t(pi_t) * 0.1 / 11
    pi_t <- t(final.tilde[, 9:21])
    
    ### Generate the (possible) closing year according to pi_t and add it
    set.seed(seed1 + 41)
    random.generation <- runif(ncol(pi_t))
    cumul.pi_t <- lower.tri(diag(nrow(pi_t)), diag = TRUE) %*% pi_t / colSums(pi_t)
    final.tilde$SetDel <- (0:12)[rowSums(random.generation > t(cumul.pi_t)) + 1]
    final1$SetDel <- pmin(12, final.tilde$SetDel + final.tilde$maxPayDel)
    
    ### Determine the settlement pattern and add it
    SetPattern <- final1[, c(1, 9:20, ncol(final1))]
    SetPattern[, 2:13] <- 0
    for (i in (0:9)) {
      names(SetPattern)[names(SetPattern) == paste("Pay0", i, sep = "")] <- paste("Open0", i, sep = "")
    }
    for (i in (10:11)) {
      names(SetPattern)[names(SetPattern) == paste("Pay", i, sep = "")] <- paste("Open", i, sep = "")
    }
    for (i in (0:11)) {
      SetPattern[, 2 + i] <- as.numeric(SetPattern$SetDel > i)
    }
    final1 <- cbind(final1, SetPattern)
    final1 <- final1[-nrow(final1), -c(21:24, ncol(final1))]
    
    
    ##################################
    ### With Re-Opening (ReOp = 1) ###
    ##################################
    
    ### Determine the difference between the time of the last payment and the reporting year
    final2$uniform <- final2$maxPayDel - final2$RepDel
    
    ### Determine the first settlement
    set.seed(seed1 + 42)
    final2$SetDel <- final2$RepDel + floor(runif(nrow(final2)) * (final2$uniform + 1))
    
    ### Determine the (possible) second settlement
    set.seed(seed1 + 43)
    final2$SetDel2 <- final2$maxPayDel + floor(runif(nrow(final2)) * (12 - final2$maxPayDel)) + 2
    
    ### Determine the settlement pattern and add it
    SetPattern <- final2[, c(1, 9:20, 24, 25)]
    SetPattern[, 2:13] <- 0
    for (i in (0:9)) {
      names(SetPattern)[names(SetPattern) == paste("Pay0", i, sep = "")] <- paste("Open0", i, sep = "")
    }
    for (i in (10:11)) {
      names(SetPattern)[names(SetPattern) == paste("Pay", i, sep = "")] <- paste("Open", i, sep = "")
    }
    for (i in (0:11)) {
      SetPattern[, 2 + i] <- as.numeric(SetPattern$SetDel > i) + as.numeric(SetPattern$SetDel < i) * as.numeric(SetPattern$SetDel2 > i)
    }
    final2 <- cbind(final2, SetPattern)
    final2 <- final2[-nrow(final2), -c(21:26, 39, 40)]
    
    
    #####################################
    ### Putting the datasets together ###
    #####################################
    
    final <- rbind(final1, final2)
    final
  }
  
  
  
  ##############
  ### Output ###
  ##############
  
  ### Calculate the number of cores
  no_cores <- detectCores() - 1
  
  ### Initiate cluster
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  ### Parallel computation
  output <- as.data.frame(foreach(which.block = 1:number.of.blocks, .combine = "rbind") %dopar% simulations(which.block))
  
  ### Close cluster
  stopCluster(cl)
  
  ### Convert LoB, cc and inj_part back to factors
  output$LoB <- factor(output$LoB)
  output$cc <- factor(output$cc)
  output$inj_part <- factor(output$inj_part)
  
  ### Order the data according to the claims number
  order1 <- order(output$ClNr)
  output <- output[order1, ]
  
  ### Output of the function
  return(output)
}
