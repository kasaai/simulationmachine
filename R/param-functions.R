# LOB 1 and 2

f.cc_12 <- function(x, alpha, beta, const){
  (exp(alpha*x/const + log(beta)) - beta) / alpha
}

f.age1_12 <- function(x, alpha, beta, const){
  sqrt(42*x/(beta*const) + 21^2 * alpha^2 / beta^2) - 21*alpha/beta + 14
}

f.age2_12 <- function(x, alpha, beta, gamma, const){
  -sqrt(-70*x/(gamma*const) + 35^2/(gamma^2)*(alpha+beta)^2 + 21*70*alpha/gamma + 10.5*70*beta/gamma) + 35*alpha/gamma + 35 * beta/gamma + 35
}

f.inj_part_12 <- function(x, alpha, beta, const){
  (exp(alpha*x/const + log(beta)) - beta) / alpha
}

# LOB 3 and 4

f.cc_34 <- function(x, alpha, beta, const){
  (exp(alpha*x/const + log(beta)) - beta) / alpha
}

f.age1_34 <- function(x, alpha, beta, const){
  -sqrt(42*x/(beta*const) + 21^2 * alpha^2 / beta^2) - 21*alpha/beta + 14
}

f.age2_34 <- function(x, alpha, beta, gamma, const){
  -sqrt(pmax(0,-70*x/(gamma*const) + 35^2/(gamma^2)*(alpha+beta)^2 + 21*70*alpha/gamma + 10.5*70*beta/gamma)) + 35*alpha/gamma + 35 * beta/gamma + 35
}

f.inj_part_34 <- function(x, alpha, beta, const){
  (exp(alpha*x/const + log(beta)) - beta) / alpha
}