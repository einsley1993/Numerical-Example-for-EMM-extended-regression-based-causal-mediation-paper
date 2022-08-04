


#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################

getwd()

library(ggplot2)
library("regmedint")
library(locfit) # for expit()
library(reshape)

library(grid)
library(gridExtra)


# ***************************************** Generate Data *****************************************#
set.seed(3104)
# Scenario 1, Model 1: M linear, Y linear
datamaker.s1.m1 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- 0.2 + 0.4*A + 0.5*C + 0.0*A*C + rnorm(n, 0, 0.5)
  Y <- 0.5 + 0.3*A + 0.2*M + k*A*M + 0.1*C + 0.0*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 1, Model 2: M logistic, Y linear
datamaker.s1.m2 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.0*A*C))
  Y <- 0.5 + 0.3*A + 0.2*M + k*A*M + 0.1*C + 0.0*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 1, Model 3: M linear, Y logistic
datamaker.s1.m3 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- (0.2 + 0.4*A + 0.5*C + 0.0*A*C + rnorm(n, 0, 0.5))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + k*A*M + 0.1*C + 0.0*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 1, Model 4: M logistic, Y logistic
datamaker.s1.m4 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.0*A*C))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + k*A*M + 0.1*C + 0.0*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 




# Scenario 2, Model 1: M linear, Y linear
datamaker.s2.m1 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- 0.2 + 0.4*A + 0.5*C + k*A*C + rnorm(n, 0, 0.5)
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.0*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 2, Model 2: M logistic, Y linear
datamaker.s2.m2 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + k*A*C))
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.0*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 2, Model 3: M linear, Y logistic
datamaker.s2.m3 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- (0.2 + 0.4*A + 0.5*C + k*A*C + rnorm(n, 0, 0.5))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.0*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 2, Model 4: M logistic, Y logistic
datamaker.s2.m4 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + k*A*C))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.0*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 


# Scenario 3, Model 1: M linear, Y linear
datamaker.s3.m1 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- 0.2 + 0.4*A + 0.5*C + 0.2*A*C + rnorm(n, 0, 0.5)
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + k*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 3, Model 2: M logistic, Y linear
datamaker.s3.m2 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.2*A*C))
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + k*A*C + 0.0*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 3, Model 3: M linear, Y logistic
datamaker.s3.m3 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- (0.2 + 0.4*A + 0.5*C + 0.2*A*C + rnorm(n, 0, 0.5))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + k*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 3, Model 4: M logistic, Y logistic
datamaker.s3.m4 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.2*A*C))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + k*A*C + 0.0*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 




# Scenario 4, Model 1: M linear, Y linear
datamaker.s4.m1 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- 0.2 + 0.4*A + 0.5*C + 0.2*A*C + rnorm(n, 0, 0.5)
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.2*A*C + k*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 4, Model 2: M logistic, Y linear
datamaker.s4.m2 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.2*A*C))
  Y <- 0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.2*A*C + k*M*C + rnorm(n, 0, 0.5) 
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 4, Model 3: M linear, Y logistic
datamaker.s4.m3 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- (0.2 + 0.4*A + 0.5*C + 0.2*A*C + rnorm(n, 0, 0.5))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.2*A*C + k*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 

# Scenario 4, Model 4: M logistic, Y logistic
datamaker.s4.m4 = function(n, k){
  C <- matrix(rnorm(n*1, 0, 2), ncol = 1)
  A <- rbinom(n, 1, expit(C + C^2))
  M <- rbinom(n, 1, expit(0.2 + 0.4*A + 0.5*C + 0.2*A*C))
  Y <- rbinom(n, 1, expit(0.5 + 0.3*A + 0.2*M + 0.5*A*M + 0.1*C + 0.2*A*C + k*M*C)/10)
  list(C = C, A = A, M = M, Y = Y)
} 


### put datamaker functions in a list
datamaker.s1 <- list(datamaker.s1.m1, datamaker.s1.m2, datamaker.s1.m3, datamaker.s1.m4)
datamaker.s2 <- list(datamaker.s2.m1, datamaker.s2.m2, datamaker.s2.m3, datamaker.s2.m4)
datamaker.s3 <- list(datamaker.s3.m1, datamaker.s3.m2, datamaker.s3.m3, datamaker.s3.m4)
datamaker.s4 <- list(datamaker.s4.m1, datamaker.s4.m2, datamaker.s4.m3, datamaker.s4.m4)
datamaker <- list(datamaker.s1, datamaker.s2, datamaker.s3, datamaker.s4)
# datamaker.all[[scenario]][[model]]

rm(list = ls(pattern = "datamaker.s"))



# varying coefficient
value <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 4))
colnames(value) <- c("s1", "s2", "s3", "s4")
value[,1] <- c(0, 0.5, 0.8)
value[,2] <- c(0.1, 0.4, 0.7)
value[,3] <- c(0.2, 0.5, 0.8)
value[,4] <- c(0.3, 0.6, 0.9)


N = 5000
### Generate data for each scenario and model type, loop over coefficients of C: 
# dat[[scenario]][[model]][[coef]]
dat <- list()
for(s in 1:4){
  dat[[s]] <- list()
  for(m in 1:4){
    dat[[s]][[m]] <- list()
    # 3 levels of coefficients
    dat[[s]][[m]] <- list(as.data.frame(datamaker[[s]][[m]](n = N, k = value[,s][1])), 
                          as.data.frame(datamaker[[s]][[m]](n = N, k = value[,s][2])), 
                          as.data.frame(datamaker[[s]][[m]](n = N, k = value[,s][3])))
  }
}




# ***************************************** Create Function *****************************************#
# ====== Add AxC, MxC product terms ====== #
regmedint_new <- function(data, m_model, y_model, 
                          yvar, avar, mvar, cvar, 
                          AM_Int, AC_Mmodel_Int, AC_Ymodel_Int, MC_Int, 
                          a0, a1, c_cond){
  
  Y <- yvar
  A <- avar
  M <- mvar
  C <- cvar
  
  # Fit models #
  if(m_model == "linear"){
    mreg_fit <- glm(M ~ A + C + I(A*C*as.numeric(AC_Mmodel_Int)), 
                    data = data)
    mreg_out <- summary(mreg_fit)
  }
  
  if(m_model == "logistic"){
    mreg_fit <- glm(M ~ A + C +  I(A*C*as.numeric(AC_Mmodel_Int)), 
                    family = binomial(),
                    data = data)
    mreg_out <- summary(mreg_fit)
  }
  
  if(y_model == "linear"){
    yreg_fit <- glm(Y ~ A + M + I(A*M*as.numeric(AM_Int)) + C + I(A*C*as.numeric(AC_Ymodel_Int)) + I(M*C*as.numeric(MC_Int)), 
                    data = data)
    yreg_out <- summary(yreg_fit)
  }
  
  if(y_model == "logistic"){
    yreg_fit <- glm(Y ~ A + M + I(A*M*as.numeric(AM_Int)) + C + I(A*C*as.numeric(AC_Ymodel_Int)) + I(M*C*as.numeric(MC_Int)), 
                    family = binomial(),
                    data = data)
    yreg_out <- summary(yreg_fit)
  }
  
  
  # Extract coefficients #
  beta <- rep(0, 4) 
  names(beta) <- c("(Intercept)", "A", "C", 
                   "I(A * C * as.numeric(AC_Mmodel_Int))")
  
  for(row_name in row.names(mreg_out$coefficients)){
    beta[row_name] <- mreg_out$coefficients[row_name,1]
  }
  
  theta <- rep(0, 7) 
  names(theta) <- c("(Intercept)", "A", "M", 
                    "I(A * M * as.numeric(AM_Int))", "C",
                    "I(A * C * as.numeric(AC_Ymodel_Int))", 
                    "I(M * C * as.numeric(MC_Int))")
 
  for(row_name in row.names(yreg_out$coefficients)){
    theta[row_name] <- yreg_out$coefficients[row_name,1]
  }
  
  
  ### Effect point estimates 
  # Model 1
  if(m_model == "linear" && y_model == "linear"){
    NDE <- (a1 - a0)*(theta[2] + 
                        theta[4]*(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond) + 
                        theta[6]*c_cond) 
    NIE <- (a1 - a0)*(theta[3] + theta[4]*a1 + theta[7]*c_cond)*(beta[2] + beta[4]*c_cond)
    TE <- NDE + NIE
  }
  
  # Model 2 
  if(m_model == "logistic" && y_model == "linear"){
    NDE <- (a1 - a0)*(theta[2] + theta[4]*(expit(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond)) + theta[6]*c_cond)
    NIE <- (theta[3] + theta[4]*a1 + theta[7]*c_cond)*
      (expit(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond) - 
         expit(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond))
    TE <- NDE + NIE
  }
  
  
  # Model 3
  if(m_model == "linear" && y_model == "logistic"){
    # sigma() function: # Extract Residual Standard Deviation 'Sigma'
    sigma2 = sigma(mreg_fit)^2 # output is a scalar
    NDE <- exp((theta[2] + theta[6]*c_cond + 
                  theta[4]*(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond)) * (a1 - a0) +
                  1/2 * sigma2^2 * theta[4] * (a1 - a0) * (2*theta[3] + theta[4]*a1 + theta[4]*a0 + 2*theta[7]*c_cond))
    NIE <- exp((a1 - a0)*(theta[3] + theta[4]*a1 + theta[7]*c_cond)*(beta[2] + beta[4]*c_cond))
    TE <- NDE * NIE
  }
  
  # Model 4
  if(m_model == "logistic" && y_model == "logistic"){
    NDE <- exp((a1 - a0)*(theta[2] + theta[6]*c_cond))*
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))/
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a0 + theta[7]*c_cond))
    NIE <- (1 + exp(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))/
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))*
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond))/
      (1 + exp(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond))
    TE <- NDE * NIE
  }
  
  # Output
  list(NDE = NDE, 
       NIE = NIE,
       TE = TE)
}


# loop over a list of C
c.list <- seq(-2, 2, 0.1)

# 1. Scenario table: values of XXX_Int terms
Int.temp <- setNames(data.frame(matrix(nrow = 4, ncol = 4)),
                     c("AM_Int", "AC_Mmodel_Int", "AC_Ymodel_Int", "MC_Int"))
Int.temp[,1] <- rep(TRUE, 4)
Int.temp[,2] <- c(FALSE, rep(TRUE, 3))
Int.temp[,3] <- c(rep(FALSE, 2), rep(TRUE, 2))
Int.temp[,4] <- c(rep(FALSE, 3), TRUE)


# 2. Model table: values of m_model and y_model
model.temp <- setNames(data.frame(matrix(nrow = 4, ncol = 2)), c("M_model", "Y_model"))
model.temp[,1] <- c("linear", "logistic", "linear", "logistic")
model.temp[,2] <- c("linear", "linear", "logistic", "logistic")

df.temp <- data.frame(matrix(nrow = length(c.list), ncol = 3))
















