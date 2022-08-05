
#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################

# ***************************************** Bias ***************************************** #
bias.melt <- list()
for(s in 1:4){
  bias.melt[[s]] <- list()
  for(m in 1:4){
    bias.melt[[s]][[m]] <- list()
    for(c in 1:3){
      bias.melt[[s]][[m]][[c]] <- as.data.frame(matrix(NA, nrow = length(c.list)*3, ncol = 3))
      colnames(bias.melt[[s]][[m]][[c]]) <- c("C", "variable", "bias") 
    }
  }
}

# Note: bias is on the same scale as NDE and NIE:
for(s in 1:4){ # loop over 4 scenarios
  for(m in 1:4){ # loop over 4 models
    for(c in 1:3){ # loop over 3 levels of coefficients of C
      for(i in 1:length(c.list)){ # row number
        # Call dataset corresponding to the scenario and the model:
        bias.melt[[s]][[m]][[c]][,1] <- est.melt[[s]][[m]][[c]]$C
        bias.melt[[s]][[m]][[c]][,2] <- est.melt[[s]][[m]][[c]]$variable
        bias.melt[[s]][[m]][[c]][,3] <- sim.mis.out.melt[[s]][[m]][[c]]$value - est.melt[[s]][[m]][[c]]$value
      }
    }
  }
}




