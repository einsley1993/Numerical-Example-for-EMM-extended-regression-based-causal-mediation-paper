


#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################

# ***************************************** True Effects ***************************************** #

library(ggplot2)
library("regmedint")
library(locfit) # for expit()
library(reshape)

library(grid)
library(gridExtra)


set.seed(3104)

# varying coefficient
value <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 4))
colnames(value) <- c("s1", "s2", "s3", "s4")
value[,1] <- c(0, 0.5, 0.8)
value[,2] <- c(0.1, 0.4, 0.7)
value[,3] <- c(0.2, 0.5, 0.8)
value[,4] <- c(0.3, 0.6, 0.9)


# loop over a list of C
c_cond <- seq(-2, 2, 0.1)

a0 <- 0; a1 <- 1

### Set up theta and beta matrices list
b <- th <- est <- list()
for(s in 1:4){
  b[[s]] <- th[[s]] <- est[[s]] <- list()
  for(m in 1:4){
    b[[s]][[m]] <- th[[s]][[m]] <- est[[s]][[m]] <- list()
  }
}

for(m in c(1, 2, 4)){
  for(k in 1:3){
    # Scenario 1; Model 1,2,4
    b[[1]][[m]][[k]] <- c(0.2, 0.4, 0.5, 0)
    th[[1]][[m]][[k]] <- c(0.5, 0.3, 0.2, value[k,1], 0.1, 0, 0)
    # Scenario 2; Model 1,2,4
    b[[2]][[m]][[k]] <- c(0.2, 0.4, 0.5, value[k,2])
    th[[2]][[m]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, 0, 0)
    # Scenario 3; Model 1,2,4
    b[[3]][[m]][[k]] <- c(0.2, 0.4, 0.5, 0.2)
    th[[3]][[m]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, value[k,3], 0)
    # Scenario 4; Model 1,2,4
    b[[4]][[m]][[k]] <- c(0.2, 0.4, 0.5, 0.2)
    th[[4]][[m]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, 0.2, value[k,4])
  }
}

# m = 3: Model 3
for(k in 1:3){
  # Scenario 1
  b[[1]][[3]][[k]] <- c(0.2, 0.4, 0.5, 0)
  th[[1]][[3]][[k]] <- c(0.5, 0.3, 0.2, value[k,1], 0.1, 0, 0)/10
  # Scenario 2
  b[[2]][[3]][[k]] <- c(0.2, 0.4, 0.5, value[k,2])
  th[[2]][[3]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, 0, 0)/10
  # Scenario 3
  b[[3]][[3]][[k]] <- c(0.2, 0.4, 0.5, 0.2)
  th[[3]][[3]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, value[k,3], 0)/10
  # Scenario 4
  b[[4]][[3]][[k]] <- c(0.2, 0.4, 0.5, 0.2)
  th[[4]][[3]][[k]] <- c(0.5, 0.3, 0.2, 0.5, 0.1, 0.2, value[k,4])/10
}



  
### Effect point estimates 
for(s in 1:4){
  for(k in 1:3){
    beta <- b[[s]][[m]][[k]]
    theta <- th[[s]][[m]][[k]]
    
    # Model 1:
    NDE <- (a1 - a0)*(theta[2] + theta[4]*(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond) + theta[6]*c_cond) 
    NIE <- (a1 - a0)*(theta[3] + theta[4]*a1 + theta[7]*c_cond)*(beta[2] + beta[4]*c_cond)
    TE <- NDE + NIE
    est[[s]][[1]][[k]] <- cbind.data.frame(NDE, NIE, TE)
    
    
    # Model 2:
    NDE <- (a1 - a0)*(theta[2] + theta[4]*(expit(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond)) + theta[6]*c_cond)
    NIE <- (theta[3] + theta[4]*a1 + theta[7]*c_cond)*
      (expit(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond) - 
         expit(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond))
    TE <- NDE + NIE
    est[[s]][[2]][[k]] <- cbind.data.frame(NDE, NIE, TE)

    
    # Model 3:
    sigma2 = 0 # truth
    NDE <- exp((theta[2] + theta[6]*c_cond + 
                  theta[4]*(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond)) * (a1 - a0) +
                 1/2 * sigma2^2 * theta[4] * (a1 - a0) * (2*theta[3] + theta[4]*a1 + theta[4]*a0 + 2*theta[7]*c_cond))
    NIE <- exp((a1 - a0)*(theta[3] + theta[4]*a1 + theta[7]*c_cond)*(beta[2] + beta[4]*c_cond))
    TE <- NDE * NIE
    est[[s]][[3]][[k]] <- cbind.data.frame(NDE, NIE, TE)
    
    
    # Model 4:
    NDE <- exp((a1 - a0)*(theta[2] + theta[6]*c_cond))*
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))/
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a0 + theta[7]*c_cond))
    NIE <- (1 + exp(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))/
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond + theta[3] + theta[4]*a1 + theta[7]*c_cond))*
      (1 + exp(beta[1] + beta[2]*a0 + beta[3]*c_cond + beta[4]*a0*c_cond))/
      (1 + exp(beta[1] + beta[2]*a1 + beta[3]*c_cond + beta[4]*a1*c_cond))
    TE <- NDE * NIE
    est[[s]][[4]][[k]] <- cbind.data.frame(NDE, NIE, TE)
    
    
  }
}


# Melt dataset
est.melt <- list()
for(s in 1:4){
  est.melt[[s]] <- list()
  for(m in 1:4){
    est.melt[[s]][[m]] <- list()
    for(k in 1:3){
      est[[s]][[m]][[k]]$C <- c_cond
      est.melt[[s]][[m]][[k]] <- melt(est[[s]][[m]][[k]], id = c("C"))
    }
  }
}



# ***************************************** Plot *****************************************#
g.tru <- list()
for(s in 1:4){
  g.tru[[s]] <- list()
  for(m in 1:4){
    g.tru[[s]][[m]] <- list()
  }
}

# Mediator and outcome model types:
model.temp[[1]] # no need to capitalize
model.temp[[2]]


setwd("/Users/yili/Desktop/regmedint_DGP_plots/ALL_PLOTS/")

### White background
for(s in 1:4){
  for(m in 1:4){
    file_name <- pdf(paste(paste("Truth-Scenario", s, "Model", m, sep = "_"), ".pdf", sep = ""), 
                     height = 3, width = 6)
    for(c in 1:3){
      
      # set up parameters (greek letter) in ggtitle:
      v = value[c,s]
      
      #
      if(s == 1){
        g.tru[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("True Effect") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[3] == .(v)))
      }
      
      if(s == 2){
        g.tru[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("True Effect") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(beta[3] == .(v)))
      }
      
      if(s == 3){
        g.tru[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("True Effect") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[5] == .(v)))
      }
      
      if(s == 4){
        g.tru[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("True Effect") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[6] == .(v)))
      }
      
    }
    
    grid.arrange(arrangeGrob(g.tru[[s]][[m]][[1]], 
                             g.tru[[s]][[m]][[2]], 
                             g.tru[[s]][[m]][[3]], 
                             ncol = 3), 
                 nrow = 1,
                 top = textGrob(paste0("Scenario ", s, ", ", 
                                       model.temp[[1]][m], " mediator model", " , ", 
                                       model.temp[[2]][m], " outcome model",  sep = " "), 
                                gp = gpar(fontsize = 15, font = 3))) 
    dev.off()
  }
}   

 






