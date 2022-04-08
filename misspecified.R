


#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################


# ***************************************** Fit misspecified model ***************************************** #

sim.mis.out <- list()
for(s in 1:4){
  sim.mis.out[[s]] <- list()
  for(m in 1:4){
    sim.mis.out[[s]][[m]] <- list(df.temp, df.temp, df.temp)
  }
}


for(s in 1:4){ # loop over 4 scenarios
  for(m in 1:4){ # loop over 4 models
    for(c in 1:3){ # loop over 3 levels of coefficients of C
      for(i in 1:length(c.list)){ # row number
        # Call dataset corresponding to the scenario and the model:
        data <- dat[[s]][[m]][[c]]
        
        if(s > 1){ # Scenarios 2-4: only have AM_Int = T
          AM_Int_value = Int.temp[s,1]
        }
        else{ # Scenario 1: AM_Int = F
          AM_Int_value = FALSE
        }
        
        sim.mis.out[[s]][[m]][[c]][i,] <- as.data.frame(regmedint_new(data = data, 
                                                                  yvar = data$Y,
                                                                  avar = data$A,
                                                                  mvar = data$M,
                                                                  cvar = data$C,
                                                                  m_model = model.temp[m,1], 
                                                                  y_model = model.temp[m,2], 
                                                                  a0 = 0, 
                                                                  a1 = 1, 
                                                                  c_cond = c.list[i],
                                                                  AM_Int = AM_Int_value,
                                                                  AC_Mmodel_Int = FALSE,
                                                                  AC_Ymodel_Int = FALSE,
                                                                  MC_Int = FALSE))
     
      }
    }
  }
  
}


sim.mis.out.melt <- list()
for(s in 1:4){
  sim.mis.out.melt[[s]] <- list()
  for(m in 1:4){
    sim.mis.out.melt[[s]][[m]] <- list()
  }
}

# Melt dataset
for(s in 1:4){
  for(m in 1:4){
    for(c in 1:3){
      colnames(sim.mis.out[[s]][[m]][[c]]) <- c("NDE", "NIE", "TE")
      sim.mis.out[[s]][[m]][[c]]$C.val <- c.list
      sim.mis.out.melt[[s]][[m]][[c]] <- melt(sim.mis.out[[s]][[m]][[c]], id = c("C.val"))
    }
  }
}



# ***************************************** Plotting *****************************************#
g.mis <- list()
for(s in 1:4){
  g.mis[[s]] <- list()
  for(m in 1:4){
    g.mis[[s]][[m]] <- list()
  }
}

setwd("/Users/yili/Desktop/regmedint_DGP_plots/ALL_PLOTS/")
### White background
for(s in 1:4){
  for(m in 1:4){
    file_name <- pdf(paste(paste("Misspecified-Scenario", s, "Model", m, sep = "_"), ".pdf", sep = ""), 
                     height = 3, width = 6)
    for(c in 1:3){
      
      # set up parameters (greek letter) in ggtitle:
      v = value[,s][c]
      
      #
      if(s == 1){
        g.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("Effect Estimate") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[3] == .(v)))
      }
      
      if(s == 2){
        g.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("Effect Estimate") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(beta[3] == .(v)))
      }
      
      if(s == 3){
        g.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("Effect Estimate") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[5] == .(v)))
      }
      
      if(s == 4){
        g.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
          geom_line(aes(color = variable, linetype = variable), size = 1.5) +
          scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
          labs(color = "Effects", linetype = "Effects") +
          xlab("Covariate Level") + ylab("Effect Estimate") +
          theme(axis.text.x = element_text(face = "bold", size = 11),
                axis.text.y = element_text(face = "bold", size = 11),
                legend.position = "none") +
          scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
          ggtitle(bquote(theta[6] == .(v)))
      }
      
      
      
    }
    
    grid.arrange(arrangeGrob(g.mis[[s]][[m]][[1]], 
                             g.mis[[s]][[m]][[2]], 
                             g.mis[[s]][[m]][[3]], 
                             ncol = 3), 
                 nrow = 1,
                 # shared.legend,
                 top = textGrob(paste0("Scenario ", s, ", ", 
                                       model.temp[[1]][m], " mediator model", "-", 
                                       model.temp[[2]][m], " outcome model",  sep = " "), 
                                gp = gpar(fontsize = 15, font = 3))) 
    dev.off()
  }
}




### not used:
library(ggpubr)
g.for.legend <- ggplot(data = sim.mis.out.melt[[1]][[1]][[1]], aes(x = C.val, y = value)) + 
  geom_line(aes(color = variable, linetype = variable), size = 1.5) +
  scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
  labs(color = "Effects", linetype = "Effects") +
  xlab("Covariate Level") + ylab("Effect Estimate") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
       
        # transparent background
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  ) 

# Extract the legend by get_legend from "ggpubr" package
shared.legend <- get_legend(g.for.legend)

# Convert to a ggplot and print
pdf("_legend.pdf", width = 3, height = 1)
ggpubr::as_ggplot(shared.legend) 
dev.off()












