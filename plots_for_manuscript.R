


#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################

# ***************************************** Plots for manuscript ***************************************** #

setwd("/Users/yili/Desktop/regmedint_DGP_plots/MAN_PLOTS_SELECTED")
# common legend
library(gridExtra)
get_legend <-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# legend for Figures 3 (true effects) and 4 (misspecified models):
g.for.legend <- ggplot(data = est.melt[[1]][[1]][[1]], aes(x = C, y = value)) + 
  geom_line(aes(color = variable, linetype = variable), size = 1.5) +
  scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
  labs(color = "Effects", linetype = "Effects") +
  xlab("Covariate Level") + ylab("Effect Estimate") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        # panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(color = NA), # get rid of legend bg
        legend.box.background = element_rect(color = NA) # get rid of legend panel bg
  ) 

shared.legend <- get_legend(g.for.legend)

# legend for Figure 5 (bias):
g.for.legend1 <- ggplot(data = est.melt[[1]][[1]][[1]], aes(x = C, y = value)) + 
  geom_line(aes(color = variable, linetype = variable), size = 1.5) +
  scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
  labs(color = "Effects", linetype = "Effects") +
  xlab("Covariate Level") + ylab("Effect Estimate") +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
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
shared.legend1 <- get_legend(g.for.legend1)




### ======== For main manuscript: 16 SELECTED PLOTS FOR MANUSCRIPT ======== ### 
# 16 selected plots for main manuscript
# run true and misspecified R codes first


### ======= Truth ====== ###
g.main <- list()
for(s in 1:4){
  g.main[[s]] <- list()
  for(m in 1:4){
    g.main[[s]][[m]] <- list()
  }
}


### 1. Figure 2: Special scenario
# Model 1-4: s = 1 to 4, c = 1
s = 1; c = 1
pdf("Special_S1_M1-4.pdf", height = 3.5, width = 10)
for(m in 1:4){
  v = value[c,s]
  g.main[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("True Effect") +
    theme(axis.text.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

grid.arrange(shared.legend,
             g.main[[1]][[1]][[1]], 
             g.main[[1]][[2]][[1]], 
             g.main[[1]][[3]][[1]],
             g.main[[1]][[4]][[1]],
             ncol = 4, 
             nrow = 2,
             layout_matrix = rbind(c(1,1), 
                                   c(2:5)),
             widths = c(2.5, 2.5, 2.5, 2.5), 
             heights = c(1, 2.5)) 
dev.off()





abcd <- c("(a). ", "(b). ", "(c). ", "(d). ")

### 2. Figure 3. Truth ###
pdf("Truth_Combine_M1-4.pdf", height = 11, width = 13)
# Model 1: s = 1 to 4, c = 2
m = 1; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("True Effect") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 2: s = 1 to 4, c = 2
m = 2; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("True Effect") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 3: s = 1 to 4, c = 2
m = 3; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("True Effect") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 4: s = 1 to 4, c = 2
m = 4; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main[[s]][[m]][[c]] <- ggplot(data = est.melt[[s]][[m]][[c]], aes(x = C, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("True Effect") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                         model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

grid.arrange(shared.legend, 
             g.main[[1]][[1]][[2]], 
             g.main[[2]][[1]][[2]], 
             g.main[[3]][[1]][[2]],
             g.main[[4]][[1]][[2]],
             g.main[[1]][[2]][[2]], 
             g.main[[2]][[2]][[2]], 
             g.main[[3]][[2]][[2]],
             g.main[[4]][[2]][[2]],
             g.main[[1]][[3]][[2]],
             g.main[[2]][[3]][[2]],
             g.main[[3]][[3]][[2]],
             g.main[[4]][[3]][[2]],
             g.main[[1]][[4]][[2]],
             g.main[[2]][[4]][[2]],
             g.main[[3]][[4]][[2]],
             g.main[[4]][[4]][[2]],
             ncol = 4, nrow = 5,
             layout_matrix = rbind(c(1,1), 
                                   c(2:5), 
                                   c(6:9), 
                                   c(10:13), 
                                   c(14:17)),
             widths = c(2.5, 2.5, 2.5, 2.5), 
             heights = c(1, 2.5, 2.5, 2.5, 2.5)
) 

dev.off()




### ======= MISSPECIFIED MODEL ====== ###
g.main.mis <- list()
for(s in 1:4){
  g.main.mis[[s]] <- list()
  for(m in 1:4){
    g.main.mis[[s]][[m]] <- list()
  }
}

### 3. Figure 4. Misspecified ###
pdf("Misspecified-Model_Combine_M1-4.pdf", height = 11, width = 15)
# Model 1: s = 1 to 4, c = 2
m = 1; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 2: s = 1 to 4, c = 2
m = 2; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 3: s = 1 to 4, c = 2
m = 3; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 4: s = 1 to 4, c = 2
m = 4; c = 2
for(s in 1:4){
  v = value[c,s]
  g.main.mis[[s]][[m]][[c]] <- ggplot(data = sim.mis.out.melt[[s]][[m]][[c]], aes(x = C.val, y = value)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#CC79A7")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

grid.arrange(shared.legend, 
             g.main.mis[[1]][[1]][[2]], 
             g.main.mis[[2]][[1]][[2]], 
             g.main.mis[[3]][[1]][[2]],
             g.main.mis[[4]][[1]][[2]],
             g.main.mis[[1]][[2]][[2]], 
             g.main.mis[[2]][[2]][[2]], 
             g.main.mis[[3]][[2]][[2]],
             g.main.mis[[4]][[2]][[2]],
             g.main.mis[[1]][[3]][[2]],
             g.main.mis[[2]][[3]][[2]],
             g.main.mis[[3]][[3]][[2]],
             g.main.mis[[4]][[3]][[2]],
             g.main.mis[[1]][[4]][[2]],
             g.main.mis[[2]][[4]][[2]],
             g.main.mis[[3]][[4]][[2]],
             g.main.mis[[4]][[4]][[2]],
             ncol = 4, nrow = 5,
             layout_matrix = rbind(c(1,1), 
                                   c(2:5), 
                                   c(6:9), 
                                   c(10:13), 
                                   c(14:17)),
             widths = c(2.5, 2.5, 2.5, 2.5), 
             heights = c(1, 2.5, 2.5, 2.5, 2.5)
) 

dev.off()




### ======= BIAS ====== ###
g.bias <- list()
for(s in 1:4){
  g.bias[[s]] <- list()
  for(m in 1:4){
    g.bias[[s]][[m]] <- list()
  }
}


### 4. Figure 5. Bias ###
pdf("Bias_M1-4.pdf", height = 11, width = 13)
# Model 1: s = 1 to 4, c = 2
m = 1; c = 2
for(s in 1:4){
  v = value[c,s]
  g.bias[[s]][[m]][[c]] <- ggplot(data =  bias.melt[[s]][[m]][[c]], aes(x = C, y = bias)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 2: s = 1 to 4, c = 2
m = 2; c = 2
for(s in 1:4){
  v = value[c,s]
  g.bias[[s]][[m]][[c]] <- ggplot(data =  bias.melt[[s]][[m]][[c]], aes(x = C, y = bias)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 3: s = 1 to 4, c = 2
m = 3; c = 2
for(s in 1:4){
  v = value[c,s]
  g.bias[[s]][[m]][[c]] <- ggplot(data =  bias.melt[[s]][[m]][[c]], aes(x = C, y = bias)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

# Model 4: s = 1 to 4, c = 2
m = 4; c = 2
for(s in 1:4){
  v = value[c,s]
  g.bias[[s]][[m]][[c]] <- ggplot(data =  bias.melt[[s]][[m]][[c]], aes(x = C, y = bias)) + 
    geom_line(aes(color = variable, linetype = variable), size = 1.5) +
    scale_linetype_manual(values = c("dotdash", "dotted", "solid")) +
    labs(color = "Effects", linetype = "Effects") +
    xlab("Covariate Level") + ylab("Effect Estimate") +
    theme(axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "none") +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(title = paste0(s, abcd[[m]], "Scenario ", s, ": ", 
                        model.temp[[1]][m], ", ", model.temp[[2]][m]))
}

grid.arrange(shared.legend1, 
             g.bias[[1]][[1]][[2]], 
             g.bias[[2]][[1]][[2]], 
             g.bias[[3]][[1]][[2]],
             g.bias[[4]][[1]][[2]],
             g.bias[[1]][[2]][[2]], 
             g.bias[[2]][[2]][[2]], 
             g.bias[[3]][[2]][[2]],
             g.bias[[4]][[2]][[2]],
             g.bias[[1]][[3]][[2]],
             g.bias[[2]][[3]][[2]],
             g.bias[[3]][[3]][[2]],
             g.bias[[4]][[3]][[2]],
             g.bias[[1]][[4]][[2]],
             g.bias[[2]][[4]][[2]],
             g.bias[[3]][[4]][[2]],
             g.bias[[4]][[4]][[2]],
             ncol = 4, nrow = 5,
             layout_matrix = rbind(c(1,1), 
                                   c(2:5), 
                                   c(6:9), 
                                   c(10:13), 
                                   c(14:17)),
             widths = c(2.5, 2.5, 2.5, 2.5), 
             heights = c(1, 2.5, 2.5, 2.5, 2.5)
) 

dev.off()






