

#############################################################
## Author: Yi Li
## Date: Mar.1, 2022
#############################################################

# ***************************************** Plots for Supplement ***************************************** #
# run correct and misspecified R codes first
# truth vs. misspecified plots side by side

library(cowplot)
# plot_grid(): multiplots function
# cowplot will change the theme automatically. 
# if want to change back to default theme set: + theme_set(theme_gray())

model.temp.new <- setNames(data.frame(matrix(nrow = 4, ncol = 2)), c("M_model", "Y_model"))
model.temp.new[,1] <- c("Linear", "Logistic", "Linear", "Logistic") # capitalize because mediator model goes first
model.temp.new[,2] <- c("linear", "linear", "logistic", "logistic")

setwd("/Users/yili/Desktop/regmedint_DGP_plots/ALL_PLOTS_panel/")
### White background
for(s in 1:4){
  for(m in 1:4){
    file_name <- pdf(paste(paste("Both-Scenario", s, "Model", m, sep = "_"), ".pdf", sep = ""), 
                     height = 3, width = 12)
    
    g1 <- plot_grid(g.tru[[s]][[m]][[1]], 
                    g.tru[[s]][[m]][[2]], 
                    g.tru[[s]][[m]][[3]],
                    nrow = 1, ncol = 3) +
      theme_set(theme_gray())
    
    g2 <- plot_grid(g.mis[[s]][[m]][[1]], 
                    g.mis[[s]][[m]][[2]], 
                    g.mis[[s]][[m]][[3]],
                    nrow = 1, ncol = 3) +
      theme_set(theme_gray())
    
    g_row <- plot_grid(g1, g2, 
                       labels = c("True effects", "Misspecified model"),
                       label_size = 12,
                       hjust = -0.1,
                       vjust = -0.1,
                       nrow = 1, ncol = 2)
    
    # now add the title
      title <- ggdraw() +
      draw_label(
        paste0("Scenario ", s, ": ", 
               model.temp.new[[1]][m], " mediator model", ", ", 
               model.temp.new[[2]][m], " outcome model"),
        fontfamily = "sans",
        fontface = 'bold',
        size = 13,
        vjust = 0,
        hjust = 0.5, # center the label (aka the plot title)
        lineheight = 0.8
      ) +
      theme(plot.margin = margin(0, 0, 0, 0))

    print(plot_grid(
      title,
      g_row,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.2, 1))  
    )
    
    dev.off()
  }
}



