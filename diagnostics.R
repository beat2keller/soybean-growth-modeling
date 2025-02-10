library(ggplot2)
library(cowplot)

# Define custom color palettes
tol3qualitative <- c("#4477AA", "#DDCC77", "#CC6677")
tol2qualitative <- c("#4477AA", "#CC6677")

# Load models
load("model/Growth0_G.RData")
Model0 <- Growth0_G

load("model/Growth3_E.GxP.RData")
Model3 <- Growth3_E.GxP

load("model/Growth6_E.GxPxPre.RData")
Model6 <- Growth6_E.GxPxPre

load("model/Senescence0_G.RData")
Model10 <- Senescence0_G

load("model/Senescence3_E.GxP.RData") 
Model13 <- Senescence3_E.GxP


# Revert transformation function
revert_transformation <- Vectorize(function(x) {
  sin(x)^2
})

generate_diagnostic_plots <- function(models, title) {
  # Define a fixed color mapping for models
  model_colors <- c(
    "Growth0_G" = "#332288",  # Dark Blue
    "Growth3_E.GxP" = "#88CCEE",  # Light Blue
    "Growth6_E.GxPxPre" = "#117733",  # Green
    "Senescence0_G" = "#DDCC77",  # Yellow
    "Senescence3_E.GxP" = "#CC6677"  # Red
  )
  
  # Filter out missing models
  models <- models[names(models) %in% names(model_colors)]
  
  # **Ensure model names are strings and consistent**
  model_names <- names(models)
  
  # Residuals vs Fitted
  residuals_df <- data.frame(
    Fitted = do.call(c, lapply(models, function(m) revert_transformation(fitted(m)))),
    Residuals = do.call(c, lapply(models, function(m) residuals(m))),
    Model = factor(rep(model_names, times = sapply(models, function(m) length(fitted(m)))), 
                   levels = model_names) # Ensure consistent levels
  )
  
  plot1 <- ggplot(residuals_df, aes(x = Fitted, y = Residuals, color = Model)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_color_manual(values = model_colors) +
    labs(title = "Fitted Values vs Residuals", x = "Fitted Values", y = "Residuals") +
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  
  # Debugging: Check if DataFrame is created properly
  print(head(residuals_df))
  
  # QQ plot of residuals
  qq_residuals_df <- data.frame(
    Residuals = do.call(c, lapply(models, function(m) residuals(m))),
    Model = factor(rep(model_names, times = sapply(models, function(m) length(residuals(m)))), 
                   levels = model_names)
  )
  
  plot2 <- ggplot(qq_residuals_df, aes(sample = Residuals, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = model_colors) +
    labs(title = "QQ Plot of Residuals") +
    theme_minimal() +
    theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  
  # QQ plot of random effect 'Asym'
  asym_values <- lapply(models, function(m) nlme::random.effects(m)$Asym)
  asym_lengths <- sapply(asym_values, length)
  
  asym_df <- data.frame(
    RandomEffect = unlist(asym_values),
    Model = factor(rep(model_names, times = asym_lengths), levels = model_names)
  )
  
  plot3 <- ggplot(asym_df, aes(sample = RandomEffect, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = model_colors) +
    labs(title = "QQ Plot of Random Effect 'Asym'") +
    theme_minimal() +
    theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  
  # QQ plot of random effect 'xmid'
  xmid_values <- lapply(models, function(m) nlme::random.effects(m)$`xmid.(Intercept)`)
  xmid_lengths <- sapply(xmid_values, length)
  
  xmid_df <- data.frame(
    RandomEffect = unlist(xmid_values),
    Model = factor(rep(model_names, times = xmid_lengths), levels = model_names)
  )
  
  plot4 <- ggplot(xmid_df, aes(sample = RandomEffect, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = model_colors) +
    labs(title = "QQ Plot of Random Effect 'xmid'") +
    theme_minimal() +
    theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
  
  # Extract legend
  legend <- get_legend(plot1)
  
  # Remove legend from plot1
  plot1 <- plot1 + theme(legend.position = "none")
  
  # Combine all four plots into a grid
  grid <- plot_grid(plot1, plot2, plot3, plot4, ncol = 2, align = "hv")
  
  plot_grid(legend, grid, ncol = 1, rel_heights = c(0.1, 1))
}

plot_A <- generate_diagnostic_plots(
  list(
    Growth0_G = Model0, 
    Growth3_E.GxP = Model3, 
    Growth6_E.GxPxPre = Model6
  ), 
  "A: Diagnostics for Growth0_G, Growth3_E.GxP, Growth6_E.GxPxPre"
)

plot_B <- generate_diagnostic_plots(
  list(
    Senescence0_G = Model10, 
    Senescence3_E.GxP = Model13
  ), 
  "B: Diagnostics for Senescence0_G, Senescence3_E.GxP"
)

final_plot <- plot_grid(plot_A, plot_B, ncol = 1, labels = c("A", "B"), label_size = 14)

ggsave("Model_diagnostics.png", final_plot, width = 10, height = 15, units = "in", dpi = 300)
print(final_plot)
