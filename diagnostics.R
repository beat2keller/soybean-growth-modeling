library(ggplot2)
library(cowplot)

# Define custom color palettes
tol3qualitative <- c("#4477AA", "#DDCC77", "#CC6677")
tol2qualitative <- c("#4477AA", "#CC6677")

# Load models
load("model/baseline9_nlme_v2.2.RData")
Model0 <- model

load("model/avg_photothermalMinus_14_nlme_v2.2.RData")
Model3 <- model

load("model/Asy_xmid_14_asym1_xmid2_scal3.RData")
Model6 <- model_baseline

load("model/baseline9_nlme_v5.1.RData")
Model10 <- model

load("model/avg_photothermalMinus_14_nlme_v5.1.RData")
Model11 <- model

# Revert transformation function
revert_transformation <- Vectorize(function(x) {
  sin(x)^2
})

# Function to generate a grid of four diagnostic plots for given models and colors
generate_diagnostic_plots <- function(models, colors, title) {
  # Residuals vs Fitted
  residuals_df <- data.frame(
    Fitted = do.call(c, lapply(models, function(m) revert_transformation(fitted(m)))),
    Residuals = do.call(c, lapply(models, function(m) residuals(m))),
    Model = factor(rep(names(models), times = sapply(models, function(m) length(fitted(m)))))
  )
  
  plot1 <- ggplot(residuals_df, aes(x = Fitted, y = Residuals, color = Model)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_color_manual(values = colors) +
    labs(title = "Fitted Values vs Residuals", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  # QQ plot of residuals
  qq_residuals_df <- data.frame(
    Residuals = do.call(c, lapply(models, function(m) residuals(m))),
    Model = factor(rep(names(models), times = sapply(models, function(m) length(residuals(m)))))
  )
  
  plot2 <- ggplot(qq_residuals_df, aes(sample = Residuals, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = colors) +
    labs(title = "QQ Plot of Residuals") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # QQ plot of random effect 'Asym'
  asym_df <- data.frame(
    RandomEffect = do.call(c, lapply(models, function(m) nlme::random.effects(m)$Asym)),
    Model = factor(rep(names(models), times = sapply(models, function(m) length(nlme::random.effects(m)$Asym))))
  )
  
  plot3 <- ggplot(asym_df, aes(sample = RandomEffect, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = colors) +
    labs(title = "QQ Plot of Random Effect 'Asym'") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # QQ plot of random effect 'xmid'
  xmid_df <- data.frame(
    RandomEffect = do.call(c, lapply(models, function(m) nlme::random.effects(m)$`xmid.(Intercept)`)),
    Model = factor(rep(names(models), times = sapply(models, function(m) length(nlme::random.effects(m)$`xmid.(Intercept)`))))
  )
  
  plot4 <- ggplot(xmid_df, aes(sample = RandomEffect, color = Model)) +
    geom_qq(alpha = 0.7) +
    geom_qq_line() +
    scale_color_manual(values = colors) +
    labs(title = "QQ Plot of Random Effect 'xmid'") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Extract legend
  legend <- get_legend(plot1)
  
  # Remove legend from plot1
  plot1 <- plot1 + theme(legend.position = "none")
  
  # Combine four plots into a grid
  grid <- plot_grid(plot1, plot2, plot3, plot4, ncol = 2, align = "hv")
  # title_plot <- ggdraw() + draw_label(title, fontface = "bold", size = 16)
  plot_grid(legend, grid, ncol = 1, rel_heights = c(0.1,  1))
}

# Generate diagnostic plots
plot_A <- generate_diagnostic_plots(
  list(Model0 = Model0, Model3 = Model3, Model6 = Model6),
  tol3qualitative,
  # "A: Diagnostics for Model0, Model3, Model6"
)

plot_B <- generate_diagnostic_plots(
  list(Model10 = Model10, Model11 = Model11),
  tol2qualitative,
  # "B: Diagnostics for Model10, Model11"
)

# Combine A and B into a single figure
final_plot <- plot_grid(plot_A, plot_B, ncol = 1, labels = c("A", "B"), label_size = 14)

# Save the combined plot
ggsave("Model_diagnostics.png", final_plot, width = 10, height = 15, units = "in", dpi = 300)

# Display the combined plot
print(final_plot)
