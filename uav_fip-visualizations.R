library(nlme)

# this file visualized the UAV FIP model diagnostics

load(("model/platform/UAV_FIP_model.RData"))

### Diagnostics of the main UAV-FIP model <- START

# QQ plot for residuals
qqnorm(residuals(uav_fip_model), main = "Q-Q Plot of Residuals")
qqline(residuals(uav_fip_model), probs = c(0.01, 0.99))

# retransform the fitted y's
fit_y <- fitted(uav_fip_model)
fit_y <- sin(fit_y)^2

plot(fit_y,residuals(uav_fip_model), col = 'steelblue3', 
     xlab = "Fitted Values", ylab = "Residuals", cex =0.5,
     main = "Fitted Values vs Residuals")

# QQ plots for the random effects
par(mfrow = c(1, 2))
qqnorm(ranef(uav_fip_model)[,1], main = 'QQ Plot of Random effect Asymptote')
qqline(ranef(uav_fip_model)[,1], probs = c(0.01, 0.99))
qqnorm(ranef(uav_fip_model)[,2], main = 'QQ Plot of Random effect xmid.')
qqline(ranef(uav_fip_model)[,2])
par(mfrow = c(1, 1))
### Diagnostics of the main UAV-FIP model <- END

