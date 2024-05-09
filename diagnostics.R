# load model
load("Asy_xmid_14_asym_xmid2_scal3.RData")
model1 <- cc_rf_scal_14
rf1 <- nlme::random.effects(model1)
revert_transformation <- Vectorize(function(x){
  sin(x)^2
})

# Plot fitted values vs r
plot(model1,
     resid(.) ~ revert_transformation(fitted(.)),
     type = c("p", "smooth"),
     col.line = 1,
     main = "Fitted Values vs Residuals (Model 1)",
     xlab = "Fitted Values",
     ylab = "Residuals")

# QQ plots
par(mfrow = c(2, 2))

qqnorm(y = residuals(model1), main = "QQ Plot of Residuals (Model 1)")
qqline(residuals(model1))

# Normal distribution of random effects
par(mfrow = c(2, 2))

qqnorm(rf1$Asym, main = "QQ Plot of Random Effect 'Asym' (Model 1)")
qqline(rf1$Asym)

qqnorm(rf1$`xmid.(Intercept)`, main = "QQ Plot of Random Effect 'xmid.(Intercept)' (Model 1)")
qqline(rf1$`xmid.(Intercept)`)

