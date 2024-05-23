# load model
load("model/Asy_xmid_14_asym1_xmid2_scal3.RData")
model <- 'insert here the model name'
rf1 <- nlme::random.effects(model)
revert_transformation <- Vectorize(function(x){
  sin(x)^2
})

# Plot fitted values vs r
plot(model,
     resid(.) ~ revert_transformation(fitted(.)),
     type = c("p", "smooth"),
     col.line = 1,
     main = "Fitted Values vs Residuals",
     xlab = "Fitted Values",
     ylab = "Residuals")

# QQ plots
par(mfrow = c(1, 1))

qqnorm(y = residuals(model), main = "QQ Plot of Residuals")
qqline(residuals(model), probs = c(0.01, .99))

# Normal distribution of random effects
par(mfrow = c(1, 2))

qqnorm(rf1$Asym, main = "QQ Plot of Random Effect 'Asym'")
qqline(rf1$Asym)

qqnorm(rf1$`xmid.(Intercept)`, main = "QQ Plot of Random Effect 'xmid'")
qqline(rf1$`xmid.(Intercept)`)



