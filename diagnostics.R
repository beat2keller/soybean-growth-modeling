# load model
load("Asym_scal_14_xmid2_scal2.RData")
model =cc_rf_scal_14
rf = nlme::random.effects(model)
revert_transformation = Vectorize(function(x){
  sin(x)^2
})
residuals = resid(model)
#plot fitted values vs r
plot(model,
     resid(.)~revert_transformation(fitted(.)),
     type=c("p","smooth"), col.line=1)


# qq

qqnorm(y = residuals)
qqline(residuals)

# normal dist of random effects

qqnorm(rf$Asym)
qqline(rf$Asym)



qqnorm(rf$`scal.(Intercept)`)
qqline(rf$`scal.(Intercept)`)


scale_location <- ggplot(data = NULL, aes(x = fitted(model), y = sqrt(abs(residuals(model, type=c("normalized")))))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Fitted values", y = "Square root of absolute residuals") +
  theme_minimal()

