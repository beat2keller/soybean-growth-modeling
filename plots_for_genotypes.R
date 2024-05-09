load("Asy_xmid_14_asym1_xmid2_scal3.RData")
df <- read.csv("data/model_data.csv")
# select genotypes
genos <- c(10001, 10002, 10003, 10004) 
df_p <- subset(df, genotype.id %in% genos)
df_p$genotype.id <- as.factor(df_p$genotype.id)

revert_transformation <- Vectorize(function(x){
  sin(x)^2
})

par(mfrow = c(2, 2))
for (i in 1:length(genos)) {
  df_sub <- df_p[df_p$genotype.id == genos[i], ]
  plot(df_sub$time_since_sowing, 
       y = revert_transformation(predict(cc_rf_scal_14, df_sub)), 
       col = "red",
       xlab = "Time Since Sowing",
       ylab = "Fitted Values with random effects",
       main = paste("Genotype:", genos[i]))
  points(df_sub$time_since_sowing, y = df_sub$value)
  legend("topleft", 
         legend = c("Observed", "Fitted"), 
         col = c("black", "red"), 
         pch = c(1, 1),
         cex = 0.5) # smaller legend
}

for (i in 1:length(genos)) {
  df_sub <- df_p[df_p$genotype.id == genos[i], ]
  plot(df_sub$time_since_sowing, 
       y = revert_transformation(predict(cc_rf_scal_14, df_sub, level = 0)), 
       col = "red",
       xlab = "Time Since Sowing",
       ylab = "Fitted Values without random effects",
       main = paste("Genotype:", genos[i]))
  points(df_sub$time_since_sowing, y = df_sub$value)
  legend("topleft", 
         legend = c("Observed", "Fitted"), 
         col = c("black", "red"), 
         pch = c(1, 1),
         cex = 0.5) # smaller legend
}
