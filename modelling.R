

# this file fits the model
# Packages
library(nlme)
library(stringr)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(merTools)
library(data.table)
# data "plot_grouped_global" is used for the grouping of the data
df = read.csv("data/model_data.csv")


########model for either UAV or FIP###############
#df = subset(df, platform =="FIP")
##################################################


# restore factor variables lost due to saving 
df$genotype.id   <- as.factor(df$genotype.id)
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepear grouping
df <- as.data.frame(df)
df <- droplevels(df)
df$Filename <- NULL
#transform outcome  
df$value        <- asin(sqrt(df$value))





# grouped DF
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
# list object for later modelling
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))


# remove outliers suggested by last year. Needed for convergence
fm1Soy.table <- as.data.frame(summary(fm1Soy.lis)$coef)
fm1Soy.table <- setDT(fm1Soy.table)
fm1Soy.table$plot_grouped_global <-1:nrow(fm1Soy.table)

outlier_plots1 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.Asym>4)]
outlier_plots2 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.xmid>100)]
outlier_plots3 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.scal>20)]
outlier_plots4 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.scal)]
outlier_plots <- c(outlier_plots1,outlier_plots2, outlier_plots3, outlier_plots4)
outlier_plots <- outlier_plots[!is.na(outlier_plots)]

df <- subset(df, !plot_grouped_global %in%outlier_plots)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
setDT(df)[,N:=nrow(.SD),by=genotype.id]
df <- subset(df, N>10)
df <- droplevels(df)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
df <- as.data.frame(df)

# repeat process
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)
#update as nlme model 
fm1Soy.nlme <- nlme( fm1Soy.lis , random = Asym + xmid ~ 1, weights = varPower())
# vectors for starting values
soyFix <- fixef( fm1Soy.nlme )
dynamic_Asym <- c(soyFix[1], rep(0, 2*(length(levels(df$genotype.id)))-1))
dynamic_xmid <- c(soyFix[2], rep(0, 2))
dynamic_scal <- c(soyFix[3], rep(0, 1))
dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +

fm6.2Soy.nlme <- update(fm3Soy.nlme, fixed = list(Asym ~ genotype.id*avg_Temperature_28  , xmid ~ avg_Temperature_28 + avg_precipitation_28 , scal ~  avg_Temperature_28  ), start = dynamic_vector, control = list (msVerbose = TRUE,  maxIter = 5000))
save(fm6.2Soy.nlme, file=paste0("final_model_nlme.RData"))


