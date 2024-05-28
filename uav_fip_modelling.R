

# this file fits the UAV-FIP model and compares it with the simple version

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

###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- START
###########################################################
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

###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- END
###########################################################

### Fitting the main UAV-FIP model <- START

uav_fip_model <- nlme( fm1Soy.lis , random = Asym + xmid ~ 1, control = nlmeControl(maxIter = 100))
fix_ef <- fixed.effects(uav_fip_model)

# length 2 as we have 2 intercepts
dynamic_Asym <- c(fix_ef[1], 0) 
dynamic_xmid <- c(fix_ef[2], 0)
dynamic_scal <- c(fix_ef[3], 0)
dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

# change platform to factor
df$platform = as.factor(df$platform)

start_time <- Sys.time()

# main UAV-FIP model
uav_fip_model <- update(uav_fip_model, 
                        fixed = list(Asym ~ platform,
                                     xmid ~ platform,
                                     scal ~  platform), 
                        start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                                maxIter = 100, 
                                                                msMaxIter =100))
end_time <- Sys.time()
print(end_time-start_time)
summary(uav_fip_model)

save(uav_fip_model, file=paste0("model/platform/UAV_FIP_model.RData"))


### Simple model <- START
# length 1 as only 1 intercept
dynamic_Asym <- c(fix_ef[1]) 
dynamic_xmid <- c(fix_ef[2])
dynamic_scal <- c(fix_ef[3])
dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

# fitting simple model <- same intercept for the two platforms
uav_fip_model_simple <- update(uav_fip_model, 
                               fixed = list(Asym ~ 1,
                                            xmid ~ 1,
                                            scal ~ 1), 
                               start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                       maxIter = 100,
                                                                       msMaxIter =100))
summary(uav_fip_model_simple)

save(uav_fip_model, file=paste0("model/platform/UAV_FIP_model-simple.RData"))

# anova for the two models
anova(uav_fip_model, uav_fip_model_simple)
### Comparison with the simple model <- END
