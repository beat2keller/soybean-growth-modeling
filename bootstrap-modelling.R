

# this file calculates the bootstrapped confidence intervals for a given subsample

# Packages
library(nlme)
library(stringr)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(merTools)
library(data.table)
library(car)
library(nlraa)
# data "plot_grouped_global" is used for the grouping of the data
df = read.csv("data/model_data.csv")

###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- START
###########################################################
# restore factor variables lost due to saving 
df$genotype.id   <- as.factor(df$genotype.id) 
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepare grouping
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

###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- END
###########################################################

#### SELECTING THE CANDIDATES
selected_genotypes = c(10004, 10009, 10014, 10015, 10018, 10020)
n = length(selected_genotypes)

# Filter the dataset to include only the selected genotypes
df <- df %>%
  filter(genotype.id %in% selected_genotypes)

#### NLME FOR THE SUBSAMPLE

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)
#update as nlme model with random effects
cc_rf_scal <- nlme( fm1Soy.lis , random = Asym+ xmid ~ 1, weights = varPower())
# vectors for starting values
soyFix <- fixef(cc_rf_scal)

no_genotypes = n
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1)) 
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal <- c(soyFix[3], rep(0,3*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

start_time <- Sys.time()

model <- update(cc_rf_scal, 
                        fixed = list(Asym ~ genotype.id,
                                     xmid ~avg_Temperature_14 + avg_precipitation_14 ,
                                     scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                        start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                                maxIter = 100, 
                                                                msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)

summary(model)
intervals(model)

save(model, file=paste0("model/bootstrap/modelbootstrap_candidates_seed42.RData"))

#### BOOTSTRAP METHOD FOR CONFIDENCE INTERVALS
# ~ 3 hours
start_time <- Sys.time()

set.seed(42) 
fit <- boot_nlme(model)
end_time <- Sys.time()

print(end_time-start_time)

save(fit, file=paste0("model/bootstrap/modelbootstrap_candidates_seed42_bootstrapped_r999.RData"))

confint(fit, type = 'perc')