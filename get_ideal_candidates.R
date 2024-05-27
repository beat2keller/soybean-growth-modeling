# This file finds the ideal candidates and gets the coefficients and confidence 
# intervals for their environmental variable effects

# Firstly, we fit the model with the sum-to-zero constraint
# most of the code is the same like in the modelling file

library(readr)
library(nlme)
library(stringr)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(merTools)
library(data.table)
# data "plot_grouped_global" is used for the grouping of the data

df = read.csv("data/model_data.csv")

# restore factor variables lost due to saving 
df$genotype.id   <- as.factor(df$genotype.id) 
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepare grouping
df <- as.data.frame(df)
df <- droplevels(df)
df$Filename <- NULL
#transform outcome  
df$value <- asin(sqrt(df$value))

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
#update as nlme model with random effects
cc_rf_scal <- nlme( fm1Soy.lis , random = Asym+ xmid ~ 1, weights = varPower())
# vectors for starting values
soyFix <- fixef(cc_rf_scal)

no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1)) 
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal <- c(soyFix[3], rep(0,3*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

# We change the option to the sum-to-zero constraint so the interactions 
# are deviations from the overall mean and not from a specific reference level

options(contrasts= c("contr.sum","contr.poly"))

# you can skip this by directly loading in the model below
start_time <- Sys.time()

model_sumtozero <- update(cc_rf_scal, 
                fixed = list(Asym ~ genotype.id,
                             xmid ~avg_Temperature_14 + avg_precipitation_14 ,
                             scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                        maxIter = 100, 
                                                        msMaxIter = 100))
end_time <- Sys.time()

# save(model_sumtozero, file=paste0("model/model_sumtozero.RData"))
print(end_time-start_time)

# this runs for up to 2 hours, you can also load it in here:
# load("model/model_sumtozero.RData")

ci <- data.frame(lower= intervals(model_sumtozero)$fixed[,1], est = intervals(model_sumtozero)$fixed[,2], upper = intervals(model_sumtozero)$fixed[,3])
ci$names <- rownames(ci)
ci$interval <- ci$upper - ci$lower



## Finding the ideal candidates:

# Now we have exported the data frame to csv, 
# it is easiest to open it in Excel and filter the genotypes according to the
# filtering criteria that are written in the report

# Excel tutorial:

# Since the sum-to-zero-constraint removes the original genotypes name and simply 
# replaces it by 1 ... n, make sure to manually match the correct genotype with 
# the optimal candidates. This can be easily done in Excel by calculating the coefficient 
# for any variable using the sum-to-zero model and the normal model with the treatment 
# constraint. Since the estimates are always the same and the treatment-constraint 
# model preserves the genotype, just get the estimate for e.g. Asymptote by 
# adding the reference level + interaction effect for the treatment constraint 
# and the overall-mean + interaction for the sum-to-zero constraint and then check 
# which genotype has the exact same estimate for that parameter.

# all this can also be done in R directly:

ci$names[]
candidate <- gsub("\\D", "Asym", ci)
indices <- grep("Asym.genotype.id", ci$names)

# extract the estimates and confidence intervals per variable
prec_intervals <- ci$interval[grep("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$names)]
rad_intervals <- ci$interval[grep("^scal\\.genotype\\.id[0-9]+:avg_radiation_14$", ci$names)]
asym_intervals <- ci$interval[grep("Asym.genotype.id", ci$names)]
scals_intervals <- ci$interval[grep("scal\\.genotype\\.id[^:]*$", ci$names)]
precs <- ci$est[grep("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$names)]
rad <- ci$est[grep("^scal\\.genotype\\.id[0-9]+:avg_radiation_14$", ci$names)]
scals <- ci$est[grep("scal\\.genotype\\.id[^:]*$", ci$names)]
asym <- ci$est[grep("Asym.genotype.id", ci$names)]

# initiate dataframe for ideal candidates
genotype_selection <- data.frame(matrix(data=NA, nrow=length(unique(df$genotype.id)), ncol=11))
colnames(genotype_selection) = c('genotype', 'prec_int','prec','rad_int','rad', 'prec_low','rad_low','scal','asymp','scals_int','asym_int')

# filter all estimates and intervals of the variables according to the filtering criteria
# explained in the report

i =1
for (i in 1:length(unique(df$genotype.id))) {
  genotype_selection[i, 1] = i
  genotype_selection[i, 2] = ifelse(prec_intervals[i] < 0.05, 1,0)
  genotype_selection[i, 4] = ifelse(rad_intervals[i] < 0.005, 1,0)
  genotype_selection[i, 3] = ifelse(precs[i] < 1, 1, 0)
  genotype_selection[i, 5] = ifelse(rad[i] < 0.1, 1, 0)
  genotype_selection[i, 6] = ifelse(precs[i] > 0.07, 1, 0)
  genotype_selection[i, 7] = ifelse(rad[i] > 0.002, 1, 0)
  genotype_selection[i, 8] = ifelse(scals[i] < 0, 1, 0)
  genotype_selection[i, 9] = ifelse(asym[i] > -0.03, 1, 0)
  genotype_selection[i, 10] = ifelse(scals_intervals[i] < 20, 1, 0)
  genotype_selection[i, 11] = ifelse(asym_intervals[i] < 0.07, 1, 0)
  i = i+1
}


genotype_selection$sum = rowSums(genotype_selection[, 2:11]) 
candidates = genotype_selection$genotype[genotype_selection$sum == 10] # get the genotypes fulfilling all criteria

# Since the sum-to-zero constraint removes the genotype-id identification, we need
# to match the candidates with the correct id by matching the calculated estimates.
# To do that we need a baseline-model with the treatment-constraint and then 
# calculate the genotype-specific Asymptote estimate for both constraints and match them

options(contrasts= c("contr.treatment","contr.poly"))
df$genotype.id <- relevel(df$genotype.id, ref= as.character(unique(df$genotype.id)[length(unique(df$genotype.id))][[1]]))

# you can skip this by loading in the model directly (see below)
start_time <- Sys.time()

model_baseline <- update(cc_rf_scal, 
                          fixed = list(Asym ~ genotype.id,
                                       xmid ~avg_Temperature_14 + avg_precipitation_14 ,
                                       scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                          start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                                  maxIter = 100, 
                                                                  msMaxIter = 100))
end_time <- Sys.time()
# save("model/Asy_xmid_14_asym_xmid2_scal3.RData")
print(end_time-start_time)

# This runs again for up to 2 hours, you can load in the model from the gitlab repo:
# load("model/Asy_xmid_14_asym_xmid2_scal3.RData")
# model_baseline = cc_rf_scal_14 # align with naming in this file

ci_baseline <- data.frame(lower= intervals(model_baseline)$fixed[,1], est = intervals(model_baseline)$fixed[,2], upper = intervals(model_baseline)$fixed[,3])
ci_baseline$names <- rownames(ci_baseline)

# calculating the genotype-specific asymptote
ci_baseline$Asym_est = NA
for (i in 2:length(unique(df$genotype.id))) {
  ci_baseline$Asym_est[i] = ci_baseline$est[i] + ci_baseline$est[1]
}

# matching the genotype from the two models
candidate_genotypes = c()
for (i in 1:length(na.omit(candidates))) {
  asym_cand = asym[candidates[i]] + ci[1,2]
  par_cand <- na.omit(ci_baseline$names[round(ci_baseline$Asym_est,5) == round(asym_cand[[1]],5)])[[1]]
  candidate <- gsub("\\D", "", par_cand)
  candidate_genotypes = c(candidate_genotypes, candidate)
}


# This way, you obtain a list of ideal candidates
# our ideal candidates are: 10004, 10009, 10014, 10015, 10018, 10020


# Next, we refit the model with each of those candidates as reference, so we get the
# correct confidence intervals


#### Getting the environmental variable main effects for the candidates

# instead of running everything above, you can also just define our candidates here:
# candidate_genotypes = c("10004","10009","10014","10015","10018","10020")

# since the sum-to-zero constraint omits the last genotype, we add it manually here 
# and later check if it fulfills the filtering criteria (in our case it doesn't)

candidate_genotypes = c(candidate_genotypes, as.character(unique(df$genotype.id)[length(unique(df$genotype.id))][[1]]))

overview_df = data.frame(matrix(data=NA, nrow=length(candidate_genotypes), ncol = 17))
colnames(overview_df) = c("Genotype",	"Scale_low",	"Scale_est",	"Scale_up",	"Scale_interval",	
                          "Radiation_low",	"Radiation_est",	"Radiation_up",	"Radiation_interval",	
                          "Precipitation_low",	"Precipitation_est",	"Precipitation_up",	
                          "Precipitation_interval",	"Asymp_low",	"Asymp_est", "Asymp_up",	"Asymp_interval")


# again, you can skip this by loading in directly the models from the repo (see below)
i = 1
for (id in candidate_genotypes) {
  
  df$genotype.id <- relevel(df$genotype.id,ref= id)
  levels(df$genotype.id)[1]
  
  start_time <- Sys.time()
  
  model_candidates <- update(cc_rf_scal, 
                  fixed = list(Asym ~ genotype.id,
                               xmid ~ avg_Temperature_14 + avg_precipitation_14 ,
                               scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                  start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                          maxIter = 100, 
                                                          msMaxIter = 100))
  end_time <- Sys.time()
  print(end_time-start_time)
  # save(model_candidates, file = paste("models/candidates/", id, "_model.RData", sep=""))

  # if you want to load in the models from the repo, comment out the update() function and run this instead:
  # load(paste("models/candidates/", id, "_model.RData", sep=""))
  # probably not even needed: model_candidates = paste("models/candidates/", id, "_model.RData", sep="")
  # model_candidates <- get(paste(id, "_model", sep=""))
  
  ci <- data.frame(lower= intervals(model_candidates)$fixed[,1], est = intervals(model_candidates)$fixed[,2], upper = intervals(model_candidates)$fixed[,3])
  ci$names <- rownames(ci)
  ci$interval <- ci$upper - ci$lower

  overview_df$Genotype[i] = id
  overview_df$Scale_low[i] = ci["scal.(Intercept)","lower"]
  overview_df$Scale_est[i] = ci["scal.(Intercept)","est"]
  overview_df$Scale_up[i] = ci["scal.(Intercept)","upper"]
  overview_df$Scale_interval[i] = overview_df$Scale_up[i] - overview_df$Scale_low[i]
  overview_df$Radiation_low[i] = ci["scal.avg_radiation_14","lower"]
  overview_df$Radiation_est[i] = ci["scal.avg_radiation_14","est"]
  overview_df$Radiation_up[i] = ci["scal.avg_radiation_14","upper"]
  overview_df$Radiation_interval[i] = overview_df$Radiation_up[i] - overview_df$Radiation_low[i]
  overview_df$Precipitation_low[i] = ci["scal.avg_precipitation_14","lower"]
  overview_df$Precipitation_est[i] = ci["scal.avg_precipitation_14","est"]
  overview_df$Precipitation_up[i] = ci["scal.avg_precipitation_14","upper"]
  overview_df$Precipitation_interval[i] = overview_df$Precipitation_up[i] - overview_df$Precipitation_low[i]
  overview_df$Asymp_low[i] = ci["Asym.(Intercept)","lower"]
  overview_df$Asymp_est[i] = ci["Asym.(Intercept)","est"]
  overview_df$Asymp_up[i] = ci["Asym.(Intercept)","upper"]
  overview_df$Asymp_interval[i] = overview_df$Asymp_up[i] - overview_df$Asymp_low[i]
  i = i+1
}

# this overview dataframe contains all the information of interest
# it is summarized in the report pdf
