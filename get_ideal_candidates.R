# This file finds the ideal candidates and gets the coefficients and confidence 
# intervals for their environmental variable effects

# Firstly, we fit the model with the sum-to-zero constraint
# most of the code is the same like in the modelling file

library(readr)
library(nlme)
# library(stringr)
library(ggplot2)
# library(dplyr)
# library(broom.mixed)
# library(merTools)
library(data.table)
# data "plot_grouped_global" is used for the grouping of the data

load("data/Growth_data.RData") 
# 
df$platform <- as.factor(df$platform)


options(contrasts= c("contr.sum","contr.poly"))

###
# no_genotypes = length(levels(df$genotype.id))
# dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
# dynamic_xmid = c(soyFix[2], rep(0,3))
# dynamic_scal = c(soyFix[3], rep(0,2*no_genotypes+1))
# 
# dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
# 
# ##
# start_time <- Sys.time()
# 
# Growth6_E.GxPxPre_Contr.Sum <- update(cc_rf_scal,
#                                       fixed = list(Asym ~ genotype.id+platform,
#                                                    xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
#                                                    scal ~ genotype.id:(avg_photothermal_14+avg_precipitation_14)+platform),
#                                       start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                                               maxIter = 100,
#                                                                               msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# save(Growth6_E.GxPxPre_Contr.Sum, file=paste0("model/", Period, "6_E.GxPxPre_Contr.Sum.RData"))

# this runs for up to 2 hours, you can also load it in here:
load("model/Growth6_E.GxPxPre_Contr.Sum.RData")

ci <- data.frame(lower= intervals(Growth6_E.GxPxPre_Contr.Sum)$fixed[,1], est = intervals(Growth6_E.GxPxPre_Contr.Sum)$fixed[,2], upper = intervals(Growth6_E.GxPxPre_Contr.Sum)$fixed[,3])
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


prec_main_effect <- 0#ci$interval[grep("scal.avg_precipitation_14", ci$names)] # to be offset
rad_main_effect <- 0#ci$interval[grep("scal.avg_photothermal_14", ci$names)] # to be offset


prec_intervals <- ci$interval[grep("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$names)]
quantile(prec_intervals)
rad_intervals <- ci$interval[grep("^scal\\.genotype\\.id[0-9]+:avg_photothermal_14$", ci$names)]
quantile(rad_intervals)
asym_intervals <- ci$interval[grep("Asym.genotype.id", ci$names)]
quantile(asym_intervals)
scals_intervals <- ci$interval[grep("scal\\.genotype\\.id[^:]*$", ci$names)]
quantile(scals_intervals)
precs <- ci$est[grep("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$names)]
quantile(precs)
rad <- ci$est[grep("^scal\\.genotype\\.id[0-9]+:avg_photothermal_14$", ci$names)]
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
  genotype_selection[i, 2] = ifelse(prec_intervals[i] < quantile(prec_intervals)[2], 1,0)
  genotype_selection[i, 3] = ifelse(rad_intervals[i] < quantile(rad_intervals)[2], 1,0)
  median(prec_main_effect+precs)
  precs_abs <- abs(prec_main_effect+precs)
  genotype_selection[i, 4] = ifelse(precs_abs[i] < quantile(precs_abs)[3], 1, 0)  # offset quantile(precs)[3]50%  0.4803419 
  rad_abs <- abs(rad_main_effect+rad)
  genotype_selection[i, 5] = ifelse(rad_abs[i] < quantile(rad_abs)[3], 1, 0) # quantile(rad)
  
  # genotype_selection[i, 6] = ifelse(centred_precs[i] < quantile(centred_precs)[2], 1, 0)  # offset quantile(precs)[3]50%  0.4803419 
  # genotype_selection[i, 7] = ifelse(rad[i] < quantile(centred_rad)[2], 1, 0) # quantile(rad)
  # 
  # genotype_selection[i, 6] = ifelse(precs[i] > quantile(precs,0.3), 1, 0)
  # genotype_selection[i, 7] = ifelse(rad[i] > quantile(rad,0.1), 1, 0)
  genotype_selection[i, 6] = ifelse(scals[i] <  quantile(scals)[2], 1, 0)
  genotype_selection[i, 7] = ifelse(asym[i] >  quantile(asym)[3], 1, 0)
  genotype_selection[i, 8] = ifelse(scals_intervals[i] < quantile(scals_intervals)[2], 1, 0)
  genotype_selection[i, 9] = ifelse(asym_intervals[i] < quantile(asym_intervals)[2], 1, 0)
  i = i+1
}


genotype_selection$sum = rowSums(genotype_selection[, c(2:5,7,9)]) 
candidates = genotype_selection$genotype[genotype_selection$sum > 5] # get the genotypes fulfilling more than ... criteria
candidates <- candidates[!is.na(candidates)]
candidates
# Since the sum-to-zero constraint removes the genotype-id identification, we need
# to match the candidates with the correct id by matching the calculated estimates.
# To do that we need a baseline-model with the treatment-constraint and then 
# calculate the genotype-specific Asymptote estimate for both constraints and match them

options(contrasts= c("contr.treatment","contr.poly"))
df$genotype.id <- relevel(df$genotype.id, ref= as.character(unique(df$genotype.id)[length(unique(df$genotype.id))][[1]]))

# you can skip this by loading in the model directly (see below)
# start_time <- Sys.time()


# Growth6_E.GxPxPre <- update(cc_rf_scal,
#                                       fixed = list(Asym ~ genotype.id+platform,
#                                                    xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
#                                                    scal ~ genotype.id:(avg_photothermal_14+avg_precipitation_14)+platform),
#                                       start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                                               maxIter = 100,
#                                                                               msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# save(Growth6_E.GxPxPre, file=paste0("model/", Period, "6_E.GxPxPre.RData"))

# this runs for up to 2 hours, you can also load it in here:
load("model/Growth6_E.GxPxPre.RData")

ci_baseline <- data.frame(lower= intervals(Growth6_E.GxPxPre)$fixed[,1], est = intervals(Growth6_E.GxPxPre)$fixed[,2], upper = intervals(Growth6_E.GxPxPre)$fixed[,3])
ci_baseline$names <- rownames(ci_baseline)

# calculating the genotype-specific asymptote
ci_baseline$Asym_est = NA
for (i in 2:length(unique(df$genotype.id))) {
  ci_baseline$Asym_est[i] = ci_baseline$est[i] + ci_baseline$est[1]
}

# matching the genotype from the two models
candidate_genotypes = c()
for (i in 1:length(na.omit(candidates))) {
  asym_cand = asym[candidates[i]] + ci[1,2] + ci[grep("platform",rownames(ci)),2]
  par_cand <- na.omit(ci_baseline$names[round(ci_baseline$Asym_est,5) == round(asym_cand[[1]],5)])[[1]]
  candidate <- gsub("\\D", "", par_cand)
  candidate_genotypes = c(candidate_genotypes, candidate)
}
candidate_genotypes

# This way, you obtain a list of ideal candidates
# our ideal candidates are: 10004, 10009, 10014, 10015, 10018, 10020


# Next, we refit the model with each of those candidates as reference, so we get the
# correct confidence intervals


#### Getting the environmental variable main effects for the candidates

# instead of running everything above, you can also just define our candidates here:
# candidate_genotypes = c("10004","10009","10014","10015","10018","10020")

# since the sum-to-zero constraint omits the last genotype, we add it manually here 
# and later check if it fulfills the filtering criteria (in our case it doesn't)

# candidate_genotypes = c(candidate_genotypes, as.character(unique(df$genotype.id)[length(unique(df$genotype.id))][[1]]))

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
                           fixed = list(Asym ~ genotype.id+platform,
                                        xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                                        scal ~ genotype.id*(avg_precipitation_14+avg_radiation_14)+platform),
                           start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                   maxIter = 100,
                                                                   msMaxIter = 100))
  
  end_time <- Sys.time()
  print(end_time-start_time)
  save(model_candidates, file = paste("model/candidates/", id, "_model.RData", sep=""))

  # if you want to load in the models from the repo, comment out the update() function and run this instead:
  load(paste("model/candidates/", id, "_model.RData", sep=""))
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
save(overview_df, file="model/overview_df.RData")




############
tol3qualitative <- c("#4477AA", "#DDCC77", "#CC6677")

# this overview dataframe contains all the information of interest
# it is summarized in the report pdf
candidate_genotypes


load("model/overview_df.RData") 
names(overview_df)[1] <- "genotype.id"
overview_melt <- melt.data.table(setDT(overview_df), id.vars = "genotype.id")
overview_melt <- subset(overview_melt, genotype.id%in%candidate_genotypes)

dt <- overview_melt[!grepl("interval",overview_melt$variable),]
dt[, c("Scale", "Level") := tstrsplit(variable, "_")]
dt$variable <- NULL
dt_cats <- dcast(dt, ...~Level)

load("avg_precipitation_radiation_14_nlme_v2.2.RData")
overview_all_df = intervals(model)$fixed
  
add_gen_id <- read.csv("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Design/2024/ids_soybean_cleaned.csv")
add_gen_id <- add_gen_id[,c("id","name")]
add_gen_id$Genotype <- add_gen_id$name
add_gen_id$genotype.id <- as.character(add_gen_id$id)
add_gen_id$name <- NULL
add_gen_id$id <- NULL
dt_cats <- merge(dt_cats, add_gen_id, by="genotype.id")



library(ggplot2)

# Create the plot
ggplot(dt_cats, aes(x = Genotype, y = est, color = Genotype, group = Genotype)) +
  geom_point(position = position_dodge(width = 0.5)) +  # Add points
  geom_errorbar(aes(ymin = low, ymax = up), width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  labs(
    x = "Scale",
    y = "Estimate",
    # title = "Estimates with Error Bars for Each Scale",
    color = "Genotype"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  facet_grid(Scale~.,scales = "free")



######

library(ggplot2)
library(data.table)

library(ggplot2)
library(data.table)

# Process new variables
Rownames <- rownames(overview_all_df)
overview_all_df <- setDT(as.data.frame(overview_all_df))
overview_all_df$rownames <- Rownames
overview_all_df[, c("Variable", "Interaction") := tstrsplit(rownames, ":", fill = NA)]

# Extract key parts from Variable
overview_all_df[, c("Scale", "gen","Genotype") := tstrsplit(Variable, "\\.", fill = NA)]

# For Interaction terms, replace NA with "None"
overview_all_df[is.na(Interaction), Interaction := "None"]

# Calculate averages across all genotypes and interactions
overview_means <- overview_all_df[!is.na(Genotype) , .(
  avg_lower = median(lower, na.rm = TRUE),
  avg_est = median(est., na.rm = TRUE),
  avg_upper = median(upper, na.rm = TRUE)
), by = .(Scale, Interaction)]

intercepts <- overview_all_df[gen=="(Intercept)" , .(
  intercept_lower = lower,
  intercept_est = est.,
  intercept_upper = upper
  
),by= .(Scale, Interaction)]

overview_means <- merge(overview_means, intercepts, by=c("Scale","Interaction"),all.x = T)
overview_means[is.na(overview_means)] <- 0

overview_means$avg_est <- overview_means$avg_est +overview_means$intercept_est
overview_means$avg_lower <- overview_means$avg_lower +overview_means$intercept_lower
overview_means$avg_upper <- overview_means$avg_upper +overview_means$intercept_upper

# Standardize Scale and Interaction names in `overview_means`
overview_means[, Scale := gsub("scal", "Scale", Scale)]
overview_means[, Interaction := gsub("avg_", "", Interaction)]  # Remove prefixes like "avg_"
overview_means[, Interaction := ifelse(is.na(Interaction), "None", Interaction)]  # Replace NA with "None"

overview_means$Scale[overview_means$Interaction=="precipitation_14"] <- "Gen:Prec"
overview_means$Scale[overview_means$Interaction=="radiation_14"] <- "Gen:Rad"

overview_means <- subset(overview_means, Scale!="xmid")

# Standardize Scale and Genotype names in `dt_cats`
dt_cats[, Scale := gsub("scal", "Scale", Scale)]
dt_cats[, Scale := gsub("Precipitation", "Gen:Prec", Scale)]
dt_cats[, Scale := gsub("Radiation", "Gen:Rad", Scale)]
dt_cats[, Scale := gsub("Asymp", "Asym", Scale)]

# Add average marker data to `overview_means`
overview_means$Genotype <- "Median"

# Plot
ggIdeal_coef <- ggplot() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = -0.2),
    strip.placement = "outside",
    panel.spacing.x = unit(-0.2, "lines"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.key.height = unit(0.5, "line"),
    legend.key.size = unit(1, "lines"),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 8),
    axis.title = element_blank()
  ) +
  geom_point(data = dt_cats, aes(x = Genotype, y = est, color = Genotype),size = 2) +  # Add points
  geom_errorbar(data = dt_cats, aes(x = Genotype, ymin = low, ymax = up), width = 0.2, position = position_dodge(width = 0.5)) +  # Error bars
  geom_point(data = overview_means, aes(x = Genotype, y = avg_est), color = "black", shape = 4, size = 2) +  # Average points
  geom_errorbar(data = overview_means, aes(x = Genotype, ymin = avg_lower, ymax = avg_upper), width = 0.3, color = "grey60") +  # Average error bars
  labs(
    x = "Scale",
    y = "Estimate",
    # title = "Estimates and Interactions with Error Bars",
    # color = "Interaction"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = tol10qualitative[c(7,8)]) +
  facet_grid(Scale ~ ., scales = "free",switch="y")
ggIdeal_coef

