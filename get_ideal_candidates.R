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
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,2*no_genotypes+1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

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
genotype_selection <- data.frame(matrix(data=NA, nrow=length(unique(df$genotype.id)), ncol=9))
colnames(genotype_selection) = c('genotype', 'prec_int','prec','rad_int','rad_int_2','rad', 'rad_2','asymp','asym_int')

# filter all estimates and intervals of the variables according to the filtering criteria
# explained in the report

i =1
for (i in 1:length(unique(df$genotype.id))) {
  genotype_selection[i, 1] = i
  genotype_selection[i, 2] = ifelse(prec_intervals[i] < quantile(prec_intervals,0.75), 1,0)
  genotype_selection[i, 3] = ifelse(rad_intervals[i] < quantile(rad_intervals,0.75), 1,0)
  genotype_selection[i, 4] = ifelse(rad_intervals[i] < quantile(rad_intervals,0.75), 1,1) #  increase the weight of rad_interval
  median(prec_main_effect+precs)
  precs_abs <- abs(prec_main_effect+precs)
  genotype_selection[i, 5] = ifelse(precs_abs[i] < quantile(precs_abs,0.025), 1, 0)  # offset quantile(precs)[3]50%  0.4803419 
  rad_abs <- abs(rad_main_effect+rad)
  genotype_selection[i, 6] = ifelse(rad_abs[i] < quantile(rad_abs,0.025), 1, 0) # 
  genotype_selection[i, 7] = ifelse(rad_abs[i] < quantile(rad_abs,0.15), 1, 1) #  increase the weight of rad
  genotype_selection[i, 8] = ifelse(rad_abs[i] < quantile(rad_abs,0.25), 1, 1) #  increase the weight of rad
    # genotype_selection[i, 8] = ifelse(asym[i] >  quantile(asym)[3], 1, 1)
  genotype_selection[i, 9] = ifelse(asym_intervals[i] < quantile(asym_intervals,0.25), 1, 1)
  genotype_selection[i, 10] = ifelse(asym_intervals[i] < quantile(asym_intervals,0.05), 1, 1)  #  increase the asym_intervals
  
  i = i+1
}


genotype_selection$sum = rowSums(genotype_selection[, c(2:10)]) 
max(genotype_selection$sum,na.rm = T)
candidates = genotype_selection$genotype[genotype_selection$sum > (max(genotype_selection$sum,na.rm = T)-1)] # get the genotypes fulfilling more than ... criteria
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

# this runs for up to 20 mins, you can also load it in here:
load("model/Growth6_E.GxPxPre.RData")

ci_baseline <- data.frame(lower= intervals(Growth6_E.GxPxPre)$fixed[,1], est = intervals(Growth6_E.GxPxPre)$fixed[,2], upper = intervals(Growth6_E.GxPxPre)$fixed[,3])
ci_baseline$names <- rownames(ci_baseline)

# calculating the genotype-specific asymptote
ci_baseline$Asym_est = NA
length(ci_baseline$names[grepl("Asym.genotype.",ci_baseline$names)])
for (i in 2:length(unique(df$genotype.id))) {
  ci_baseline$Asym_est[i] = ci_baseline$est[i] + ci_baseline$est[1]
}

# matching the genotype from the two models
candidate_genotypes = c()
for (i in 1:length(na.omit(candidates))) {
  asym_cand = asym[candidates[i]] + ci$est[1] + ci$est[grep("Asym.platform",rownames(ci))] ## candidate effect + intercept + platfrom
  par_cand <- (ci_baseline$names[round(ci_baseline$Asym_est,5)  == round(asym_cand,5)])
  par_cand <- par_cand[grepl("Asym",par_cand)]
  candidate <- gsub("\\D", "", par_cand)
  if(length(candidate)==0){candidate <- "10001"} # fix me
  print(candidate)
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
for (id in candidate_genotypes[1]) {
  
  df$genotype.id <- relevel(df$genotype.id,ref= id)
  levels(df$genotype.id)[1]
  
  start_time <- Sys.time()
  
  model_candidates <- update(cc_rf_scal,
                              fixed = list(Asym ~ genotype.id+platform,
                                           xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                                           scal ~ genotype.id:(avg_photothermal_14+avg_precipitation_14)+platform),
                              start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                      maxIter = 200,
                                                                      msMaxIter = 200))

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
  # overview_df$Scale_low[i] = ci["scal.(Intercept)","lower"]
  # overview_df$Scale_est[i] = ci["scal.(Intercept)","est"]
  # overview_df$Scale_up[i] = ci["scal.(Intercept)","upper"]
  # overview_df$Scale_interval[i] = overview_df$Scale_up[i] - overview_df$Scale_low[i]
  overview_df$Photothermal_low[i] = ci[paste0("scal.genotype.id",id,":avg_photothermal_14"),"lower"]
  overview_df$Photothermal_est[i] = ci[paste0("scal.genotype.id",id,":avg_photothermal_14"),"est"]
  overview_df$Photothermal_up[i] = ci[paste0("scal.genotype.id",id,":avg_photothermal_14"),"upper"]
  overview_df$Photothermal_interval[i] = overview_df$photothermal_up[i] - overview_df$photothermal_low[i]
  overview_df$Precipitation_low[i] = ci[paste0("scal.genotype.id",id,":avg_precipitation_14"),"lower"]
  overview_df$Precipitation_est[i] = ci[paste0("scal.genotype.id",id,":avg_precipitation_14"),"est"]
  overview_df$Precipitation_up[i] = ci[paste0("scal.genotype.id",id,":avg_precipitation_14"),"upper"]
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
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")


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

load("model/Growth6_E.GxPxPre.RData")
overview_all_df = intervals(Growth6_E.GxPxPre)$fixed
  
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
overview_all_df[, c("Scale", "gen","genotype") := tstrsplit(Variable, "\\.", fill = NA)]

overview_all_df$Scale[grep("Intercept",overview_all_df$Variable)] <- "Intercept"
# For Interaction terms, replace NA with "None"
overview_all_df[is.na(Interaction), Interaction := "None"]


overview_all_df$genotype.id <- gsub("id","",overview_all_df$genotype)
overview_all_df$Selection <- "Remaining population"
overview_all_df <- merge(overview_all_df, add_gen_id, by="genotype.id")
overview_all_df$Selection[overview_all_df$genotype.id%in%candidate_genotypes] <- overview_all_df$Genotype[overview_all_df$genotype.id%in%candidate_genotypes]
overview_all_df$variable <- overview_all_df$Scale
overview_all_df$variable[overview_all_df$Scale=="Asym"] <- "Asym"
overview_all_df$variable[overview_all_df$Interaction=="avg_photothermal_14"] <- "G:P"
overview_all_df$variable[overview_all_df$Interaction=="avg_precipitation_14"] <- "G:Pre"

p <- subset(overview_all_df,Scale!="Intercept")

p[,mean_est:=mean(est.),by=variable]
# 
ggIdeal_coef <- ggplot(p, aes(x = Genotype, y = est., color = Selection)) +xlab("Breeding line")+ylab("Coefficient")+
  geom_point(size =0.5 ) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="grey") +  # Add error bars
  geom_point(data=subset(p, genotype.id%in%candidate_genotypes),size =1.5 )+ 
  geom_errorbar(data=subset(p, genotype.id%in%candidate_genotypes),aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="black") +  # Add error bars
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_blank(),text = element_text(size=8),axis.title.y = element_blank())+
  scale_color_manual(values =c(tol12qualitative[c(2,10,11)],"grey"))+
  geom_hline(aes(yintercept=mean_est),linetype="dashed")+
  facet_grid(variable~.,scales = "free",switch="both")

ggIdeal_coef
####



