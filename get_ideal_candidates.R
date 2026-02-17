library(nlme)



load("data/Growth_data.RData")
load("model/Growth_E.GxPTxP_Contr.Sum.RData")
load("model/Growth_E.GxPTxP.RData")

library(nlme)

ci <- data.frame(
  lower = intervals(Growth_E.GxPTxP_Contr.Sum, which="fixed")$fixed[,1],
  est   = intervals(Growth_E.GxPTxP_Contr.Sum, which="fixed")$fixed[,2],
  upper = intervals(Growth_E.GxPTxP_Contr.Sum, which="fixed")$fixed[,3]
)
ci$names    <- rownames(ci)
ci$interval <- ci$upper - ci$lower

geno_levels <- levels(droplevels(getData(Growth_E.GxPTxP_Contr.Sum)$genotype.id))
nG <- length(geno_levels)

# ---- extractor for scal.* terms (names contain genotype CODES like 10001)
get_by_genocode <- function(regex, vec, geno_levels) {
  hit <- grepl(regex, ci$names)
  nm  <- ci$names[hit]
  v   <- vec[hit]
  code <- sub(".*genotype\\.id", "", sub("(:.*)$", "", nm))   # "10001"
  out  <- rep(NA_real_, length(geno_levels))
  idx  <- match(code, geno_levels)
  out[idx[!is.na(idx)]] <- v[!is.na(idx)]
  out
}

# ---- extractor for Asym.* terms (names contain indices like genotype.id1)
get_by_index <- function(regex, vec, nG) {
  hit <- grepl(regex, ci$names)
  nm  <- ci$names[hit]
  v   <- vec[hit]
  id  <- as.integer(sub(".*genotype\\.id", "", nm))          # 1..(nG-1)
  out <- rep(NA_real_, nG)
  ok  <- !is.na(id) & id >= 1 & id <= (nG-1)
  out[id[ok]] <- v[ok]
  out[nG] <- -sum(out[1:(nG-1)], na.rm=TRUE)                 # implicit contr.sum level
  out
}

# ---- extract correctly
prec_intervals <- get_by_genocode("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$interval, geno_levels)
rad_intervals  <- get_by_genocode("^scal\\.genotype\\.id[0-9]+:avg_photothermal_14$",  ci$interval, geno_levels)
precs          <- get_by_genocode("^scal\\.genotype\\.id[0-9]+:avg_precipitation_14$", ci$est,      geno_levels)
rad            <- get_by_genocode("^scal\\.genotype\\.id[0-9]+:avg_photothermal_14$",  ci$est,      geno_levels)

asym_eff       <- get_by_index("^Asym\\.genotype\\.id[0-9]+$", ci$est, nG)

# ---- thresholds (computed on available values; avoids quantile NA errors)
q_prec_int <- quantile(prec_intervals[!is.na(prec_intervals)], 0.25)
q_rad_int  <- quantile(rad_intervals[!is.na(rad_intervals)],   0.25)
q_prec     <- quantile(abs(precs[!is.na(precs)]),              0.05)
q_rad      <- quantile(abs(rad[!is.na(rad)]),                  0.05)
q_asym     <- quantile(asym_eff[!is.na(asym_eff)],             0.75)

genotype_selection <- data.frame(
  genotype = geno_levels,
  prec_int = ifelse(prec_intervals < q_prec_int, 1, 0),
  rad_int  = ifelse(rad_intervals  < q_rad_int,  1, 0),
  prec     = ifelse(abs(precs)     < q_prec,     1, 0),
  rad      = ifelse(abs(rad)       < q_rad,      1, 0),
  asymp    = ifelse(asym_eff       > q_asym,     1, 0)
)

genotype_selection$sum <- rowSums(genotype_selection[,2:6], na.rm=FALSE)

max_sum <- max(genotype_selection$sum, na.rm=TRUE)
candidates <- genotype_selection$genotype[genotype_selection$sum > (max_sum - 1)]
candidates <- candidates[!is.na(candidates)]

cat("nG:", nG, "\n")
cat("Max score:", max_sum, "\n")
cat("N candidates:", length(candidates), "\n")
candidates




write.csv(data.frame(candidates),
          file = "model/candidates/candidate_genotypes.csv",
          row.names = FALSE)

# This way, you obtain a list of ideal candidates
# our ideal candidates are: 
candidate_genotypes <- as.character(read.csv("model/candidates/candidate_genotypes.csv")[,1])
candidate_genotypes

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
  
  ci <- data.frame(
    lower = intervals(model_candidates, which = "fixed")$fixed[, 1],
    est   = intervals(model_candidates, which = "fixed")$fixed[, 2],
    upper = intervals(model_candidates, which = "fixed")$fixed[, 3]
  )
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
candidate_genotypes <- as.character(read.csv("model/candidates/candidate_genotypes.csv")[,1])
candidate_genotypes


load("model/overview_df.RData") 
names(overview_df)[1] <- "genotype.id"
overview_melt <- melt.data.table(setDT(overview_df), id.vars = "genotype.id")
overview_melt <- subset(overview_melt, genotype.id%in%candidate_genotypes)

dt <- overview_melt[!grepl("interval",overview_melt$variable),]
dt[, c("Scale", "Level") := tstrsplit(variable, "_")]
dt$variable <- NULL
dt_cats <- dcast(dt, ...~Level)

load("model/Growth_E.GxPTxP.RData")
overview_all_df = intervals(Growth_E.GxPTxP,which = "fixed")$fixed
  
add_gen_id <- read.csv("data/ids_soybean_cleaned.csv")
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
overview_all_df$variable[overview_all_df$Interaction=="avg_photothermal_14"] <- "G:PT"
overview_all_df$variable[overview_all_df$Interaction=="avg_precipitation_14"] <- "G:P"

p <- subset(overview_all_df,Scale!="Intercept")
unique(subset(p, genotype.id%in%candidate_genotypes)$Genotype)
p[,mean_est:=mean(est.),by=variable]
levels(as.factor(p$Selection))

# 
ggIdeal_coef <- ggplot(p, aes(x = Genotype, y = est., color = Selection)) +xlab("Breeding line")+ylab("Coefficient")+
  geom_point(size =0.5 ) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="grey") +  # Add error bars
  geom_point(data=subset(p, genotype.id%in%candidate_genotypes),size =1.5 )+ 
  geom_errorbar(data=subset(p, genotype.id%in%candidate_genotypes),aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="black") +  # Add error bars
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_blank(),text = element_text(size=8),axis.title.y = element_blank())+
  scale_color_manual(values =c(tol12qualitative[c(5,7,10)],"grey"))+
  geom_hline(aes(yintercept=mean_est),linetype="dashed")+
  facet_grid(variable~.,scales = "free",switch="both")

ggIdeal_coef
####



