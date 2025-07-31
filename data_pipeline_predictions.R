# this file joins the weather with the soybean data for modelling. Creates model_data.csv
# libraries
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")


library(data.table)
library(dplyr)
library(nlme)

# load data
soybean_data = fread("data/soybean_data_for_modelling.csv")
candidate_genotypes <- read.csv("model/candidates/candidate_genotypes.csv", 
                 colClasses = "character",
                 header = TRUE)[,1]

#
weathter_data_modelling = fread("data/weather_data_for_modelling.csv")
weathter_data <- melt.data.table(weathter_data_modelling, id.vars=c("WeatherVariable","Location","Date","Year","Measure_7","Measure_14","Measure_21"),measure.vars = c("CumulativeDailyMean"),variable.name = "WeatherCalcVar",value.name="WeatherValue") ##,"SDoverTimeDailyMean"
# remove NA
weathter_data <- weathter_data[!is.na(weathter_data$WeatherValue),]


# only keep the necessary subset of the weather data
Weather_data_sub <- subset(weathter_data,as.character(Date)%in%as.character(c(soybean_data$Date,soybean_data$date_of_sowing)))
# include weather at sowing as varible
WeatherAtSowing <- unique(Weather_data_sub)
WeatherAtSowing$WeatherValueAtSowing <- WeatherAtSowing$WeatherValue


load("model/Growth_E.GxPTxP.RData")
Growth_E.GxPTxP

coefs <- fixed.effects(Growth_E.GxPTxP)
coefs <- as.data.frame(coefs)
coefs$Model <-  "Growth_E.GxPTxP"

coefs$term <- rownames(coefs)

coefs$variable <- rownames(coefs)
coefs$variable[grepl("Asym",coefs$term)] <- "Asym"
coefs$variable[grepl("scal",coefs$term)] <- "scal"
coefs$variable[grepl("mid",coefs$term)] <- "mid"
coefs$variable[grepl(":avg_",coefs$term)] <- "Interaction"
coefs$variable[grepl(":avg_rad",coefs$term)] <- "InteractionRad"
coefs$variable[grepl(":avg_temp",coefs$term)] <- "InteractionTemp"
coefs$variable[grepl(":avg_hum",coefs$term)] <- "InteractionHum"
coefs$variable[grepl(":avg_prec",coefs$term)] <- "InteractionPrec"
coefs$variable[grepl(":avg_phot",coefs$term)] <- "InteractionPhot"

coefs <- setDT(coefs)[,c("ModelVar","genotype.id"):= tstrsplit(term, "genotype.id", fixed=TRUE)[1:2]]
coefs <- setDT(coefs)[,c("genotype.id","Interaction_term"):= tstrsplit(genotype.id, ":avg_", fixed=TRUE)[1:2]]
# coefs$coefs_abs <-  abs(coefs$coefs)
# coefs <- as.data.frame(coefs)
# coefs[order(coefs$variable,coefs$coefs_abs),]

coefs_max <- coefs[,list(max=max(coefs),max_genotype=genotype.id[coefs==max(coefs)],min=min(coefs),min_genotype=genotype.id[coefs==min(coefs)]),by=.(variable,Model)]
coefs_max <- coefs_max[grepl("Interaction",coefs_max$variable)]

coefs_abs <- coefs
coefs_abs$coefs <- abs(coefs_abs$coefs)
coefs_max_abs <- coefs_abs[grepl("Interaction",coefs_abs$variable),list(max=max(coefs),max_genotype=genotype.id[coefs==max(coefs)],min=min(coefs),min_genotype=genotype.id[coefs==min(coefs)]),by=.(variable,Model)]

selected_genotype <- c(coefs_max_abs$max_genotype,coefs_max_abs$min_genotype)
selected_genotype_prec <- coefs_max_abs[grepl("InteractionPrec",coefs_max_abs$variable)]
selected_genotype_prec <- c(selected_genotype_prec$max_genotype,selected_genotype_prec$min_genotype)
selected_genotype_prec <- selected_genotype_prec[1] # 10024 has coef very close to zero

library(data.table)

# --- Step 1. Specify prediction settings ---


growing_dates <- lapply(2015:2022, function(Year) {
  # Generate sequence of dates from May 1 to July 30 of the target year
  pred_dates <- seq(as.Date(paste0(Year, "-05-01")),
                    as.Date(paste0(Year, "-07-30")),
                    by = "day")
  pred_dates <- data.frame(Date=pred_dates, time_since_sowing=1:length(pred_dates))
  return(pred_dates)  # Ensure the sequence is returned
})
growing_dates <- rbindlist(growing_dates)


### identify exterm years from original weather data
# weather_mean <- subset(weather, Date%in%growing_dates$Date&WeatherVariable%in%c("PhotothermalProd","PrecipitationCap"))
# weather_mean$Year <- format(as.Date(weather_mean$Date), "%Y")
# 
# weather_mean <- weather_mean[,mean(value),by=.(WeatherVariable,Year,Location)]
# weather_mean[order(weather_mean$V1,weather_mean$WeatherVariable,decreasing = T),]


weathter_data_sub <- subset(weathter_data, Date%in%growing_dates$Date&WeatherVariable%in%c("PhotothermalProd","PrecipitationCap","Temperature","Radiation"))

weathter_data_sub <- weathter_data_sub[order(weathter_data_sub$WeatherVariable,weathter_data_sub$Date),]
weathter_data_sub[, time_since_sowing:=1:nrow(.SD),by=.(WeatherVariable,Location,Year)]


weather_scenario_mean <- weathter_data_sub[,list(Measure_7=mean(Measure_7),Measure_14=mean(Measure_14), Measure_21=mean(Measure_21), WeatherValue=mean(WeatherValue)),by=.(WeatherVariable,time_since_sowing,WeatherCalcVar)]
weather_scenario_mean$scenario <- "Average"
weather_scenario_mean$Location <- "Across"
weather_scenario_mean$Date <- NA
weather_scenario_mean$Year <- "Average"

scenario_warmest <- subset(weathter_data_sub, Location=="Delley"&Year=="2015")
weather_scenario_warmest <- weather_scenario_mean
weather_scenario_warmest$Measure_14[weather_scenario_warmest$WeatherVariable=="PhotothermalProd"] <- scenario_warmest$Measure_14[scenario_warmest$WeatherVariable=="PhotothermalProd"]
weather_scenario_warmest$scenario <- "Warm"


# scenario_hot <- weather_scenario_warmest
# scenario_hot$Measure_14 <- 1.1*scenario_hot$Measure_14 
# weather_scenario_hot <- weather_scenario_mean
# weather_scenario_hot$Measure_14[weather_scenario_hot$WeatherVariable=="PhotothermalProd"] <- scenario_hot$Measure_14[scenario_hot$WeatherVariable=="PhotothermalProd"]
# weather_scenario_hot$scenario <- "Hot (scenario)"

scenario_coldest <- subset(weathter_data_sub, Location=="Delley"&Year=="2021") # Eschikon was colder, but Delley had less radiation
weather_scenario_coldest <- weather_scenario_mean
weather_scenario_coldest$Measure_14[weather_scenario_coldest$WeatherVariable=="PhotothermalProd"] <- scenario_coldest$Measure_14[scenario_coldest$WeatherVariable=="PhotothermalProd"]
weather_scenario_coldest$scenario <- "Cold"

scenario_rainy  <- subset(weathter_data_sub, Location=="Eschikon"&Year=="2016") # 2021 second rainiest. Eschikon 2016 rained the most.
weather_scenario_rainy <- weather_scenario_mean
# weather_scenario_rainy$Measure_14[weather_scenario_rainy$WeatherVariable=="PrecipitationCap"] <- scenario_rainy$Measure_14[scenario_rainy$WeatherVariable=="PrecipitationCap"]
weather_scenario_rainy$Measure_14[weather_scenario_rainy$WeatherVariable=="PrecipitationCap"] <- scenario_rainy$Measure_21[scenario_rainy$WeatherVariable=="PrecipitationCap"]/21*14 # smooth weather data little
weather_scenario_rainy$scenario <- "Rainy"

scenario_dry <- subset(weathter_data_sub, Location=="Delley"&Year=="2022") # make sense
weather_scenario_dry <- weather_scenario_mean
weather_scenario_dry$Measure_14[weather_scenario_dry$WeatherVariable=="PrecipitationCap"] <- scenario_dry$Measure_14[scenario_dry$WeatherVariable=="PrecipitationCap"]
weather_scenario_dry$Measure_21[weather_scenario_dry$WeatherVariable=="PrecipitationCap"] <- scenario_dry$Measure_21[scenario_dry$WeatherVariable=="PrecipitationCap"]
weather_scenario_dry$scenario <- "Dry"


weathter_data_scenarios <- rbind(weather_scenario_mean,weather_scenario_warmest, weather_scenario_coldest, weather_scenario_rainy, weather_scenario_dry)
# new_soybean <- merge(new_soybean, mean_vals, by = "genotype.id", all.x = TRUE)



# selected_genotype <- selected_genotype[c(1,3)]

growing_dates_sub = growing_dates[seq(1, nrow(growing_dates), 3), ]

select_geno <- c(selected_genotype,candidate_genotypes)
select_geno <- select_geno[!duplicated(select_geno)]


new_soybean <- NULL
for (genotype in select_geno) {
    soybean_sub <- growing_dates_sub
  soybean_sub$genotype.id <- genotype
new_soybean <- rbind(new_soybean,soybean_sub)
}

new_soybean$platform <- "FIP"
new_soybean <- new_soybean[grepl("2015",new_soybean$Date),]
new_soybean$Date <- NULL

WeatherDataToPreodict <- merge.data.table(new_soybean, weathter_data_scenarios, by=c("time_since_sowing"), all.x = T, all.y = F, allow.cartesian=TRUE)
# WeatherDataToPreodict <- WeatherDataToPreodict[order(WeatherDataToPreodict$Date),]

WeatherDataToPreodict_cast <- dcast.data.table(WeatherDataToPreodict, scenario+time_since_sowing+genotype.id+platform~WeatherVariable, value.var=c("Measure_7","Measure_14","Measure_21")) # ~WeatherVariable+WeatherCalcVar


model_df = data.frame(matrix(nrow=nrow(WeatherDataToPreodict_cast), ncol=0))
model_df$value <- WeatherDataToPreodict_cast$value
model_df$time_since_sowing <- WeatherDataToPreodict_cast$time_since_sowing
model_df$genotype.id  <- WeatherDataToPreodict_cast$genotype.id
model_df$platform = WeatherDataToPreodict_cast$platform
model_df$scenario <- WeatherDataToPreodict_cast$scenario
model_df$genotype.id   <- as.factor(model_df$genotype.id)

# measure variables
model_df$avg_temperature_56 <- (WeatherDataToPreodict_cast$Measure_56_Temperature)
model_df$avg_temperature_28 <- (WeatherDataToPreodict_cast$Measure_28_Temperature)
model_df$avg_precipitation_56 <- (WeatherDataToPreodict_cast$Measure_56_Precipitation)
model_df$avg_precipitation_28 <- (WeatherDataToPreodict_cast$Measure_28_Precipitation)
model_df$avg_radiation_28 <- (WeatherDataToPreodict_cast$Measure_28_RadiationCap)
model_df$avg_photothermal_28<- (WeatherDataToPreodict_cast$Measure_28_PhotoThermalCap)
model_df$avg_vpd_28 <- (WeatherDataToPreodict_cast$Measure_28_VPD)
model_df$avg_humidity_28 <- (WeatherDataToPreodict_cast$Measure_28_Humidity)

model_df$avg_temperature_14 <- (WeatherDataToPreodict_cast$Measure_14_Temperature)
model_df$avg_precipitation_14 <- (WeatherDataToPreodict_cast$Measure_14_PrecipitationCap)
model_df$avg_precipitation_28 <- (WeatherDataToPreodict_cast$Measure_28_PrecipitationCap)
model_df$avg_radiation_14 <- (WeatherDataToPreodict_cast$Measure_14_Radiation)
model_df$avg_photothermal_14<- (WeatherDataToPreodict_cast$Measure_14_PhotothermalProd)
model_df$avg_humidity_14 <- (WeatherDataToPreodict_cast$Measure_14_Humidity)
model_df$avg_vpd_14 <- (WeatherDataToPreodict_cast$Measure_14_VPD)

# add genotype id

add_gen_id <- read.csv("data/ids_soybean_cleaned.csv")
add_gen_id <- add_gen_id[,c("id","name")]
add_gen_id$Genotype <- add_gen_id$name
add_gen_id$genotype.id <- as.character(add_gen_id$id)
add_gen_id$name <- NULL
add_gen_id$id <- NULL
model_df <- merge(model_df, add_gen_id, by="genotype.id")

# Load necessary packages
require(ggplot2)
require(data.table)
require(nlme)

# --- Step 4. Run predictions using the loaded model ---
model_df$fit <- predict(Growth_E.GxPTxP, newdata = model_df, level = 0)

# Bootstrapping to estimate confidence intervals
set.seed(123)  # For reproducibility
n_boot <- 1000  # Number of bootstrap samples
n_rows <- nrow(model_df)  # Number of observations
boot_preds <- matrix(NA, nrow = n_rows, ncol = n_boot)

# Extract residuals (Ensure they match dataset length)
residuals_fit <- residuals(Growth_E.GxPTxP, level = 0)  # Fixed effects residuals
if (length(residuals_fit) != n_rows) {
  residuals_fit <- residuals_fit[1:n_rows]  # Trim if too long (ensure same length)
}

# Bootstrapping loop
for (i in 1:n_boot) {
  # Resample residuals of correct length
  resampled_resid <- sample(residuals_fit, size = n_rows, replace = TRUE)  
  boot_preds[, i] <- model_df$fit + resampled_resid  # Add resampled residuals
}

# Compute standard error from bootstrapped predictions
se.fit <- apply(boot_preds, 1, sd)

# Compute confidence intervals
model_df$CI_upper <- sin(model_df$fit + 1.96 * se.fit)^2  # Upper 95% CI
model_df$CI_lower <- sin(model_df$fit - 1.96 * se.fit)^2  # Lower 95% CI

# Compute transformed fit
model_df$Fit <- sin(model_df$fit)^2 
model_df <- setDT(model_df)

# Subset based on selected genotypes
p <- model_df
p <- subset(p, genotype.id %in% as.integer(selected_genotype))
p$Scenario <- "Extrem environment: Photothermal product"
p$Scenario[p$genotype.id %in% selected_genotype_prec] <- "Extrem environment: Precipitation"


p1 <- subset(model_df, genotype.id %in% as.integer(candidate_genotypes))
p1$Scenario <- "Extrem environment: Photothermal product"
p2 <- subset(model_df, genotype.id %in% as.integer(candidate_genotypes))
p2$Scenario <- "Extrem environment: Precipitation"

p <- rbind(p,p1,p2)

p$GxE <- "stable"
p$GxE[p$genotype.id%in%coefs_max_abs$max_genotype] <- "unstable"
# p$GxE[p$genotype.id%in%coefs_max_abs$min_genotype] <- "unstable"
p <- subset(p, !genotype.id%in%coefs_max_abs$min_genotype[1]) ## fix me

p <- subset(p, !(Scenario == "Extrem environment: Photothermal product" & scenario %in% c("Rainy", "Dry")))
p <- subset(p, !(Scenario == "Extrem environment: Precipitation" & scenario %in% c("Cold", "Warm", "Hot (scenario)")))

# Define color palette
tol6qualitative = c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677", "#AA4499")

# Create the plot with confidence intervals
ggFit <- ggplot(data = p, aes(time_since_sowing, Fit, color = Genotype, shape = scenario)) +
  ylab("Canopy cover (%)") + xlab("Days after sowing (d)")+
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.key.size = unit(0.9, "lines"),
    legend.position = "top",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 9)
  ) +
  # geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = genotype.id), alpha = 0.2, color = NA) +  # Add CI
  geom_point(size = 2, alpha = 1) +
  scale_color_manual(name="Genotype",values = tol6qualitative) +
  scale_shape_manual("Environment \n (Scenario)" ,values = c(16,17,4,5,15)) +
  geom_line(size = 0.5, alpha = 0.5, aes(linetype = GxE)) +
  ylim(c(0,1)) +
  guides(color = guide_legend(nrow=2))+  guides(shape = guide_legend(nrow=2))+ guides(linetype = guide_legend(nrow=2))+
  facet_grid(. ~ Scenario, scale = "free", switch = "y")

ggFit



p <- melt.data.table(model_df, measure.vars = c( "avg_precipitation_14", "avg_photothermal_14"),variable.name = "Weather")
p$value <- p$value/14
p$value[p$Weather=="avg_precipitation_28"] <- p$value[p$Weather=="avg_precipitation_28"]/2

p$Scenario <- "Photothermal product"
p$Scenario[grepl("precip",p$Weather)] <- "Precipitation"
p <- subset(p, !(Scenario=="Photothermal product"& scenario%in%c("Rainy","Dry")))
p <- subset(p, !(Scenario=="Precipitation"& scenario%in%c("Cold","Warm","Hot (scenario)")))


ggWeather <- ggplot(data=p,aes(time_since_sowing, value,  shape=scenario))+ ylab("Environment")+xlab("Days after sowing (d)")+
  theme_bw()+theme(strip.placement = "outside", strip.background = element_blank(),
      legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
      panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_line(color="grey")+
  geom_point(size=2, alpha=1)+
  scale_color_manual(values = tol6qualitative)+
  scale_shape_manual(values = c(16,17,4,5,15)) +
  # geom_smooth(aes(Date, Fit,group=genotype.id),size=0.1)+
  # ylim(c(0,1))+
  facet_wrap(.~Scenario,scale="free",switch="both")
ggWeather





p <- model_df
p <- subset(p, genotype.id%in%as.integer(candidate_genotypes))

tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")

require(ggplot2)

ggplot(data=p,aes(time_since_sowing, Fit, color=genotype.id, shape=scenario))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_line(size=0.5, alpha=0.5)+
  geom_point(size=2, alpha=1)+
  scale_color_manual(values = tol6qualitative)+
  # scale_shape_manual(values =c(19,8))+
  # geom_smooth(size=0.1,fill=NA)+
  ylim(c(0,1))
  # facet_grid(.~Scenario,scale="free",switch="both")



############
tol3qualitative <- c("#4477AA", "#DDCC77", "#CC6677")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")



load("model/Growth_E.GxPTxP.RData")
overview_all_df = intervals(Growth_E.GxPTxP,which = "fixed")$fixed


######
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
overview_all_df$Selection[overview_all_df$genotype.id%in%select_geno] <- overview_all_df$Genotype[overview_all_df$genotype.id%in%select_geno]
overview_all_df$variable <- overview_all_df$Scale
overview_all_df$variable[overview_all_df$Scale=="Asym"] <- "Asym"
overview_all_df$variable[overview_all_df$Interaction=="avg_photothermal_14"] <- "G:P"
overview_all_df$variable[overview_all_df$Interaction=="avg_precipitation_14"] <- "G:Pre"

p <- subset(overview_all_df,Scale!="Intercept")

p[,mean_est:=mean(est.),by=variable]
# 
ggIdeal_coef <- ggplot(p, aes(x = Genotype, y = est., color = Selection)) +xlab("Genotype")+ylab("Coefficient")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"),
      strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"),
      legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_blank(),text = element_text(size=8))+
  geom_point(size =0.5 ) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="grey") +  # Add error bars
  geom_point(data=subset(p, genotype.id%in%select_geno),size =2 )+ 
  geom_errorbar(data=subset(p, genotype.id%in%select_geno),aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5), color="black") +  # Add error bars
  scale_color_manual(values =c(tol6qualitative,"grey"))+
  geom_hline(aes(yintercept=mean_est),linetype="dashed")+
  facet_grid(variable~.,scales = "free",switch="both")

ggIdeal_coef
####

require(cowplot)

first_row <- plot_grid(ggFit, ggWeather, ggIdeal_coef,  ncol = 1, rel_heights = c(1.2,0.5,0.7), labels = c("AUTO"))  #,vjust=0.5+
first_row

# ggsave("Predictions_scenarios_soybean.pdf", width = 180, height = 220, units = "mm", dpi = 100, first_row)
