# this file joins the weather with the soybean data for modelling. Creates model_data.csv
# libraries
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")


library(data.table)
library(dplyr)

# load data
soybean_data = fread("data/soybean_data_for_modelling.csv")
#
weathter_data_modelling = fread("data/weather_data_for_modelling.csv")
weathter_data <- melt.data.table(weathter_data_modelling, id.vars=c("WeatherVariable","Location","Date","Year","Measure_7","Measure_14","Measure_28","Measure_56"),measure.vars = c("CumulativeDailyMean"),variable.name = "WeatherCalcVar",value.name="WeatherValue") ##,"SDoverTimeDailyMean"
# remove NA
weathter_data <- weathter_data[!is.na(weathter_data$WeatherValue),]


# only keep the necessary subset of the weather data
Weather_data_sub <- subset(weathter_data,as.character(Date)%in%as.character(c(soybean_data$Date,soybean_data$date_of_sowing)))
# include weather at sowing as varible
WeatherAtSowing <- unique(Weather_data_sub)
WeatherAtSowing$WeatherValueAtSowing <- WeatherAtSowing$WeatherValue


load("model/Growth7_E.GxRxPre.RData")
Growth7_E.GxRxPre

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
# weather_mean <- subset(weather, Date%in%growing_dates$Date&WeatherVariable%in%c("PhotothermalTime","PrecipitationCap"))
# weather_mean$Year <- format(as.Date(weather_mean$Date), "%Y")
# 
# weather_mean <- weather_mean[,mean(value),by=.(WeatherVariable,Year,Location)]
# weather_mean[order(weather_mean$V1,weather_mean$WeatherVariable,decreasing = T),]


weathter_data_sub <- subset(weathter_data, Date%in%growing_dates$Date&WeatherVariable%in%c("PhotothermalTime","PrecipitationCap","Temperature","Radiation"))

weathter_data_sub <- weathter_data_sub[order(weathter_data_sub$WeatherVariable,weathter_data_sub$Date),]
weathter_data_sub[, time_since_sowing:=1:nrow(.SD),by=.(WeatherVariable,Location,Year)]


weather_scenario_mean <- weathter_data_sub[,list(Measure_7=mean(Measure_7),Measure_14=mean(Measure_14), Measure_28=mean(Measure_28), Measure_56=mean(Measure_56), WeatherValue=mean(WeatherValue)),by=.(WeatherVariable,time_since_sowing,WeatherCalcVar)]
weather_scenario_mean$scenario <- "Average"
weather_scenario_mean$Location <- "Across"
weather_scenario_mean$Date <- NA
weather_scenario_mean$Year <- "Average"

weather_scenario_warmest <- subset(weathter_data_sub, Location=="Delley"&Year=="2015")
weather_scenario_warmest$scenario <- "Warm"

weather_scenario_coldest <- subset(weathter_data_sub, Location=="Delley"&Year=="2021") # really?
weather_scenario_coldest$scenario <- "Cold"

weather_scenario_rainy <- subset(weathter_data_sub, Location=="Eschikon"&Year=="2016") # make senes
weather_scenario_rainy$scenario <- "Rainy"

weather_scenario_dry <- subset(weathter_data_sub, Location=="Delley"&Year=="2022") # make senes
weather_scenario_dry$scenario <- "Dry"


weathter_data_scenarios <- rbind(weather_scenario_mean,weather_scenario_warmest, weather_scenario_coldest, weather_scenario_rainy, weather_scenario_dry)
# new_soybean <- merge(new_soybean, mean_vals, by = "genotype.id", all.x = TRUE)


new_soybean <- growing_dates
new_soybean$genotype.id <- 10056
new_soybean$platform <- "FIP"
new_soybean <- new_soybean[grepl("2015",new_soybean$Date),]
new_soybean$Date <- NULL

WeatherDataToPreodict <- merge.data.table(new_soybean, weathter_data_scenarios, by=c("time_since_sowing"), all.x = T, all.y = F, allow.cartesian=TRUE)
# WeatherDataToPreodict <- WeatherDataToPreodict[order(WeatherDataToPreodict$Date),]

WeatherDataToPreodict_cast <- dcast.data.table(WeatherDataToPreodict, scenario+time_since_sowing+genotype.id+platform~WeatherVariable, value.var=c("Measure_7","Measure_14","Measure_56","Measure_28")) # ~WeatherVariable+WeatherCalcVar


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
model_df$avg_photothermal_14<- (WeatherDataToPreodict_cast$Measure_14_PhotoThermal)
model_df$avg_humidity_14 <- (WeatherDataToPreodict_cast$Measure_14_Humidity)
model_df$avg_vpd_14 <- (WeatherDataToPreodict_cast$Measure_14_VPD)

# model_df$gdd_temperature <- (WeatherDataToPreodict_cast$Sowing_to_Measure_Temperature)/model_df$time_since_sowing # add growing degree day (gdd)

model_df

# --- Step 4. Run predictions using the loaded model ---
# (Assuming your nonlinear mixed-effects model Growth7_E.GxRxPre is already loaded)
model_df$fit <-  predict(Growth7_E.GxRxPre, newdata = model_df, level = 0)
# --- View the first few rows of the new prediction dataset ---
head(model_df)
model_df$Fit <- sin(model_df$fit)^2 
p <- model_df



require(ggplot2)

ggFit <- ggplot(data=p,aes(time_since_sowing, Fit, color=scenario, shape=scenario))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=2, alpha=0.5)+
  # geom_smooth(aes(Date, Fit,group=genotype.id),size=0.1)+
  facet_grid(.~genotype.id,scale="free",switch="both", labeller = label_parsed)
ggFit

