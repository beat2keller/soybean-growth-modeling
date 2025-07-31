# this file joins the weather with the soybean data for modelling. Creates model_data.csv
# libraries
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")


library(data.table)
library(dplyr)

# load data
soybean_data = fread("data/soybean_data_for_modelling.csv")
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


PhenoWeatherData <- merge.data.table(soybean_data, Weather_data_sub, by=c("Location","Date","Year"), all.x = T, all.y = F, allow.cartesian=TRUE)
PhenoWeatherData <- merge.data.table(PhenoWeatherData, WeatherAtSowing[,c("Location","Date","Year","WeatherVariable","WeatherValueAtSowing")], by.x=c("WeatherVariable","Location","date_of_sowing","Year"), by.y=c("WeatherVariable","Location","Date","Year"), all.x = T, all.y = F, allow.cartesian=TRUE)

PhenoWeatherData <- PhenoWeatherData[!is.na(PhenoWeatherData$WeatherVariable),]
# sowing to measure variables
PhenoWeatherData$Sowing_to_Measure <- PhenoWeatherData$WeatherValue-PhenoWeatherData$WeatherValueAtSowing
PhenoWeatherData <- PhenoWeatherData[order(PhenoWeatherData$Date),]
PhenoWeatherData_cast <- dcast.data.table(PhenoWeatherData, Filename+genotype.name+genotype.id+Year+Location+year_site.UID+plot_number+plot.UID+plot_grouped+range+row+Date+value+variable+variable.1+date_of_sowing+platform+Period~WeatherVariable, value.var=c("Sowing_to_Measure","Measure_7","Measure_14","Measure_21")) # ~WeatherVariable+WeatherCalcVar


# create new variables 
PhenoWeatherData_cast$time_since_sowing =( as.numeric(as.Date(PhenoWeatherData_cast$Date )-as.Date( PhenoWeatherData_cast$date_of_sowing)))

# rename variables, and keep only a subset of the variables

model_df = data.frame(matrix(nrow=nrow(PhenoWeatherData_cast), ncol=0))
model_df$UID          <- PhenoWeatherData_cast$plot.UID
model_df$year         <- PhenoWeatherData_cast$Year
model_df$value <- PhenoWeatherData_cast$value
model_df$time_since_sowing <- PhenoWeatherData_cast$time_since_sowing
model_df$genotype.id  <- PhenoWeatherData_cast$genotype.id
model_df$date         <- PhenoWeatherData_cast$Date
model_df$plot_grouped <- PhenoWeatherData_cast$plot_grouped
model_df$year_site.UID <- PhenoWeatherData_cast$year_site.UID
model_df$range <- PhenoWeatherData_cast$range
model_df$row <- PhenoWeatherData_cast$row
model_df$Filename <- PhenoWeatherData_cast$Filename
model_df$Location <- PhenoWeatherData_cast$Location
model_df$platform = PhenoWeatherData_cast$platform
model_df$time = PhenoWeatherData_cast$time
model_df$period = PhenoWeatherData_cast$Period

# type conversiion
model_df$year          <- ordered(as.numeric(model_df$year))
model_df$genotype.id   <- as.factor(model_df$genotype.id)
model_df$plot_grouped   <- ordered(as.factor(model_df$plot_grouped))
model_df$date          <- as.Date(model_df$date)

# measure variables
model_df$avg_temperature_21 <- (PhenoWeatherData_cast$Measure_21_Temperature)
model_df$avg_precipitation_21 <- (PhenoWeatherData_cast$Measure_21_Precipitation)
model_df$avg_radiation_21 <- (PhenoWeatherData_cast$Measure_21_RadiationCap)
model_df$avg_photothermal_21<- (PhenoWeatherData_cast$Measure_21_PhotothermalProd)
model_df$avg_vpd_21 <- (PhenoWeatherData_cast$Measure_21_VPD)
model_df$avg_humidity_21 <- (PhenoWeatherData_cast$Measure_21_Humidity)

model_df$avg_temperature_14 <- (PhenoWeatherData_cast$Measure_14_Temperature)
model_df$avg_precipitation_14 <- (PhenoWeatherData_cast$Measure_14_PrecipitationCap)
model_df$avg_precipitation_21 <- (PhenoWeatherData_cast$Measure_21_PrecipitationCap)
model_df$avg_radiation_14 <- (PhenoWeatherData_cast$Measure_14_Radiation)
model_df$avg_photothermal_14<- (PhenoWeatherData_cast$Measure_14_PhotothermalProd)
model_df$avg_photothermalunit_14<- (PhenoWeatherData_cast$Measure_14_PhotothermalUnit)
model_df$avg_humidity_14 <- (PhenoWeatherData_cast$Measure_14_Humidity)
model_df$avg_vpd_14 <- (PhenoWeatherData_cast$Measure_14_VPD)

model_df$gdd_temperature <- (PhenoWeatherData_cast$Sowing_to_Measure_Temperature)/model_df$time_since_sowing # add growing degree day (gdd)

###### add row per plot information
model_df$Row_per_plot <- 3
model_df$Row_per_plot[model_df$year_site.UID%in%c("FPSB004","FPSB005","FPSB007")] <- 9
######

model_df <- model_df[!is.na(model_df$value),]
model_df <- model_df[!is.nan(model_df$value),]
model_df <- model_df[!is.na(model_df$genotype.id),]



#### new grouping ----------
df_for_grouping = model_df[,c("range","row","year_site.UID")]
df_for_grouping = unique(df_for_grouping)
setDT(df_for_grouping)[,row_min:=row-min(row)+1,by=.(year_site.UID )]
setDT(df_for_grouping)[,row_max:=max(row),by=.(year_site.UID )]
# df_for_grouping$row_min[df_for_grouping$row_max==13&df_for_grouping$year_site.UID%in%c("FPSB016","FPSB005")]
# hist(df_for_grouping$row_min)

df_for_grouping <- df_for_grouping[order(df_for_grouping$year_site.UID,df_for_grouping$row_min, df_for_grouping$range),]
df_for_grouping <- df_for_grouping %>%
  mutate(plot_grouped_global = paste0(year_site.UID, "_", ceiling(row_min/ 6 ), "_", ceiling(range/ 2)))
  # mutate(plot_grouped_global = paste0(year_site.UID, "_", round(c(1:max(row))/6+0.1, digits = 0)*6, "_", round(c(1:max(range))/6+0.1, digits = 0)*6))

setDT(df_for_grouping)[,N_per_group:=nrow(.SD),by=.(plot_grouped_global )]
df_for_grouping$plot_grouped_global[df_for_grouping$N_per_group<3] <- paste(df_for_grouping$year_site.UID[df_for_grouping$N_per_group<3],"_13_united" )

print(paste0("Number of groups: ", length(unique(df_for_grouping$plot_grouped_global))))
df <- merge(df_for_grouping, model_df,by=c("range","row","year_site.UID"))

# df[5550:5555,]
# save file -----
df <- setDT(df)
write.csv(df, "data/model_data.csv")
