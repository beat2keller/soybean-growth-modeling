# this file joins the weather with the soybean data for modelling. Creates model_data.csv
# libraries
library(data.table)
library(dplyr)

# load data
weathter_data = fread("data/weather_data_for_modelling.csv")
soybean_data = fread("data/soybean_data_for_modelling.csv")

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
# calculate new measures : still unclear

# commented out on 2.4 based on changes made in the original repo: 
#PhenoWeatherData <- PhenoWeatherData[,list(Sowing_to_Measure=mean(Sowing_to_Measure, na.rm=T), Measure_7=mean(Measure_7, na.rm=T), Measure_14=mean(Measure_14, na.rm=T), Measure_56=mean(Measure_56, na.rm=T), Measure_28=mean(Measure_28, na.rm=T)), by=.(Filename,genotype.name,genotype.id,Year,Location,year_site.UID,plot_number,plot.UID,plot_grouped,range,row,Date,value,variable,variable.1,WeatherVariable,WeatherCalcVar,date_of_sowing, platform)]
PhenoWeatherData_cast <- dcast.data.table(PhenoWeatherData, Filename+genotype.name+genotype.id+Year+Location+year_site.UID+plot_number+plot.UID+plot_grouped+range+row+Date+value+variable+variable.1+date_of_sowing+platform~WeatherVariable, value.var=c("Sowing_to_Measure","Measure_7","Measure_14","Measure_56","Measure_28")) # ~WeatherVariable+WeatherCalcVar


# make changes that were made in the original modelling file -----

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
# type conversiion
model_df$year          <- ordered(as.numeric(model_df$year))
model_df$genotype.id   <- as.factor(model_df$genotype.id)
model_df$plot_grouped   <- ordered(as.factor(model_df$plot_grouped))
model_df$date          <- as.Date(model_df$date)

# measure variables
model_df$avg_Temperature_56 <- (PhenoWeatherData_cast$Measure_56_Temperature)
model_df$avg_Temperature_28 <- (PhenoWeatherData_cast$Measure_28_Temperature)
model_df$avg_precipitation_56 <- (PhenoWeatherData_cast$Measure_56_Precipitation)
model_df$avg_precipitation_28 <- (PhenoWeatherData_cast$Measure_28_Precipitation)
model_df$avg_radiation_28 <- (PhenoWeatherData_cast$Measure_28_RadiationCap)
model_df$avg_photothermal_28<- (PhenoWeatherData_cast$Measure_28_PhotoThermalCap)
model_df$avg_vpd_28 <- (PhenoWeatherData_cast$Measure_28_VPD)
model_df$avg_humidity_28 <- (PhenoWeatherData_cast$Measure_28_Humidity)

model_df$avg_Temperature_14 <- (PhenoWeatherData_cast$Measure_14_Temperature)
model_df$avg_precipitation_14 <- (PhenoWeatherData_cast$Measure_14_Precipitation)
model_df$avg_radiation_14 <- (PhenoWeatherData_cast$Measure_14_RadiationCap)
model_df$avg_photothermal_14<- (PhenoWeatherData_cast$Measure_14_PhotoThermalCap)
model_df$avg_humidity_14 <- (PhenoWeatherData_cast$Measure_14_Humidity)
model_df$avg_vpd_14 <- (PhenoWeatherData_cast$Measure_14_VPD)


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
df_for_grouping <- df_for_grouping[order(df_for_grouping$year_site.UID,df_for_grouping$row, df_for_grouping$range),]
df_for_grouping <- df_for_grouping %>%
  mutate(plot_grouped_global = paste0(year_site.UID, "_", ceiling(row/ 6 ), "_", ceiling(range/ 2)))
print(paste0("Number of groups: ", length(unique(df_for_grouping$plot_grouped_global))))
df <- merge(df_for_grouping, model_df,by=c("range","row","year_site.UID"))

# save file -----

write.csv(df, "data/model_data.csv")
