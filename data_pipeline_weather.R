### This file uses the imputed weather dataframe to make a weather df that can be joined with the soybean data
# Packages ------
library(data.table)
weather_load <- data.table::fread("data/Weather_imputed.csv")

unique(weather_load$WeatherVariable)

# feature engineering ----
# Split the df into two for better manipulation 
df_w_radiation  = subset(weather_load,WeatherVariable=="Radiation")
df_w_radiation$value[df_w_radiation$value>800] <- 800 # impose a cap on radiation
df_w_radiation$WeatherVariable <- "RadiationCap" # rename variable
weather <- rbind(weather_load, df_w_radiation)

df_w_temperature <- subset(weather,WeatherVariable=="Temperature")
df_w_temperature$value <- df_w_temperature$value*df_w_radiation$value # make phototermal variable
df_w_temperature$WeatherVariable <- "PhotoThermalCap"
weather <- rbind(weather, df_w_temperature)


df_w_precipitation <- subset(weather,WeatherVariable=="Precipitation")

library(zoo)
# Calculating rolling mean with a window length of 20
df_w_precipitation[, RollingMean := frollmean(value, n = 12)]
# hist(df_w_precipitation$RollingMean)
df_w_precipitation$prop_to_2 <- df_w_precipitation$RollingMean/2
df_w_precipitation$prop_to_2[df_w_precipitation$prop_to_2<1] <- 1
df_w_precipitation$WeatherVariable <- "PrecipitationCap"
df_w_precipitation$value <- df_w_precipitation$value/df_w_precipitation$prop_to_2
hist(df_w_precipitation$value)
df_w_precipitation$prop_to_2 <- NULL
df_w_precipitation$RollingMean <- NULL

weather <- rbind(weather, df_w_precipitation)


# Renaming Lindau to Eschikon, necessary because of the name the weather api assigns

weather[weather$Location == "Lindau",]$Location = "Eschikon"


# we remove the na values. Based on the project from 2023 they only appeared in the winter and are thus not relevant
sum(is.na(weather$value))
weather = weather[!is.na(weather$value),]

# # Daytime / Nighttime variable ---------
#skipped for now as they are not used in the modelling


# format column -----
weather$Date <- as.Date(weather$DateTime)
weather_Eschikon = weather[weather$Location == "Eschikon",]
weather_Delley = weather[weather$Location == "Delley",]
weather = rbind(weather_Eschikon, weather_Delley)
# Group data and calculate mean and sd, use two dfs for different computation-----
Weather_data_Melt1 <- weather[, list(dailymean=mean(value, na.rm = T),dailySD=sd(value, na.rm = T)), by=.(Date, Location, WeatherVariable) ] #, Imputed
# Weather_data_Melt2 <- subset(weather,WeatherVariable=="Precipitation")[, list(dailymean=sum(value, na.rm = T),dailySD=sd(value, na.rm = T)), by=.(Date, Location, WeatherVariable) ] #, Imputedum(value, na.rm = T),dailySD=sd(value, na.rm = T)), by=.(Date, Location, WeatherVariable) ] #, Imputed



# define measure variables (toDo: What they mean):

Weather_data_cumulative <- Weather_data_Melt1
Weather_data_cumulative$Year <- format(as.Date(Weather_data_cumulative$Date, format="%Y-%m-%d"),"%Y")
Weather_data_cumulative[,CumulativeDailyMean:=cumsum(dailymean), by=.(Location, WeatherVariable, Year)]
Weather_data_cumulative[,cum_value:=cumsum(dailymean), by=.(Location, WeatherVariable, Year)]
Weather_data_cumulative[,Measure_7:=cum_value - shift(cum_value, fill = first(cum_value),n=7), by=.(Year,WeatherVariable,Location) ]
Weather_data_cumulative[,Measure_14:=cum_value - shift(cum_value, fill = first(cum_value),n=14), by=.(Year,WeatherVariable,Location)  ]
Weather_data_cumulative[,Measure_28:=cum_value - shift(cum_value, fill = first(cum_value),n=28), by=.(Year,WeatherVariable,Location)  ]
Weather_data_cumulative[,Measure_56:=cum_value - shift(cum_value, fill = first(cum_value),n=56), by=.(Year,WeatherVariable,Location)  ]
Weather_data_cumulative$cum_value <- NULL


# Weather_data_Melt_cumulative <- melt.data.table(Weather_data_cumulative, id.vars=c("WeatherVariable","Location","Date","Year","Measure_7","Measure_14","Measure_28","Measure_56"),measure.vars = c("CumulativeDailyMean"),variable.name = "WeatherCalcVar",value.name="WeatherValue") ##,"SDoverTimeDailyMean"
# # remove NA
# Weather_data_Melt_cumulative <- Weather_data_Melt_cumulative[!is.na(Weather_data_Melt_cumulative$WeatherValue),]
# #print(nrow(Weather_data_Melt_cumulative))
# 

# save df -------
write.csv(Weather_data_cumulative, "data/weather_data_for_modelling.csv")

