# this file joins the weather with the soybean data for modelling. Creates model_data.csv
weathter_data = read.csv("data/weather_data_for_modelling.csv")
soybean_data = read.csv("data/soybean_data_for_modelling.csv")


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
PhenoWeatherData <- data.table(PhenoWeatherData)
PhenoWeatherData <- PhenoWeatherData[,list(Sowing_to_Measure=mean(Sowing_to_Measure, na.rm=T), Measure_7=mean(Measure_7, na.rm=T), Measure_14=mean(Measure_14, na.rm=T), Measure_56=mean(Measure_56, na.rm=T), Measure_28=mean(Measure_28, na.rm=T)), by=.(Filename,genotype.name,genotype.id,Year,Location,year_site.UID,plot_number,plot.UID,plot_grouped,range,row,Date,value,variable,variable.1,WeatherVariable,WeatherCalcVar,date_of_sowing)]
PhenoWeatherData_cast <- dcast.data.table(PhenoWeatherData, Filename+genotype.name+genotype.id+Year+Location+year_site.UID+plot_number+plot.UID+plot_grouped+range+row+Date+value+variable+variable.1+date_of_sowing~WeatherVariable, value.var=c("Sowing_to_Measure","Measure_7","Measure_14","Measure_56","Measure_28")) # ~WeatherVariable+WeatherCalcVar


# create new variables 
PhenoWeatherData_cast$time_since_sowing =( as.numeric(as.Date(PhenoWeatherData_cast$Date )-as.Date( PhenoWeatherData_cast$date_of_sowing)))



write.csv(PhenoWeatherData_cast, "data/model_data.csv")
