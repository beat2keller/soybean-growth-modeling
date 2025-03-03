setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

#' ---
#' title: "Initial weather analysis"
#' output: html_notebook
#' editor_options: 
#'   markdown: 
#'     wrap: 72
#' ---
#' 
#' ### **Load libraries**
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(stringr)
library(readr)
library(plyr)
library(dplyr)
# library(useful)
library(utils)
library(data.table)
library(plantecophys)
# library(gstat)
# library(spdep)
# library(sp)
library(tidyr)

library(zoo)

#' 
#' Load data from all stations from original Agrometeo files.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# # https://gitlab.ethz.ch/ftschurr/fip_weather
# 
# library(rstudioapi)
# wd_suggestion <- paste(unlist( strsplit(getSourceEditorContext()$path ,split="/"))[1:(length( unlist(strsplit(getSourceEditorContext()$path ,split="/")))-2)],collapse="/")
# # wd_suggestion <- "~/Agrometeo/fip_weather"
# fnames = list.files(path=wd_suggestion,pattern="agrometeo_",recursive=T)
# fnames<-fnames[ grepl("raw_data",fnames) ]
# fnames
# 
# lapply(c(175,103,110,126,208),function(x) clean_weather_data(fnames, start_date_data, end_date_data, station_id=x))
# 
# lapply(c(40,63,64,55),function(x) clean_weather_data(fnames, start_date_data, end_date_data, station_id=x))
# 



Location <- c("Delley","Lindau", "Wuelflingen", "Seegraeben", "Staefa", "Otelfingen", "Praz", "Gorgier", "Dudingen")
stations <- data.frame(DELLEY=40, LINDAU =175, WUELFLINGEN=110, SEEGRAEBEN= 103,  STAEFA=126,OTELFINGEN=208, PRAZ=63, GORGIER=64, DUDINGEN=55) 
# add variables manually here from agrometeo
Latitude <- c(46.9168306, 47.4459100, 47.5148711, 47.3465904, 47.2362977, 47.4620723, 46.9533858, 46.9069761, 46.8276018)
Longitude <- c(6.9676783, 8.6803400, 8.7022257, 8.7723184, 8.7451843, 8.4029133, 7.0934319, 6.7916711, 7.2024677)

stations <- cbind(t(stations),Location,Latitude,Longitude)
stations <- as.data.frame(stations)
names(stations)[1] <- "ID"
# Combine to a data frame
# spatial_data <- data.frame(locations, latitude, longitude)
# colnames(spatial_data) <- c("Location", "Latitude", "Longitude")
# rm(latitude, longitude)
spatial_data <- stations
# Change file location to Raw_Agrometeo to avoid double-importing
curr_dir <- getwd()
data_dir <- file.path(curr_dir, "data", "Agrometeo")
setwd(data_dir)

pattern_name <- "Weather_data_raw_"
  fnames = list.files(pattern=pattern_name, recursive=T)
  print(fnames)
  df = ldply(fnames, function(filename) {
    dum = fread(filename,header=T, sep="," , na.strings=c("NA","","x"))
    #If you want to add the filename as well on the column
    # df$filename <- filename
    return(dum) })
 
  #df$Latitude <- spatial_data$latitude[spatial_data$locations == x]
  #df$Longitude <- spatial_data$longitude[spatial_data$locations == x]
  # assign(paste("CHE_Weather", loc, sep = ""), df)
  # rm(loc_low, fnames, pattern_name, df, loc)
  names(df)[names(df) == "Location"] <- "ID"
  
CHE_Weather <- merge(spatial_data, df, by="ID" )
# Add empty row to location if windspeed variable is not available
# Add names of data frames into a vector to later use to rbind them

# weather_frames <- vector(length = length(locations))
# i <- 1
# for (loc in locations) {
#   temp_df_name <- paste("CHE_Weather", loc, sep = "")
#   temp_df <- get(temp_df_name)
#   
#   # Add empty row to location if variable is not available
#   if(!any(grepl("Windgeschwindigkeit mittel \\(m/sec\\)", names(temp_df)))) {
#     # Add an empty column with the desired name
#     temp_df <- temp_df %>%
#       mutate("Windgeschwindigkeit mittel (m/sec)" = NA)
#   }
  
#   # Shorten names
#   loc_cap <- toupper(loc)
#   names(temp_df) <- gsub(paste(loc_cap, "- "), "",names(temp_df))
#   
#   assign(paste("CHE_Weather", loc, sep = ""), temp_df)
#   weather_frames[i] <- paste("CHE_Weather", loc, sep = "")
#   i = i+1
#   #rm(temp_df, temp_df_name, loc, loc_cap)
# }

# Group data frames
# CHE_Weather <- do.call(rbind, lapply(weather_frames, get))
CHE_Weather <- as.data.table(CHE_Weather)

CHE_Weather$DateTime <- as.POSIXct(CHE_Weather$date, format = "%Y-%m-%d %H:%M", tz = "UTC")

CHE_Weather[c(1:3,1000,100000),c("date","DateTime")]

CHE_Weather$DateTime[duplicated(paste(CHE_Weather$DateTime,CHE_Weather$variable,CHE_Weather$ID))] <- CHE_Weather$DateTime[duplicated(paste(CHE_Weather$DateTime,CHE_Weather$variable,CHE_Weather$ID))]+60
CHE_Weather <- dcast(CHE_Weather, ...~variable)

CHE_Weather <- CHE_Weather[order(CHE_Weather$DateTime),]
CHE_Weather[c(1:50),c("date","DateTime")]


names(CHE_Weather)[grepl("RH_mean",names(CHE_Weather))] <- c("Humidity")
names(CHE_Weather)[grepl("pr_mom",names(CHE_Weather))] <- c("Precipitation")
names(CHE_Weather)[grepl("5cm",names(CHE_Weather))] <- c("AboveGroundTemperature")
names(CHE_Weather)[grepl("temp_2m_mean",names(CHE_Weather))] <- c("Temperature")
# names(CHE_Weather)[grepl("10 cm",names(CHE_Weather))] <- c("BelowGroundTemperature")
names(CHE_Weather)[grepl("global_radiation",names(CHE_Weather))] <- c("Radiation")
names(CHE_Weather)[grepl("Windgeschwindigkeit",names(CHE_Weather))] <- c("Windspeed")
names(CHE_Weather)[grepl("solar_energie",names(CHE_Weather))] <- c("Solarenergy")

##############################
# Add empty row where Lindau / Delley do not have any data
##############################

# Create a vector of all the hours between the date range of interest
all_hours_Lindau <- seq(as.POSIXct("2015-01-01 00:00:00"), as.POSIXct("2022-12-31 23:00:00"), by = "hour")
all_hours_Delley <- seq(as.POSIXct("2018-01-01 00:00:00"), as.POSIXct("2020-12-31 23:00:00"), by = "hour")

# Filter the "Lindau" and "Delley" data for the date range of interest
Lindau_data <- subset(CHE_Weather, Location == "Lindau" & DateTime >= as.POSIXct("2015-01-01 00:00:00") & DateTime <= as.POSIXct("2022-12-31 23:00:00"))
Delley_data <- subset(CHE_Weather, Location == "Delley" & DateTime >= as.POSIXct("2018-01-01 00:00:00") & DateTime <= as.POSIXct("2020-12-31 23:00:00"))

# Use the `table()` function to count the number of observations for each hour in the "Lindau" and "Delley" data
Lindau_count <- table(format(Lindau_data$DateTime, "%Y-%m-%d %H:%M"))
Delley_count <- table(format(Delley_data$DateTime, "%Y-%m-%d %H:%M"))

# Create an empty row with the same number of columns as "CHE_Weather"
empty_row <- data.frame(matrix(ncol = ncol(CHE_Weather), nrow = 1))
colnames(empty_row) <- colnames(CHE_Weather)

# Loop over missing hours and add an empty row to the "CHE_Weather" dataframe
for (hour in setdiff(format(all_hours_Lindau, "%Y-%m-%d %H:%M"), names(Lindau_count))) {
  empty_row$Location <- "Lindau"
  empty_row$ID <- 175
  empty_row$DateTime <- as.POSIXct(hour, format = "%Y-%m-%d %H:%M")
  CHE_Weather <- rbind(CHE_Weather, empty_row)
}

for (hour in setdiff(format(all_hours_Delley, "%Y-%m-%d %H:%M"), names(Delley_count))) {
  empty_row$Location <- "Delley"
  empty_row$ID <- 40
  empty_row$DateTime <- as.POSIXct(hour, format = "%Y-%m-%d %H:%M")
  CHE_Weather <- rbind(CHE_Weather, empty_row)
}

CHE_Weather$DateTime[duplicated(paste(CHE_Weather$DateTime,CHE_Weather$variable,CHE_Weather$Location))] 
CHE_Weather$DateTime[duplicated(paste(CHE_Weather$DateTime,CHE_Weather$variable,CHE_Weather$ID))] 

CHE_Weather <- CHE_Weather[!duplicated(paste(CHE_Weather$DateTime,CHE_Weather$variable,CHE_Weather$ID)),] # fix me

# Sort the "CHE_Weather" dataframe by "DateTime" and "Location"
CHE_Weather <- CHE_Weather[order(CHE_Weather$DateTime, CHE_Weather$Location), ]

CHE_Weather <- data.frame(CHE_Weather)

# Add spatial data
# CHE_Weather <- merge(CHE_Weather, spatial_data, by = "Location")
#' 
names(CHE_Weather)
# Change order of columns
# CHE_Weather <- CHE_Weather %>%
  # select(Location, Temperature, Humidity, Precipitation, Radiation, Solarenergy, BelowGroundTemperature, AboveGroundTemperature, Windspeed, DateTime, Latitude, Longitude) #Removed Date, Hour, 

#' 
#' Organize DF in wide format
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Keep only 2015-2022
start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2022-12-31 23:59:59", tz = "UTC")
CHE_Weather <- CHE_Weather[CHE_Weather$DateTime >= start_date & CHE_Weather$DateTime <= end_date, ]

CHE_Weather$Humidity <- as.numeric(CHE_Weather$Humidity)
CHE_Weather$Radiation <- as.numeric(CHE_Weather$Radiation)

# p <- subset(CHE_Weather, DateTime> "2021-12-31 23:59:59")
unique(CHE_Weather$Location)

######
WeatherVariables <- c("Temperature", "Humidity", "Precipitation", "Radiation", "Solarenergy") # windspeed not in all locations


CHE_Weather_melt <- melt.data.table(setDT(CHE_Weather), measure.vars = WeatherVariables, variable.name = "WeatherVariable")
CHE_Weather_melt$Outlier <- "No"
CHE_Weather_melt$Outlier[CHE_Weather_melt$value< -20] <- "Yes"
CHE_Weather_melt$Outlier[CHE_Weather_melt$value< 10&CHE_Weather_melt$WeatherVariable=="Humidity"] <- "Yes"


###########
remove_outliers <- function(x,IQR_times, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.025,0.5, .975), na.rm = na.rm, ...)
  H <- IQR_times #* IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[2] - H* (qnt[2]-qnt[1]) )] <- "Yes"
  y[x > (qnt[2] +  H* (qnt[3]-qnt[2]) )] <- "Yes"
  n_removed <- length(y[y=="Yes"])
  y[y!="Yes"] <- "No"
  print(paste("removed",n_removed))
  
  return(y)
}
########

# CHE_Weather_melt[CHE_Weather_melt$WeatherVariable!="Precipitation"&CHE_Weather_melt$Outlier!="Yes",] <- CHE_Weather_melt[CHE_Weather_melt$WeatherVariable!="Precipitation"&CHE_Weather_melt$Outlier!="Yes",][,Outlier:=remove_outliers(value,1.5),by=.(Location, WeatherVariable) ]
CHE_Weather_melt[CHE_Weather_melt$WeatherVariable!="Precipitation", roll_sd := rollapply(value, width=24,sd, fill=NA, align = "right"), by = .(Location, WeatherVariable)]
p <- CHE_Weather_melt$roll_sd[CHE_Weather_melt$roll_sd==0]
length(p[!is.na(p)])
CHE_Weather_melt$Outlier[CHE_Weather_melt$roll_sd==0] <- "Yes"

p <- CHE_Weather_melt
p$Year <-  as.factor(format(p$DateTime,"%Y"))
unique(p$Year )
ggplot()+ geom_boxplot(data=subset(p, Outlier=="No"), aes(x=Year, y=value, color=Year))+
  geom_point(data=subset(p, Outlier=="Yes"),aes(x=Year, y=value), color="grey")+
  facet_grid(WeatherVariable~Location, scales = "free", switch="both", labeller = label_parsed)

p$Date <-  as.Date(p$DateTime)


ggplot(p)+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  # geom_errorbar(data=p, aes(x=Date, y=dailymean, ymin=dailymean-dailySD, ymax=dailymean+dailySD),color="grey",width=1)+
  geom_point(data=subset(p, Outlier=="Yes"), aes(x=Date, y=value, color=Year, shape=Outlier),size=1.25)+
  # geom_area(aes(x=Date, y=value, fill=variable), alpha=0.4, position = "identity")+
  # scale_color_manual(values = tol7qualitative)+
  # scale_fill_manual(values = tol7qualitative)+
  scale_shape_manual(values = c(4,16))+
  facet_grid(WeatherVariable~Location, scales = "free", switch="both", labeller = label_parsed)+
  # geom_vline(data=SelectedDates, aes(xintercept = Date), linetype="dashed")+
  # geom_hline(aes(yintercept = FinalEe))+
  # ylim(aes(c(0.2, maxY)))+
  # scale_x_datetime(date_labels="%m", date_breaks = "6 hours")+
  guides(color = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))

ggWeather <- ggplot(p)+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  # geom_errorbar(data=p, aes(x=Date, y=dailymean, ymin=dailymean-dailySD, ymax=dailymean+dailySD),color="grey",width=1)+
  geom_point(data=subset(p, Outlier=="No"), aes(x=Date, y=value, color=Year, shape=Outlier),size=0.5)+
  # geom_area(aes(x=Date, y=value, fill=variable), alpha=0.4, position = "identity")+
  # scale_color_manual(values = tol7qualitative)+
  # scale_fill_manual(values = tol7qualitative)+
  # scale_shape_manual(values = c(4,16))+
  facet_grid(WeatherVariable~Location, scales = "free", switch="both", labeller = label_parsed)+
  # geom_vline(data=SelectedDates, aes(xintercept = Date), linetype="dashed")+
  # geom_hline(aes(yintercept = FinalEe))+
  # ylim(aes(c(0.2, maxY)))+
  # scale_x_datetime(date_labels="%m", date_breaks = "6 hours")+
  guides(color = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))

ggsave(paste0("WeatherOverview.png"),  width = 190, height = 250, units = "mm", dpi = 100,ggWeather)


CHE_Weather_melt$DateTime[duplicated(paste(CHE_Weather_melt$DateTime,CHE_Weather_melt$variable,CHE_Weather_melt$ID,CHE_Weather_melt$WeatherVariable))]

CHE_Weather_cleaned <- subset(CHE_Weather_melt)
CHE_Weather_cleaned$value[CHE_Weather_cleaned$Outlier=="Yes"] <- NA
CHE_Weather_cleaned$Outlier <- NULL
CHE_Weather_cleaned$roll_sd <- NULL

# CHE_Weather_cleaned$DateTime[duplicated(paste(CHE_Weather_cleaned$Location,CHE_Weather_cleaned$WeatherVariable, CHE_Weather_cleaned$DateTime))] <- CHE_Weather_cleaned$DateTime[duplicated(paste(CHE_Weather_cleaned$Location,CHE_Weather_cleaned$WeatherVariable, CHE_Weather_cleaned$DateTime))]-60*30
# CHE_Weather_cleaned$DateTime[duplicated(paste(CHE_Weather_cleaned$DateTime,CHE_Weather_cleaned$variable,CHE_Weather_cleaned$ID,CHE_Weather_cleaned$WeatherVariable))]


CHE_Weather_cleaned <- dcast.data.table(CHE_Weather_cleaned, ...~WeatherVariable, value.var = "value")
#' Add PhotothermalTime and VPD variables where possible to data from other
#' stations
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Add VPD

# Identify rows with no missing values
complete_rows <- complete.cases(CHE_Weather_cleaned$Humidity, CHE_Weather_cleaned$Temperature)

# Apply function only to non-NA rows
CHE_Weather_cleaned$VPD[complete_rows] <- RHtoVPD(CHE_Weather_cleaned$Humidity[complete_rows], CHE_Weather_cleaned$Temperature[complete_rows], Pa = 101)

## Add PhotothermalTime

# Identify rows with no missing values
complete_rows <- complete.cases(CHE_Weather_cleaned$Radiation, CHE_Weather_cleaned$Temperature)

# Apply function only to non-NA rows
CHE_Weather_cleaned$PhotothermalProd[complete_rows] <- CHE_Weather_cleaned$Temperature[complete_rows] * CHE_Weather_cleaned$Radiation[complete_rows]
CHE_Weather_cleaned$PhotothermalUnit[complete_rows] <- (CHE_Weather_cleaned$Temperature-6)[complete_rows] * CHE_Weather_cleaned$Radiation[complete_rows] # minus 6°C base temperature

# CHE_Weather_cleaned$PhotothermalRatio[complete_rows] <- CHE_Weather_cleaned$Radiation[complete_rows]/CHE_Weather_cleaned$Temperature[complete_rows] 
# CHE_Weather_cleaned$PhotothermalRatio[is.infinite(CHE_Weather_cleaned$PhotothermalRatio)] <- NA
CHE_Weather_cleaned$ID <- NULL
CHE_Weather_cleaned$date <- NULL


# Reshape data
CHE_Weather_wide <- reshape(CHE_Weather_cleaned, direction = "wide", idvar = "DateTime", timevar = "Location")

#' 
#' ### Impute data
#' 
#' Linear regression to impute weather variables
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
weather_var <- setdiff(colnames(CHE_Weather_cleaned), c("Hour", "Date", "DateTime", "Location", "Latitude","Longitude","Windspeed","ID","date")) # Windspeed excluded as there are no measurements in any alternative locations

# p <- CHE_Weather_wide[is.na(CHE_Weather_wide$ID.Delley),]

CHE_Weather_wide_imp <- CHE_Weather_wide

for (var in weather_var) {
  for (loc in Location) {
    temp_name <- paste(var, ".", loc, sep = "")
    assign(paste("varname_", loc, sep = ""), temp_name)
  }
  varname_Lindau_imp <- paste(var, ".Lindau", ".imp", sep = "")
  varname_Delley_imp <- paste(var, ".Delley", ".imp", sep = "")
  #varname_e <- paste(var, ".Lindau", sep = "")
  #varname_s <- paste(var, ".Seegraeben", sep = "")
  #varname_w <- paste(var, ".Wuelflingen", sep = "")
  #varname_st <- paste(var, ".Staefa", sep = "")
  #varname_o <- paste(var, ".Otelfingen", sep = "")
  
  # !!Specify regression here!!
  # Lindau
  formula_Lindau <- paste (varname_Lindau, "~", varname_Wuelflingen, "+", varname_Seegraeben, "+", varname_Staefa, "+", varname_Otelfingen)
  model_Lindau <- lm(formula_Lindau, CHE_Weather_wide_imp, na.action=na.exclude)
  CHE_Weather_wide_imp[[varname_Lindau_imp]] <- predict(model_Lindau, newdata = CHE_Weather_wide_imp)
  
  # Delley
  formula_Delley <- paste (varname_Delley, "~", varname_Dudingen, "+", varname_Gorgier, "+", varname_Praz)
  model_Delley <- lm(formula_Delley, CHE_Weather_wide_imp, na.action=na.exclude)
  CHE_Weather_wide_imp[[varname_Delley_imp]] <- predict(model_Delley, newdata = CHE_Weather_wide_imp)
  
  # Find variable names that start with "varname_"
  var_names <- ls()
  to_remove <- grepl("^varname_", var_names)

  # Remove temporary variables that start with "varname_", and others
  rm(var, formula_Delley, formula_Lindau, list = var_names[to_remove])
}


#' 
#' Some investigative plots
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Precipitation

CHE_Weather_wide_imp <- CHE_Weather_wide_imp %>% arrange(DateTime)

plot(CHE_Weather_wide_imp$DateTime, CHE_Weather_wide_imp$Precipitation.Lindau.imp)
points(CHE_Weather_wide_imp$DateTime, CHE_Weather_wide_imp$Precipitation.Lindau, col = "red")

plot(CHE_Weather_wide_imp$DateTime[CHE_Weather_wide_imp$DateTime >= "2018-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2018-07-06 23:00:00"], CHE_Weather_wide_imp$Precipitation.Lindau[CHE_Weather_wide_imp$DateTime >= "2018-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2018-07-06 23:00:00"], xlab = "Date", ylab = "Precipitation Lindau", type = "l", ylim = c(-3, 15))

lines(CHE_Weather_wide_imp$DateTime[CHE_Weather_wide_imp$DateTime >= "2018-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2018-07-06 23:00:00"], CHE_Weather_wide_imp$Precipitation.Lindau.imp[CHE_Weather_wide_imp$DateTime >= "2018-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2018-07-06 23:00:00"], col = "red")

legend("topright", legend = c("True values", "Imputed values"), col = c("black", "red"), lty = 1)

plot(CHE_Weather_wide_imp$DateTime[CHE_Weather_wide_imp$DateTime >= "2019-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2019-07-06 23:00:00"], CHE_Weather_wide_imp$Precipitation.Lindau[CHE_Weather_wide_imp$DateTime >= "2019-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2019-07-06 23:00:00"], xlab = "Date", ylab = "Precipitation Lindau", type = "l", ylim = c(-3, 15))
lines(CHE_Weather_wide_imp$DateTime[CHE_Weather_wide_imp$DateTime >= "2019-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2019-07-06 23:00:00"], CHE_Weather_wide_imp$Precipitation.Lindau.imp[CHE_Weather_wide_imp$DateTime >= "2019-06-30 00:00:00" & CHE_Weather_wide_imp$DateTime <= "2019-07-06 23:00:00"], col = "red")

legend("topright", legend = c("True values", "Imputed values"), col = c("black", "red"), lty = 1)

#' 
#' ### Saving data frame to CSV
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Gather data

CHE_Weather_imp_melt <- melt.data.table(CHE_Weather_wide_imp, id.vars=c("DateTime"))
CHE_Weather_imp_melt[, c("WeatherVariable", "Location","Predicted") := tstrsplit(variable, ".", fixed=TRUE)]
CHE_Weather_imp_melt$variable <- NULL
CHE_Weather_imp_melt$Predicted[is.na(CHE_Weather_imp_melt$Predicted)] <- "Measured"
CHE_Weather_imp_melt <- dcast.data.table(CHE_Weather_imp_melt, ...~Predicted)
CHE_Weather_imp_melt$value <- CHE_Weather_imp_melt$Measured
CHE_Weather_imp_melt$Imputed <- "Measured"
CHE_Weather_imp_melt$Imputed[is.na(CHE_Weather_imp_melt$value)] <- "Imputed"
CHE_Weather_imp_melt$value[is.na(CHE_Weather_imp_melt$value)] <- CHE_Weather_imp_melt$imp[is.na(CHE_Weather_imp_melt$value)] 

Long_lat <- subset(CHE_Weather_imp_melt, WeatherVariable%in%c("Latitude","Longitude"))
Long_lat <- unique(Long_lat[,2:4])
Long_lat <- na.omit(Long_lat)
Long_lat <- dcast.data.table(Long_lat, Location~WeatherVariable, value.var = "Measured")


Weather_imputed <-  subset(CHE_Weather_imp_melt, !WeatherVariable%in%c("Latitude","Longitude")) 
Weather_imputed <- merge(Weather_imputed, Long_lat, by="Location")
Weather_imputed$value <- as.numeric(Weather_imputed$value)

setwd(paste0(curr_dir,"/data"))
head(Weather_imputed)
Weather_imputed$imp <- NULL
Weather_imputed$Measured <- NULL
Weather_imputed[,nrow(.SD),by=.(WeatherVariable,Location)]

# write.csv(subset(Weather_imputed,Location%in%c("Lindau","Delley")), "Weather_imputed.csv", quote=F, row.names=F)


p <- subset(Weather_imputed, Imputed=="Imputed")
p <- p[!is.na(p$value),]
p
#####
# Weather_imputed <- fread("./data/weather/Weather_imputed.csv")
p <- subset(Weather_imputed, WeatherVariable%in%c("Temperature","Radiation","PhotothermalProd")&Location%in%c("Lindau","Delley"))
p$Year <-  as.factor(format(p$DateTime,"%Y"))
p$Date <-  as.Date(p$DateTime)




ggplot(p)+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  # geom_errorbar(data=p, aes(x=Date, y=dailymean, ymin=dailymean-dailySD, ymax=dailymean+dailySD),color="grey",width=1)+
  geom_point(data=p, aes(x=Date, y=value, color=Year, shape=Imputed),size=1.25)+
  # geom_area(aes(x=Date, y=value, fill=variable), alpha=0.4, position = "identity")+
  # scale_color_manual(values = tol7qualitative)+
  # scale_fill_manual(values = tol7qualitative)+
  scale_shape_manual(values = c(4,16))+
  facet_grid(WeatherVariable~Location+Imputed, scales = "free", switch="both", labeller = label_parsed)+
  # geom_vline(data=SelectedDates, aes(xintercept = Date), linetype="dashed")+
  # geom_hline(aes(yintercept = FinalEe))+
  # ylim(aes(c(0.2, maxY)))+
  # scale_x_datetime(date_labels="%m", date_breaks = "6 hours")+
  guides(color = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))

ggsave(paste0("Weather_Imputed.png"),  width = 190, height = 250, units = "mm", dpi = 100)


setwd(curr_dir)
