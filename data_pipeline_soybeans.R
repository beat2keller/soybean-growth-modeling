setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

# This pipeline uses soybean_pixels_data.csv to make a df that can be joined with the weather data
# output is call soybean_data_for_modelling.csv
# packages ---------
library(data.table)
library(stringr)
library(tidyverse)
#load data ---------
soybeans_FIP_UAV <- fread("data/soybean_pixels_data.csv")
# soybeans_FIP_UAV <- subset(soybeans_FIP_UAV, Period=="Growth"&year_site.UID=="FPSB016")

# New classification of growth and senescence period:
# to account for the fact that the maximum canopy cover might not be the actual end of the growth phase,
# but occur after a long fluctuation, we take the max value of the first long continuous growth period as end of growth.
# Therefore, we check in the neighbourhood of the max value if there was a decline. 
# If so, this indicates fluctuation and we take the max vale before the fluctuation as end of growth.

soybeans_FIP_UAV <- soybeans_FIP_UAV[order(soybeans_FIP_UAV$date),]
soybeans_FIP_UAV$id_platform <- paste0(soybeans_FIP_UAV$plot.UID) #,soybeans_FIP_UAV$platform


# for (id in unique(soybeans_FIP_UAV$id_platform)) {
#   subs = subset(soybeans_FIP_UAV, id_platform == id)
#   max = which(subs$Canopy_cover == max(subs$Canopy_cover)) # find the index of the maximum value
#   c_max = min(which(subs$Canopy_cover > max(subs$Canopy_cover)-0.02))  # find the minimum index with a canopy cover close to the maximum (0.02 under)
#   rel = max(subs$Canopy_cover) # store the maximum value
#   if (max > c_max) {
#     for (i in c_max:(max-1)) {
#       if (subs$Canopy_cover[i] > subs$Canopy_cover[i+1]) { # check if there is a decline closely before the max is reached (indication for fluctuation)
#         rel = subs$Canopy_cover[i] # if so, take the maximum value before the decline as the end of growth
#         break } } }
#   growth_end = which(subs$Canopy_cover==rel) # define the index of the end of growth
#   for (i in 1:nrow(subs)) {  # define the observations before end of growth as "Growth" and after as "Senescence"
#     if (i <= growth_end) {soybeans_FIP_UAV[which(soybeans_FIP_UAV[,"plot.UID"] == id), "Period"][i,]  = "Growth"}
#     if (i > growth_end) {soybeans_FIP_UAV[which(soybeans_FIP_UAV[,"plot.UID"] == id), "Period"][i,] = "Senescence"}
#   }
# }

# for (id in unique(soybeans_FIP_UAV$id_platform )) {
#   subs = subset(soybeans_FIP_UAV, id_platform==id)
#   max = which(subs$Canopy_cover == max(subs$Canopy_cover))# find the index of the maximum value
#   CmaxES <- (which(subs$Canopy_cover > max(subs$Canopy_cover)-0.02))
#   c_min <- CmaxES[length(CmaxES)]# CmaxES[1]
#   # lengthCmax <- length(CmaxES)-1
#   # if (lengthCmax>1) {
#   #   for (i in 1:lengthCmax) {
#   #     ii <- CmaxES[i]
#   #     if (subs$Canopy_cover[ii] < subs$Canopy_cover[ii+1]) {
#   #       c_min = CmaxES[i+1]  }
#   #   } }
#   # 
#     growth_end = c_min#which(subs$Canopy_cover==rel) # define the index of the end of growth
#   for (i in 1:nrow(subs)) {  # define the observations before end of growth as "Growth" and after as "Senescence"
#     if (i <= growth_end) {soybeans_FIP_UAV[which(soybeans_FIP_UAV[,"plot.UID"] == id), "Period"][i,]  = "Growth"}
#     if (i > growth_end) {soybeans_FIP_UAV[which(soybeans_FIP_UAV[,"plot.UID"] == id), "Period"][i,] = "Senescence"}
#   }
# }

# rename variables
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "plot.range"] <- "range"
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "plot.row"] <- "row"
soybeans_FIP_UAV$value <- soybeans_FIP_UAV$value_relative # this creates an additional column value that has the same entries
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "Date"] <- "date"
# restrict to subsets of the data that are needed for the model
soybeans = subset(soybeans_FIP_UAV, variable == "Canopy_cover")
# clean data 
soybeans = unique(soybeans)
soybeans = soybeans[!is.na(soybeans$value),]
soybeans = soybeans[!is.na(soybeans$genotype.id),]
soybeans <- soybeans[,c("Filename","genotype.name","genotype.id","plot.UID","range","row","Time","location","year_site.UID" ,"date","variable","Period","time_since_sowing","date_of_sowing","platform", "value")]
soybeans = unique(soybeans)
soybeans[soybeans$value < 0,]$value = 0 # do not allow negative CC
# order the df
soybeans = soybeans[order(soybeans$variable, soybeans$plot.UID, soybeans$date),]
#transform the plot UIDs into strings of integers
soybeans$plot_number = strtoi(str_sub(soybeans$plot.UID, -3), 10)




# Time since sowing ------
soybeans$date <- as.Date(soybeans$date)
soybeans$year = year(soybeans$date)


# Change wrongly measured fields ## fixed
# 
# temp = soybeans[soybeans$year_site.UID=='FPSB006',]$range
# soybeans[soybeans$year_site.UID=='FPSB006',]$range = soybeans[soybeans$year_site.UID=='FPSB006',]$row
# soybeans[soybeans$year_site.UID=='FPSB006',]$row = temp


# Spatial grouping -----
max_rows_and_ranges = soybeans %>% group_by(year_site.UID) %>% summarize(max_row = max(row))
soybeans = merge(soybeans, max_rows_and_ranges, all.x = TRUE)
soybeans$row_grouped = floor((soybeans$row - 1)/ 3) + 1
soybeans[soybeans$max_row %% 3 == 1 & soybeans$row == soybeans$max_row,]$row_grouped = soybeans[soybeans$max_row %% 3 == 1 & soybeans$row == soybeans$max_row,]$row_grouped - 1

soybeans <- soybeans %>%
  group_by(year_site.UID, range, row_grouped) %>%
  mutate(plot_grouped = as.factor(cur_group_id()))
# indicator for closeness to border is omitted
soybeans <- soybeans[, !names(soybeans) %in% c('distance_to_border', 'distance_to_border_row_grouped', 'distance_to_border_range', 'max_row_grouped', 'max_range')]
# we keep the plot_groupded variable, but we might define a new grouping later 


# rename variables again -------

colnames(soybeans)[colnames(soybeans) == "value"] <- "CC"
# symbols like / and % create problems later on, they should not be used in variable names
colnames(soybeans) <- gsub("/", "", colnames(soybeans))
colnames(soybeans) <- gsub("%", "", colnames(soybeans))



# filling in missing dates - take from R file of prev. year
df_temp = subset(soybeans)
unique_field_UIDs = sort(unique(soybeans$year_site.UID))
for (field_UID in unique_field_UIDs) {
  df_field = df_temp[df_temp$year_site.UID == field_UID,]
  plot_numbers = unique(df_field$plot_number)
  for (plot_number in plot_numbers) {
    df_plot = df_field[df_field$plot_number == plot_number,]
    field_dates = sort(unique(df_field$date))
    missing_dates = setdiff(as.character(field_dates), as.character(df_plot$date))
    df_plot_max_PH = df_plot[df_plot$date == max(df_plot$date),]
    for (missing_date in missing_dates) {
      df_plot_max_PH$date = as.Date(missing_date)
      df_temp = rbind(df_temp, df_plot_max_PH)
    }
  }
}

soybeans = left_join(soybeans,df_temp )

# renaming
soybeans$Date <- soybeans$date
soybeans$date <- NULL # dropping lower case date column
soybeans$Location <- soybeans$location
soybeans$Year <- as.character(soybeans$year)
soybeans <- setDT(soybeans)
soybeans <- unique(soybeans)

#pivot longer
soybeans_melt = melt.data.table(soybeans, measure.vars = c("CC"))


# sowing <- subset(soybeans_melt,variable.1=="CC")[!duplicated(plot.UID),]
# sowing$Date <- as.Date(sowing$date_of_sowing)+5
# sowing$time_since_sowing <- 5
# sowing$value <- 0.0001
# sowing$Filename <- NA
# 
# soybeans_melt <- rbind(soybeans_melt, sowing)


# considering only the growth period, this pipelines returns the same data points as the data_aggregation_cc file from last year.
# Nut without plotting, spatial_cc and other things unnecessary for the modelling

write.csv(soybeans_melt, "data/soybean_data_for_modelling.csv",row.names = F)

