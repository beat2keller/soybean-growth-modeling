# This pipeline uses soybean_pixels_data.csv to make a df that can be joined with the weather data
# output is call soybean_data_for_modelling.csv
# packages ---------
library(data.table)
library(stringr)
library(tidyverse)
#load data ---------
soybeans_FIP_UAV <- fread("data/soybean_pixels_data.csv")

# rename variables
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "plot.range"] <- "range"
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "plot.row"] <- "row"
soybeans_FIP_UAV$value <- soybeans_FIP_UAV$value_relative # this creates an additional column value that has the same entries
colnames(soybeans_FIP_UAV)[colnames(soybeans_FIP_UAV) == "Date"] <- "date"
# restrict to subsets of the data that are needed for the model
soybeans = subset(soybeans_FIP_UAV, Period=="Growth" &  variable == "Canopy_cover")
# clean data 
soybeans = unique(soybeans)
soybeans = soybeans[!is.na(soybeans$value),]
soybeans = soybeans[!is.na(soybeans$genotype.id),]
soybeans <- soybeans[,c("Filename","genotype.name","genotype.id","plot.UID","range","row","Time","location","year_site.UID" ,"date","variable","Period","time_since_sowing","date_of_sowing","value")]

soybeans[soybeans$value < 0,]$value = 0 # do not allow negative CC
# order the df
soybeans = soybeans[order(soybeans$variable, soybeans$plot.UID, soybeans$date),]
#transform the plot UIDs into strings of integers
soybeans$plot_number = strtoi(str_sub(soybeans$plot.UID, -3), 10)




# Time since sowing ------
soybeans$date <- as.Date(soybeans$date)
soybeans$year = year(soybeans$date)


# Change wrongly measured fields

temp = soybeans[soybeans$year_site.UID=='FPSB006',]$range
soybeans[soybeans$year_site.UID=='FPSB006',]$range = soybeans[soybeans$year_site.UID=='FPSB006',]$row
soybeans[soybeans$year_site.UID=='FPSB006',]$row = temp


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

# considering only the growth period, this pipelines returns the same data points as the data_aggregation_cc file from last year.
# Nut without plotting, spatial_cc and other things unnecessary for the modelling

write.csv(soybeans_melt, "data/soybean_data_for_modelling.csv")

