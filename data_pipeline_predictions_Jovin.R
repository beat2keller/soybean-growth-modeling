############################################################################
###                          Setup environment                            ###
############################################################################

# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)  
library(scales)  
library(gridExtra)

# 1. Set working directory
## Make sure working directory is in folder "soybean-growth-modeling"

# 2. Define the path to the specific model
model_path <- "model/Growth7_E.GxRxPre.RData"

# 3. Load the model into a new environment
env <- new.env()
load(model_path, envir = env)

# 4. Extract the model object dynamically
model_name <- ls(env)[1]  
loaded_model <- get(model_name, envir = env)

# 5. Load the dataset (soybean data)
df <- read.csv("data/model_data.csv")
df$date <- as.Date(df$date)
df$genotype.id <- as.factor(df$genotype.id)
df$platform <- as.factor(df$platform)

# 6. Extract valid genotypes from model coefficients
valid_genotypes <- unique(gsub(
  "Asym\\.genotype\\.id", "",
  grep("^Asym\\.genotype\\.id[0-9]+$",
       rownames(summary(loaded_model)$tTable), value = TRUE)
))
df$genotype.id <- factor(df$genotype.id, levels = valid_genotypes)

# 7. Generate predictions, excluding unknown genotypes
df$predicted_value <- NA  
df_known <- df %>% filter(!is.na(genotype.id))  
df$predicted_value[!is.na(df$genotype.id)] <-
  predict(loaded_model, newdata = df_known, level = 0)

# 8. Load the weather data
weather_data_modelling <- fread("data/weather_data_for_modelling.csv")
weather_data <- melt.data.table(
  weather_data_modelling,
  id.vars = c("WeatherVariable","Location","Date","Year","Measure_7","Measure_14","Measure_28","Measure_56"),
  measure.vars = c("CumulativeDailyMean"),
  variable.name = "WeatherCalcVar",
  value.name = "WeatherValue"
)
weather_data <- weather_data[!is.na(weather_data$WeatherValue),]  # Remove NAs

# 9. Convert Date to Date format
weather_data[, Date := as.Date(Date)]

# 10. Convert weather_data from long to wide format
weather_data_wide <- weather_data %>%
  filter(WeatherCalcVar == "CumulativeDailyMean") %>%
  select(Location, Date, WeatherVariable, Measure_7, Measure_14, Measure_28, Measure_56) %>%
  pivot_longer(cols = starts_with("Measure_"),
               names_to = "Measure_Type",
               values_to = "Value") %>%
  mutate(Measure_Type = gsub("Measure_", "", Measure_Type)) %>%
  unite("Weather_Measure", WeatherVariable, Measure_Type, sep = "_") %>%
  pivot_wider(names_from = Weather_Measure, values_from = Value, values_fill = NA) %>%
  rename_with(~ gsub("measure_", "", .), -c(Location, Date)) %>%
  rename_with(~ paste0("avg_", .), -c(Location, Date)) %>%
  rename_with(tolower)

# 11. Add the year column to weather_data_wide
weather_data_wide <- weather_data_wide %>%
  mutate(year = as.integer(format(date, "%Y")))

# 12. Prepare the soybean data by adding `month_day` & unifying `Location`
df <- df %>%
  mutate(month_day = format(date, "%m-%d")) %>%
  rename(location = Location)

# Also add `month_day` to weather_data_wide & unify name
weather_data_wide <- weather_data_wide %>%
  mutate(month_day = format(date, "%m-%d")) %>%
  rename(location = location)

# 13. Common predict function (merging by month_day + location)
predict_growth <- function(model, df_soy, genotype, weather_wide, median_year) {
  df_geno <- df_soy %>%
    filter(genotype.id == genotype,
           year == median_year,
           period == "Growth")
  
  genotype_location <- unique(df_geno$location)
  if (length(genotype_location) > 1) {
    warning("⚠️ Genotype appears in multiple locations. Using first location found.")
    genotype_location <- genotype_location[1]
    df_geno <- df_geno %>% filter(location == genotype_location)
  }
  
  # Filter weather to the same location
  weather_filtered <- weather_wide %>%
    filter(location == genotype_location)
  
  # Merge by month_day + location
  df_pred <- df_geno %>%
    select(-starts_with("avg_")) %>%
    left_join(weather_filtered, by = c("month_day", "location"))
  
  # If no rows left, skip
  if (nrow(df_pred) == 0) {
    warning("⚠️ No valid rows after merging by month_day + location.")
    df_pred$predicted_value <- NA
    return(df_pred)
  }
  
  df_pred$predicted_value <- predict(model, newdata = df_pred, level = 0)
  return(df_pred)
}

# 14. Common plot function that only takes one UID for actual values
common_plot_growth <- function(df1, df2, df3, df_actual, 
                               genotype_name, title_prefix, 
                               line_labels, # vector of 3 labels for df1, df2, df3
                               actual_label) {
  ggplot() +
    geom_line(data = df1,
              aes(x = date.x, y = predicted_value,
                  color = line_labels[1]),
              linewidth = 1.2
    ) +
    geom_line(data = df2,
              aes(x = date.x, y = predicted_value,
                  color = line_labels[2]),
              linewidth = 1.2
    ) +
    geom_line(data = df3,
              aes(x = date.x, y = predicted_value,
                  color = line_labels[3]),
              linewidth = 1.2
    ) +
    geom_line(
      data = df_actual %>%
        filter(UID == unique(df_actual$UID)[1]),  # pick one UID for actual
      aes(x = date, y = value, color = actual_label),
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste(title_prefix, "-", genotype_name),
      x = "Date",
      y = "Canopy Coverage",
      color = "Legend"
    ) +
    theme_minimal()
}


############################################################################
###                  1) Temperature Script                               ###
############################################################################

# -- Step A: Identify years by temperature
extreme_years_temp <- df %>%
  group_by(year) %>%
  summarise(mean_temp = mean(avg_temperature_14, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_temp)

min_year_temp <- extreme_years_temp$year[1]
max_year_temp <- extreme_years_temp$year[nrow(extreme_years_temp)]
median_year_temp <- extreme_years_temp$year[
  which.min(abs(extreme_years_temp$mean_temp - median(extreme_years_temp$mean_temp)))
]

# -- Step B: Set up data frames for min, max, median
weather_min_year_temp <- weather_data_wide %>% filter(year == min_year_temp)
weather_max_year_temp <- weather_data_wide %>% filter(year == max_year_temp)
weather_median_year_temp <- weather_data_wide %>% filter(year == median_year_temp)

# -- Step C: Choose random genotype from the median year
genotypes_in_median_year_temp <- unique(df$genotype.id[df$year == median_year_temp])
random_genotype_temp <- sample(genotypes_in_median_year_temp, 1)

# -- Step D: Extract actual values for the median temp year
df_median_year_temp <- df %>%
  filter(year == median_year_temp, period == "Growth")

# -- Step E: Predict growth
pred_min_temp <- predict_growth(loaded_model, df, random_genotype_temp, weather_min_year_temp, median_year_temp)
pred_max_temp <- predict_growth(loaded_model, df, random_genotype_temp, weather_max_year_temp, median_year_temp)
pred_med_temp <- predict_growth(loaded_model, df, random_genotype_temp, weather_median_year_temp, median_year_temp)

# -- Step F: sin^2 transform if desired
pred_min_temp$predicted_value <- sin(pred_min_temp$predicted_value)^2
pred_max_temp$predicted_value <- sin(pred_max_temp$predicted_value)^2
pred_med_temp$predicted_value <- sin(pred_med_temp$predicted_value)^2

# -- Step G: Plot
temp_plot <- common_plot_growth(
  df1 = pred_min_temp,
  df2 = pred_max_temp,
  df3 = pred_med_temp,
  df_actual = df_median_year_temp,
  genotype_name = as.character(random_genotype_temp),
  title_prefix = "Temperature Merge",
  line_labels = c(
    paste("Prediction with lowest temp in year", min_year_temp),
    paste("Prediction with highest temp in year", max_year_temp),
    paste("Prediction with temp in year", median_year_temp)
  ),
  actual_label = paste("Actual growth in year", median_year_temp)
)
print(temp_plot)


############################################################################
###                  2) Radiation Script                                 ###
############################################################################

# -- Step A: Identify years by radiation
extreme_years_rad <- df %>%
  group_by(year) %>%
  summarise(mean_radiation = mean(avg_radiation_14, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_radiation)

min_year_rad <- extreme_years_rad$year[1]
max_year_rad <- extreme_years_rad$year[nrow(extreme_years_rad)]
median_year_rad <- extreme_years_rad$year[
  which.min(abs(extreme_years_rad$mean_radiation - median(extreme_years_rad$mean_radiation)))
]

# -- Step B: Weather subsets
weather_min_year_rad <- weather_data_wide %>% filter(year == min_year_rad)
weather_max_year_rad <- weather_data_wide %>% filter(year == max_year_rad)
weather_median_year_rad <- weather_data_wide %>% filter(year == median_year_rad)

# -- Step C: Random genotype from median year
genotypes_in_median_year_rad <- unique(df$genotype.id[df$year == median_year_rad])
random_genotype_rad <- sample(genotypes_in_median_year_rad, 1)

# -- Step D: Actual values from median radiation year
df_median_year_rad <- df %>%
  filter(year == median_year_rad, period == "Growth")

# -- Step E: Predictions
pred_min_rad <- predict_growth(loaded_model, df, random_genotype_rad, weather_min_year_rad, median_year_rad)
pred_max_rad <- predict_growth(loaded_model, df, random_genotype_rad, weather_max_year_rad, median_year_rad)
pred_med_rad <- predict_growth(loaded_model, df, random_genotype_rad, weather_median_year_rad, median_year_rad)

# -- Step F: sin^2 transform
pred_min_rad$predicted_value <- sin(pred_min_rad$predicted_value)^2
pred_max_rad$predicted_value <- sin(pred_max_rad$predicted_value)^2
pred_med_rad$predicted_value <- sin(pred_med_rad$predicted_value)^2

# -- Step G: Plot
rad_plot <- common_plot_growth(
  df1 = pred_min_rad,
  df2 = pred_max_rad,
  df3 = pred_med_rad,
  df_actual = df_median_year_rad,
  genotype_name = as.character(random_genotype_rad),
  title_prefix = "Radiation Merge",
  line_labels = c(
    paste("Prediction with lowest radiation in year", min_year_rad),
    paste("Prediction with highest radiation in year", max_year_rad),
    paste("Prediction with radiation in year", median_year_rad)
  ),
  actual_label = paste("Actual growth in year", median_year_rad)
)
print(rad_plot)


############################################################################
###             3) Precipitation Script                                 ###
############################################################################

# -- Step A: Identify years by precipitation
extreme_years_precip <- df %>%
  group_by(year) %>%
  summarise(mean_precip = mean(avg_precipitation_14, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_precip)

min_year_precip <- extreme_years_precip$year[1]
max_year_precip <- extreme_years_precip$year[nrow(extreme_years_precip)]
median_year_precip <- extreme_years_precip$year[
  which.min(abs(extreme_years_precip$mean_precip - median(extreme_years_precip$mean_precip)))
]

# -- Step B: Weather subsets
weather_min_year_precip <- weather_data_wide %>% filter(year == min_year_precip)
weather_max_year_precip <- weather_data_wide %>% filter(year == max_year_precip)
weather_median_year_precip <- weather_data_wide %>% filter(year == median_year_precip)

# -- Step C: Random genotype
genotypes_in_median_year_precip <- unique(df$genotype.id[df$year == median_year_precip])
random_genotype_precip <- sample(genotypes_in_median_year_precip, 1)

# -- Step D: Actual median precipitation
df_median_year_precip <- df %>%
  filter(year == median_year_precip, period == "Growth")

# -- Step E: Predict
pred_min_precip <- predict_growth(loaded_model, df, random_genotype_precip, weather_min_year_precip, median_year_precip)
pred_max_precip <- predict_growth(loaded_model, df, random_genotype_precip, weather_max_year_precip, median_year_precip)
pred_med_precip <- predict_growth(loaded_model, df, random_genotype_precip, weather_median_year_precip, median_year_precip)

# -- Step F: sin^2 transform
pred_min_precip$predicted_value <- sin(pred_min_precip$predicted_value)^2
pred_max_precip$predicted_value <- sin(pred_max_precip$predicted_value)^2
pred_med_precip$predicted_value <- sin(pred_med_precip$predicted_value)^2

# -- Step G: Plot
precip_plot <- common_plot_growth(
  df1 = pred_min_precip,
  df2 = pred_max_precip,
  df3 = pred_med_precip,
  df_actual = df_median_year_precip,
  genotype_name = as.character(random_genotype_precip),
  title_prefix = "Precipitation Merge",
  line_labels = c(
    paste("Prediction with lowest precipitation in year", min_year_precip),
    paste("Prediction with highest precipitation in year", max_year_precip),
    paste("Prediction with precipitation in year", median_year_precip)
  ),
  actual_label = paste("Actual growth in year", median_year_precip)
)
print(precip_plot)
