### get the file directory 
setwd("~/Desktop/R")
getwd()

### The code contains five sections of different analyses.
### install packages used in this project
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


### for GLMs, when using different site dataset, you should just change the name of the following csv.
point1 <- read.csv("point1.csv", header=FALSE)
point1 <- slice(point1, -c(1))

### Function to calculate VPD
calculate_vpd <- function(temperature, relative_humidity) {
  # Constants for water vapor pressure calculation
  a <- 17.269
  b <- 237.3
  
  # Calculate saturation vapor pressure
  saturation_vapor_pressure <- 6.1078 * exp(a * temperature / (temperature + b))
  
  # Calculate VPD
  vpd <- saturation_vapor_pressure - (relative_humidity * saturation_vapor_pressure / 100)
  
  return(vpd)
}

### dataframe to calculate VPD
RH_point1 <- data.frame(Height = (point1$V1), RH = (point1$V4), Temperature = (point1$V3), time = (point1$V2))
RH_point1$RH <- as.numeric(RH_point1$RH)
RH_point1$Temperature <- as.numeric(RH_point1$Temperature)
### Convert time_column to POSIXct format
RH_point1$time <- as.POSIXct(RH_point1$time, format = "%Y-%m-%d %H:%M:%S")

### Calculate VPD using the function
VPD <- calculate_vpd(RH_point1$Temperature, RH_point1$RH)

### dataframe VPD
VPD_point1 <- data.frame(
  Height = as.numeric(RH_point1$Height),
  Temperature = RH_point1$Temperature,
  VPD = VPD,
  time = RH_point1$time
)

### Filter data within the time range (13:00 to 17:00) for each day
filtered_data <- VPD_point1 %>%
  filter(hour(time) >= 13 & hour(time) <= 17)

filtered_data$Height <- factor(filtered_data$Height)

### Fit the GLMs for temperature
model_T <- glm(Temperature ~ Height, data = filtered_data)
summary(model_T)

### Create the boxplot
ggplot_VPD <- ggplot(filtered_data, aes(x = Height, y = VPD, group = Height)) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE) +
  labs(x = "Height", y = "VPD") +
  theme_minimal()
print(ggplot_VPD)

### Fit the GLMs for VPD
model_VPD <- glm(VPD ~ Height, data = filtered_data)
summary(model_VPD)





### Second part of analyses: Exponential decay model
### Exponential decay model for VPD
### Using the dataset 'log_VPD.csv' here
data_VPD <- read.csv("log_VPD.csv", header=TRUE)
data_VPD <- na.omit(data_VPD)

### Fit an exponential decay model for VPD
model_exponential_VPD <- nls(X ~ A * exp(-k * data_VPD$X), data = data_VPD, start = list(A = 1, k = 0.1))

### Get the summary of the model
summary(model_exponential_VPD)

### Calculate R-squared value
rsquared <- 1 - sum(residuals(model_exponential_VPD)^2) / sum((data_VPD$X - mean(data_VPD$X))^2)
print(paste("R-squared:", rsquared))

### Extract k value and A value
k_VPD_value <- coef(model_exponential_VPD)['k']
A_VPD_value <- summary(model_exponential_VPD)$coefficients["A", "Estimate"]

### Predict using the fitted model
predicted_values <- predict(model_exponential_VPD, newdata = data.frame(Height = data$Height))

### Create the plot for VPD
ggplot(data_VPD, aes(x = Height, y = X)) +
  geom_point() +
  geom_line(aes(y = predicted_values), color = "red") +
  labs(x = "Height", y = "VPD") +
  ggtitle("Exponential Decay Model (VPD)") +
  annotate("text", x = max(data_VPD$Height), y = max(predicted_values), 
           label = paste("R-squared:", round(rsquared, 3)), color = "black", hjust = 1.5, vjust = 5) +
  annotate("text", x = max(data_VPD$Height), y = max(predicted_values) - 0.1, 
           label = paste("k value:", format(k_VPD_value, digits = 3)), 
           color = "black", hjust = 1.5, vjust = 1) +
  annotate("text", x = max(data_VPD$Height), y = max(predicted_values) - 0.05, 
           label = paste("A value:", format(A_VPD_value, digits = 3)), color = "black", hjust = 1.5, vjust = 0) +
  theme(plot.title = element_text(hjust = 0.5))



### Exponential decay model for T
### Using the dataset 'log_T.csv' here
data_T <- read.csv("log_T.csv", header=TRUE)
data_T <- na.omit(data_T)

### Fit an exponential decay model for T
model_exponential_T <- nls(X ~ A * exp(-k * data_T$X), data = data_T, start = list(A = 1, k = 0.1))

### Get the summary of the model
summary(model_exponential_T)

### Calculate R-squared value for this temperature exponential decay model
rsquared <- 1 - sum(residuals(model_exponential_T)^2) / sum((data_T$X - mean(data_T$X))^2)
print(paste("R-squared:", rsquared))

### Extract k value and A value
k_T_value <- coef(model_exponential_T)['k']
A_T_value <- summary(model_exponential_T)$coefficients["A", "Estimate"]

### Predict using the fitted model
predicted_values <- predict(model_exponential_T, newdata = data.frame(Height = data_T$Height))

### Create the plot
ggplot(data_T, aes(x = Height, y = X)) +
  geom_point() +
  geom_line(aes(y = predicted_values), color = "red") +
  labs(x = "Height", y = "T") +
  ggtitle("Exponential Decay Model (T)") +
  annotate("text", x = max(data_T$Height), y = max(predicted_values), 
           label = paste("R-squared:", round(rsquared, 3)), color = "black", hjust = 1.5, vjust = 5.5) +
  annotate("text", x = max(data_T$Height), y = max(predicted_values) - 0.1, 
           label = paste("k value:", format(k_T_value, digits = 3)), 
           color = "black", hjust = 1.5, vjust = 0.5) +
  annotate("text", x = max(data_T$Height), y = max(predicted_values) - 0.05, 
           label = paste("A value:", format(A_T_value, digits = 3)), color = "black", hjust = 1.5, vjust = 0) +
  theme(plot.title = element_text(hjust = 0.5))




### Third part of the analyses
### Relation between the magnitude of LAI and the extent of microclimatic variables
### Using dataset 'LAI.csv' here.

data <- read.csv("LAI.csv")
data <- data.frame(Site = data$site, LAI = data$LAI, T.differ = data$T.differences, VPD.differ = data$VPD.differences)

### Calculate correlation (T)
correlation_result <- cor.test(data$LAI, data$T.differ)

### R-squared and p-value
r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]

### plot of correlation (T)
ggplot(data, aes(x = T.differ, y = LAI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Temperature", y = "LAI", title = "LAI vs Temperature") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  
  ) +
  annotate("text", x = min(data$T.differ), y = max(data$LAI), 
           label = sprintf("Correlation coefficient = %.2f\np-value = %.4f", 
                           correlation_result$estimate, correlation_result$p.value),
           hjust = 0, vjust = 1, color = "black") 

### Correlation analyses (VPD)
### Calculate correlation (VPD)
correlation_result <- cor.test(data$LAI, data$VPD.differ)

### R-squared and p-value for VPD
r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]

### plot of correlation (T)
ggplot(data, aes(x = VPD.differ, y = LAI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "VPD", y = "LAI", title = "LAI vs VPD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  
  ) +
  annotate("text", x = min(data$VPD.differ), y = max(data$LAI), 
           label = sprintf("Correlation coefficient = %.2f\np-value = %.4f", 
                           correlation_result$estimate, correlation_result$p.value),
           hjust = 0, vjust = 1, color = "black")  




### Next part of analyses (A new method, so do again for the dataframe establishment)
### Pay attention to change the dataset for whole 12 sites, the following is an example for point1
### Just change the 'point1.csv' to other sites' datatset.
### buffer time calculation
point1 <- read.csv("point1.csv", header=FALSE)
point1 <- slice(point1, -c(1))

### Function to calculate VPD
calculate_vpd <- function(temperature, relative_humidity) {
  a <- 17.269
  b <- 237.3
  saturation_vapor_pressure <- 6.1078 * exp(a * temperature / (temperature + b))
  vpd <- saturation_vapor_pressure - (relative_humidity * saturation_vapor_pressure / 100)
  return(vpd)
}

### dataframe to calculate VPD
RH_point1 <- data.frame(Height = (point1$V1), RH = (point1$V4), Temperature = (point1$V3), time = (point1$V2))
RH_point1$RH <- as.numeric(RH_point1$RH)
RH_point1$Temperature <- as.numeric(RH_point1$Temperature)
### Convert time_column to POSIXct format
RH_point1$time <- as.POSIXct(RH_point1$time, format = "%Y-%m-%d %H:%M:%S")

### Calculate VPD using the function
VPD <- calculate_vpd(RH_point1$Temperature, RH_point1$RH)

### dataframe VPD
VPD_point1 <- data.frame(
  Height = as.numeric(RH_point1$Height),
  Temperature = RH_point1$Temperature,
  VPD = VPD,
  time = RH_point1$time
)

filtered_data$Height <- factor(filtered_data$Height)

### Filter data within the time range (13:00 to 17:00) for each day
filtered_data <- VPD_point1 %>%
  filter(hour(time) >= 13 & hour(time) <= 17)
### Filter data for heights 0 and 8
filtered_data <- filtered_data %>%
  filter(Height %in% c(0, 8))
filtered_data <- filtered_data %>%
  mutate(Date = as.Date(time))

### Calculate the mean temperature for each day and precalculate min/max times
temperature_mean_by_day <- filtered_data %>%
  group_by(Date) %>%
  summarize(Temperature_mean = mean(Temperature),
            min_time = min(time),
            max_time = max(time))

### Create the plot with facets and mean temperature lines using geom_segment
ggplot(filtered_data, aes(x = time, y = Temperature)) +
  geom_line(aes(group = Height, color = Height)) +
  geom_segment(data = temperature_mean_by_day,
               aes(x = min_time, xend = max_time,
                   y = Temperature_mean, yend = Temperature_mean),
               linetype = "dashed", color = "red") +
  labs(x = "Time", y = "Temperature", color = "Height") +
  theme_minimal() +
  facet_wrap(~ Date, ncol = 1)

### find the intersection points (time)
intersection_points <- filtered_data %>%
  left_join(temperature_mean_by_day, by = "Date") %>%
  group_by(Height, Date) %>%
  filter(lag(Temperature) < Temperature_mean & Temperature >= Temperature_mean) %>%
  summarise(intersection_time = time)
print(intersection_points)

### Extract date portion of intersection_time
intersection_points$date <- as.Date(intersection_points$intersection_time)

### Group by Height and date, and calculate the earliest time for each day
earliest_times <- intersection_points %>%
  group_by(Height, date) %>%
  summarise(earliest_time = min(intersection_time))

print(earliest_times)

### calculate the time-lag
time_diff_data <- earliest_times %>%
  filter(Height %in% c(0, 8)) %>%
  pivot_wider(names_from = Height, values_from = earliest_time) %>%
  mutate(time_difference = `8` - `0`)

### Exclude NA values
valid_time_diff_data <- time_diff_data %>%
  filter(!is.na(time_difference))
valid_time_diff_data$time_difference <- abs(valid_time_diff_data$time_difference)

### Calculate the mean of valid time differences
mean_time_difference_T <- mean(valid_time_diff_data$time_difference, na.rm = TRUE)
### print the mean
print(mean_time_difference_T)




### for time-lag of VPD responses
### Calculate the mean VPD for each day and precalculate min/max times
VPD_mean_by_day <- filtered_data %>%
  group_by(Date) %>%
  summarize(VPD_mean = mean(VPD),
            min_time = min(time),
            max_time = max(time))

### Create the plot with facets and mean VPD lines using geom_segment
ggplot(filtered_data, aes(x = time, y = VPD)) +
  geom_line(aes(group = Height, color = Height)) +
  geom_segment(data = VPD_mean_by_day,
               aes(x = min_time, xend = max_time,
                   y = VPD_mean, yend = VPD_mean),
               linetype = "dashed", color = "red") +
  labs(x = "Time", y = "VPD", color = "Height") +
  theme_minimal() +
  facet_wrap(~ Date, ncol = 1)

### find the interception points
intersection_points <- filtered_data %>%
  left_join(VPD_mean_by_day, by = "Date") %>%
  group_by(Height, Date) %>%
  filter(lag(VPD) < VPD_mean & VPD >= VPD_mean) %>%
  summarise(intersection_time = time)
print(intersection_points)

### Extract date portion of intersection_time
intersection_points$date <- as.Date(intersection_points$intersection_time)

### Group by Height and date, and calculate the earliest time for each day
earliest_times <- intersection_points %>%
  group_by(Height, date) %>%
  summarise(earliest_time = min(intersection_time))

### print the earliest times
print(earliest_times)
### calculate the time-lag
time_diff_data <- earliest_times %>%
  filter(Height %in% c(0, 8)) %>%
  pivot_wider(names_from = Height, values_from = earliest_time) %>%
  mutate(time_difference = `8` - `0`)

### Exclude NA values
valid_time_diff_data <- time_diff_data %>%
  filter(!is.na(time_difference))
valid_time_diff_data$time_difference <- abs(valid_time_diff_data$time_difference)

### Calculate the mean of valid time differences
mean_time_difference_VPD <- mean(valid_time_diff_data$time_difference, na.rm = TRUE)
## print the mean
print(mean_time_difference_VPD)




### After preparation of the data
### It is restored in 'buffer.csv'
### This section is the correlation between the magnitude of LAI and the extent of time-lag

### Load buffer data
data <- read.csv("buffer.csv")
data <- data.frame(Site = data$X, LAI = data$LAI, T.differ = data$T..seconds., VPD.differ = data$VPD..seconds.)

### Correlation analysis
correlation_result <- cor.test(data$LAI, data$T.differ)
correlation_coefficient <- correlation_result$estimate

r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]

### Data visualization: Scatter plot with correlation coefficient, linear regression line, and model information
ggplot(data, aes(x = T.differ, y = LAI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Time", y = "LAI", title = "LAI vs Time-lag (T)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  
  ) +
  annotate("text", x = min(data$T.differ), y = max(data$LAI), 
           label = sprintf("Correlation coefficient = %.2f\np-value = %.4f", 
                           correlation_result$estimate, correlation_result$p.value),
           hjust = 0, vjust = 1, color = "black")  # Add correlation info



### Calculate correlation (VPD)
correlation_result <- cor.test(data$LAI, data$VPD.differ)

r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]

### plot of correlation (VPD)
ggplot(data, aes(x = VPD.differ, y = LAI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "time", y = "LAI", title = "LAI vs Time-lag (VPD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  
  ) +
  annotate("text", x = min(data$VPD.differ), y = max(data$LAI), 
           label = sprintf("Correlation coefficient = %.2f\np-value = %.4f", 
                           correlation_result$estimate, correlation_result$p.value),
           hjust = 0, vjust = 1, color = "black")  








