sink("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UKUS_output.txt", append = FALSE, split = TRUE)
# Load required packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(gridExtra)
library(dplyr)
library(gridExtra)
library(grid)


# Load the Excel file
data <- read_excel("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UK_Comp_US_GBRPROINDMISMEI.xlsx")

# View the structure and summary
str(data)
summary(data)

# Convert date column and check for NAs
data$DATE <- as.Date(data$DATE)
sum(is.na(data))

# Time Series Plot
ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = GBRPROINDMISMEI, color = "UK")) +
  geom_line(aes(y = USAPROINDMISMEI, color = "US")) +
  labs(title = "Industrial Production Index (UK vs US)",
       x = "Date", y = "Index Value", color = "Country") +
  theme_minimal()

# Summary Statistics
summary_stats <- data %>%
  select(-DATE) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max, median = median), na.rm = TRUE)
print(summary_stats)

# Summary table for presentation
# Recreate summary as before
summary_stats <- data %>%
  select(-DATE) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max, median = median), na.rm = TRUE)

# Rename columns for clarity
colnames(summary_stats) <- c(
  "UK_Mean", "US_Mean",
  "UK_SD", "US_SD",
  "UK_Min", "US_Min",
  "UK_Max", "US_Max",
  "UK_Median", "US_Median"
)

# Convert to data frame for reshaping
summary_df <- as.data.frame(summary_stats)

# Reshape to have countries as rows and stats as columns
summary_table <- data.frame(
  Country = c("UK", "US"),
  Mean = c(summary_df$UK_Mean, summary_df$US_Mean),
  `Std Dev` = c(summary_df$UK_SD, summary_df$US_SD),
  Min = c(summary_df$UK_Min, summary_df$US_Min),
  Max = c(summary_df$UK_Max, summary_df$US_Max),
  Median = c(summary_df$UK_Median, summary_df$US_Median)
)

# View formatted table
print(summary_table)




# Create Time Series Objects
uk_ts <- ts(data$GBRPROINDMISMEI, start = c(1948, 1), frequency = 12)
us_ts <- ts(data$USAPROINDMISMEI, start = c(1948, 1), frequency = 12)

# Plot Run Sequence (Time Series)
plot.ts(uk_ts, main = "UK Run Sequence Plot", ylab = "UK Index")
plot.ts(us_ts, main = "US Run Sequence Plot", ylab = "US Index")

# Lag Plots
lag.plot(uk_ts, lags = 1, main = "UK Lag Plot")
lag.plot(us_ts, lags = 1, main = "US Lag Plot")

# Histogram
hist(uk_ts, breaks = 30, main = "Histogram of UK Index", xlab = "UK Index", col = "lightblue")
hist(us_ts, breaks = 30, main = "Histogram of US Index", xlab = "US Index", col = "lightgreen")

# Normal Probability Plot (Q-Q Plot)
qqnorm(uk_ts, main = "Normal Q-Q Plot - UK")
qqline(uk_ts)

qqnorm(us_ts, main = "Normal Q-Q Plot - US")
qqline(us_ts)

# Four-Plot Diagnostic (UK Index)
# Create time series object
uk_ts <- ts(data$GBRPROINDMISMEI, start = c(1948, 1), frequency = 12)

#  Run Sequence Plot
p1 <- ggplot(data, aes(x = DATE, y = GBRPROINDMISMEI)) +
  geom_line(color = "steelblue") +
  labs(title = "Run Sequence Plot", x = "Date", y = "UK Index") +
  theme_minimal()

# Lag Plot (lag-1)
lag_df <- data.frame(x = head(uk_ts, -1), y = tail(uk_ts, -1))
p2 <- ggplot(lag_df, aes(x = x, y = y)) +
  geom_point(color = "darkgreen") +
  labs(title = "Lag Plot (Lag 1)", x = "Lag(x)", y = "x") +
  theme_minimal()

#  Histogram
p3 <- ggplot(data, aes(x = GBRPROINDMISMEI)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  labs(title = "Histogram", x = "UK Index", y = "Frequency") +
  theme_minimal()

# Q-Q Plot
p4 <- ggplot(data, aes(sample = GBRPROINDMISMEI)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot") +
  theme_minimal()

# Arrange in 2x2 Grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Correlation
cor(data$GBRPROINDMISMEI, data$USAPROINDMISMEI, use = "complete.obs")

# stationarity Check (ADF Test)
adf.test(uk_ts)
adf.test(us_ts)


############################################################
###########################################################
######EDA FOR UKIndexOfProduction##########

# Load required packages
library(tidyverse)
library(readr)

# Load the CSV, skip metadata
data <- read_csv("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UKIndexOfProduction-15Aug2024.csv", skip = 6)

# Check structure
#str(data)
head(data)

# Rename first column to "Year"
colnames(data)[1] <- "Year"

# Convert Year to numeric
data$Year <- as.numeric(data$Year)

# Drop rows with NA in Year
data <- data[!is.na(data$Year), ]

# Select relevant columns for analysis (example: columns 2 to 5)
selected_data <- data %>% select(Year, 2:5)

# Plot Trends
selected_data_long <- pivot_longer(selected_data, -Year, names_to = "Sector", values_to = "Value")

ggplot(selected_data_long, aes(x = Year, y = as.numeric(Value), color = Sector)) +
  geom_line() +
  labs(title = "Production Index by Sector", x = "Year", y = "Value (£m)") +
  theme_minimal()

# Summary Statistics
summary_stats <- selected_data %>%
  summarise(across(.cols = where(is.numeric), list(mean = mean, sd = sd, median = median, min = min, max = max), na.rm = TRUE))
print(summary_stats)

#for Summary table
names(selected_data)[-1]

# Transpose the summary_stats matrix
summary_matrix <- t(as.matrix(summary_stats))

# Define proper column names for stats
colnames_matrix <- c("Mean", "Std Dev", "Median", "Min", "Max")

# Rebuild as a data frame with proper labels
summary_table_sectors_df <- data.frame(
  Sector = names(selected_data)[-1],
  Mean = round(summary_matrix[1, ], 2),
  `Std Dev` = round(summary_matrix[2, ], 2),
  Median = round(summary_matrix[3, ], 2),
  Min = round(summary_matrix[4, ], 2),
  Max = round(summary_matrix[5, ], 2)
)

# Print the final formatted table
print(summary_table_sectors_df)


# Histograms for selected sectors
selected_data %>%
  pivot_longer(-Year, names_to = "Sector", values_to = "Value") %>%
  ggplot(aes(x = as.numeric(Value))) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~Sector, scales = "free") +
  labs(title = "Distribution of Sectoral Production Values", x = "Value (£m)", y = "Count") +
  theme_minimal()


# Create Q-Q plots for each selected sector (loop through columns 2 to 5)
for (col_name in names(selected_data)[-1]) {
  p <- ggplot(selected_data, aes(sample = as.numeric(.data[[col_name]]))) +
    stat_qq() +
    stat_qq_line(color = "blue") +
    labs(
      title = paste("Normal Q-Q Plot -", col_name),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  print(p)  #Print after assigning to a variable
}


# === Confirmatory Data Analysis Section ===
# === Confirmatory Modeling ===

# Differencing
uk_diff <- diff(uk_ts, differences = 1)
us_diff <- diff(us_ts, differences = 1)

adf.test(uk_diff)
adf.test(us_diff)

# ARIMA
uk_arima <- auto.arima(uk_ts)
us_arima <- auto.arima(us_ts)

summary(uk_arima)
summary(us_arima)

# Forecast ARIMA
uk_arima_fc <- forecast(uk_arima, h = 12)
us_arima_fc <- forecast(us_arima, h = 12)

autoplot(uk_arima_fc) + ggtitle("UK ARIMA Forecast")
autoplot(us_arima_fc) + ggtitle("US ARIMA Forecast")

# ETS
uk_ets <- ets(uk_ts, model = "ZZZ")
us_ets <- ets(us_ts, model = "ZZZ")

summary(uk_ets)
summary(us_ets)

# Forecast ETS
uk_ets_fc <- forecast(uk_ets, h = 12)
us_ets_fc <- forecast(us_ets, h = 12)

autoplot(uk_ets_fc) + ggtitle("UK ETS Forecast")
autoplot(us_ets_fc) + ggtitle("US ETS Forecast")

# Accuracy Comparison
accuracy(uk_arima_fc)
accuracy(uk_ets_fc)
accuracy(us_arima_fc)
accuracy(us_ets_fc)

# Residual Checks
checkresiduals(uk_arima)
checkresiduals(uk_ets)
checkresiduals(us_arima)
checkresiduals(us_ets)

# Decomposition
uk_stl <- stl(uk_ts, s.window = "periodic")
autoplot(uk_stl) + ggtitle("UK STL Decomposition")

us_stl <- stl(us_ts, s.window = "periodic")
autoplot(us_stl) + ggtitle("US STL Decomposition")

# Convert to data frame for modeling
#=========================
# Time Series Linear Regression Model & Residuals (UK and US)
#=========================

# === Function to Plot Individual Forecasts Separately with Labels ===
plot_lm_model_with_labels_single <- function(ts_data, label = "UK") {
  df <- data.frame(
    Time = time(ts_data),
    Value = as.numeric(ts_data)
  )
  
  train_end <- which(df$Time >= 2001)[1] - 1
  valid_end <- which(df$Time >= 2004)[1] - 1
  
  train <- df[1:train_end, ]
  valid <- df[(train_end + 1):valid_end, ]
  future <- df[(valid_end + 1):nrow(df), ]
  
  lm_model <- lm(Value ~ Time, data = train)
  valid$Predicted <- predict(lm_model, newdata = valid)
  future$Predicted <- predict(lm_model, newdata = future)
  
  train$Set <- "Training"
  valid$Set <- "Validation"
  future$Set <- "Future"
  
  y_min <- min(df$Value)
  y_max <- max(df$Value)
  y_pad <- (y_max - y_min) * 0.15
  
  # Forecast Plot
  p_forecast <- ggplot() +
    geom_line(data = df, aes(x = Time, y = Value), color = "black") +
    geom_line(data = train, aes(x = Time, y = Value), color = "blue", linewidth = 0.6) +
    geom_line(data = valid, aes(x = Time, y = Predicted), color = "skyblue", linewidth = 0.8) +
    geom_line(data = future, aes(x = Time, y = Predicted), color = "orange", linewidth = 0.8) +
    geom_vline(xintercept = df$Time[c(train_end, valid_end)], linetype = "dashed") +
    annotate("text", x = mean(train$Time), y = y_max + y_pad, label = "Training", size = 4, fontface = "bold") +
    annotate("text", x = mean(valid$Time), y = y_max + y_pad, label = "Validation", size = 4, fontface = "bold") +
    annotate("text", x = mean(future$Time), y = y_max + y_pad, label = "Future", size = 4, fontface = "bold") +
    ylim(y_min, y_max + y_pad * 1.5) +
    labs(title = paste("Time Series Linear Regression Forecast -", label),
         x = "Time", y = paste(label, "Index"))
  
  # Residuals Plot
  train$Residuals <- resid(lm_model)
  valid$Residuals <- valid$Value - valid$Predicted
  residuals_df <- bind_rows(train[, c("Time", "Residuals")], valid[, c("Time", "Residuals")])
  
  res_min <- min(residuals_df$Residuals)
  res_max <- max(residuals_df$Residuals)
  res_pad <- (res_max - res_min) * 0.15
  
  p_resid <- ggplot(residuals_df, aes(x = Time, y = Residuals)) +
    geom_line(color = "black") +
    geom_vline(xintercept = df$Time[c(train_end, valid_end)], linetype = "dashed") +
    annotate("text", x = mean(train$Time), y = res_max + res_pad, label = "Training", size = 4, fontface = "bold") +
    annotate("text", x = mean(valid$Time), y = res_max + res_pad, label = "Validation", size = 4, fontface = "bold") +
    labs(title = paste("Residuals from Linear Model -", label),
         x = "Time", y = "Residuals") +
    ylim(res_min, res_max + res_pad * 1.5)
  
  grid.arrange(p_forecast, p_resid, nrow = 2)
}

# Plot individually for UK and US
plot_lm_model_with_labels_single(uk_ts, label = "UK")
plot_lm_model_with_labels_single(us_ts, label = "US")

sink()
