##############################################################
# ANA 535 – Course Project: UK vs US Industrial Production Forecasting
# ------------------------------------------------------------
# Author(s): Naresh Anbazhagan, Shanmugapriya Mohankumar
# Description:
#   - Compares UK and US industrial production trends
#   - Uses ARIMA, ETS, STL, Linear Regression, SARIMA (Python cross-validation)
#   - Integrates logging and plot export with helper methods
#
# Environment: R 4.x on macOS, RStudio
##############################################################

# ======================
# Required Libraries
# ======================

reqd_pkgs <- c(
  "readxl", "tidyverse", "ggplot2", "forecast", "tseries", 
  "gridExtra", "dplyr", "grid"
)

installed <- rownames(installed.packages())
to_install <- setdiff(reqd_pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(reqd_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
if (!requireNamespace("ggplotify", quietly = TRUE)) {
  install.packages("ggplotify")
}
library(ggplotify)
cat("All libraries successfully loaded.\n\n")


# ======================
# Working Directory Setup
# ======================

# Commented out: Mac-specific local path
# setwd("/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/CourseProject")

setwd("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/")
# Create output folders regardless of OS
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)


# ======================
# Helper Functions
# ======================

save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  png(file = paste0("output/plots/", filename), width = width, height = height, res = res)
  print(expr)
  dev.off()
  invisible(NULL)
}

# Use absolute path for logging (Windows)
start_log <- function(filename = "C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UKUS_output.txt") {
  while (sink.number() > 0) sink(NULL)
  sink(file = filename, append = FALSE, split = TRUE)
  start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Course Project Execution Started: ", start_time, " =====\n\n")
}

end_log <- function() {
  end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Course Project Execution Ended: ", end_time, " =====\n\n")
  sink()
}

log_section <- function(title) {
  cat("\n\n===== ", title, " =====\n\n")
}

reset_par <- function() {
  par(mfrow = c(1, 1))
}

# ======================
# Start Logging
# ======================
start_log()
log_section("Step 1: Data Import and Setup")


# ======================
# Step 1: Load Excel and Plot Raw Data
# ======================

log_section("Step 1: Load Excel and Plot Raw Data")

# Load the Excel file
data <- read_excel("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UK_Comp_US_GBRPROINDMISMEI.xlsx")

# View the structure and summary
str(data)
summary(data)

# Convert date column and check for NAs
data$DATE <- as.Date(data$DATE)
cat("Number of missing values:", sum(is.na(data)), "\n")

# Time Series Plot: UK vs US
ts_plot <- ggplot(data, aes(x = DATE)) +
  geom_line(aes(y = GBRPROINDMISMEI, color = "UK")) +
  geom_line(aes(y = USAPROINDMISMEI, color = "US")) +
  labs(title = "Industrial Production Index (UK vs US)",
       x = "Date", y = "Index Value", color = "Country") +
  theme_minimal()

# Display plot
print(ts_plot)

# Save plot
save_plot("uk_us_index_plot.png", ts_plot)

# Summary Statistics
summary_stats <- data %>%
  select(-DATE) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max, median = median), na.rm = TRUE)

print(summary_stats)

# Rename columns for clarity
colnames(summary_stats) <- c(
  "UK_Mean", "US_Mean",
  "UK_SD", "US_SD",
  "UK_Min", "US_Min",
  "UK_Max", "US_Max",
  "UK_Median", "US_Median"
)

print("Renamed summary statistics:")
print(summary_stats)

# ======================
# Step 2: Summary Table, Time Series Setup, and EDA Plots
# ======================

log_section("Step 2: Summary Table, Time Series Setup, and EDA Plots")

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

# Run Sequence Plots
save_plot("uk_run_sequence.png", plot.ts(uk_ts, main = "UK Run Sequence Plot", ylab = "UK Index"))
save_plot("us_run_sequence.png", plot.ts(us_ts, main = "US Run Sequence Plot", ylab = "US Index"))

# Lag Plots
save_plot("uk_lag_plot.png", lag.plot(uk_ts, lags = 1, main = "UK Lag Plot"))
save_plot("us_lag_plot.png", lag.plot(us_ts, lags = 1, main = "US Lag Plot"))

# Histograms
save_plot("uk_histogram.png", hist(uk_ts, breaks = 30, main = "Histogram of UK Index", xlab = "UK Index", col = "lightblue"))
save_plot("us_histogram.png", hist(us_ts, breaks = 30, main = "Histogram of US Index", xlab = "US Index", col = "lightgreen"))
# ======================
# Step 3: Q-Q Plots, Composite Diagnostics, Correlation, ADF
# ======================

log_section("Step 3: Q-Q Plots, Composite Diagnostics, Correlation, ADF")

# Q-Q Plot – UK
save_plot("uk_qq_plot.png", {
  qqnorm(uk_ts, main = "Normal Q-Q Plot - UK")
  qqline(uk_ts)
})

# Q-Q Plot – US
save_plot("us_qq_plot.png", {
  qqnorm(us_ts, main = "Normal Q-Q Plot - US")
  qqline(us_ts)
})

# === 2x2 Composite Diagnostic Plots (UK) ===

# Run Sequence Plot
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

# Histogram
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

# Save composite diagnostic grid
save_plot("uk_diagnostic_composite.png", grid.arrange(p1, p2, p3, p4, ncol = 2), width = 1000, height = 800)

# Correlation between UK and US
cor_val <- cor(data$GBRPROINDMISMEI, data$USAPROINDMISMEI, use = "complete.obs")
cat("Correlation between UK and US Production Indices:", cor_val, "\n")

# ADF Tests
uk_adf <- adf.test(uk_ts)
us_adf <- adf.test(us_ts)

cat("\nUK ADF Test:\n")
print(uk_adf)

cat("\nUS ADF Test:\n")
print(us_adf)


# ======================
# Step 4: EDA for UKIndexOfProduction (by Sector)
# ======================

log_section("Step 4: EDA for UKIndexOfProduction – Sectoral Trends")

# Load required packages
library(tidyverse)
library(readr)

# Load the CSV, skip metadata
data <- read_csv("C:/Users/priya/OneDrive/Documents/personal/Mcdaniel/4sem-Mcdaniel/course8-Forecasting/Courseproject/UKIndexOfProduction-15Aug2024.csv", skip = 6)

# Inspect and clean
head(data)
colnames(data)[1] <- "Year"
data$Year <- as.numeric(data$Year)
data <- data[!is.na(data$Year), ]

# Select relevant columns for analysis
selected_data <- data %>% select(Year, 2:5)

# ==== Trend Line Plot ====
selected_data_long <- pivot_longer(selected_data, -Year, names_to = "Sector", values_to = "Value")

trend_plot <- ggplot(selected_data_long, aes(x = Year, y = as.numeric(Value), color = Sector)) +
  geom_line() +
  labs(title = "Production Index by Sector", x = "Year", y = "Value (£m)") +
  theme_minimal()

print(trend_plot)
save_plot("sector_trend_plot.png", trend_plot)

# ==== Summary Statistics ====
summary_stats <- selected_data %>%
  summarise(across(.cols = where(is.numeric), list(mean = mean, sd = sd, median = median, min = min, max = max), na.rm = TRUE))

print(summary_stats)

# Prepare labeled summary table
summary_matrix <- t(as.matrix(summary_stats))
colnames_matrix <- c("Mean", "Std Dev", "Median", "Min", "Max")

summary_table_sectors_df <- data.frame(
  Sector = names(selected_data)[-1],
  Mean = round(summary_matrix[1, ], 2),
  `Std Dev` = round(summary_matrix[2, ], 2),
  Median = round(summary_matrix[3, ], 2),
  Min = round(summary_matrix[4, ], 2),
  Max = round(summary_matrix[5, ], 2)
)

print("Summary Table by Sector:")
print(summary_table_sectors_df)

# ==== Histograms (Faceted) ====
hist_plot <- selected_data %>%
  pivot_longer(-Year, names_to = "Sector", values_to = "Value") %>%
  ggplot(aes(x = as.numeric(Value))) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~Sector, scales = "free") +
  labs(title = "Distribution of Sectoral Production Values", x = "Value (£m)", y = "Count") +
  theme_minimal()

print(hist_plot)
save_plot("sector_histograms.png", hist_plot)

# ==== Q-Q Plots (One per Sector) ====
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
  
  print(p)
  save_plot(paste0("qqplot_", col_name, ".png"), p)
}


# ======================
# Step 5: Confirmatory Data Analysis and Modeling
# ======================

log_section("Step 5: Confirmatory Data Analysis and Modeling")

# --- Differencing and ADF Tests ---
uk_diff <- diff(uk_ts, differences = 1)
us_diff <- diff(us_ts, differences = 1)

cat("ADF Test – UK Differenced:\n")
print(adf.test(uk_diff))

cat("ADF Test – US Differenced:\n")
print(adf.test(us_diff))

# --- ARIMA Models ---
uk_arima <- auto.arima(uk_ts)
us_arima <- auto.arima(us_ts)

cat("UK ARIMA Summary:\n")
print(summary(uk_arima))

cat("US ARIMA Summary:\n")
print(summary(us_arima))

# Forecast ARIMA
uk_arima_fc <- forecast(uk_arima, h = 12)
us_arima_fc <- forecast(us_arima, h = 12)

# Plot and save ARIMA forecasts
p_uk_arima <- autoplot(uk_arima_fc) + ggtitle("UK ARIMA Forecast")
p_us_arima <- autoplot(us_arima_fc) + ggtitle("US ARIMA Forecast")

print(p_uk_arima)
save_plot("uk_arima_forecast.png", p_uk_arima)

print(p_us_arima)
save_plot("us_arima_forecast.png", p_us_arima)

# --- ETS Models ---
uk_ets <- ets(uk_ts, model = "ZZZ")
us_ets <- ets(us_ts, model = "ZZZ")

cat("UK ETS Summary:\n")
print(summary(uk_ets))

cat("US ETS Summary:\n")
print(summary(us_ets))

# Forecast ETS
uk_ets_fc <- forecast(uk_ets, h = 12)
us_ets_fc <- forecast(us_ets, h = 12)

# Plot and save ETS forecasts
p_uk_ets <- autoplot(uk_ets_fc) + ggtitle("UK ETS Forecast")
p_us_ets <- autoplot(us_ets_fc) + ggtitle("US ETS Forecast")

print(p_uk_ets)
save_plot("uk_ets_forecast.png", p_uk_ets)

print(p_us_ets)
save_plot("us_ets_forecast.png", p_us_ets)

# --- Accuracy Comparison ---
cat("Accuracy – UK ARIMA:\n")
print(accuracy(uk_arima_fc))

cat("Accuracy – UK ETS:\n")
print(accuracy(uk_ets_fc))

cat("Accuracy – US ARIMA:\n")
print(accuracy(us_arima_fc))

cat("Accuracy – US ETS:\n")
print(accuracy(us_ets_fc))

# --- Residual Diagnostics ---
save_plot("uk_arima_residuals.png", checkresiduals(uk_arima))
save_plot("us_arima_residuals.png", checkresiduals(us_arima))

save_plot("uk_ets_residuals.png", checkresiduals(uk_ets))
save_plot("us_ets_residuals.png", checkresiduals(us_ets))

# --- STL Decomposition Plots ---
p_uk_stl <- autoplot(stl(uk_ts, s.window = "periodic")) + ggtitle("UK STL Decomposition")
p_us_stl <- autoplot(stl(us_ts, s.window = "periodic")) + ggtitle("US STL Decomposition")

print(p_uk_stl)
save_plot("uk_stl_decomposition.png", p_uk_stl)

print(p_us_stl)
save_plot("us_stl_decomposition.png", p_us_stl)


# ======================
# Step 6: Time Series Linear Regression Forecast – UK and US
# ======================

log_section("Step 6: Time Series Linear Regression Forecast – UK and US")

# --- Updated Plot Function with Return Value ---
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
  
  # Combine and return
  arranged_plot <- grid.arrange(p_forecast, p_resid, nrow = 2)
  return(arranged_plot)
}

# --- Generate and Save Plots ---

# UK
uk_lm_plot <- plot_lm_model_with_labels_single(uk_ts, label = "UK")
ggsave("output/plots/uk_linear_model_forecast.png", plot = as.ggplot(uk_lm_plot), width = 10, height = 8, dpi = 300)

# US
us_lm_plot <- plot_lm_model_with_labels_single(us_ts, label = "US")
ggsave("output/plots/us_linear_model_forecast.png", plot = as.ggplot(us_lm_plot), width = 10, height = 8, dpi = 300)

# Validate number of plots saved
plot_dir <- "output/plots/"

if (dir.exists(plot_dir)) {
  plot_files <- list.files(path = plot_dir, pattern = "\\.png$", full.names = TRUE)
  cat("Number of PNG plots found in", plot_dir, ":", length(plot_files), "\n")
  print(basename(plot_files))  # Optional: print filenames
} else {
  cat("Plot directory not found:", plot_dir, "\n")
}


end_log()