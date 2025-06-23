#My working directory
setwd("D:/Research/Lightning strike study")


#Required libraries
library(readxl)
library(forecast)
library(tseries)
library(car)
library(ggplot2)
library(lmtest)

#Upload data
data<-read_excel("data_AHC.xlsx", sheet="lightning strike deaths data")

# Plot the deaths time series
ggplot(data, aes(x = Year, y = deaths)) +
  geom_line(color = "#b22222", size = 1.2) +                        # Bold firebrick line
  geom_point(color = "#b22222", size = 2.5) +                      # Emphasize data points
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  theme_minimal(base_size = 14) +                                  # Clean theme with larger text
  labs(
    title = "",
    x = "Year",
    y = "Number of Deaths"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#2c3e50"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Create time series of deaths
deaths_ts <- ts(data$deaths, start = 1990)

# Check multicollinearity using VIF
lm_model <- lm(deaths ~ max_temp + min_temp + mean_temp + humid + prep, data = data)
vif(lm_model)

# Augmented Dickey-Fuller Test (check stationarity)
adf.test(ts(data$deaths))

# 1st differencing
diff_deaths <- diff(data$deaths)

# Check stationarity again
adf.test(ts(diff_deaths))

#ACF and PACF checking
ggAcf(deaths_ts)
ggPacf(deaths_ts)

# Create xreg matrix with selected predictors
xreg <- as.matrix(data[, c("max_temp","min_temp", "prep")])

# Fit ARIMAX model
arimax_model <- auto.arima(deaths_ts, xreg = xreg, d=1,stepwise = F,trace=T)

# Model summary
summary(arimax_model)
coeftest(arimax_model)
confint(arimax_model)#confidence interval

# Check residuals
checkresiduals(arimax_model)

#Fitting ARIMA model for next 7 years forecasting
arima_model<-auto.arima(deaths_ts,max.q = 7, d=1,stepwise = F,trace=T)
summary (arima_model)

pred<-forecast(arima_model,h=7)

# Convert to data frame for custom ggplot
forecast_df <- data.frame(
  Year = as.numeric(time(pred$mean)),
  Forecast = as.numeric(pred$mean),
  Lo95 = as.numeric(pred$lower[, "95%"]),
  Hi95 = as.numeric(pred$upper[, "95%"])
)

# Actual data frame
actual_df <- data.frame(
  Year = as.numeric(time(deaths_ts)),
  Deaths = as.numeric(deaths_ts)
)

#plot
ggplot() +
  geom_line(data = actual_df, aes(x = Year, y = Deaths), color = "#2C3E50", size = 1) +
  geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lo95, ymax = Hi95), fill = "lightblue", alpha = 0.4) +
  geom_line(data = forecast_df, aes(x = Year, y = Forecast), color = "#0073C2FF", size = 1.2) +
  geom_point(data = forecast_df, aes(x = Year, y = Forecast), color = "#0073C2FF", size = 2) +
  ggtitle("") +
  xlab("Year") + ylab("Number of Deaths") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
