
# 1. Data Loading

data(AirPassengers)
ts_data <- AirPassengers
plot(ts_data)

# 2. Trend Analysis

trend_model <- lm(ts_data ~ time(ts_data))
summary(trend_model)

abline(trend_model, col="blue")

# 3. T-test

early <- window(ts_data, end=c(1954,12))
late <- window(ts_data, start=c(1955,1))

t.test(early, late)


# 4. Multiple Linear regression

month_factor <- as.factor(cycle(ts_data))
multi_model <- lm(ts_data ~ time(ts_data) + month_factor)

summary(multi_model)

# 5. Polynomial regression

poly_model <- lm(ts_data ~ poly(time(ts_data), 2))
summary(poly_model)

# 6. Non-Linear / Exponential Model

log_model <- lm(log(ts_data) ~ time(ts_data))
summary(log_model)

# 7. Moving Average

install.packages("forecast")

library(forecast)
ma <- ma(ts_data, order=12)
plot(ts_data)
lines(ma, col="blue")

# 8. Seasonal Indices

decomp <- decompose(ts_data)
plot(decomp)

seasonal_index <- decomp$seasonal

# 9. ACF and PACF

acf(ts_data)
pacf(ts_data)

# 10. Stationarity Test

install.packages("tseries")
library(tseries)
adf.test(ts_data)

# 11. ARIMA Model

fit <- auto.arima(ts_data)
summary(fit)

forecast_values <- forecast(fit, h=12)
plot(forecast_values)