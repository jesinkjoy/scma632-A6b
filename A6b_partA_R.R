# Load necessary libraries
library(quantmod)
library(rugarch)
library(tseries)

# Get the data for Apple
ticker <- "AAPL"

# Download the data
getSymbols(ticker, from = "2021-01-01", to = "2024-01-01")
data <- get(ticker)

# Calculate log returns to represent volatility
data$Returns <- diff(log(Cl(data)))
data <- na.omit(data)

# Fit an ARCH model
spec_arch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), 
                        mean.model = list(armaOrder = c(0, 0)), 
                        distribution.model = "norm")
fit_arch <- ugarchfit(spec = spec_arch, data = data$Returns)
print(fit_arch)

# Plot the conditional volatility for ARCH model
plot(fit_arch, which = 3)  # 3 is for conditional sigma (volatility)

# Fit a GARCH model
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0)), 
                         distribution.model = "norm")
fit_garch <- ugarchfit(spec = spec_garch, data = data$Returns)
print(fit_garch)

# Plot the conditional volatility for GARCH model
plot(fit_garch, which = 3)  

# Fit a GARCH model for forecasting
spec_forecast <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                            mean.model = list(armaOrder = c(0, 0)), 
                            distribution.model = "norm")
fit_forecast <- ugarchfit(spec = spec_forecast, data = 100 * data$Returns)

# Forecasting
forecasts <- ugarchforecast(fit_forecast, n.ahead = 90)

# Print forecast results
print(fitted(forecasts))
print(sigma(forecasts))

# Extract forecasted conditional volatility and plot
forecasted_volatility <- sigma(forecasts)
plot(forecasted_volatility, type = "l", main = "Forecasted Conditional Volatility (GARCH)", xlab = "Time", ylab = "Volatility")
