library(dplyr)

# Citation: Karamolegkos, S., & Koulouriotis, D. E. (2025).
# Advancing short-term load forecasting with decomposed Fourier ARIMA:
# A case study on the Greek energy market. Energy, 325, 135854.
# https://doi.org/10.1016/j.energy.2025.135854

farima <- function(data, h = 365, include_periodic = TRUE) {
  x <- data$value
  n <- length(x)

  # Step 1: Fourier Transform for Periodic Components
  u <- fft(as.numeric(x))

  # Step 2: Identifying Significant Frequencies
  mag <- Mod(u)
  alpha <- 0.2
  threshold <- alpha * max(mag)
  sig_idx <- which(mag > threshold)
  u_sig <- rep(0 + 0i, length(u))
  u_sig[sig_idx] <- u[sig_idx]
  p <- Re(fft(u_sig, inverse = TRUE) / length(u_sig))

  # Step 3: Removing Periodic Components
  y_prime <- x - p

  # Step 4: Trend Modeling
  tr <- lm(y_prime ~ data$date)

  # Step 5: Removing the Trend
  r <- residuals(tr)

  # Step 6: ARIMA Modeling
  arima <- forecast::auto.arima(r)

  # Step 7: Forecasting Each Component
  last_date <- max(data$date)
  future_dates <- seq(last_date + 1, by = "day", length.out = h)

  # Step 7a: Forecasting the Periodic Component
  cycle_len <- n # align index of p with time series end
  p_extended <- rep(
    p,
    length.out = cycle_len * (ceiling((n + h) / cycle_len))
  )
  # take next h values following the end of the original series
  p_future <- p_extended[(n + 1):(n + h)]

  # Step 7b: Forecasting the Trend Component
  time_idx <- as.numeric(data$date)
  tr2 <- lm(y_prime ~ time_idx)
  trend_pred <- predict(
    tr2,
    newdata = data.frame(time_idx = as.numeric(future_dates))
  )

  # Step 7c: Forecasting the ARIMA Component
  resid_fc <- forecast::forecast(arima, h = h)
  r_future <- as.numeric(resid_fc$mean)

  # Step 8: Combining Forecasts
  y_future <- trend_pred + r_future
  if (include_periodic) {
    y_future <- y_future + p_future
  } else {
    p_mean <- rep(mean(p), h)
    y_future <- y_future + p_mean
  }

  # Return values
  xr <- if (include_periodic) x else y_prime + rep(mean(p), n)
  result <- data.frame(
    date = data$date, value = as.numeric(xr), forecasted = rep(FALSE, n)
  )
  future_result <- data.frame(
    date = future_dates, value = y_future, forecasted = rep(TRUE, h)
  )
  rbind(result, future_result)
}
