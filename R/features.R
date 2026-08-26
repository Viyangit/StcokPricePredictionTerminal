# Time-series feature engineering. Indicators use only current and past observations.

FEATURE_COLS <- c(
  "Open",
  "High",
  "Low",
  "Close",
  "Volume",
  "Previous_Close",
  "Previous_Open",
  "Previous_High",
  "Previous_Low",
  "Previous_Volume",
  "MA_5",
  "MA_10",
  "MA_20",
  "MA_50",
  "RSI",
  "MACD",
  "Daily_Return",
  "Volatility_20"
)

feature_column_names <- function() {
  FEATURE_COLS
}

#' Build model features and the next-day close target.
#'
#' Target_Close is lead(Close, 1): today's row predicts the next trading day's close.
#' Moving averages, RSI, and MACD are computed on the chronological Close series so
#' they never look ahead.
create_features <- function(data) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop_app("Insufficient historical data for prediction.")
  }

  data <- data[order(data$Date), , drop = FALSE]
  close <- as.numeric(data$Close)

  sma_or_na <- function(n) {
    as.numeric(TTR::SMA(close, n = n))
  }

  rsi_vals <- tryCatch(
    as.numeric(TTR::RSI(close, n = APP_CONFIG$rsi_n)),
    error = function(e) rep(NA_real_, length(close))
  )

  macd_vals <- tryCatch(
    {
      macd_mat <- TTR::MACD(
        close,
        nFast = APP_CONFIG$macd_fast,
        nSlow = APP_CONFIG$macd_slow,
        nSig = APP_CONFIG$macd_signal
      )
      as.numeric(macd_mat[, "macd"])
    },
    error = function(e) rep(NA_real_, length(close))
  )

  featured <- dplyr::mutate(
    data,
    Previous_Close = dplyr::lag(Close, 1),
    Previous_Open = dplyr::lag(Open, 1),
    Previous_High = dplyr::lag(High, 1),
    Previous_Low = dplyr::lag(Low, 1),
    Previous_Volume = dplyr::lag(Volume, 1),
    MA_5 = sma_or_na(5L),
    MA_10 = sma_or_na(10L),
    MA_20 = sma_or_na(20L),
    MA_50 = sma_or_na(50L),
    RSI = rsi_vals,
    MACD = macd_vals,
    Daily_Return = (Close - Previous_Close) / Previous_Close,
    Target_Close = dplyr::lead(Close, 1)
  )

  featured$Volatility_20 <- as.numeric(
    zoo::rollapply(
      as.numeric(featured$Daily_Return),
      width = APP_CONFIG$vol_window,
      FUN = function(x) stats::sd(x),
      fill = NA,
      align = "right",
      partial = FALSE
    )
  )

  featured
}

#' Split featured data into train/test frames plus the latest inference row.
prepare_model_frames <- function(featured) {
  feature_cols <- feature_column_names()
  needed_cols <- c("Date", feature_cols, "Target_Close")
  missing_cols <- setdiff(needed_cols, names(featured))
  if (length(missing_cols) > 0) {
    stop_app("Prediction could not be generated.")
  }

  featured <- featured[order(featured$Date), , drop = FALSE]
  latest <- featured[nrow(featured), , drop = FALSE]
  latest_x <- as.data.frame(latest[, feature_cols, drop = FALSE], stringsAsFactors = FALSE)

  if (anyNA(latest_x)) {
    stop_app("Prediction could not be generated.")
  }

  complete_idx <- stats::complete.cases(featured[, c(feature_cols, "Target_Close"), drop = FALSE])
  complete <- featured[complete_idx, , drop = FALSE]
  complete <- complete[order(complete$Date), , drop = FALSE]

  if (nrow(complete) < APP_CONFIG$min_complete_rows) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  split <- split_chronological(complete, APP_CONFIG$train_ratio)

  list(
    train_x = as.data.frame(split$train[, feature_cols, drop = FALSE], stringsAsFactors = FALSE),
    train_y = as.numeric(split$train$Target_Close),
    test_x = as.data.frame(split$test[, feature_cols, drop = FALSE], stringsAsFactors = FALSE),
    test_y = as.numeric(split$test$Target_Close),
    test_close = as.numeric(split$test$Close),
    test_dates = safe_as_date(split$test$Date),
    latest_x = latest_x,
    current_price = as.numeric(latest$Close[[1]]),
    latest_date = safe_as_date(latest$Date[[1]]),
    featured = featured,
    complete = complete
  )
}
