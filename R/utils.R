# Application configuration and shared helpers.

APP_CONFIG <- list(
  min_observations = 80L,
  min_complete_rows = 40L,
  train_ratio = 0.8,
  ntree = 300L,
  seed = 42L,
  rsi_n = 14L,
  vol_window = 20L,
  macd_fast = 12L,
  macd_slow = 26L,
  macd_signal = 9L,
  neutral_pct = 0.05,
  recent_table_rows = 15L
)

DISCLAIMER_TEXT <- paste(
  "This application provides machine-learning-based forecasts for educational",
  "and research purposes only. It is not financial advice, an investment",
  "recommendation, or a guarantee of future stock prices. Actual market prices",
  "may differ substantially from model predictions."
)

KNOWN_USER_MESSAGES <- c(
  "Please enter a stock symbol.",
  "Alpha Vantage API key is not configured.",
  "Alpha Vantage API key is not configured. Save a real key in .Renviron (replace YOUR_API_KEY_HERE) and try again.",
  "Stock symbol not found.",
  "Unable to retrieve historical data.",
  "API request limit reached. Please try again later.",
  "Insufficient historical data to train the prediction model.",
  "Insufficient historical data for prediction.",
  "Prediction could not be generated."
)

#' Raise a user-facing application error (never a raw stack trace).
stop_app <- function(message) {
  err <- errorCondition(message, class = "stock_app_error")
  stop(err)
}

is_stock_app_error <- function(e) {
  inherits(e, "stock_app_error")
}

#' Map any caught error to a short, safe message.
user_facing_error <- function(e) {
  msg <- conditionMessage(e)
  if (identical(msg, "premium_full_history_required")) {
    return("Insufficient historical data to train the prediction model.")
  }
  if (is_stock_app_error(e)) {
    return(msg)
  }
  if (!is.null(msg) && msg %in% KNOWN_USER_MESSAGES) {
    return(msg)
  }
  "Prediction could not be generated."
}

normalize_symbol <- function(symbol) {
  if (is.null(symbol) || length(symbol) == 0) {
    return("")
  }
  toupper(trimws(as.character(symbol[[1]])))
}

validate_symbol_input <- function(symbol) {
  symbol <- normalize_symbol(symbol)
  if (!nzchar(symbol)) {
    stop_app("Please enter a stock symbol.")
  }
  if (!grepl("^[A-Z0-9][A-Z0-9._-]{0,31}$", symbol)) {
    stop_app("Please enter a stock symbol.")
  }
  symbol
}

find_renviron_path <- function() {
  candidates <- unique(c(
    file.path(getwd(), ".Renviron"),
    ".Renviron",
    file.path(dirname(normalizePath("app.R", mustWork = FALSE)), ".Renviron")
  ))
  for (path in candidates) {
    if (nzchar(path) && file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }
  }
  ""
}

load_app_renviron <- function() {
  path <- find_renviron_path()
  if (!nzchar(path) || !file.exists(path)) {
    return(invisible(FALSE))
  }
  suppressWarnings(readRenviron(path))
  invisible(TRUE)
}

get_alpha_vantage_api_key <- function() {
  load_app_renviron()
  key <- trimws(Sys.getenv("ALPHA_VANTAGE_API_KEY", unset = ""))
  if (identical(key, "YOUR_API_KEY_HERE")) {
    stop_app("Alpha Vantage API key is not configured. Save a real key in .Renviron (replace YOUR_API_KEY_HERE) and try again.")
  }
  if (!nzchar(key)) {
    stop_app("Alpha Vantage API key is not configured.")
  }
  key
}

#' Classify Alpha Vantage JSON error keys into a friendly message.
#' Returns NULL when the payload looks like a successful time series.
map_alpha_vantage_error <- function(payload) {
  if (!is.list(payload)) {
    return("Unable to retrieve historical data.")
  }

  if (!is.null(payload[["Time Series (Daily)"]]) &&
      length(payload[["Time Series (Daily)"]]) > 0) {
    return(NULL)
  }

  note <- payload[["Note"]]
  information <- payload[["Information"]]
  error_message <- payload[["Error Message"]]

  combined <- paste(c(note, information, error_message), collapse = " ")

  if (is_premium_only_text(combined)) {
    return("premium_full_history_required")
  }

  if (is_rate_limit_text(combined)) {
    return("API request limit reached. Please try again later.")
  }

  if (!is.null(error_message) && nzchar(error_message)) {
    return("Stock symbol not found.")
  }

  if (!is.null(information) && nzchar(information)) {
    return("Unable to retrieve historical data.")
  }

  "Unable to retrieve historical data."
}

is_rate_limit_text <- function(text) {
  grepl("25 requests|1 request per second|call frequency|spread out your free API|higher API call frequency",
        text, ignore.case = TRUE)
}

is_premium_only_text <- function(text) {
  grepl("premium|subscribe|outputsize=full", text, ignore.case = TRUE)
}

infer_currency <- function(symbol) {
  symbol <- normalize_symbol(symbol)
  if (grepl("\\.(BSE|NSE|NS|BO)$", symbol)) {
    return("INR")
  }
  if (grepl("\\.LON$", symbol)) {
    return("GBP")
  }
  "USD"
}

currency_prefix <- function(currency) {
  switch(currency,
    USD = "$",
    INR = "Rs ",
    GBP = "GBP ",
    "$"
  )
}

format_price <- function(x, currency = "USD") {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("--")
  }
  paste0(
    currency_prefix(currency),
    formatC(as.numeric(x[[1]]), format = "f", digits = 2, big.mark = ",")
  )
}

format_change <- function(x, currency = "USD") {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("--")
  }
  value <- as.numeric(x[[1]])
  sign_char <- if (value > 0) "+" else ""
  paste0(sign_char, format_price(value, currency))
}

format_percent <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("--")
  }
  value <- as.numeric(x[[1]])
  sign_char <- if (value > 0) "+" else ""
  paste0(sign_char, formatC(value, format = "f", digits = digits), "%")
}

format_metric <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || !is.finite(x[[1]])) {
    return("--")
  }
  formatC(as.numeric(x[[1]]), format = "f", digits = digits)
}

prediction_direction <- function(predicted_price, current_price, neutral_pct = APP_CONFIG$neutral_pct) {
  if (!is.finite(predicted_price) || !is.finite(current_price) || current_price == 0) {
    return("NEUTRAL")
  }
  pct <- abs((predicted_price - current_price) / current_price) * 100
  if (pct < neutral_pct) {
    return("NEUTRAL")
  }
  if (predicted_price > current_price) {
    return("UP")
  }
  "DOWN"
}

direction_display <- function(direction) {
  switch(direction,
    UP = "UP",
    DOWN = "DOWN",
    "NEUTRAL"
  )
}

forecast_reliability <- function(directional_accuracy, mae_pct) {
  if (!is.finite(directional_accuracy) || !is.finite(mae_pct)) {
    return("UNKNOWN")
  }
  if (directional_accuracy >= 0.60 && mae_pct < 3) {
    return("HIGH")
  }
  if (directional_accuracy >= 0.52 || mae_pct < 5) {
    return("MODERATE")
  }
  "LOW"
}

safe_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(x)
}

safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}
