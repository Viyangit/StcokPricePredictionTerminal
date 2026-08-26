# Historical market data from the Alpha Vantage TIME_SERIES_DAILY API.

REQUIRED_OHLCV_COLS <- c("Date", "Open", "High", "Low", "Close", "Volume")

#' Fetch daily OHLCV from Alpha Vantage TIME_SERIES_DAILY.
#'
#' @param symbol Stock ticker (for example AAPL or RELIANCE.BSE).
#' @param api_key Alpha Vantage API key (never logged or returned).
#' @param outputsize "full" or "compact". Compact is the free endpoint (~100 days).
#' @return A data.frame with Date, Open, High, Low, Close, Volume sorted by Date.
get_stock_data <- function(symbol, api_key, outputsize = "full") {
  symbol <- normalize_symbol(symbol)
  if (!nzchar(api_key)) {
    stop_app("Alpha Vantage API key is not configured.")
  }
  if (!outputsize %in% c("full", "compact")) {
    outputsize <- "full"
  }

  resp <- tryCatch(
    {
      av_url <- paste0(
        "https://www.alphavantage.co/query?",
        "function=TIME_SERIES_DAILY",
        "&symbol=", utils::URLencode(symbol, reserved = TRUE),
        "&outputsize=", outputsize,
        "&datatype=json",
        "&apikey=", utils::URLencode(api_key, reserved = TRUE)
      )
      httr2::request(av_url) |>
        httr2::req_user_agent("StockPredictionCLI/1.0 (R educational app)") |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_timeout(60) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      stop_app("Unable to retrieve historical data.")
    }
  )

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop_app("Unable to retrieve historical data.")
  }

  payload <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop_app("Unable to retrieve historical data.")
    }
  )

  mapped <- map_alpha_vantage_error(payload)
  if (!is.null(mapped)) {
    stop_app(mapped)
  }

  series <- payload[["Time Series (Daily)"]]
  if (is.null(series) || length(series) == 0) {
    stop_app("Stock symbol not found.")
  }

  json_chr <- function(rec, key) {
    val <- rec[[key]]
    if (is.null(val) || length(val) == 0) {
      return(NA_character_)
    }
    as.character(val[[1]])
  }

  dates <- names(series)
  rows <- lapply(dates, function(d) {
    rec <- series[[d]]
    data.frame(
      Date = d,
      Open = json_chr(rec, "1. open"),
      High = json_chr(rec, "2. high"),
      Low = json_chr(rec, "3. low"),
      Close = json_chr(rec, "4. close"),
      Volume = json_chr(rec, "5. volume"),
      stringsAsFactors = FALSE
    )
  })

  raw <- dplyr::bind_rows(rows)
  out <- clean_ohlcv(raw)
  out
}

#' Clean and type-convert an OHLCV table. Does not enforce the minimum-row rule.
clean_ohlcv <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    stop_app("Unable to retrieve historical data.")
  }

  missing_cols <- setdiff(REQUIRED_OHLCV_COLS, names(df))
  if (length(missing_cols) > 0) {
    stop_app("Unable to retrieve historical data.")
  }

  out <- df[, REQUIRED_OHLCV_COLS, drop = FALSE]
  out$Date <- safe_as_date(out$Date)
  out$Open <- safe_as_numeric(out$Open)
  out$High <- safe_as_numeric(out$High)
  out$Low <- safe_as_numeric(out$Low)
  out$Close <- safe_as_numeric(out$Close)
  out$Volume <- safe_as_numeric(out$Volume)

  out <- out[!is.na(out$Date), , drop = FALSE]
  valid_price <- is.finite(out$Open) & is.finite(out$High) & is.finite(out$Low) &
    is.finite(out$Close) &
    out$Open > 0 & out$High > 0 & out$Low > 0 & out$Close > 0
  out <- out[valid_price, , drop = FALSE]

  valid_volume <- is.finite(out$Volume) & out$Volume >= 0
  out <- out[valid_volume, , drop = FALSE]

  out <- out[!duplicated(out$Date), , drop = FALSE]
  out <- out[order(out$Date), , drop = FALSE]
  rownames(out) <- NULL

  if (nrow(out) == 0) {
    stop_app("Unable to retrieve historical data.")
  }

  out
}

#' Validate cleaned OHLCV against modeling requirements.
validate_ohlcv <- function(data) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  missing_cols <- setdiff(REQUIRED_OHLCV_COLS, names(data))
  if (length(missing_cols) > 0) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  if (nrow(data) < APP_CONFIG$min_observations) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  if (any(duplicated(data$Date))) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  if (is.unsorted(data$Date, strictly = TRUE)) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  prices <- c(data$Open, data$High, data$Low, data$Close)
  if (any(!is.finite(prices) | prices <= 0)) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  if (any(!is.finite(data$Volume) | data$Volume < 0)) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  invisible(TRUE)
}

ohlcv_is_sufficient <- function(data) {
  is.data.frame(data) && nrow(data) >= APP_CONFIG$min_observations
}

should_retry_compact <- function(av_error) {
  if (is.null(av_error) || !nzchar(av_error)) {
    return(TRUE)
  }
  if (grepl("API request limit reached", av_error, ignore.case = TRUE)) {
    return(FALSE)
  }
  if (grepl("not found", av_error, ignore.case = TRUE)) {
    return(FALSE)
  }
  if (grepl("not configured", av_error, ignore.case = TRUE)) {
    return(FALSE)
  }
  TRUE
}

#' Fetch TIME_SERIES_DAILY from Alpha Vantage only.
#' Tries outputsize=full, then compact if full history is unavailable.
fetch_historical_data <- function(symbol) {
  symbol <- normalize_symbol(symbol)
  api_key <- get_alpha_vantage_api_key()

  av_error <- NULL
  av_data <- tryCatch(
    get_stock_data(symbol, api_key, outputsize = "full"),
    error = function(e) {
      av_error <<- if (is_stock_app_error(e)) conditionMessage(e) else "Unable to retrieve historical data."
      NULL
    }
  )

  if (ohlcv_is_sufficient(av_data)) {
    validate_ohlcv(av_data)
    return(list(data = av_data, source = "Alpha Vantage"))
  }

  compact_error <- NULL
  compact_data <- NULL
  if (should_retry_compact(av_error)) {
    Sys.sleep(1.2)
    compact_data <- tryCatch(
      get_stock_data(symbol, api_key, outputsize = "compact"),
      error = function(e) {
        compact_error <<- if (is_stock_app_error(e)) conditionMessage(e) else "Unable to retrieve historical data."
        NULL
      }
    )
    if (ohlcv_is_sufficient(compact_data)) {
      validate_ohlcv(compact_data)
      return(list(data = compact_data, source = "Alpha Vantage"))
    }
  }

  stop_app(choose_alpha_vantage_failure_message(av_data, av_error, compact_data, compact_error))
}

choose_alpha_vantage_failure_message <- function(av_data, av_error, compact_data = NULL, compact_error = NULL) {
  if (!is.null(av_error) && identical(av_error, "premium_full_history_required") &&
      (is.null(compact_data) || nrow(compact_data) < APP_CONFIG$min_observations)) {
    return("Insufficient historical data to train the prediction model.")
  }
  if (!is.null(av_error) && grepl("API request limit reached", av_error, ignore.case = TRUE)) {
    return("API request limit reached. Please try again later.")
  }
  msgs <- c(av_error, compact_error)
  if (any(grepl("not found", msgs, ignore.case = TRUE))) {
    return("Stock symbol not found.")
  }
  if ((!is.null(av_data) && nrow(av_data) < APP_CONFIG$min_observations) ||
      (!is.null(compact_data) && nrow(compact_data) < APP_CONFIG$min_observations)) {
    return("Insufficient historical data to train the prediction model.")
  }
  if (!is.null(av_error) && nzchar(av_error) && av_error %in% KNOWN_USER_MESSAGES) {
    return(av_error)
  }
  if (!is.null(compact_error) && nzchar(compact_error) && compact_error %in% KNOWN_USER_MESSAGES) {
    return(compact_error)
  }
  "Unable to retrieve historical data."
}
