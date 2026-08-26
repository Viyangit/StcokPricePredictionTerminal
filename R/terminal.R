# Terminal-only report printer. No web UI.

TERM_WIDTH <- 72L

term_line <- function(char = "=", width = TERM_WIDTH) {
  paste(rep(char, width), collapse = "")
}

term_print <- function(...) {
  cat(..., sep = "")
  cat("\n")
}

read_symbol_line <- function(prompt) {
  if (interactive()) {
    return(trimws(readline(prompt)))
  }
  cat(prompt)
  flush.console()
  line <- tryCatch(
    readLines("stdin", n = 1L, warn = FALSE),
    error = function(e) character(0)
  )
  if (length(line) == 0) {
    return("")
  }
  trimws(as.character(line[[1]]))
}

print_banner <- function() {
  term_print(term_line("="))
  term_print("  AI STOCK PRICE PREDICTION SYSTEM")
  term_print("  Next-day close forecast  |  Random Forest  |  Alpha Vantage")
  term_print(term_line("="))
  term_print("")
}

print_error_banner <- function(message) {
  term_print("")
  term_print(term_line("-"))
  term_print("  ERROR")
  term_print(paste0("  ", message))
  term_print(term_line("-"))
  term_print("")
}

cli_progress <- function(value, message) {
  pct <- max(0, min(100, round(as.numeric(value) * 100)))
  cat(sprintf("  [%3d%%] %s\n", pct, message))
  flush.console()
}

kv_row <- function(label, value, label_width = 30L) {
  sprintf("  %-*s %s", label_width, paste0(label, ":"), value)
}

downsample <- function(x, width) {
  n <- length(x)
  if (n <= width) {
    return(x)
  }
  x[round(seq(1, n, length.out = width))]
}

print_ascii_chart <- function(values, height = 8L, width = 56L, title = NULL) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) < 2L) {
    return(invisible(NULL))
  }
  if (!is.null(title)) {
    term_print(paste0("  ", title))
  }

  y <- downsample(values, width)
  n <- length(y)
  ymin <- min(y)
  ymax <- max(y)
  if (!is.finite(ymin) || !is.finite(ymax)) {
    return(invisible(NULL))
  }
  if (ymax <= ymin) {
    ymax <- ymin + 1
  }

  scaled <- pmax(1L, pmin(height, as.integer(round((y - ymin) / (ymax - ymin) * (height - 1)) + 1L)))
  grid <- matrix(" ", nrow = height, ncol = n)
  for (i in seq_len(n)) {
    grid[height - scaled[[i]] + 1L, i] <- "*"
  }

  for (row in seq_len(height)) {
    axis_val <- ymax - (row - 1) * (ymax - ymin) / (height - 1)
    term_print(sprintf("  %10s |%s", formatC(axis_val, format = "f", digits = 2), paste(grid[row, ], collapse = "")))
  }
  term_print(paste0("             +", paste(rep("-", n), collapse = "")))
}

print_ascii_compare_chart <- function(actual, predicted, height = 8L, width = 56L, title = NULL) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  actual <- actual[ok]
  predicted <- predicted[ok]
  if (length(actual) < 2L) {
    return(invisible(NULL))
  }
  if (!is.null(title)) {
    term_print(paste0("  ", title))
    term_print("  Legend: A = actual close   P = predicted close   X = overlap")
  }

  if (length(actual) > width) {
    idx <- round(seq(1, length(actual), length.out = width))
    actual <- actual[idx]
    predicted <- predicted[idx]
  }
  n <- length(actual)
  ymin <- min(c(actual, predicted))
  ymax <- max(c(actual, predicted))
  if (ymax <= ymin) {
    ymax <- ymin + 1
  }

  scale_y <- function(v) {
    pmax(1L, pmin(height, as.integer(round((v - ymin) / (ymax - ymin) * (height - 1)) + 1L)))
  }
  sa <- scale_y(actual)
  sp <- scale_y(predicted)
  grid <- matrix(" ", nrow = height, ncol = n)
  for (i in seq_len(n)) {
    ra <- height - sa[[i]] + 1L
    rp <- height - sp[[i]] + 1L
    if (ra == rp) {
      grid[ra, i] <- "X"
    } else {
      grid[ra, i] <- "A"
      grid[rp, i] <- "P"
    }
  }

  for (row in seq_len(height)) {
    axis_val <- ymax - (row - 1) * (ymax - ymin) / (height - 1)
    term_print(sprintf("  %10s |%s", formatC(axis_val, format = "f", digits = 2), paste(grid[row, ], collapse = "")))
  }
  term_print(paste0("             +", paste(rep("-", n), collapse = "")))
}

print_recent_table <- function(ohlcv, n = APP_CONFIG$recent_table_rows) {
  tbl <- ohlcv[order(ohlcv$Date, decreasing = TRUE), , drop = FALSE]
  tbl <- utils::head(tbl, n)
  term_print("  RECENT HISTORICAL PRICES")
  term_print(sprintf(
    "  %-12s %12s %12s %12s %12s %14s",
    "Date", "Open", "High", "Low", "Close", "Volume"
  ))
  term_print(paste0("  ", paste(rep("-", 78), collapse = "")))
  for (i in seq_len(nrow(tbl))) {
    term_print(sprintf(
      "  %-12s %12.2f %12.2f %12.2f %12.2f %14.0f",
      as.character(tbl$Date[[i]]),
      tbl$Open[[i]],
      tbl$High[[i]],
      tbl$Low[[i]],
      tbl$Close[[i]],
      tbl$Volume[[i]]
    ))
  }
}

print_prediction_report <- function(res) {
  currency <- res$currency
  mae <- res$metrics$mae
  mae_pct <- if (is.finite(res$current_price) && res$current_price != 0) {
    (mae / res$current_price) * 100
  } else {
    NA_real_
  }
  reliability <- forecast_reliability(res$metrics$directional_accuracy, mae_pct)
  range_low <- res$predicted_price - mae
  range_high <- res$predicted_price + mae

  term_print("")
  term_print(term_line("="))
  term_print(sprintf(
    "  STOCK: %-12s  Latest trading date: %s  Direction: %s",
    res$symbol,
    as.character(res$latest_date),
    direction_display(res$direction)
  ))
  term_print(term_line("="))
  term_print(kv_row("Current price", format_price(res$current_price, currency)))
  term_print(kv_row("Predicted next-day price", format_price(res$predicted_price, currency)))
  term_print(kv_row("Expected price change", format_change(res$expected_change, currency)))
  term_print(kv_row("Expected percentage change", format_percent(res$expected_change_percent)))
  term_print(term_line("-"))
  term_print("  NEXT-DAY ACCURACY AND MODEL PERFORMANCE")
  term_print("  Tomorrow's close is not known yet. Metrics use the unseen chronological test set.")
  term_print("  Typical error around the next-day forecast is predicted price +/- MAE.")
  term_print("")
  term_print(kv_row("Direction accuracy", paste0(format_metric(res$metrics$directional_accuracy * 100, 1), "%")))
  term_print(kv_row("Typical error (MAE)", format_price(mae, currency)))
  term_print(kv_row(
    "Expected range",
    paste(format_price(range_low, currency), "-", format_price(range_high, currency))
  ))
  term_print(kv_row("Reliability", reliability))
  term_print(kv_row("RMSE", format_metric(res$metrics$rmse)))
  term_print(kv_row("R-squared", format_metric(res$metrics$r_squared, 3)))
  term_print(kv_row("MAPE", paste0(format_metric(res$metrics$mape, 2), "%")))
  term_print(term_line("-"))
  print_ascii_chart(res$ohlcv$Close, title = "HISTORICAL CLOSING PRICE")
  term_print("")
  if (!is.null(res$featured) && "MA_20" %in% names(res$featured)) {
    print_ascii_chart(res$featured$MA_20, title = "20-DAY MOVING AVERAGE")
    term_print("")
  }
  if (!is.null(res$test_compare) && nrow(res$test_compare) > 1) {
    print_ascii_compare_chart(
      res$test_compare$Actual,
      res$test_compare$Predicted,
      title = "ACTUAL VS PREDICTED (test set)"
    )
    term_print("")
  }
  term_print(term_line("-"))
  print_recent_table(res$ohlcv)
  term_print(term_line("="))
  term_print(paste0("  ", DISCLAIMER_TEXT))
  term_print(term_line("="))
  term_print("")
}
