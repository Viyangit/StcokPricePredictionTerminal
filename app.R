# AI Stock Price Prediction System — terminal CLI (pure R).

required_packages <- c(
  "httr2",
  "dplyr",
  "TTR",
  "zoo",
  "randomForest"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing R packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(TTR)
  library(zoo)
  library(randomForest)
})

source("R/utils.R", local = FALSE)
source("R/data.R", local = FALSE)
source("R/features.R", local = FALSE)
source("R/model.R", local = FALSE)
source("R/terminal.R", local = FALSE)
load_app_renviron()

is_quit_command <- function(text) {
  toupper(trimws(text)) %in% c("Q", "QUIT", "EXIT")
}

run_symbol <- function(symbol) {
  out <- tryCatch(
    {
      validate_symbol_input(symbol)
      get_alpha_vantage_api_key()
      cat("\n")
      run_prediction_pipeline(symbol, cli_progress)
    },
    error = function(e) {
      list(error = user_facing_error(e))
    }
  )

  if (!is.null(out$error)) {
    print_error_banner(out$error)
    return(invisible(FALSE))
  }

  print_prediction_report(out)
  invisible(TRUE)
}

run_interactive <- function() {
  term_print("  Enter a stock symbol to forecast the next close.")
  term_print("  Examples: AAPL, MSFT, TSLA, IBM, GOOGL, AMZN, RELIANCE.BSE, TCS.BSE")
  term_print("  Type Q to quit.")
  term_print("")

  repeat {
    symbol <- read_symbol_line("  Stock symbol: ")
    if (!nzchar(symbol) && !interactive()) {
      break
    }
    if (is_quit_command(symbol)) {
      term_print("  Goodbye.")
      break
    }
    if (!nzchar(symbol)) {
      term_print("  Please enter a stock symbol.")
      next
    }
    run_symbol(symbol)
  }
}

main <- function() {
  print_banner()
  args <- commandArgs(trailingOnly = TRUE)
  args <- args[nzchar(args)]

  if (length(args) >= 1L) {
    run_symbol(args[[1]])
    return(invisible(NULL))
  }

  run_interactive()
}

main()
