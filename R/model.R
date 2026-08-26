# Random forest training, chronological evaluation, and next-day prediction.

#' Oldest observations train, newest observations test. Never shuffled.
split_chronological <- function(featured, ratio = APP_CONFIG$train_ratio) {
  n <- nrow(featured)
  n_train <- floor(n * ratio)
  n_test <- n - n_train

  if (n_train < 25L || n_test < 8L) {
    stop_app("Insufficient historical data to train the prediction model.")
  }

  list(
    train = featured[seq_len(n_train), , drop = FALSE],
    test = featured[seq.int(n_train + 1L, n), , drop = FALSE]
  )
}

train_model <- function(training_features, training_target, ntree = APP_CONFIG$ntree) {
  training_features <- as.data.frame(training_features, stringsAsFactors = FALSE)
  training_target <- as.numeric(training_target)

  if (nrow(training_features) == 0L || length(training_target) == 0L) {
    stop_app("Prediction could not be generated.")
  }
  if (anyNA(training_features) || anyNA(training_target)) {
    stop_app("Prediction could not be generated.")
  }
  if (!is.finite(ntree) || ntree < 1) {
    ntree <- APP_CONFIG$ntree
  }

  model <- tryCatch(
    {
      set.seed(APP_CONFIG$seed)
      randomForest::randomForest(
        x = training_features,
        y = training_target,
        ntree = as.integer(ntree)
      )
    },
    error = function(e) {
      stop_app("Prediction could not be generated.")
    }
  )

  model
}

calculate_metrics <- function(actual, predicted, previous_close) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  previous_close <- as.numeric(previous_close)

  ok <- is.finite(actual) & is.finite(predicted) & is.finite(previous_close)
  actual <- actual[ok]
  predicted <- predicted[ok]
  previous_close <- previous_close[ok]

  if (length(actual) == 0L) {
    stop_app("Prediction could not be generated.")
  }

  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))

  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

  actual_dir <- sign(actual - previous_close)
  predicted_dir <- sign(predicted - previous_close)
  directional_accuracy <- mean(actual_dir == predicted_dir)
  mape <- mean(abs((actual - predicted) / actual)) * 100

  list(
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    directional_accuracy = directional_accuracy,
    mape = mape,
    n_test = length(actual)
  )
}

evaluate_model <- function(model, test_features, test_target, test_close) {
  preds <- tryCatch(
    as.numeric(stats::predict(model, newdata = as.data.frame(test_features))),
    error = function(e) {
      stop_app("Prediction could not be generated.")
    }
  )

  metrics <- calculate_metrics(test_target, preds, test_close)
  metrics$predicted <- preds
  metrics
}

predict_next_day <- function(model, latest_features, current_price) {
  predicted_price <- tryCatch(
    as.numeric(stats::predict(model, newdata = as.data.frame(latest_features)))[[1]],
    error = function(e) {
      stop_app("Prediction could not be generated.")
    }
  )

  if (!is.finite(predicted_price) || !is.finite(current_price) || current_price == 0) {
    stop_app("Prediction could not be generated.")
  }

  expected_change <- predicted_price - current_price
  expected_change_percent <- (expected_change / current_price) * 100
  direction <- prediction_direction(predicted_price, current_price)

  list(
    current_price = current_price,
    predicted_price = predicted_price,
    expected_change = expected_change,
    expected_change_percent = expected_change_percent,
    direction = direction
  )
}

#' End-to-end forecast used by the terminal CLI.
run_prediction_pipeline <- function(symbol, set_progress = function(value, message) invisible(NULL)) {
  symbol <- validate_symbol_input(symbol)

  set_progress(0.12, "Fetching historical data...")
  fetched <- fetch_historical_data(symbol)

  set_progress(0.40, "Preparing data...")
  featured <- create_features(fetched$data)
  prepared <- prepare_model_frames(featured)

  set_progress(0.68, "Training machine learning model...")
  model <- train_model(prepared$train_x, prepared$train_y, ntree = APP_CONFIG$ntree)
  evaluation <- evaluate_model(model, prepared$test_x, prepared$test_y, prepared$test_close)

  set_progress(0.90, "Generating next-day prediction...")
  forecast <- predict_next_day(model, prepared$latest_x, prepared$current_price)

  test_compare <- data.frame(
    Date = prepared$test_dates,
    Actual = prepared$test_y,
    Predicted = evaluation$predicted,
    stringsAsFactors = FALSE
  )

  list(
    error = NULL,
    symbol = symbol,
    latest_date = prepared$latest_date,
    currency = infer_currency(symbol),
    current_price = forecast$current_price,
    predicted_price = forecast$predicted_price,
    expected_change = forecast$expected_change,
    expected_change_percent = forecast$expected_change_percent,
    direction = forecast$direction,
    metrics = list(
      rmse = evaluation$rmse,
      mae = evaluation$mae,
      r_squared = evaluation$r_squared,
      directional_accuracy = evaluation$directional_accuracy,
      mape = evaluation$mape,
      n_test = evaluation$n_test
    ),
    ohlcv = fetched$data,
    featured = featured,
    test_compare = test_compare
  )
}
