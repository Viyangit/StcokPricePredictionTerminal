# AI Stock Price Prediction System

A complete end-to-end **next-day stock closing price** forecasting tool written **entirely in R**. Results print in the **terminal**. There is no web UI.

The user flow is:

1. Enter a stock symbol (or pass it as an argument).
2. The program fetches history from **Alpha Vantage**, trains a Random Forest, and prints the next-day close forecast.

## Problem statement

Predict the **next trading day's closing price** from information available at the end of the latest trading day. This is a supervised regression problem on a financial time series. Random shuffling would leak the future into the past, so every split and every indicator is strictly chronological.

## System architecture

```
USER ENTERS STOCK SYMBOL
        |
        v
FETCH HISTORICAL DATA FROM ALPHA VANTAGE
  TIME_SERIES_DAILY  (full, then compact if needed)
        |
        v
DATA CLEANING AND VALIDATION
        |
        v
FEATURE ENGINEERING  (lags, SMA, RSI, MACD, returns, volatility, next-day target)
        |
        v
CHRONOLOGICAL TRAIN/TEST SPLIT  (oldest 80% / newest 20%)
        |
        v
RANDOM FOREST TRAINING
        |
        v
MODEL EVALUATION ON UNSEEN TEST DATA
        |
        v
NEXT-DAY PRICE PREDICTION FROM THE LATEST FEATURE ROW
        |
        v
TERMINAL REPORT: RESULTS + ASCII CHARTS + METRICS
```

Project layout:

```
StockPrediction/
|
|-- app.R
|
|-- R/
|   |-- data.R
|   |-- features.R
|   |-- model.R
|   |-- terminal.R
|   |-- utils.R
|
|-- README.md
|-- .gitignore
|-- .Renviron.example
```

- `app.R` — terminal CLI entry point.
- `R/data.R` — Alpha Vantage `TIME_SERIES_DAILY` request, JSON parsing, cleaning, validation.
- `R/features.R` — lags, moving averages, RSI, MACD, returns, volatility, next-day target.
- `R/model.R` — chronological split, Random Forest, metrics, next-day prediction.
- `R/terminal.R` — formatted terminal report and ASCII charts.
- `R/utils.R` — input validation, API key handling, formatting, friendly errors.

## Data source

**Only source:** [Alpha Vantage](https://www.alphavantage.co/documentation/) `TIME_SERIES_DAILY`.

Request shape:

```
https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=IBM&outputsize=full&datatype=json&apikey=YOUR_KEY
```

Fields used:

- Date
- Open
- High
- Low
- Close
- Volume

The program requests `outputsize=full` first. On a **free** Alpha Vantage key, full history is a premium feature, so the same API is called again with `outputsize=compact` (~100 most recent trading days).

## API setup

1. Create a free (or premium) key at [Alpha Vantage](https://www.alphavantage.co/support/#api-key).
2. Copy `.Renviron.example` to `.Renviron` in the project root.
3. Replace the placeholder with your key:

```
ALPHA_VANTAGE_API_KEY=YOUR_API_KEY_HERE
```

4. Restart the R session so `Sys.getenv("ALPHA_VANTAGE_API_KEY")` can see the value.

If the key is missing, the program prints:

```
Alpha Vantage API key is not configured.
```

The key is never printed in the terminal, logs from this app, or source files.

## API key configuration

Do **not** hard-code the key. The app reads:

```r
api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
```

`.Renviron` is git-ignored. Only `.Renviron.example` is committed.

## Feature engineering

Features are built in `create_features()` from chronological OHLCV. Indicators use TTR/zoo on the past-and-present window only.

**Price features:** Open, High, Low, Close, Volume

**Lag features:** Previous_Close, Previous_Open, Previous_High, Previous_Low, Previous_Volume

**Moving averages:** MA_5, MA_10, MA_20, MA_50 via `TTR::SMA()`

**Momentum:** RSI via `TTR::RSI()`

**MACD:** MACD line via `TTR::MACD()`

**Returns:** Daily_Return = (Close - Previous_Close) / Previous_Close

**Volatility:** 20-day rolling standard deviation of Daily_Return (`zoo::rollapply`, right-aligned)

**Target:** `Target_Close = dplyr::lead(Close, 1)` — the next trading day's close.

Rows with NA after indicator warm-up are dropped **for training and testing**. The latest row is kept separately for inference; its target is NA by design and is never used as an input.

## Machine learning model

Baseline model: **Random Forest regression** (`randomForest`).

```r
set.seed(42)
model <- randomForest(
  x = training_features,
  y = training_target,
  ntree = 300
)
```

`ntree` and the seed are configurable in `APP_CONFIG` inside `R/utils.R` (default 300 trees, seed 42).

Train/test split: oldest 80% train, newest 20% test. No `sample()`, no random split.

## Model evaluation

Metrics are computed **only on the held-out chronological test set**:

| Metric | Definition |
| --- | --- |
| RMSE | `sqrt(mean((actual - predicted)^2))` |
| MAE | `mean(abs(actual - predicted))` |
| R-squared | `1 - SS_res / SS_tot` |
| Directional accuracy | Share of days where the predicted next close moves in the same UP/DOWN direction as the actual next close, relative to that day's close |

The report never fabricates these numbers.

## Next-day prediction process

1. Take the latest trading day's engineered features (no future values).
2. Predict with the trained forest.
3. `current_price` = latest Close.
4. `predicted_price` = model output.
5. `expected_change` = predicted_price - current_price.
6. `expected_change_percent` = (expected_change / current_price) * 100.
7. Direction: **UP** if predicted > current, **DOWN** if predicted < current, **NEUTRAL** if the percentage move is approximately zero.

## Installation

This project requires [R](https://cran.r-project.org/) 4.1+ (native pipe `|>`) and the packages below.

```r
install.packages(c(
  "httr2",
  "dplyr",
  "TTR",
  "zoo",
  "randomForest"
))
```

## Running the application

From the project root, in a terminal:

```bash
Rscript app.R
```

Then type a symbol (for example `AAPL`) and press Enter. Type `Q` to quit.

One-shot run:

```bash
Rscript app.R AAPL
```

On Windows, if PowerShell says `Rscript` is not recognized, open a **new** terminal, or call R by full path:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" app.R AAPL
```

Set `ALPHA_VANTAGE_API_KEY` in `.Renviron` before starting.

## Example stock symbols

The symbol list is **not** hard-coded. Any ticker supported by Alpha Vantage can be entered.

United States:

- AAPL
- MSFT
- TSLA
- GOOGL
- AMZN
- IBM

India / BSE (Alpha Vantage):

- RELIANCE.BSE
- TCS.BSE
- INFY.BSE

## Limitations

- Free Alpha Vantage keys are rate-limited, and `outputsize=full` is a **premium** capability. Compact responses are typically ~100 days. That is enough to train, but less history than a premium key.
- Random Forest next-day price models are educational. Markets are noisy; a high R-squared on close-to-close levels can still mean weak **directional** skill.
- Predictions are not adjusted for splits/dividends beyond whatever Alpha Vantage returns.
- The program does not place trades, connect to a broker, or provide personalized advice.

## Financial disclaimer

This application provides machine-learning-based forecasts for educational and research purposes only. It is not financial advice, an investment recommendation, or a guarantee of future stock prices. Actual market prices may differ substantially from model predictions.
