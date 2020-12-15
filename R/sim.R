#' Run a Forecast Simulation 
#'
#' Simulates time series data from a specified ARIMA/SARIMA model, fits various models to the simulated series, and then evaluates predictive performance. 
#'
#' @param n_obs Length of the simulated time series. 
#' @param n_test Length of the simulated time series held out as a test set. 
#' @param periods Seasonal periods for the simualted time series. 
#' @param arima_period Seasonal period used when fitting the ARIMA model. 
#' @param orders See orders in \link[smooth]{ssarima}.
#' @param ar See AR in \link[smooth]{ssarima}.
#' @param ma See MA in \link[smooth]{ssarima}.
#' @param k Number of fourier terms included as predictors. 
#' @param lags Lags of the simulated time series included as regressors in the ARIMA and DHR models. 
#' @param xgb_params List of hyperparameters for the xgboost model.
#' @param rf_params List of hyperparameters for the randomForest model.
#' @param svm_params List of hyperparameters for the svm model.
#' @param ml_k Number of fourier terms included as features in the machine learning models. 
#' @param xreg_corr Option to add covariates with a specified correlation with the series.   
#' @param burn Option to use a burn-in when simulated the time series data.
#' @param seed Option to set the seed. 
#'
#' @return List of root mean squared errors and runtimes for each method. 
#' @export
#'
sim <- function(n_obs, n_test, periods, arima_period, 
                orders, ar, ma, k = NULL, lags = NULL, 
                xgb_params, rf_params, svm_params, ml_k = k, auto_season = F,
                xreg_corr = NULL, method = "ML", burn = 100, seed = 1234) {
  # setup ----
  starttime <- Sys.time()
  set.seed(seed)
  n_train <- n_obs - n_test
  if (!is.list(orders)) {
    stop("orders must be a list: orders = list(ar = c(...), i = c(...), ma = c(...))")
  }
  # simulate data ----
  set.seed(seed)
  sim_series <- smooth::sim.ssarima(orders = orders, lags = periods, obs = n_obs + burn,
                                    nsim = 1, frequency = 1, 
                                    AR = ar, MA = ma, constant = F)
  # reset periods 
  if (length(periods) > 1 & periods[1] == 1) {
    periods <- periods[-1]
  }
  if (!is.null(k)) {
    k <- ifelse(k > periods / 2, floor(periods / 2), k)
  }
  if (!is.null(ml_k)) {
    ml_k <- ifelse(ml_k > periods / 2, floor(periods / 2), ml_k)
  }
  # pull out data
  sim_series <- as.numeric(sim_series$data[-c(1:burn)])
  sim_series <- forecast::msts(sim_series, seasonal.periods = periods)
  # sets
  sim_series_train <- subset(sim_series, end = n_train)
  sim_series_test <- subset(sim_series, start = n_train + 1)
  # create data frame of series
  data <- data.frame(y = as.numeric(sim_series))
  # add xreg
  if (!is.null(xreg_corr)) {
    set.seed(seed)
    for (i in 1:length(xreg_corr)) {
      x <- rnorm(n_obs, mean(sim_series), sd(sim_series))
      x <- xreg_corr[i] * sim_series + sqrt(1 - xreg_corr[i]) * x
      data[[paste0("x_", i)]] <- as.numeric(x)
    } 
    predictors <- names(dplyr::select(data, dplyr::starts_with("x")))
  } else {
    predictors <- NULL
  }
  # orders
  arima_order <- c(orders$ar[1], orders$i[1], orders$ma[1])
  # seasonal orders
  if (arima_period > 1) {
    # seasonal period index
    index <- match(arima_period, periods)
    arima_sorder <- c(orders$ar[index], orders$i[index], orders$ma[index])
  } else {
    arima_sorder <- c(0, 0, 0)
  }
  # arima model
  arima_model <- dhr_cv(sim_series, n_test, lags = NULL, data, x_include = predictors,
                        order = arima_order, s_order = arima_sorder, s_period = arima_period, 
                        fourier_k = NULL, method = method, seed = seed)
  # dhr model
  dhr_model <- dhr_cv(sim_series, n_test, lags = NULL, data, x_include = predictors,
                      order = arima_order, fourier_k = k, method = method, seed = seed)
  # xgboost model
  xgb_model <- mlts_cv(sim_series, "xgb", n_test, lags, data, predictors, 
                       fourier_k = ml_k, auto_season = auto_season, 
                       params = xgb_params, seed = seed) 
  # random forest model
  rf_model <- mlts_cv(sim_series, "rf", n_test, lags, data, predictors, 
                      fourier_k = ml_k, auto_season = auto_season, 
                      params = rf_params, seed = seed) 
  # svm model
  svm_model <- mlts_cv(sim_series, "svm", n_test, lags, data, predictors, 
                       fourier_k = ml_k, auto_season = auto_season, 
                       params = svm_params, seed = seed) 
  # naive method
  naive_lag <- ifelse(max(periods) < n_test, n_test, max(periods))
  naive_obs <- as.numeric(sim_series)
  naive_train_obs <- head(naive_obs, n_train)
  naive_test_obs <- tail(naive_obs, n_test)
  naive_pred <- c(rep(NA, naive_lag), naive_obs[seq_len(length(sim_series) - naive_lag)])
  naive_fitted <- head(naive_pred, n_train)
  naive_pred <- tail(naive_pred, n_test) 
  naive_rmse <- sqrt(mean((naive_test_obs - naive_pred)^2))
  naive_mape <- mean(abs((naive_test_obs - naive_pred) / naive_test_obs)) * 100
  
  # output list ----
  out <- list()
  out$freq <- periods
  # series
  out$sim_series <- sim_series
  out$test_series <- sim_series_test
  out$train_series <- sim_series_train
  # models
  out$arima <- arima_model
  out$dhr <- dhr_model
  out$xgb <- xgb_model
  out$rf <- rf_model
  out$svm <- svm_model
  # naive
  out$naive_pred <- naive_pred
  out$naive_fitted <- naive_fitted
  out$naive_rmse <- naive_rmse
  out$naive_mape <- naive_mape
  # n_train
  out$n_train <- n_train
  out$n_sim <- n_obs
  runtime <- difftime(Sys.time(), starttime, units = "mins")
  out$runtime <- runtime
  # return output list
  out
}
