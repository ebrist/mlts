#' Dynamic Harmonic Regression for Time Series with Cross-Validation
#'
#' This function fits a dynamic harmonic regression model to time series data
#'  and evaluates predictive performance using cross-validation. 
#'
#' @param y A time series object (ts or msts).
#' @param h The number of periods for forecasting. If multiple values are provided, a rolling forecast origin is utilized. 
#' @param lags Lags of the series. If provided, these will be added as regressors in the model. 
#' @param x A data frame of external regressors. The number of rows must match the length of y. 
#' @param x_include A vector of column names of x to include as regressors in the model. 
#'   By default, all columns of x are included as regressors in the model.
#' @param x_include_lags A vector of column names of x where lags of these columns are added as regressors in the model. 
#'   By default, only lags of the series are added as regressors when the lag argument is utilized. 
#'   This provides the option to also include lags of the external regressors.    
#' @param x_include_dummy A vector of column names of x where these columns are transformed to a set of dummy regressors. 
#' @param order See order in \link[forecast]{Arima}
#' @param s_order See seasonal in \link[forecast]{Arima}
#' @param s_period The seasonal frequency.
#' @param fourier_k The number of fourier terms to add as regressors. 
#'   This vector must have the same length as the number of seasonal periods of y, 
#'   and each value must not exceed half of the size of the seasonal period. 
#' @param auto_season_dummy If true, dummy variables for the seasonal periods are added as regressors. 
#' @param drift See include.drift in \link[forecast]{Arima}
#' @param constant See include.constant in \link[forecast]{Arima}
#' @param mean See include.mean in \link[forecast]{Arima}
#' @param roll_update Option to update the model when the forecast origin is rolled forward.
#' @param seed Option to specify the seed.
#' @param verbose Option to print info to console.
#' @param out_tbl_width Option to set the info table width when verbose = T.
#'
#' @return A list. See the following details: \cr
#'  y - the original time series. \cr
#'  x_train - the subset of regressors used in training. \cr
#'  y_train - the subset of y used in training. \cr
#'  x_test - the subset of regressors used in testing. \cr
#'  y_test - the subset of y used in testing. \cr
#'  y_fitted - the fitted values (from the training stage). \cr
#'  y_hat - the predicted values (from the testing stage). \cr
#'  train_rmse - the training root mean squared error. \cr
#'  train_mape - the training mean absolute percentage error. \cr
#'  test_rmse - the testing (cross-validated) root mean squared error. \cr
#'  test_mape - the testing (cross-validated) mean absolute percentage error. \cr
#'  mod - the final model (an Arima object). \cr
#'  rt - the total runtime. \cr
#'   
#' @export
#' 
dhr_cv <- function(y, h, lags = NULL, x = NULL, x_include = names(x), 
                   x_include_lags = NULL, x_include_dummy = NULL, order = c(0, 0, 0),
                   s_order = c(0, 0, 0), s_period = 1, fourier_k = NULL, 
                   auto_season_dummy = F, drift = T, constant = T, mean = T,
                   roll_update = F, method = "ML", 
                   seed = 1, verbose = F, out_tbl_width = 140) {
  st <- Sys.time()
  # starting message
  if (verbose) {
    pander::pander("\n")
    pander::pander(paste0("Fitting DHR with ARIMA(", order[1], ", ", 
                          order[2], ", ", order[3], ") errors \n"))
  }
  # empty vectors 
  y_hat <- numeric()
  # initial data
  data <- data.frame(y = as.numeric(y))
  # add x if not null 
  if (!is.null(x)) {
    if (nrow(x) != length(y)) {
      stop(paste0("nrow(x) = ", nrow(x), " must match length(y) = ", length(y)))
    }
    # add variables
    data <- dplyr::bind_cols(data, dplyr::select(x, c(x_include, 
                                                      x_include_dummy, 
                                                      x_include_lags)))
    # add dummy variables
    if (!is.null(x_include_dummy)) {
      data <- create_dummies(data, x_include_dummy)
    }
    # add x lags
    if (any(lags > 0) & !is.null(x_include_lags)) {
      data <- create_lags(data, lags, x_include_lags)
      # remove lag only predictors
      x_rm <- x_include_lags[!x_include_lags %in% x_include]
      data <- dplyr::select(data, -x_rm)
    }
  }
  # seasonal periods from msts
  periods <- attr(y, "msts")
  # seasonal periods from ts
  if (is.null(periods)) {
    periods <- stats::frequency(y)
  }
  # add seasonal dummy predictors
  if (auto_season_dummy) {
    # seasonal factor variables
    for (i in 1:length(periods)) {
      data[[paste0("sp_", periods[i], "_")]] <- rep(1:periods[i], 
                                                    length.out = nrow(data))
    }
    # seasonal dummy variables
    dummy_vars <- names(dplyr::select(data, dplyr::starts_with("sp_")))
    data <- create_dummies(data, dummy_vars)
  }
  # add fourier predictors
  if (!is.null(fourier_k)) {
    fourier_data <- data.frame(forecast::fourier(y, K = fourier_k))
    data <- dplyr::bind_cols(data, fourier_data)
  }
  # add lag response predictors
  if (any(lags > 0)) {
    data <- create_lags(data, lags, "y")
  }
  # remove NA's
  if (!is.null(lags)) {
    data <- data[-c(1:max(lags)), ]
  }
  # n_train
  n_train <- nrow(data) - sum(h)
  # check train length
  if (n_train <= 30) {
    warning(paste0("Small training set: ", n_train, (" obs.")))
  }
  # empty mod
  mod <- NULL
  # rolling forecasts
  for (i in 1:(length(h))) {
    if (verbose) {
      pander::pander(paste0("Forecasting time ", n_train + 1, " to ", n_train + h[i], "\n"))
    }
    # train data
    x_train <- as.matrix(data[1:n_train, !names(data) %in% c("y"), drop = F])
    # train response
    y_train <- ts(as.numeric(data[1:n_train, "y"]), frequency = s_period)
    # test data
    x_test <- as.matrix(data[(1 + n_train):(n_train + h[i]), 
                             !names(data) %in% c("y"), drop = F])
    # model
    if (is.null(mod)) {
      if (ncol(x_train) == 0) {
        mod <- forecast::Arima(y_train, order = order, seasonal = s_order,
                               include.mean = mean, include.drift = drift, 
                               include.constant = constant, method = method)
        y_hat <- c(y_hat, unname(forecast::forecast(mod, h = h[i])$mean))
      } else {
        mod <- forecast::Arima(y_train, order = order, xreg = x_train, seasonal = s_order, 
                               include.mean = mean, include.drift = drift, 
                               include.constant = constant, method = method)
        y_hat <- c(y_hat, unname(forecast::forecast(mod, h = h[i], xreg = x_test)$mean))
      }
    } else {
      if (ncol(x_train) == 0) {
        mod <- forecast::Arima(y_train, order = order, seasonal = s_order,
                               include.mean = mean, include.drift = drift, 
                               include.constant = constant, method = method,
                               model = if (roll_update) {NULL} else {mod})
        y_hat <- c(y_hat, unname(forecast::forecast(mod, h = h[i])$mean))
      } else {
        mod <- forecast::Arima(y_train, order = order, xreg = x_train, seasonal = s_order, 
                               include.mean = mean, include.drift = drift, 
                               include.constant = constant, method = method,
                               model = if (roll_update) {NULL} else {mod})
        y_hat <- c(y_hat, unname(forecast::forecast(mod, h = h[i], xreg = x_test)$mean))
      }
    }
    # update training length
    n_train <- n_train + h[i]
  }
  # observed response (test)
  y_test <- tail(data$y, sum(h))
  y_train <- head(data$y, nrow(data) - sum(h))
  y_fitted <- mod$fitted[1:length(y_train)]
  # train metrics
  train_rmse <- sqrt(mean((y_train - y_fitted)^2))
  train_mape <- mean(abs((y_train - y_fitted) / y_train)) * 100
  # test metrics
  test_rmse <- sqrt(mean((y_test - y_hat)^2))
  test_mape <- mean(abs((y_test - y_hat) / y_test)) * 100
  # print
  if (verbose) {
    pander::pander(paste("Mean of Test Series:", round(mean(y_test), 6), "\n"))
    pander::pander(paste("Test RMSE:", round(test_rmse, 6), "\n"))
    if (all(y_test != 0)) {
      pander::pander(paste("Test MAPE:", round(test_mape, 6), "%\n"))
    }
  }
  # runtime
  rt <- difftime(Sys.time(), st, units = "mins")
  # output list
  list(y = y, x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,
       y_fitted = y_fitted, y_hat = y_hat, 
       train_rmse = train_rmse, train_mape = train_mape,
       test_rmse = test_rmse, test_mape = test_mape, 
       mod = mod, rt = rt)
}
