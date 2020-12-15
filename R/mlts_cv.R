#' Machine Learning for Time Series with Cross-Validation
#' 
#' This function fits a machine learning model to time series data
#'  and evaluates predictive performance using cross-validation. 
#'
#' @param y A time series object (ts or msts).
#' @param method The machine learning method. Options are one of ("xgb", "rf", "svm").
#' @param h The number of periods for forecasting. If multiple values are provided, a rolling forecast origin is utilized. 
#' @param lags lags of the response to be used as predictors.
#' @param x A data frame of external features. The number of rows must match the length of y. 
#' @param x_include A vector of column names of x to include as features in the model. 
#'   By default, all columns of x are included as features in the model.
#' @param x_include_lags A vector of column names of x where lags of these columns are added as features in the model. 
#'   By default, only lags of the series are added as features when the lag argument is utilized. 
#'   This provides the option to also include lags of the external features.    
#' @param x_include_dummy A vector of column names of x where these columns are transformed to a set of dummy features. 
#' @param fourier_k The number of fourier terms to add as features. 
#'   This vector must have the same length as the number of seasonal periods of y, 
#'   and each value must not exceed half of the size of the seasonal period. 
#' @param auto_season_dummy If true, dummy variables for the seasonal periods are added as features. 
#' @param params Optional list of hyperparameter values for the selected machine learning method.
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
#'  importance - variable importance if method = "xgb" or method - "rf". \cr
#'  rt - the total runtime. \cr
#'  params - the hyperparameters. \cr 
#'  
#' @export
#' 
mlts_cv <- function(y, method = "xgb", h, lags = max(h),
                    x = NULL, x_include = names(x), 
                    x_include_lags = NULL, x_include_dummy = NULL,
                    fourier_k = NULL, auto_season = F, params = NULL,
                    roll_update = F, seed = 1, verbose = F, out_tbl_width = 140) {
  st <- Sys.time()
  set.seed(seed)
  # output table width
  pander::panderOptions("table.split.table", out_tbl_width)
  # check if y is a time series object
  if (!is.ts(y)) {
    stop("y must be a time series object")
  }
  if (!method %in% c("xgb", "rf", "svm")) {
    stop("method must be one of (\"xgb\", \"rf\", \"svm\")")
  }
  # seasonal periods from msts
  periods <- attr(y, "msts")
  # seasonal periods from ts
  if (is.null(periods)) {
    periods <- stats::frequency(y)
  }
  # check lags
  if (min(lags) < max(h)) {
    warning("Response lag smaller than the forecast horizon.")
  }
  # coerce params to a list 
  params <- as.list(params)
  # print start message 
  if (verbose) {
    pander::pander("\n")
    pander::pander(paste0("Fitting ", method, "\n"))
    if (length(params) == 0) {
      pander::pander(paste0("  Using all default hyperparameters... \n"))
    } else {
      pander::pander(as.data.frame(params))
    }
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
  # add seasonal dummy predictors
  if (auto_season) {
    # seasonal factor variables
    for (i in 1:length(periods)) {
      data[[paste0("sp_", periods[i], "_")]] <- rep(1:periods[i], length.out = nrow(data))
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
  # check rows
  if (ncol(data) <= 1) {
    stop(paste0("No predictors available for training. ",
                "Add predictors to x or include lags of the response."))
  }
  # n_train
  n_train <- nrow(data) - sum(h)
  # check train length
  if (n_train < 2) {
    stop(paste0("Not enough training data."))
  } else if (n_train <= 30) {
    warning(paste0("Small training set: ", n_train, (" obs.")))
  }
  # empty mod
  mod <- NULL
  # rolling forecasts
  for (i in 1:(length(h))) {
    if (verbose) {
      pander::pander(paste0("Forecasting time ", n_train + max(lags) + 1, " to ", 
                             n_train + max(lags) + h[i], "\n"))
    }
    # train data
    x_train <- as.matrix(data[1:n_train, !names(data) %in% c("y"), drop = F])
    # train response
    y_train <- as.numeric(data[1:n_train, "y"])
    # test data
    x_test <- as.matrix(data[(1 + n_train):(n_train + h[i]), 
                             !names(data) %in% c("y"), drop = F])
    # fit model
    if (method == "xgb") {
      # xgboost data
      xgb_train <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
      xgb_test <- xgboost::xgb.DMatrix(data = x_test)
      # fit model
      if (roll_update | is.null(mod)) {
        set.seed(seed)
        mod <- xgboost::xgb.train(params, data = xgb_train, 
                                  evaluation = "rmse", 
                                  nrounds = ifelse(is.null(params$nrounds),
                                                   100, params$nrounds))
      }
      # predictions
      y_hat <- c(y_hat, predict(mod, xgb_test))
      importance <- xgboost::xgb.importance(model = mod)
    } else if (method == "rf") {
      if (roll_update | is.null(mod)) {
        # fit RF
        set.seed(seed)
        mod <- do.call(randomForest::randomForest, 
                       c(list(x = x_train, y = y_train), params))
      }
      # predictions
      y_hat <- c(y_hat, predict(mod, x_test))
      importance <- randomForest::importance(mod)
    } else if (method == "svm") {
      if (roll_update | is.null(mod)) {
        # fit SVM
        set.seed(seed)
        mod <- do.call(e1071::svm, c(list(x = x_train, y = y_train), params))
      }
      # predictions
      y_hat <- c(y_hat, predict(mod, x_test))
      importance <- NULL
    }
    # fitted values
    if (i == 1) {
      y_fitted <- predict(mod, x_train)
      # train metrics
      train_rmse <- sqrt(mean((y_train - y_fitted)^2))
      train_mape <- mean(abs((y_train - y_fitted) / y_train)) * 100
    }
    # update training length
    n_train <- n_train + h[i]
  }
  # observed response (test)
  y_test <- tail(as.numeric(y), sum(h))
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
       mod = mod, importance = importance, rt = rt, params = unlist(params))
}

