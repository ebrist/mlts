#' Run Multiple Forecast Simulations
#' 
#' Generates multiple simulations by making repeated calls to \link[MLTS]{sim}. 
#'
#' @param n_obs Length of the simulated time series. 
#' @param n_rep Number of replicate time series. 
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
run_sims <- function(n_obs, n_rep, n_test, periods, arima_period,
                     orders, ar, ma, k = NULL, lags = NULL, 
                     xgb_params, rf_params, svm_params, ml_k = k, auto_season = F,
                     xreg_corr = NULL, burn = 100, method = "ML", seed = 1234) {
  seeds <- seed + 0:(n_rep - 1)
  # output list
  out <- list()
  n_sims <- length(n_obs)
  out$n_test <- n_test
  out$n_rep <- n_rep
  out$n_sim <- n_obs
  out$freq <- periods
  out$arima_rmse <- out$dhr_rmse <- out$xgb_rmse <- 
    out$rf_rmse <- out$svm_rmse <- numeric(n_sims)
  out$runtime_arima <- out$runtime_dhr <- out$runtime_xgb <- 
    out$runtime_rf <- out$runtime_svm <- numeric(n_sims)
  # run sims
  for (i in 1:n_sims) {
    arima_rmse <- arima_rt <- dhr_rmse <- dhr_rt <- xgb_rmse <- xgb_rt <- 
      rf_rmse <- rf_rt <- svm_rmse <- svm_rt <- numeric(n_rep)
    for (j in 1:n_rep) {
      # run reps
      pander::pander(paste0("Running Simulation ", i, " of ", n_sims, 
                            "; Replication ", j, " of ", n_rep, ". \n"))
      simulation <- sim(n_obs[i], n_test[i], periods, arima_period,
                        orders, ar, ma, k, lags, xgb_params, rf_params, svm_params, 
                        ml_k, auto_season, xreg_corr, method, burn, seeds[j])
      arima_rmse[j] <- simulation$arima$test_rmse; arima_rt[j] <- simulation$arima$rt
      dhr_rmse[j] <- simulation$dhr$test_rmse; dhr_rt[j] <- simulation$dhr$rt
      xgb_rmse[j] <- simulation$xgb$test_rmse; xgb_rt[j] <- simulation$xgb$rt
      rf_rmse[j] <- simulation$rf$test_rmse; rf_rt[j] <- simulation$rf$rt
      svm_rmse[j] <- simulation$svm$test_rmse; svm_rt[j] <- simulation$svm$rt
    }
    # runtimes
    runtimes <- round(c(ARIMA = sum(arima_rt), DHR = sum(dhr_rt), XGB = sum(xgb_rt),
                      RF = sum(rf_rt), SVM = sum(svm_rt)), 4)
    pander::pander(paste("Simulation ", i, " of ", n_sims, " complete. Total Runtimes:"))
    pander::pander(runtimes)
    # store output
    out$arima_rmse[i] <- mean(arima_rmse)
    out$dhr_rmse[i] <- mean(dhr_rmse)
    out$xgb_rmse[i] <- mean(xgb_rmse)
    out$rf_rmse[i] <- mean(rf_rmse)
    out$svm_rmse[i] <- mean(svm_rmse)
    out$runtime_arima[i] <- runtimes[[1]]
    out$runtime_dhr[i] <- runtimes[[2]]
    out$runtime_xgb[i] <- runtimes[[3]]
    out$runtime_rf[i] <- runtimes[[4]]
    out$runtime_svm[i] <- runtimes[[5]]
  }
  # return output list
  out
}