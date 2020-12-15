#' Print Results from Multiple Simulations 
#'
#' @param out An output list from multiple simulations produced by \link[MLTS]{run_sims}.
#'
#' @return A tibble of simulation results. 
#' 
#' @export
#' 
print_sims <- function(out) {
  best_model <- character()
  for (i in 1:length(out$n_sim)) {
    rmse <- c(out$arima_rmse[i], out$dhr_rmse[i], out$xgb_rmse[i],
              out$rf_rmse[i], out$svm_rmse[i])
    if (out$arima_rmse[i] == min(rmse)) {
      best_model[i] <- "ARIMA"
    } else if (out$dhr_rmse[i] == min(rmse)) {
      best_model[i] <- "DHR"
    } else if (out$xgb_rmse[i] == min(rmse)) {
      best_model[i] <- "XGB"
    } else if (out$rf_rmse[i] == min(rmse)) {
      best_model[i] <- "RF"
    } else {
      best_model[i] <- "SVM"
    }
  }
  # output table
  t <- tibble::tibble(`Train` = out$n_sim - ceiling(out$n_test), 
                      `Test` = ceiling(out$n_test),
                      `ARIMA RMSE` = out$arima_rmse,
                      `DHR RMSE` = out$dhr_rmse,
                      `XGB RMSE` = out$xgb_rmse,
                      `RF RMSE` = out$rf_rmse,
                      `SVM RMSE` = out$svm_rmse,
                      `Best Model` = best_model)
  t
}