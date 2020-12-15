#' Plot Simulation Forecasts
#'
#' @param sim Output list from a simulation produced by \link[MLTS]{sim}.
#' @param interactive Option to make the plot interactive. 
#'
#' @return A list of ggplot or ggplotly objects.
#' @export
#' 
plot_sim <- function(sim, interactive = T) {
  # na fill length
  na_length <- length(sim$train_series) - length(sim$xgb$y_fitted)
  n_train <- length(sim$train_series)
  
  dhr_pred = c(rep(NA, n_train - length(sim$dhr$y_fitted)), 
               sim$dhr$y_fitted, sim$dhr$y_hat)
  xgb_pred = c(rep(NA, n_train - length(sim$xgb$y_fitted)), 
               sim$xgb$y_fitted, sim$xgb$y_hat)
  rf_pred = c(rep(NA, n_train - length(sim$rf$y_fitted)), 
               sim$rf$y_fitted, sim$rf$y_hat)
  svm_pred = c(rep(NA, n_train - length(sim$svm$y_fitted)), 
               sim$svm$y_fitted, sim$svm$y_hat)
  # plot data
  plot_data <- tibble::tibble(y = c(sim$train_series, sim$test_series),
                              arima_pred = c(sim$arima$y_fitted, sim$arima$y_hat),
                              dhr_pred, xgb_pred, rf_pred, svm_pred, 
                              set = c(rep("train", length(sim$train_series)),
                                      rep("test", length(sim$test_series))))
  # add time variable
  plot_data$time <- 1:nrow(plot_data)
  # limits for forecast area
  ymax <- max(plot_data$y) * 1.1
  ymin <- min(plot_data$y) * 1.1
  xmin <- length(sim$train_series)
  xmax <- nrow(plot_data) * 1.1
  # base plot
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = y, color = "Simulated Series")) +
    ggplot2::geom_segment(x = xmin, xend = xmin, y = ymin, 
                 yend = ymax, size = 0.5, linetype = "dashed") +
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = 0.2)
  # arima plot
  arima_plot <- g +
    ggplot2::geom_line(data = plot_data, 
                       ggplot2::aes(y = arima_pred, color = "Predictions")) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "Time", y = "Value", title = "ARIMA Predictions") +
    ggplot2::scale_color_manual("", breaks = c("Simulated Series", "ARIMA Predictions"),
                                values = c("#FB9902", "#347B98"))
  # dhr plot
  dhr_plot <- g +
    ggplot2::geom_line(data = plot_data, 
                       ggplot2::aes(y = dhr_pred, color = "Predictions")) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "Time", y = "Value", title = "DHR Predictions") +
    ggplot2::scale_color_manual("", breaks = c("Simulated Series", "DHR Predictions"),
                                values = c("#FB9902", "#347B98"))
  # xgboost plot
  xgb_plot <- g +
    ggplot2::geom_line(data = plot_data, 
                       ggplot2::aes(y = xgb_pred, color = "Predictions")) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "Time", y = "Value", title = "XGBoost Predictions") +
    ggplot2::scale_color_manual("", breaks = c("Simulated Series", "XGBoost Predictions"),
                                values = c("#FB9902", "#347B98"))
  # rf plot
  rf_plot <- g +
    ggplot2::geom_line(data = plot_data, 
                       ggplot2::aes(y = rf_pred, color = "Predictions")) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "Time", y = "Value", title = "RF Predictions") +
    ggplot2::scale_color_manual("", breaks = c("Simulated Series", "RF Predictions"),
                                values = c("#FB9902", "#347B98"))
  # svm plot
  svm_plot <- g +
    ggplot2::geom_line(data = plot_data, 
                       ggplot2::aes(y = svm_pred, color = "Predictions")) +
    ggplot2::theme_bw() + 
    ggplot2::labs(x = "Time", y = "Value", title = "SVM Predictions") +
    ggplot2::scale_color_manual("", breaks = c("Simulated Series", "SVM Predictions"),
                                values = c("#FB9902", "#347B98"))
  # return plots
  if (interactive) {
    return(list(arima_plot = plotly::ggplotly(arima_plot),
                dhr_plot = plotly::ggplotly(dhr_plot), 
                xgb_plot = plotly::ggplotly(xgb_plot),
                rf_plot = plotly::ggplotly(rf_plot),
                svm_plot = plotly::ggplotly(svm_plot)))
  }
  list(arima_plot, dhr_plot, xgb_plot, rf_plot, svm_plot)
}
