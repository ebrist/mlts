#' Create Lag Variables
#'
#' Takes a data frame and adds new columns that are lags of existing columns. 
#'
#' @param data A data frame.
#' @param lags A numeric vector of lags. 
#' @param vars A character vector of column names. 
#' 
#' @details For each column name, the specified lags are added as new columns to the data frame. 
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # create lags of 2 and 4 of mpg
#' mtcars_mpg_lags <- create_lags(mtcars, c(2, 4), "mpg")
#' head(mtcars_mpg_lags)
#' 
create_lags <- function(data, lags, vars) {
  for (p in vars) {
    for (i in 1:length(lags)) {
      lag_values <- c(rep(NA, lags[i]), data[seq_len(length(data[[p]]) - lags[i]) , p])
      data[, paste0(p, "_lag_", lags[i])] <- as.numeric(lag_values)
    }
  }
  data.frame(data)
}
