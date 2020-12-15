#' Create Dummy Variables
#' 
#' Creates a set of dummy variables from specified columns in a data frame. 
#'
#' @param data A data frame.
#' @param vars A character vector of column names. These columns will be transformed to 
#'   a set of dummy variables. Both the original column and the first dummy variable are dropped. 
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # transform the cyl column to a set of dummy variables
#' mtcars_cyl_dummies <- create_dummies(mtcars, "cyl")
#' head(mtcars_cyl_dummies)
#' 
create_dummies <- function(data, vars) {
  # loop over each factor
  for (p in vars) {
    # coerce p to a factor 
    data[[p]] <- as.factor(data[[p]])
    # use model.matrix to create dummy variables and remove intercept column
    new_cols <- data.frame(data.frame(model.matrix( ~ data[[p]]))[, -1])
    # rename the columns using the levels of p
    names(new_cols) <- paste0(p, levels(data[[p]])[-1])
    # add new columns and remove old column
    data <- dplyr::select(dplyr::bind_cols(data, new_cols), -p)
  }
  # return data
  data
}
