
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Impute missing values in a dataset
#'
#' This function replaces missing values in a dataset with appropriate imputed values.
#'
#' @param data A data frame containing missing values.
#' @param method The imputation method to be used (e.g., "mean", "median", "knn").
#' @return A data frame with missing values imputed.
#' @examples
#' data <- data.frame(x = c(1, 2, NA, 4, 5), y = c(NA, 2, 3, 4, NA))
#' imputed_data <- impute_missing(data, method = "mean")
#' imputed_data
impute_missing <- function(data, method) {
  if (method == "mean") {
    imputed_data <- data
    for (col in names(data)) {
      imputed_data[is.na(imputed_data[, col]), col] <- mean(data[, col], na.rm = TRUE)
    }
    return(imputed_data)
  } else if (method == "median") {
    imputed_data <- data
    for (col in names(data)) {
      imputed_data[is.na(imputed_data[, col]), col] <- median(data[, col], na.rm = TRUE)
    }
    return(imputed_data)
  } else {
    stop("Invalid imputation method. Supported methods: 'mean', 'median'")
  }
}


# Package 1 (Adrian Brenner)
