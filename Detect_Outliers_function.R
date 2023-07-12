#' Detect outliers in a numeric vector
#'
#' This function identifies outliers in a numeric vector based on a specified threshold.
#'
#' @param vector A numeric vector.
#' @param threshold The threshold value to determine outliers.
#' @return A logical vector indicating the presence of outliers.
#' @examples
#' vector <- c(1, 2, 3, 10, 100, 4, 5)
#' outliers <- detect_outliers(vector, threshold = 2)
#' outliers
detect_outliers <- function(vector, threshold) {
  lower_bound <- median(vector) - threshold * IQR(vector)
  upper_bound <- median(vector) + threshold * IQR(vector)
  outliers <- vector < lower_bound | vector > upper_bound
  return(outliers)
}

# Package 2 (Adrian Brenner)
